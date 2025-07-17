

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"
#include "Tolerator.h"


using namespace llvm;
using tolerator::Tolerator;


namespace tolerator {

char Tolerator::ID = 0;

}


bool
Tolerator::runOnModule(Module& m) {
  LLVMContext &Ctx = m.getContext();
  IRBuilder<> Builder(Ctx);

  FunctionCallee H_read = m.getOrInsertFunction(
        "handle_invalid_read",
        FunctionType::get(Builder.getVoidTy(), false)
    );

  FunctionCallee H_write = m.getOrInsertFunction(
        "handle_invalid_write",
        FunctionType::get(Builder.getVoidTy(), false)
    );

  FunctionCallee H_free = m.getOrInsertFunction(
        "handle_invalid_free",
        FunctionType::get(Builder.getVoidTy(), false)
    );

  FunctionCallee H_div0 = m.getOrInsertFunction(
        "handle_division_by_zero",
        FunctionType::get(Builder.getVoidTy(), false)
    );

  for (Function &F : m) {
        if (F.isDeclaration()) continue;
        SmallVector<Instruction*, 32> WorkList;
        for (BasicBlock &BB : F)
            for (Instruction &I : BB)
                if (isa<LoadInst>(&I)
                 || isa<StoreInst>(&I)
                 || (isa<CallInst>(&I)
                     && cast<CallInst>(&I)->getCalledFunction()
                     && cast<CallInst>(&I)->getCalledFunction()->getName() == "free")
                 || (isa<BinaryOperator>(&I)
                     && (cast<BinaryOperator>(&I)->getOpcode() == Instruction::SDiv
                      || cast<BinaryOperator>(&I)->getOpcode() == Instruction::UDiv
                      || cast<BinaryOperator>(&I)->getOpcode() == Instruction::SRem
                      || cast<BinaryOperator>(&I)->getOpcode() == Instruction::URem)))
                    WorkList.push_back(&I);

        for (Instruction *I : WorkList) {
            BasicBlock *OrigBB = I->getParent();
            BasicBlock *ContBB = OrigBB->splitBasicBlock(I, OrigBB->getName() + ".cont");
            OrigBB->getTerminator()->eraseFromParent();
            BasicBlock *ErrBB = BasicBlock::Create(Ctx, OrigBB->getName() + ".err", OrigBB->getParent(), ContBB);

            Builder.SetInsertPoint(ErrBB);
            if (dyn_cast<LoadInst>(I))
                Builder.CreateCall(H_read);
            else if (dyn_cast<StoreInst>(I))
                Builder.CreateCall(H_write);
            else if (CallInst *CI = dyn_cast<CallInst>(I))
                if (CI->getCalledFunction() && CI->getCalledFunction()->getName() == "free")
                    Builder.CreateCall(H_free);
                else
                    Builder.CreateCall(H_div0);
            else
                Builder.CreateCall(H_div0);
            Builder.CreateUnreachable();

            Builder.SetInsertPoint(OrigBB);
            Value *Cond = nullptr;
            if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
                Cond = Builder.CreateICmpEQ(
                    LI->getPointerOperand(),
                    Constant::getNullValue(LI->getPointerOperand()->getType())
                );
            } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
                Cond = Builder.CreateICmpEQ(
                    SI->getPointerOperand(),
                    Constant::getNullValue(SI->getPointerOperand()->getType())
                );
            } else if (CallInst *CI = dyn_cast<CallInst>(I)) {
                Value *Ptr = CI->getArgOperand(0);
                Cond = Builder.CreateICmpNE(
                    Ptr,
                    Constant::getNullValue(Ptr->getType())
                );
            } else {
                BinaryOperator *BOp = cast<BinaryOperator>(I);
                Value *Divisor = BOp->getOperand(1);
                Cond = Builder.CreateICmpEQ(
                    Divisor,
                    ConstantInt::get(Divisor->getType(), 0)
                );
            }
            Builder.CreateCondBr(Cond, ErrBB, ContBB);
        }
    }

  return true;
}



