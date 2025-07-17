

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
    for (BasicBlock &BB : F) {
      SmallVector<Instruction*, 8> WorkList;
      for (Instruction &I : BB) {
        if (isa<LoadInst>(&I) ||
            isa<StoreInst>(&I) ||
            (auto *CI = dyn_cast<CallInst>(&I) &&
             CI->getCalledFunction() &&
             CI->getCalledFunction()->getName() == "free") ||
            isa<BinaryOperator>(&I) &&
              (I.getOpcode() == Instruction::SDiv ||
               I.getOpcode() == Instruction::UDiv ||
               I.getOpcode() == Instruction::FDiv)) {
          WorkList.push_back(&I);
        }
      }
      for (Instruction *I : reverse(WorkList)) {
        BasicBlock *OrigBB = I->getParent();
        BasicBlock *ContBB = OrigBB->splitBasicBlock(I, OrigBB->getName() + ".cont");
        OrigBB->getTerminator()->eraseFromParent();
        BasicBlock *ErrBB = BasicBlock::Create(Ctx, OrigBB->getName() + ".err", OrigBB->getParent(), ContBB);

        IRBuilder<> ErrB(ErrBB);
        if (isa<LoadInst>(I))
          ErrB.CreateCall(H_read);
        else if (isa<StoreInst>(I))
          ErrB.CreateCall(H_write);
        else if (auto *CI = dyn_cast<CallInst>(I))
          ErrB.CreateCall(H_free);
        else
          ErrB.CreateCall(H_div0);
        ErrB.CreateUnreachable();

        IRBuilder<> CondB(OrigBB);
        Value *Cond = nullptr;
        if (auto *LI = dyn_cast<LoadInst>(I)) {
          Cond = CondB.CreateICmpEQ(
            LI->getPointerOperand(),
            Constant::getNullValue(LI->getPointerOperand()->getType()));
        } else if (auto *SI = dyn_cast<StoreInst>(I)) {
          Cond = CondB.CreateICmpEQ(
            SI->getPointerOperand(),
            Constant::getNullValue(SI->getPointerOperand()->getType()));
        } else if (auto *CI = dyn_cast<CallInst>(I)) {
          Value *Ptr = CI->getArgOperand(0);
          Cond = CondB.CreateICmpEQ(
            Ptr,
            Constant::getNullValue(Ptr->getType()));
        } else {
          auto *BOp = cast<BinaryOperator>(I);
          Value *Divisor = BOp->getOperand(1);
          if (BOp->getOpcode() == Instruction::FDiv)
            Cond = CondB.CreateFCmpUEQ(
              Divisor,
              ConstantFP::getZeroValueForNegation(Divisor->getType()));
          else
            Cond = CondB.CreateICmpEQ(
              Divisor,
              ConstantInt::get(Divisor->getType(), 0));
        }
        CondB.CreateCondBr(Cond, ErrBB, ContBB);
      }
    }
  }

  return true;
}

static RegisterPass<Tolerator> X("tolerator", "Tolerator Pass", false, false);