

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
  auto& context = m.getContext();
  Type *voidTy = Type::getVoidTy(Ctx);

  // This analysis just prints a message when the program starts or exits.
  // You should modify this code as you see fit.
  auto* voidTy = Type::getVoidTy(context);

  auto helloworld = m.getOrInsertFunction("ToLeRaToR_helloworld", voidTy);
  appendToGlobalCtors(m, llvm::cast<Function>(helloworld.getCallee()), 0);

  auto goodbyeworld = m.getOrInsertFunction("ToLeRaToR_goodbyeworld", voidTy);
  appendToGlobalDtors(m, llvm::cast<Function>(goodbyeworld.getCallee()), 0);

  // Task 1
    auto *voidTy = Builder.getVoidTy();

    FunctionCallee H_read  = M.getOrInsertFunction("handle_invalid_read",
                                FunctionType::get(voidTy, false));
    FunctionCallee H_write = M.getOrInsertFunction("handle_invalid_write",
                                FunctionType::get(voidTy, false));
    FunctionCallee H_free  = M.getOrInsertFunction("handle_invalid_free",
                                FunctionType::get(voidTy, false));
    FunctionCallee H_div0  = M.getOrInsertFunction("handle_division_by_zero",
                                FunctionType::get(voidTy, false));

    for (Function &F : M) {
      if (F.isDeclaration())
        continue;

      SmallVector<Instruction*, 32> WorkList;
      for (BasicBlock &BB : F)
        for (Instruction &I : BB)
          if (isa<LoadInst>(&I) ||
              isa<StoreInst>(&I) ||
              (isa<CallInst>(&I) &&
               cast<CallInst>(&I)->getCalledFunction() &&
               cast<CallInst>(&I)->getCalledFunction()->getName() == "free") ||
              (auto *BOp = dyn_cast<BinaryOperator>(&I)) &&
                (BOp->getOpcode() == Instruction::SDiv ||
                 BOp->getOpcode() == Instruction::UDiv ||
                 BOp->getOpcode() == Instruction::SRem ||
                 BOp->getOpcode() == Instruction::URem))
          {
            WorkList.push_back(&I);
          }

      for (Instruction *I : WorkList) {
        BasicBlock *OrigBB = I->getParent();
        BasicBlock *ContBB = OrigBB->splitBasicBlock(I, OrigBB->getName() + ".cont");
        OrigBB->getTerminator()->eraseFromParent();
        BasicBlock *ErrBB = BasicBlock::Create(Ctx, OrigBB->getName() + ".err", &F, ContBB);

        Builder.SetInsertPoint(ErrBB);
        if (isa<LoadInst>(I)) {
          Builder.CreateCall(H_read, {});
        } else if (isa<StoreInst>(I)) {
          Builder.CreateCall(H_write, {});
        } else if (CallInst *CI = dyn_cast<CallInst>(I)) {
          if (CI->getCalledFunction()->getName() == "free")
            Builder.CreateCall(H_free, {});
        } else {
          Builder.CreateCall(H_div0, {});
        }
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
          Value *Div = cast<BinaryOperator>(I)->getOperand(1);
          Cond = Builder.CreateICmpEQ(
            Div,
            ConstantInt::get(Div->getType(), 0)
          );
        }
        Builder.CreateCondBr(Cond, ErrBB, ContBB);
      }
    }
                                

}



