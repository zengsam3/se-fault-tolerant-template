

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "Tolerator.h"


using namespace llvm;
using tolerator::Tolerator;


namespace tolerator {

char Tolerator::ID = 0;

}


bool
Tolerator::runOnModule(Module& m) {
  auto& context = m.getContext();

  // This analysis just prints a message when the program starts or exits.
  // You should modify this code as you see fit.
  auto* voidTy = Type::getVoidTy(context);

  auto helloworld = m.getOrInsertFunction("ToLeRaToR_helloworld", voidTy);
  appendToGlobalCtors(m, llvm::cast<Function>(helloworld.getCallee()), 0);

  auto goodbyeworld = m.getOrInsertFunction("ToLeRaToR_goodbyeworld", voidTy);
  appendToGlobalDtors(m, llvm::cast<Function>(goodbyeworld.getCallee()), 0);

  // Task 1
  FunctionCallee trapRead = m.getOrInsertFunction(
    "handle_invalid_read",
    FunctionType::get(voidTy, false));

  FunctionCallee trapWrite = m.getOrInsertFunction(
    "handle_invalid_write",
    FunctionType::get(voidTy, false));

  FunctionCallee trapFree = m.getOrInsertFunction(
    "handle_invalid_free",
    FunctionType::get(voidTy, false));

  FunctionCallee trapDiv = m.getOrInsertFunction(
    "handle_division_by_zero",
    FunctionType::get(voidTy, false));

  for (Function &F : m) {
    for (BasicBlock &BB : F) {
      for (Instruction &I : BB) {
        IRBuilder<> B(&I);

        if (isa<LoadInst>(&I)) {
          B.CreateCall(trapRead);
        } else if (isa<StoreInst>(&I)) {
          B.CreateCall(trapWrite);
        } else if (auto *CI = dyn_cast<CallInst>(&I)) {
          if (Function *Callee = CI->getCalledFunction())
            if (Callee->getName() == "free")
              B.CreateCall(trapFree);
        } else if (auto *BO = dyn_cast<BinaryOperator>(&I)) {
          if (BO->getOpcode() == Instruction::SDiv ||
              BO->getOpcode() == Instruction::UDiv) {
            Value *den = BO->getOperand(1);
            Value *isZero = B.CreateICmpEQ(
              den, ConstantInt::get(den->getType(), 0));

            BasicBlock *origBB = BO->getParent();
            BasicBlock *contBB = origBB->splitBasicBlock(
              BO->getIterator(), "div.cont");
            BasicBlock *trapBB = BasicBlock::Create(
              context, "div.trap", &F);

            origBB->getTerminator()->eraseFromParent();
            IRBuilder<> B2(origBB);
            B2.CreateCondBr(isZero, trapBB, contBB);

            IRBuilder<> B3(trapBB);
            B3.CreateCall(trapDiv);
            B3.CreateUnreachable();
          }
        }
      }
    }
  }


  return true;
}

