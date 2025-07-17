

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

  SmallVector<ObjInfo,16> Locals, Globals, Heaps;

  for (Function &F : m) {
    for (BasicBlock &BB : F)
      for (Instruction &I : BB)
        if (AllocaInst *AI = dyn_cast<AllocaInst>(&I)) {
          uint64_t sz = m.getDataLayout().getTypeAllocSize(AI->getAllocatedType());
          Locals.push_back({AI, sz});
        }
  }


  for (GlobalVariable &G : m.globals()) {
    if (!G.isDeclaration()) {
      uint64_t sz = m.getDataLayout().getTypeAllocSize(G.getValueType());
      Globals.push_back({&G, sz});
    }
  }


  SmallVector<CallInst*,8> Mallocs;

  for (Function &F : m)
    for (BasicBlock &BB : F)
      for (Instruction &I : BB)
        if (auto *CI = dyn_cast<CallInst>(&I))
          if (CI->getCalledFunction() && CI->getCalledFunction()->getName() == "malloc")
            Mallocs.push_back(CI);

  for (CallInst *CI : Mallocs)
    if (auto *CIarg = dyn_cast<ConstantInt>(CI->getArgOperand(0)))
      Heaps.push_back({CI, CIarg->getZExtValue()});


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
            if (isa<LoadInst>(I))       Builder.CreateCall(H_read);
            else if (isa<StoreInst>(I)) Builder.CreateCall(H_write);
            else if (CallInst *CI = dyn_cast<CallInst>(I)) {
              if (CI->getCalledFunction()->getName() == "free")
                Builder.CreateCall(H_free);
              else
                Builder.CreateCall(H_div0);
            } else {
              Builder.CreateCall(H_div0);
            }
            Builder.CreateUnreachable();

            Builder.SetInsertPoint(OrigBB);
            Value *ptrOrDiv = nullptr;
            if (LoadInst *LI = dyn_cast<LoadInst>(I))
              ptrOrDiv = LI->getPointerOperand();
            else if (StoreInst *SI = dyn_cast<StoreInst>(I))
              ptrOrDiv = SI->getPointerOperand();
            else if (BinaryOperator *BOp = dyn_cast<BinaryOperator>(I))
              ptrOrDiv = BOp->getOperand(1);
            else if (CallInst *CI = dyn_cast<CallInst>(I))
              ptrOrDiv = CI->getArgOperand(0);

            Value *inRange = nullptr;
            if (isa<LoadInst>(I) || isa<StoreInst>(I) ||
                (isa<CallInst>(I) && cast<CallInst>(I)->getCalledFunction()->getName() == "free")) {
              Value *nullCheck = Builder.CreateICmpEQ(ptrOrDiv, Constant::getNullValue(ptrOrDiv->getType()));
              inRange = nullCheck;
              for (auto &O : Locals) {
                Value *lo = Builder.CreateICmpUGE(ptrOrDiv, O.base);
                Value *hi = Builder.CreateICmpULT(ptrOrDiv,
                                Builder.CreateConstantGEP1_64(O.base, O.size));
                inRange = Builder.CreateOr(inRange, Builder.CreateAnd(lo, hi));
              }
              for (auto &O : Globals) {
                Value *lo = Builder.CreateICmpUGE(ptrOrDiv, O.base);
                Value *hi = Builder.CreateICmpULT(ptrOrDiv,
                                Builder.CreateConstantGEP1_64(O.base, O.size));
                inRange = Builder.CreateOr(inRange, Builder.CreateAnd(lo, hi));
              }
              for (auto &O : Heaps) {
                Value *eq = Builder.CreateICmpEQ(ptrOrDiv, O.base);
                inRange = Builder.CreateOr(inRange, eq);
              }
            } else {
              Value *zero = ConstantInt::get(ptrOrDiv->getType(), 0);
              inRange = Builder.CreateICmpNE(ptrOrDiv, zero);
            }

            Value *bad = Builder.CreateNot(inRange);
            Builder.CreateCondBr(bad, ErrBB, ContBB);
          }
        }
  return true;
}
