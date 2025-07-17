

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


  return true;
}



