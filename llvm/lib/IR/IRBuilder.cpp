//===- IRBuilder.cpp - Builder for LLVM Instrs ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the IRBuilder class, which is used as a convenient way
// to create LLVM instructions with a consistent and simplified interface.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/None.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/LoopInfoImpl.h"
#include <cassert>
#include <cstdint>
#include <vector>

using namespace llvm;

/// CreateGlobalString - Make a new global variable with an initializer that
/// has array of i8 type filled in with the nul terminated string value
/// specified.  If Name is specified, it is the name of the global variable
/// created.
GlobalVariable *IRBuilderBase::CreateGlobalString(StringRef Str,
                                                  const Twine &Name,
                                                  unsigned AddressSpace,
                                                  Module *M) {
  Constant *StrConstant = ConstantDataArray::getString(Context, Str);
  if (!M)
    M = BB->getParent()->getParent();
  auto *GV = new GlobalVariable(
      *M, StrConstant->getType(), true, GlobalValue::PrivateLinkage,
      StrConstant, Name, nullptr, GlobalVariable::NotThreadLocal, AddressSpace);
  GV->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
  GV->setAlignment(Align(1));
  return GV;
}

Type *IRBuilderBase::getCurrentFunctionReturnType() const {
  assert(BB && BB->getParent() && "No current function!");
  return BB->getParent()->getReturnType();
}

Value *IRBuilderBase::getCastedInt8PtrValue(Value *Ptr) {
  auto *PT = cast<PointerType>(Ptr->getType());
  if (PT->getElementType()->isIntegerTy(8))
    return Ptr;

  // Otherwise, we need to insert a bitcast.
  return CreateBitCast(Ptr, getInt8PtrTy(PT->getAddressSpace()));
}

static CallInst *createCallHelper(Function *Callee, ArrayRef<Value *> Ops,
                                  IRBuilderBase *Builder,
                                  const Twine &Name = "",
                                  Instruction *FMFSource = nullptr,
                                  ArrayRef<OperandBundleDef> OpBundles = {}) {
  CallInst *CI = Builder->CreateCall(Callee, Ops, OpBundles, Name);
  if (FMFSource)
    CI->copyFastMathFlags(FMFSource);
  return CI;
}

Value *IRBuilderBase::CreateVScale(Constant *Scaling, const Twine &Name) {
  Module *M = GetInsertBlock()->getParent()->getParent();
  assert(isa<ConstantInt>(Scaling) && "Expected constant integer");
  Function *TheFn =
      Intrinsic::getDeclaration(M, Intrinsic::vscale, {Scaling->getType()});
  CallInst *CI = createCallHelper(TheFn, {}, this, Name);
  return cast<ConstantInt>(Scaling)->getSExtValue() == 1
             ? CI
             : CreateMul(CI, Scaling);
}

CallInst *IRBuilderBase::CreateMemSet(Value *Ptr, Value *Val, Value *Size,
                                      MaybeAlign Align, bool isVolatile,
                                      MDNode *TBAATag, MDNode *ScopeTag,
                                      MDNode *NoAliasTag) {
  Ptr = getCastedInt8PtrValue(Ptr);
  Value *Ops[] = {Ptr, Val, Size, getInt1(isVolatile)};
  Type *Tys[] = { Ptr->getType(), Size->getType() };
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(M, Intrinsic::memset, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  if (Align)
    cast<MemSetInst>(CI)->setDestAlignment(Align->value());

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

CallInst *IRBuilderBase::CreateElementUnorderedAtomicMemSet(
    Value *Ptr, Value *Val, Value *Size, Align Alignment, uint32_t ElementSize,
    MDNode *TBAATag, MDNode *ScopeTag, MDNode *NoAliasTag) {

  Ptr = getCastedInt8PtrValue(Ptr);
  Value *Ops[] = {Ptr, Val, Size, getInt32(ElementSize)};
  Type *Tys[] = {Ptr->getType(), Size->getType()};
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(
      M, Intrinsic::memset_element_unordered_atomic, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  cast<AtomicMemSetInst>(CI)->setDestAlignment(Alignment);

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

CallInst *IRBuilderBase::CreateMemTransferInst(
    Intrinsic::ID IntrID, Value *Dst, MaybeAlign DstAlign, Value *Src,
    MaybeAlign SrcAlign, Value *Size, bool isVolatile, MDNode *TBAATag,
    MDNode *TBAAStructTag, MDNode *ScopeTag, MDNode *NoAliasTag) {
  Dst = getCastedInt8PtrValue(Dst);
  Src = getCastedInt8PtrValue(Src);

  Value *Ops[] = {Dst, Src, Size, getInt1(isVolatile)};
  Type *Tys[] = { Dst->getType(), Src->getType(), Size->getType() };
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(M, IntrID, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  auto* MCI = cast<MemTransferInst>(CI);
  if (DstAlign)
    MCI->setDestAlignment(*DstAlign);
  if (SrcAlign)
    MCI->setSourceAlignment(*SrcAlign);

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  // Set the TBAA Struct info if present.
  if (TBAAStructTag)
    CI->setMetadata(LLVMContext::MD_tbaa_struct, TBAAStructTag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

static void createCheckAndTrapFunction_WASM_SBX(Module &M) {
    LLVMContext &Context = M.getContext();
    llvm::IRBuilder<> Builder(Context);

    // Check if the 'iso_mem_check_and_trap' function already exists in the module
    if (Function *ExistingFn = M.getFunction("iso_mem_check_and_trap")) {
        // If the function already exists, don't recreate it
        return;
    }

    // Create the function signature for the 'iso_mem_check_and_trap' function
    FunctionType *FnType = FunctionType::get(Type::getVoidTy(Context),
                                             {Type::getInt1Ty(Context)},
                                             false);

    // Create the function with InternalLinkage to make it static
    Function *CheckAndTrapFn = Function::Create(FnType, Function::InternalLinkage,
                                                "iso_mem_check_and_trap", M);

    // Mark the function as always inline
    CheckAndTrapFn->addFnAttr(Attribute::AlwaysInline);

    // Create the entry, trap, and end blocks
    BasicBlock *EntryBB = BasicBlock::Create(Context, "entry", CheckAndTrapFn);
    BasicBlock *TrapBB = BasicBlock::Create(Context, "trap", CheckAndTrapFn);
    BasicBlock *EndBB = BasicBlock::Create(Context, "end", CheckAndTrapFn);

    // Set up the builder and add instructions to the entry block
    Builder.SetInsertPoint(EntryBB);

    // Get the function argument (boolean condition)
    Argument *ConditionArg = &*CheckAndTrapFn->arg_begin();

    // Create the conditional branch
    Builder.CreateCondBr(ConditionArg,EndBB, TrapBB);

    // Fill in the trap block
    Builder.SetInsertPoint(TrapBB);

    // Call the trap intrinsic after printing
    Function *TrapFn = Intrinsic::getDeclaration(&M, Intrinsic::trap);
    Builder.CreateCall(TrapFn);

    // Mark unreachable after trap
    Builder.CreateUnreachable();

    // Fill in the end block
    Builder.SetInsertPoint(EndBB);
    Builder.CreateRetVoid();
}

static void createOrGetCheckAndTrap_WASM_Function(IRBuilderBase &Builder, Module *M_, Value *ConditionVal) {
    // Retrieve the function 'iso_mem_check_and_trap' from the module
    Function *CheckAndTrapFn = M_->getFunction("iso_mem_check_and_trap");

    // If the function doesn't exist, create it using createCheckAndTrapFunction_WASM_SBX
    if (!CheckAndTrapFn) {
        createCheckAndTrapFunction_WASM_SBX(*M_);
        CheckAndTrapFn = M_->getFunction("iso_mem_check_and_trap");

        // Ensure that the function is created correctly
        if (!CheckAndTrapFn) {
            llvm::errs() << "Error: Unable to find or create 'iso_mem_check_and_trap' function.\n";
            return;
        }
    }

    // Create a call to the 'iso_mem_check_and_trap' function with the provided ConditionVal
    Builder.CreateCall(CheckAndTrapFn, {ConditionVal});
}

static void createCheckAndTrapFunctionHeapSBX(Module &M) {
    LLVMContext &Context = M.getContext();
    llvm::IRBuilder<> Builder(Context);

    // Check if the 'iso_heap_check_and_trap' function already exists in the module
    if (Function *ExistingFn = M.getFunction("iso_heap_check_and_trap")) {
        return;
    }

    // Create the function signature for the 'iso_mem_check_and_trap' function
    // Takes an i1 (boolean) condition and a pointer address (int64)
    FunctionType *FnType = FunctionType::get(Type::getVoidTy(Context),
                                             {Type::getInt1Ty(Context),
                                              Type::getInt64Ty(Context)}, // Condition input and address
                                             false);

    // Create the function with InternalLinkage to make it static
    Function *CheckAndTrapFn = Function::Create(FnType, Function::InternalLinkage,
                                                "iso_heap_check_and_trap", M);

    // Mark the function as always inline
    CheckAndTrapFn->addFnAttr(Attribute::AlwaysInline);

    // Create the entry, slowpath, trap, and end blocks
    BasicBlock *EntryBB = BasicBlock::Create(Context, "entry", CheckAndTrapFn);
    BasicBlock *SlowPathBB = BasicBlock::Create(Context, "slowpath", CheckAndTrapFn);
    BasicBlock *EndBB = BasicBlock::Create(Context, "end", CheckAndTrapFn);

    // Set up the builder and add instructions to the entry block
    Builder.SetInsertPoint(EntryBB);

    // Get the function arguments (i1 condition and pointer address)
    Argument *ConditionArg = &*CheckAndTrapFn->arg_begin();
    Argument *AddrArg = &*std::next(CheckAndTrapFn->arg_begin(), 1);

    // Branch based on the condition check
    Builder.CreateCondBr(ConditionArg, EndBB, SlowPathBB);

    // Handle the slow path (call the tainted memory check function)
    Builder.SetInsertPoint(SlowPathBB);

    // Module reference from the current insertion point
    Module *M_ = Builder.GetInsertBlock()->getParent()->getParent();

    // Cast AddrArg (int64) to i8* (void*)
    Type *VoidPtrType = Type::getInt8PtrTy(Context);
    Value *AddrArgAsVoidPtr = Builder.CreateIntToPtr(AddrArg, VoidPtrType, "addr_to_ptr");

    // Set up the argument array (AddrArgAsVoidPtr is the source)
    Value *Ops[] = {AddrArgAsVoidPtr};

    // Get the declaration for the tainted memory check function
    Function *TaintedMemCheckDecl = Intrinsic::SandboxTaintedMemCheckFunction(M_);

    // Call the tainted memory check function
    Builder.CreateCall(TaintedMemCheckDecl, Ops);

    // After the call, return void
    Builder.CreateRetVoid();

    // Fill in the end block
    Builder.SetInsertPoint(EndBB);
    Builder.CreateRetVoid();
}

static void createCheckAndTrapFunctionHeapSBX_withIndex(Module &M) {
    LLVMContext &Context = M.getContext();
    llvm::IRBuilder<> Builder(Context);

    // Check if the 'iso_heap_check_and_trap' function already exists in the module
    if (Function *ExistingFn = M.getFunction("iso_heap_check_and_trap")) {
        return;
    }

    // Create the function signature for the 'iso_mem_check_and_trap' function
    // Takes an i1 (boolean) condition, a pointer address (int64), and an index (int64)
    FunctionType *FnType = FunctionType::get(Type::getVoidTy(Context),
                                             {Type::getInt64Ty(Context), // Address
                                              Type::getInt64Ty(Context)}, // Index
                                             false);

    // Create the function with InternalLinkage to make it static
    Function *CheckAndTrapFn = Function::Create(FnType, Function::InternalLinkage,
                                                "iso_heap_check_and_trap", M);

    // Mark the function as always inline
    CheckAndTrapFn->addFnAttr(Attribute::AlwaysInline);

    // Create the entry, slowpath, trap, and end blocks
    BasicBlock *EntryBB = BasicBlock::Create(Context, "entry", CheckAndTrapFn);
    BasicBlock *SlowPathBB = BasicBlock::Create(Context, "slowpath", CheckAndTrapFn);
    BasicBlock *EndBB = BasicBlock::Create(Context, "end", CheckAndTrapFn);

    // Set up the builder and add instructions to the entry block
    Builder.SetInsertPoint(EntryBB);

    // Get the function arguments (i1 condition, pointer address, and index)
    Argument *AddrArg = &*CheckAndTrapFn->arg_begin();
    Argument *IndexArg = &*std::next(CheckAndTrapFn->arg_begin(), 1);

    // Load lowerbound_1 and upperbound_1 values
    GlobalVariable *lowerbound_1 = M.getNamedGlobal("lowerbound_1");
    GlobalVariable *upperbound_1 = M.getNamedGlobal("upperbound_1");

    Value *lowerboundVal_1 = Builder.CreateAlignedLoad(Type::getInt64Ty(Context), lowerbound_1, llvm::Align(8));
    Value *upperboundVal_1 = Builder.CreateAlignedLoad(Type::getInt64Ty(Context), upperbound_1, llvm::Align(8));

    // Compute Offset with Heap and add the index
    Value *OffsetValWithHeap = AddrArg; // Starting from address
    Value *OffsetValWithHeapPlusMaxIndex = Builder.CreateAdd(OffsetValWithHeap, IndexArg, "OffsetValWithIndex");

    // Generate the ICmp conditions
    Value *LowerChk = Builder.CreateICmpUGE(OffsetValWithHeapPlusMaxIndex, lowerboundVal_1, "IsoHeap.LowerCheck");
    Value *UpperChk = Builder.CreateICmpULE(OffsetValWithHeapPlusMaxIndex, upperboundVal_1, "IsoHeap.UpperCheck");

    // Combine the checks into a single range check condition
    Value *RangeCheck = Builder.CreateAnd(LowerChk, UpperChk, "IsoHeap._1_RangeCheck");

    // Branch based on the range check condition
    Builder.CreateCondBr(RangeCheck, EndBB, SlowPathBB);

    // Handle the slow path (call the tainted memory check function)
    Builder.SetInsertPoint(SlowPathBB);

    // Module reference from the current insertion point
    Module *M_ = Builder.GetInsertBlock()->getParent()->getParent();

    // Cast AddrArg (int64) to i8* (void*)
    Type *VoidPtrType = Type::getInt8PtrTy(Context);
    Value *AddrArgAsVoidPtr = Builder.CreateIntToPtr(AddrArg, VoidPtrType, "addr_to_ptr");

    // Set up the argument array (AddrArgAsVoidPtr is the address, and IndexArg is the index)
    Value *Ops[] = {AddrArgAsVoidPtr, IndexArg};

    // Get the declaration for the tainted memory check function
    Function *TaintedMemCheckDecl = Intrinsic::SandboxTaintedMemCheckFunction_2(M_);

    // Call the tainted memory check function
    Builder.CreateCall(TaintedMemCheckDecl, Ops);

    // After the call, return void
    Builder.CreateRetVoid();

    // Fill in the end block
    Builder.SetInsertPoint(EndBB);
    Builder.CreateRetVoid();
}

static void createOrGetCheckAndTrap_HEAP_Function_with_index(IRBuilderBase &Builder, Module *M_,
                                                             Value* AddrArg, Value* IndexArg) {
    // Retrieve the function 'iso_mem_check_and_trap' from the module
    Function *CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");

    // If the function doesn't exist, create it using createCheckAndTrapFunction_WASM_SBX
    if (!CheckAndTrapFn) {
        createCheckAndTrapFunctionHeapSBX_withIndex(*M_);
        CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");

        // Ensure that the function is created correctly
        if (!CheckAndTrapFn) {
            llvm::errs() << "Error: Unable to find or create 'iso_heap_check_and_trap' function.\n";
            return;
        }
    }

    // Create a call to the 'iso_mem_check_and_trap' function with the provided ConditionVal
    Builder.CreateCall(CheckAndTrapFn, {AddrArg, IndexArg});
}

static void createOrGetCheckAndTrap_HEAP_Function(IRBuilderBase &Builder, Module *M_, Value *ConditionVal, Value* AddrArg) {
    // Retrieve the function 'iso_mem_check_and_trap' from the module
    Function *CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");

    // If the function doesn't exist, create it using createCheckAndTrapFunction_WASM_SBX
    if (!CheckAndTrapFn) {
        createCheckAndTrapFunctionHeapSBX(*M_);
        CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");

        // Ensure that the function is created correctly
        if (!CheckAndTrapFn) {
            llvm::errs() << "Error: Unable to find or create 'iso_heap_check_and_trap' function.\n";
            return;
        }
    }

    // Create a call to the 'iso_mem_check_and_trap' function with the provided ConditionVal
    Builder.CreateCall(CheckAndTrapFn, {ConditionVal, AddrArg});
}

CallInst *IRBuilderBase::CreateMemCpyInline(Value *Dst, MaybeAlign DstAlign,
                                            Value *Src, MaybeAlign SrcAlign,
                                            Value *Size) {
  Dst = getCastedInt8PtrValue(Dst);
  Src = getCastedInt8PtrValue(Src);
  Value *IsVolatile = getInt1(false);

  Value *Ops[] = {Dst, Src, Size, IsVolatile};
  Type *Tys[] = {Dst->getType(), Src->getType(), Size->getType()};
  Function *F = BB->getParent();
  Module *M = F->getParent();
  Function *TheFn = Intrinsic::getDeclaration(M, Intrinsic::memcpy_inline, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  auto *MCI = cast<MemCpyInlineInst>(CI);
  if (DstAlign)
    MCI->setDestAlignment(*DstAlign);
  if (SrcAlign)
    MCI->setSourceAlignment(*SrcAlign);

  return CI;
}

CallInst *IRBuilderBase::CreateElementUnorderedAtomicMemCpy(
    Value *Dst, Align DstAlign, Value *Src, Align SrcAlign, Value *Size,
    uint32_t ElementSize, MDNode *TBAATag, MDNode *TBAAStructTag,
    MDNode *ScopeTag, MDNode *NoAliasTag) {
  assert(DstAlign >= ElementSize &&
         "Pointer alignment must be at least element size");
  assert(SrcAlign >= ElementSize &&
         "Pointer alignment must be at least element size");
  Dst = getCastedInt8PtrValue(Dst);
  Src = getCastedInt8PtrValue(Src);

  Value *Ops[] = {Dst, Src, Size, getInt32(ElementSize)};
  Type *Tys[] = {Dst->getType(), Src->getType(), Size->getType()};
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(
      M, Intrinsic::memcpy_element_unordered_atomic, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  // Set the alignment of the pointer args.
  auto *AMCI = cast<AtomicMemCpyInst>(CI);
  AMCI->setDestAlignment(DstAlign);
  AMCI->setSourceAlignment(SrcAlign);

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  // Set the TBAA Struct info if present.
  if (TBAAStructTag)
    CI->setMetadata(LLVMContext::MD_tbaa_struct, TBAAStructTag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

CallInst *IRBuilderBase::CreateMemMove(Value *Dst, MaybeAlign DstAlign,
                                       Value *Src, MaybeAlign SrcAlign,
                                       Value *Size, bool isVolatile,
                                       MDNode *TBAATag, MDNode *ScopeTag,
                                       MDNode *NoAliasTag) {
  Dst = getCastedInt8PtrValue(Dst);
  Src = getCastedInt8PtrValue(Src);

  Value *Ops[] = {Dst, Src, Size, getInt1(isVolatile)};
  Type *Tys[] = { Dst->getType(), Src->getType(), Size->getType() };
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(M, Intrinsic::memmove, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  auto *MMI = cast<MemMoveInst>(CI);
  if (DstAlign)
    MMI->setDestAlignment(*DstAlign);
  if (SrcAlign)
    MMI->setSourceAlignment(*SrcAlign);

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

CallInst *IRBuilderBase::CreateElementUnorderedAtomicMemMove(
    Value *Dst, Align DstAlign, Value *Src, Align SrcAlign, Value *Size,
    uint32_t ElementSize, MDNode *TBAATag, MDNode *TBAAStructTag,
    MDNode *ScopeTag, MDNode *NoAliasTag) {
  assert(DstAlign >= ElementSize &&
         "Pointer alignment must be at least element size");
  assert(SrcAlign >= ElementSize &&
         "Pointer alignment must be at least element size");
  Dst = getCastedInt8PtrValue(Dst);
  Src = getCastedInt8PtrValue(Src);

  Value *Ops[] = {Dst, Src, Size, getInt32(ElementSize)};
  Type *Tys[] = {Dst->getType(), Src->getType(), Size->getType()};
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(
      M, Intrinsic::memmove_element_unordered_atomic, Tys);

  CallInst *CI = createCallHelper(TheFn, Ops, this);

  // Set the alignment of the pointer args.
  CI->addParamAttr(0, Attribute::getWithAlignment(CI->getContext(), DstAlign));
  CI->addParamAttr(1, Attribute::getWithAlignment(CI->getContext(), SrcAlign));

  // Set the TBAA info if present.
  if (TBAATag)
    CI->setMetadata(LLVMContext::MD_tbaa, TBAATag);

  // Set the TBAA Struct info if present.
  if (TBAAStructTag)
    CI->setMetadata(LLVMContext::MD_tbaa_struct, TBAAStructTag);

  if (ScopeTag)
    CI->setMetadata(LLVMContext::MD_alias_scope, ScopeTag);

  if (NoAliasTag)
    CI->setMetadata(LLVMContext::MD_noalias, NoAliasTag);

  return CI;
}

static CallInst *getReductionIntrinsic(IRBuilderBase *Builder, Intrinsic::ID ID,
                                    Value *Src) {
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  Type *Tys[] = { Src->getType() };
  auto Decl = Intrinsic::getDeclaration(M, ID, Tys);
  return createCallHelper(Decl, Ops, Builder);
}
 static CallInst *CreateTaintedPtrMemCheckInternal(IRBuilderBase *Builder, Value *Src){
 Module *M = Builder->GetInsertBlock()->getParent()->getParent();
 Value *Ops[] = {Src};
 auto *Decl = Intrinsic::SandboxTaintedMemCheckFunction(M);
 return createCallHelper(Decl, Ops, Builder);
}

static CallInst *CreateregisterTaintedFunctionInternal(IRBuilderBase *Builder, Value *Src){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  auto *Decl = Intrinsic::SandboxRegisterTaintedFunction(M);
  return createCallHelper(Decl, Ops, Builder);
}

static CallInst *CreateregisterCallbackFunctionInternal(IRBuilderBase *Builder, Value *Src){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  auto *Decl = Intrinsic::SandboxRegisterTaintedFunction(M);
  return createCallHelper(Decl, Ops, Builder);
}

static CallInst *CreateunregisterCallbackFunctionInternal(IRBuilderBase *Builder, Value *Src){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  auto *Decl = Intrinsic::SandboxUNRegisterCallbackFunction(M);
  return createCallHelper(Decl, Ops, Builder);
}

static CallInst *CreateIsLegalCallEdgeCheckInternal(IRBuilderBase *Builder,
                                                    Value *pValue) {
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {pValue};
  auto *Decl = Intrinsic::CreateIsLegalCallEdgeCheckInternal(M);
  return Builder->CreateCall(Decl, Ops);
}
static CallInst *CreateCondlTaintedO2PtrInternal(IRBuilderBase *Builder, Value *Src){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  auto *Decl = Intrinsic::SandboxCondlTaintedO2PtrFunction(M);
  return createCallHelper(Decl, Ops, Builder);
}

static CallInst *CreateCondlTaintedPToOInternal(IRBuilderBase *Builder, Value *Src){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Src};
  auto *Decl = Intrinsic::SandboxTaintedPtr2OFunction(M);
  return createCallHelper(Decl, Ops, Builder);
}

static CallInst *createTaintedOffset2Ptr(IRBuilderBase *Builder, Value *Offset){
    Module *M = Builder->GetInsertBlock()->getParent()->getParent();
    Value *Ops[] = {Offset};
    auto *Decl = Intrinsic::Offset2Pointer(M);
    return createCallHelper(Decl, Ops, Builder);
}

static CallInst *sbxInit(IRBuilderBase *Builder){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  auto *Decl = Intrinsic::InitSbx(M);
  //create a call to this function
  return Builder->CreateCall(Decl);
}

static CallInst *fetchSbxHeapAddress(IRBuilderBase *Builder){
  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = { 0 };
  auto *Decl = Intrinsic::fetchSbxHeapAddress(M);
  //create a call to this function
  return Builder->CreateCall(Decl);
}

static CallInst *fetchSbxHeapBound(IRBuilderBase *Builder, Module *M_){
  if (M_ == nullptr)
    M_ = Builder->GetInsertBlock()->getParent()->getParent();
//  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = { 0 };
  auto *Decl = Intrinsic::fetchSbxHeapBound(M_);
  //create a call to this function
  return Builder->CreateCall(Decl);
}
//#define DEBUG_SANITY_CHECK

static CallInst *Call_Verify_Addr_WASMSBX(IRBuilderBase *Builder, Module *M_, Value *Address, Value *MaxIndex) {
  // If the module is not provided, get it from the current insertion point
  if (M_ == nullptr)
    M_ = Builder->GetInsertBlock()->getParent()->getParent();

  // Fetch the declaration for the intrinsic or function to check the address
  auto *Decl = Intrinsic::Call_Verify_Addr_WASMSBX(M_);

  // Ensure that the declaration is of the correct function type
  assert(Decl && "Intrinsic 'c_verify_addr_wasmsbx' not found!");

  // Create an array of the operands (Address and MaxIndex)
  Value *Ops[] = { Address, MaxIndex };

  // Create and return the call instruction, which returns an i1
  return Builder->CreateCall(Decl, Ops);
}

static CallInst *Call_Verify_Addr_HEAPSBX(IRBuilderBase *Builder, Module *M_, Value *Address, Value *MaxIndex) {
    // If the module is not provided, get it from the current insertion point
    if (M_ == nullptr)
        M_ = Builder->GetInsertBlock()->getParent()->getParent();

    // Fetch the declaration for the intrinsic or function to check the address
    auto *Decl = Intrinsic::Call_Verify_Addr_HEAPSBX(M_);

    // Ensure that the declaration is of the correct function type
    assert(Decl && "Intrinsic 'c_verify_addr_heapsbx' not found!");

    // Create an array of the operands (Address and MaxIndex)
    Value *Ops[] = { Address, MaxIndex };

    // Create and return the call instruction, which returns an i1
    return Builder->CreateCall(Decl, Ops);
}

static Value *addHeap_condition(IRBuilderBase *Builder, Module *M_, Value *Address) {
    if (M_ == nullptr)
        M_ = Builder->GetInsertBlock()->getParent()->getParent();

    LLVMContext &Context = M_->getContext();

    GlobalVariable *lowerbound_1 = M_->getNamedGlobal("lowerbound_1");
    GlobalVariable *upperbound_1 = M_->getNamedGlobal("upperbound_1");
    BasicBlock *CurrentBB = Builder->GetInsertBlock();

    if (!lowerbound_1 || !upperbound_1) {
        llvm::errs() << "Error: Global variables not found!\n";
        return nullptr;
    }

    // Load the global variables
    Value *lowerboundVal_1 = Builder->CreateAlignedLoad(Type::getInt64Ty(Context), lowerbound_1, llvm::Align(8));
    Value *upperboundVal_1 = Builder->CreateAlignedLoad(Type::getInt64Ty(Context), upperbound_1, llvm::Align(8));

    // Adjust Address with MaxIndex if necessary
    Value *OffsetValWithHeap = Address;

    Value *OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;

    // Generate the ICmp conditions
    Value *LowerChk = Builder->CreateICmpUGE(OffsetValWithHeapPlusMaxIndex, lowerboundVal_1, "IsoHeap.LowerCheck");
    Value *UpperChk = Builder->CreateICmpULE(OffsetValWithHeapPlusMaxIndex, upperboundVal_1, "IsoHeap.UpperCheck");

    // Combine the checks using AND
    Value *RangeCheck = Builder->CreateAnd(LowerChk, UpperChk, "IsoHeap._1_RangeCheck");
    return RangeCheck;
}

static Value *addWasm_condition(IRBuilderBase *Builder, Module *M_, Value *Address) {
    if (M_ == nullptr)
        M_ = Builder->GetInsertBlock()->getParent()->getParent();

    // Fetch global variables
    GlobalVariable *sbxHeapRange = M_->getNamedGlobal("sbxHeapRange");
    GlobalVariable *sbxHeapBase = M_->getNamedGlobal("sbxHeap");

    if (!sbxHeapRange || !sbxHeapBase) {
        llvm::errs() << "Error: Global variable 'sbxHeapRange' or 'sbxHeapBase' not found!\n";
        return nullptr;
    }

    // Check if the global values are already loaded in the current basic block
    Value *SbxHeapRangeLoadedVal = nullptr;

    if (!SbxHeapRangeLoadedVal) {
        SbxHeapRangeLoadedVal = Builder->CreateAlignedLoad(
                llvm::Type::getInt64Ty(M_->getContext()), sbxHeapRange, llvm::Align(8), false);
    }


    //Value *OffsetValWithHeap = Builder->CreateAdd(SbxHeapBaseLoadedVal, Address);
    Value *OffsetValWithHeap = Builder->CreateAnd(Address, ConstantInt::get(Address->getType(), 0xFFFFFFFF));
    Value *OffsetValWithHeapPlusMaxIndex = nullptr;
    if (OffsetValWithHeap->getType()->getIntegerBitWidth() < 64) {
        OffsetValWithHeapPlusMaxIndex = Builder->CreateZExt(OffsetValWithHeap, llvm::Type::getInt64Ty(M_->getContext()), "OffsetValWithHeap64");
        Value *ConditionVal = Builder->CreateICmpULT(OffsetValWithHeapPlusMaxIndex, SbxHeapRangeLoadedVal, "SandMem.TaintCheck");
        return ConditionVal;
    } else {
        OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;
    }

    if (!isa<Instruction>(OffsetValWithHeapPlusMaxIndex) ||
        !cast<Instruction>(OffsetValWithHeapPlusMaxIndex)->isBinaryOp() ||
        cast<BinaryOperator>(OffsetValWithHeapPlusMaxIndex)->getOpcode() != Instruction::And) {

        // Perform an AND operation with UINT32_MAX (0xFFFFFFFF)
        Value *Mask = ConstantInt::get(llvm::Type::getInt64Ty(M_->getContext()), UINT32_MAX);
        OffsetValWithHeapPlusMaxIndex = Builder->CreateAnd(OffsetValWithHeapPlusMaxIndex, Mask, "OffsetValMasked");
    }

    Value *ConditionVal = Builder->CreateICmpULT(OffsetValWithHeapPlusMaxIndex, SbxHeapRangeLoadedVal, "SandMem.TaintCheck");
    return ConditionVal;
}

static void
Call_Check_and_trap_WASMSBX_within_loop(IRBuilderBase *Builders, Module *M_, llvm::BasicBlock *CurBB, Value *Address,
                                      Value *MaxIndex, Instruction *TargetInstr) {
#ifdef DEBUG_SANITY_CHECK

    Call_Verify_Addr_WASMSBX(Builders, M_, Address, MaxIndex);
#else
    if (M_ == nullptr)
        M_ = Builders->GetInsertBlock()->getParent()->getParent();

    // Fetch global variables
    GlobalVariable *sbxHeapRange = M_->getNamedGlobal("sbxHeapRange");
    GlobalVariable *sbxHeapBase = M_->getNamedGlobal("sbxHeap");

    if (!sbxHeapRange || !sbxHeapBase) {
        llvm::errs() << "Error: Global variable 'sbxHeapRange' or 'sbxHeapBase' not found!\n";
        return;
    }

    // Check if the global values are already loaded in the current basic block
    Value *SbxHeapRangeLoadedVal = nullptr;

    BasicBlock *CurrentBB = CurBB;
    IRBuilder<> Builder(CurrentBB);
    Builder.SetInsertPoint(TargetInstr->getNextNode());


    if (!SbxHeapRangeLoadedVal) {
        SbxHeapRangeLoadedVal = Builder.CreateAlignedLoad(
                llvm::Type::getInt64Ty(M_->getContext()), sbxHeapRange, llvm::Align(8), false);
    }

    Value *OffsetValWithHeap = Builder.CreateAnd(Address, ConstantInt::get(Address->getType(), 0xFFFFFFFF));

    Value *OffsetValWithHeapPlusMaxIndex = nullptr;
    if (OffsetValWithHeap->getType()->getIntegerBitWidth() < 64) {
        OffsetValWithHeapPlusMaxIndex = Builder.CreateZExt(OffsetValWithHeap, llvm::Type::getInt64Ty(M_->getContext()), "OffsetValWithHeap64");
    } else {
        OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;
    }

    auto *ConstMaxIndex = dyn_cast<ConstantInt>(MaxIndex);
    // Check if MaxIndex is a constant with value -1
    if (ConstMaxIndex && ConstMaxIndex->isMinusOne()) {
    }
    else
    {
        OffsetValWithHeapPlusMaxIndex = Builder.CreateAdd(OffsetValWithHeapPlusMaxIndex, MaxIndex, "SbxHeapRangePlusMaxIndex");
    }

    Value *ConditionVal = Builder.CreateICmpULT(OffsetValWithHeapPlusMaxIndex, SbxHeapRangeLoadedVal, "SandMem.TaintCheck");

    Instruction *Term = CurrentBB->getTerminator();
    Value *ExistingTaintCheck = nullptr;
    BasicBlock *ExistingSanityCheckBB = nullptr;

    if (auto *Br = dyn_cast<BranchInst>(Term)) {
        if (!Br->isConditional()) {
            ExistingSanityCheckBB = Br->getSuccessor(0);
            if (ExistingSanityCheckBB->getName().startswith("sanityCheck")) {
                if (auto *SanityCheckBr = dyn_cast<BranchInst>(ExistingSanityCheckBB->getTerminator())) {
                    ExistingTaintCheck = SanityCheckBr->getCondition();
                    IRBuilder<> SanityCheckBuilder(ExistingSanityCheckBB);
                    SanityCheckBuilder.SetInsertPoint(ExistingSanityCheckBB->getTerminator());

                    Value *CombinedCondition = SanityCheckBuilder.CreateAnd(ExistingTaintCheck, ConditionVal, "CombinedTaintCheck");
                    SanityCheckBr->setCondition(CombinedCondition);
                    return;
                }
            }
        }
    }
    // Assuming Builder, M_, and ConditionVal are already defined
    createOrGetCheckAndTrap_WASM_Function(Builder, M_, ConditionVal);
    return;
#endif
}

static void
Call_Check_and_trap_HEAPSBX_within_loop(IRBuilderBase *Builders, Module *M_, llvm::BasicBlock *CurBB, Value *Address,
                                        Value *MaxIndex, Instruction *TargetInstr) {
#ifdef DEBUG_SANITY_CHECK

    Call_Verify_Addr_HEAPSBX(Builders, M_, Address, MaxIndex);
#else
    if (M_ == nullptr)
        M_ = Builders->GetInsertBlock()->getParent()->getParent();

    BasicBlock *CurrentBB = CurBB;
    IRBuilder<> Builder(CurrentBB);
    Builder.SetInsertPoint(TargetInstr->getNextNode());

//    // Vector to store newly added instructions
//    std::vector<Instruction *> NewInstructions;
//
//    GlobalVariable *lowerbound_1 = M_->getNamedGlobal("lowerbound_1");
//    GlobalVariable *upperbound_1 = M_->getNamedGlobal("upperbound_1");
//
//    if (!lowerbound_1 || !upperbound_1) {
//        llvm::errs() << "Error: Global variables not found!\n";
//        return;
//    }
//
//    // Load the global variables
//    Value *lowerboundVal_1 = Builder.CreateAlignedLoad(Type::getInt64Ty(M_->getContext()), lowerbound_1, llvm::Align(8));
//    Value *upperboundVal_1 = Builder.CreateAlignedLoad(Type::getInt64Ty(M_->getContext()), upperbound_1, llvm::Align(8));
//
//    NewInstructions.push_back(cast<Instruction>(lowerboundVal_1));
//    NewInstructions.push_back(cast<Instruction>(upperboundVal_1));
//
//    // Adjust Address with MaxIndex if necessary
//    Value *OffsetValWithHeap = Address;
//    NewInstructions.push_back(cast<Instruction>(OffsetValWithHeap));
//
//    Value *OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;
//
//    auto *ConstMaxIndex = dyn_cast<ConstantInt>(MaxIndex);
//    if (!ConstMaxIndex || !ConstMaxIndex->isMinusOne()) {
//        OffsetValWithHeapPlusMaxIndex = Builder.CreateAdd(OffsetValWithHeapPlusMaxIndex, MaxIndex, "IsoHeapRangePlusMaxIndex");
//        NewInstructions.push_back(cast<Instruction>(OffsetValWithHeapPlusMaxIndex));
//    }
//
//    // Generate the ICmp conditions
//    Value *LowerChk = Builder.CreateICmpUGE(OffsetValWithHeapPlusMaxIndex, lowerboundVal_1, "IsoHeap.LowerCheck");
//    Value *UpperChk = Builder.CreateICmpULE(OffsetValWithHeapPlusMaxIndex, upperboundVal_1, "IsoHeap.UpperCheck");
//
//    // Combine the checks using AND
//    Value *RangeCheck = Builder.CreateAnd(LowerChk, UpperChk, "IsoHeap._1_RangeCheck");
//
//    // Check if there's an existing taint check, and combine conditions
//    Instruction *Term = CurrentBB->getTerminator();
//    BasicBlock *ExistingSanityCheckBB = nullptr;
//    if (Term)
//    {
//        if (auto *Br = dyn_cast<BranchInst>(Term)) {
//            if (!Br->isConditional()) {
//                ExistingSanityCheckBB = Br->getSuccessor(0);
//                if (ExistingSanityCheckBB->getName().startswith("IsoHeap._1_RangeCheck")) {
//                    if (auto *SanityCheckBr = dyn_cast<BranchInst>(ExistingSanityCheckBB->getTerminator())) {
//                        Value *ExistingTaintCheck = SanityCheckBr->getCondition();
//                        IRBuilder<> SanityCheckBuilder(ExistingSanityCheckBB);
//                        SanityCheckBuilder.SetInsertPoint(ExistingSanityCheckBB->getTerminator());
//
//                        // Combine the conditions
//                        RangeCheck = SanityCheckBuilder.CreateAnd(ExistingTaintCheck, RangeCheck, "IsoHeap._1_RangeCheck");
//                        SanityCheckBr->setCondition(RangeCheck);
//                        return;
//                    }
//                }
//            }
//        }
//    }
    // Assuming Builder, M_, and ConditionVal are already defined
    createOrGetCheckAndTrap_HEAP_Function_with_index(Builder, M_, Address, MaxIndex);
    return;
#endif
}

static void
Call_Check_and_trap_HEAPSBX(IRBuilderBase *Builder, Module *M_, Value *Address, Value *MaxIndex) {

#ifdef DEBUG_SANITY_CHECK

    Call_Verify_Addr_HEAPSBX(Builder, M_, Address, MaxIndex);
#else
    if (M_ == nullptr)
        M_ = Builder->GetInsertBlock()->getParent()->getParent();

//    LLVMContext &Context = M_->getContext();
//
//    // Vector to store newly added instructions
//    std::vector<Instruction *> NewInstructions;
//
//    GlobalVariable *lowerbound_1 = M_->getNamedGlobal("lowerbound_1");
//    GlobalVariable *upperbound_1 = M_->getNamedGlobal("upperbound_1");
//    BasicBlock *CurrentBB = Builder->GetInsertBlock();
//
//    if (!lowerbound_1 || !upperbound_1) {
//        llvm::errs() << "Error: Global variables not found!\n";
//        return;
//    }
//
//    // Load the global variables
//    Value *lowerboundVal_1 = Builder->CreateAlignedLoad(Type::getInt64Ty(Context), lowerbound_1, llvm::Align(8));
//    Value *upperboundVal_1 = Builder->CreateAlignedLoad(Type::getInt64Ty(Context), upperbound_1, llvm::Align(8));
//
//    NewInstructions.push_back(cast<Instruction>(lowerboundVal_1));
//    NewInstructions.push_back(cast<Instruction>(upperboundVal_1));
//
//    // Adjust Address with MaxIndex if necessary
//    Value *OffsetValWithHeap = Address;
//    NewInstructions.push_back(cast<Instruction>(OffsetValWithHeap));
//
//    Value *OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;
//
//    auto *ConstMaxIndex = dyn_cast<ConstantInt>(MaxIndex);
//    if (!ConstMaxIndex || !ConstMaxIndex->isMinusOne()) {
//        OffsetValWithHeapPlusMaxIndex = Builder->CreateAdd(OffsetValWithHeapPlusMaxIndex, MaxIndex, "IsoHeapRangePlusMaxIndex");
//        NewInstructions.push_back(cast<Instruction>(OffsetValWithHeapPlusMaxIndex));
//    }
//
//    // Generate the ICmp conditions
//    Value *LowerChk = Builder->CreateICmpUGE(OffsetValWithHeapPlusMaxIndex, lowerboundVal_1, "IsoHeap.LowerCheck");
//    Value *UpperChk = Builder->CreateICmpULE(OffsetValWithHeapPlusMaxIndex, upperboundVal_1, "IsoHeap.UpperCheck");
//
//    // Combine the checks using AND
//    Value *RangeCheck = Builder->CreateAnd(LowerChk, UpperChk, "IsoHeap._1_RangeCheck");
//
//    // Check if there's an existing taint check, and combine conditions
//    Instruction *Term = CurrentBB->getTerminator();
//    BasicBlock *ExistingSanityCheckBB = nullptr;
//    if (Term)
//    {
//        if (auto *Br = dyn_cast<BranchInst>(Term)) {
//            if (!Br->isConditional()) {
//                ExistingSanityCheckBB = Br->getSuccessor(0);
//                if (ExistingSanityCheckBB->getName().startswith("IsoHeap._1_RangeCheck")) {
//                    if (auto *SanityCheckBr = dyn_cast<BranchInst>(ExistingSanityCheckBB->getTerminator())) {
//                        Value *ExistingTaintCheck = SanityCheckBr->getCondition();
//                        IRBuilder<> SanityCheckBuilder(ExistingSanityCheckBB);
//                        SanityCheckBuilder.SetInsertPoint(ExistingSanityCheckBB->getTerminator());
//
//                        // Combine the conditions
//                        RangeCheck = SanityCheckBuilder.CreateAnd(ExistingTaintCheck, RangeCheck, "IsoHeap._1_RangeCheck");
//                        SanityCheckBr->setCondition(RangeCheck);
//                        return;
//                    }
//                }
//            }
//        }
//    }
    // Call the check and trap function with the final condition
    createOrGetCheckAndTrap_HEAP_Function_with_index(*Builder, M_, Address,MaxIndex);
#endif
}

static void
Call_Check_and_trap_WASMSBX(IRBuilderBase *Builder, Module *M_, Value *Address, Value *MaxIndex) {

#ifdef DEBUG_SANITY_CHECK

    Call_Verify_Addr_WASMSBX(Builder, M_, Address, MaxIndex);
#else
    if (M_ == nullptr)
        M_ = Builder->GetInsertBlock()->getParent()->getParent();

    // Metadata kind ID for the "SanityCheck" metadata
    LLVMContext &Context = M_->getContext();

    // Vector to store newly added instructions
    std::vector<Instruction *> NewInstructions;

    // Fetch global variables
    GlobalVariable *sbxHeapRange = M_->getNamedGlobal("sbxHeapRange");
    GlobalVariable *sbxHeapBase = M_->getNamedGlobal("sbxHeap");

    if (!sbxHeapRange || !sbxHeapBase) {
        llvm::errs() << "Error: Global variable 'sbxHeapRange' or 'sbxHeapBase' not found!\n";
        return;
    }

    // Check if the global values are already loaded in the current basic block
    Value *SbxHeapRangeLoadedVal = nullptr;
    Value *sbxHeapBaseVal = nullptr;

    BasicBlock *CurrentBB = Builder->GetInsertBlock();
    llvm::Instruction* insertPoint = nullptr;

    if (!SbxHeapRangeLoadedVal) {
        SbxHeapRangeLoadedVal = Builder->CreateAlignedLoad(
                llvm::Type::getInt64Ty(M_->getContext()), sbxHeapRange, llvm::Align(8), false);
        NewInstructions.push_back(cast<Instruction>(SbxHeapRangeLoadedVal));
    }
    sbxHeapBaseVal = Builder->CreateAlignedLoad(
            llvm::Type::getInt64Ty(M_->getContext()), sbxHeapBase, llvm::Align(8), false);

    //Value *OffsetValWithHeap = Builder->CreateAdd(SbxHeapBaseLoadedVal, Address);
    Value *OffsetValWithHeap = Builder->CreateAnd(Address, ConstantInt::get(Address->getType(), 0xFFFFFFFF));
    NewInstructions.push_back(cast<Instruction>(OffsetValWithHeap));

    Value *OffsetValWithHeapPlusMaxIndex = nullptr;
    if (OffsetValWithHeap->getType()->getIntegerBitWidth() < 64) {
        OffsetValWithHeapPlusMaxIndex = Builder->CreateZExt(OffsetValWithHeap, llvm::Type::getInt64Ty(M_->getContext()), "OffsetValWithHeap64");
        NewInstructions.push_back(cast<Instruction>(OffsetValWithHeapPlusMaxIndex));
    } else {
        OffsetValWithHeapPlusMaxIndex = OffsetValWithHeap;
    }

    auto *ConstMaxIndex = dyn_cast<ConstantInt>(MaxIndex);
    // Check if MaxIndex is a constant with value -1
    if (ConstMaxIndex && ConstMaxIndex->isMinusOne()) {
    }
    else
    {
        OffsetValWithHeapPlusMaxIndex = Builder->CreateAdd(OffsetValWithHeapPlusMaxIndex, MaxIndex, "SbxHeapRangePlusMaxIndex");
        NewInstructions.push_back(cast<Instruction>(OffsetValWithHeapPlusMaxIndex));
    }

    Value *ConditionVal = Builder->CreateICmpULT(OffsetValWithHeapPlusMaxIndex, SbxHeapRangeLoadedVal, "SandMem.TaintCheck");

    Instruction *Term = CurrentBB->getTerminator();
    Value *ExistingTaintCheck = nullptr;
    BasicBlock *ExistingSanityCheckBB = nullptr;

    if (auto *Br = dyn_cast<BranchInst>(Term)) {
        if (!Br->isConditional()) {
            ExistingSanityCheckBB = Br->getSuccessor(0);
            if (ExistingSanityCheckBB->getName().startswith("sanityCheck")) {
                if (auto *SanityCheckBr = dyn_cast<BranchInst>(ExistingSanityCheckBB->getTerminator())) {
                    ExistingTaintCheck = SanityCheckBr->getCondition();
                    IRBuilder<> SanityCheckBuilder(ExistingSanityCheckBB);

                    SanityCheckBuilder.SetInsertPoint(ExistingSanityCheckBB->getTerminator());

                    Value *CombinedCondition = SanityCheckBuilder.CreateAnd(ExistingTaintCheck, ConditionVal, "CombinedTaintCheck");
                    SanityCheckBr->setCondition(CombinedCondition);
                    return;
                }
            }
        }
    }

    createOrGetCheckAndTrap_WASM_Function(*Builder, M_, ConditionVal);
    return;
#endif
}

static void EmitHeap_SBX_sanity_check(IRBuilderBase *Builder, Module *M_, Value *Address, Value *MaxIndex) {
    LLVMContext &Context = M_->getContext();

    // Retrieve the 'iso_heap_check_and_trap' function from the module, or create it if it doesn't exist
    Function *CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");
    if (!CheckAndTrapFn) {
        createCheckAndTrapFunctionHeapSBX(*M_);
        CheckAndTrapFn = M_->getFunction("iso_heap_check_and_trap");
    }

    // Ensure the function is found
    if (!CheckAndTrapFn) {
        llvm::errs() << "Error: Unable to find or create 'iso_heap_check_and_trap' function.\n";
        return;
    }

    // Cast Address to an i64 type
    Value *PtrAsInt = Builder->CreatePtrToInt(Address, Type::getInt64Ty(Context));

    // Check if MaxIndex is the constant -1
    if (llvm::ConstantInt *ConstMaxIndex = llvm::dyn_cast<llvm::ConstantInt>(MaxIndex)) {
        if (ConstMaxIndex->isMinusOne()) {
            // If MaxIndex is -1, pass Address as-is
            Builder->CreateCall(CheckAndTrapFn, {PtrAsInt});
            return;
        }
    }

    // If MaxIndex is not -1, add MaxIndex to Address
    Value *AdjustedAddress = Builder->CreateAdd(PtrAsInt, MaxIndex);

    // Pass the adjusted address to the function
    Builder->CreateCall(CheckAndTrapFn, {AdjustedAddress});
}

CallInst *IRBuilderBase::CreateFAddReduce(Value *Acc, Value *Src) {
  Module *M = GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Acc, Src};
  auto Decl = Intrinsic::getDeclaration(M, Intrinsic::vector_reduce_fadd,
                                        {Src->getType()});
  return createCallHelper(Decl, Ops, this);
}

CallInst *IRBuilderBase::CreateFMulReduce(Value *Acc, Value *Src) {
  Module *M = GetInsertBlock()->getParent()->getParent();
  Value *Ops[] = {Acc, Src};
  auto Decl = Intrinsic::getDeclaration(M, Intrinsic::vector_reduce_fmul,
                                        {Src->getType()});
  return createCallHelper(Decl, Ops, this);
}

CallInst *IRBuilderBase::CreateAddReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_add, Src);
}

CallInst *IRBuilderBase::CreateMulReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_mul, Src);
}

CallInst *IRBuilderBase::CreateAndReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_and, Src);
}

CallInst *IRBuilderBase::CreateOrReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_or, Src);
}

CallInst *IRBuilderBase::CreateXorReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_xor, Src);
}

CallInst *IRBuilderBase::CreateIntMaxReduce(Value *Src, bool IsSigned) {
  auto ID =
      IsSigned ? Intrinsic::vector_reduce_smax : Intrinsic::vector_reduce_umax;
  return getReductionIntrinsic(this, ID, Src);
}

CallInst *IRBuilderBase::CreateTaintedPtrMemCheck(Value* Src){
    //if the parsed Source Value is not a void pointer type, it must be casted to a void pointer -->
    if(Src->getType() != Type::getInt8PtrTy(this->getContext()))
    {
      //cast it to void pointer
      Src = CreateIntToPtr(Src,Type::getInt8PtrTy(this->getContext()));
    }
    return CreateTaintedPtrMemCheckInternal(this, Src);
}

CallInst *IRBuilderBase::registerTaintedFunction(Value *Src)
{
    //if the parsed Source Value is not a void pointer type, it must be casted to a void pointer -->
    if(Src->getType() != Type::getInt8PtrTy(this->getContext()))
    {
      //cast it to void pointer
      Src = CreateBitCast(Src,Type::getInt8PtrTy(this->getContext()));
    }
    return CreateregisterTaintedFunctionInternal(this, Src);
}

CallInst *IRBuilderBase::registerCallbackFunction(Value *Src)
{
    //if the parsed Source Value is not a void pointer type, it must be casted to a void pointer -->
    if(Src->getType() != Type::getInt8PtrTy(this->getContext()))
    {
      //cast it to void pointer
      Src = CreateBitCast(Src,Type::getInt8PtrTy(this->getContext()));
    }
    return CreateregisterCallbackFunctionInternal(this, Src);
}

CallInst *IRBuilderBase::unregisterCallbackFunction(Value *Src)
{
    //if the parsed Source Value is not a void pointer type, it must be casted to a void pointer -->
    if(Src->getType() != Type::getInt8PtrTy(this->getContext()))
    {
      //cast it to void pointer
      Src = CreateBitCast(Src,Type::getInt8PtrTy(this->getContext()));
    }
    return CreateunregisterCallbackFunctionInternal(this, Src);
}

CallInst *IRBuilderBase::CreateIsLegalCallEdgeCheck(Value *pValue) {
    return CreateIsLegalCallEdgeCheckInternal(this, pValue);
}
CallInst *IRBuilderBase::CreateTaintedOffset2Ptr(Value *Offset){
    //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->

    if(Offset->getType()->isTaintedPtrTy())
    {
        //cast it to uint32 pointer
        Offset = CreatePtrToInt(Offset,Type::getInt32Ty(this->getContext()));
    }
    return createTaintedOffset2Ptr(this, Offset);
}
CallInst *IRBuilderBase::InitSbx(){
  //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->

  return sbxInit(this);
}

CallInst *IRBuilderBase::FetchSbxHeapAddress(){
  //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->

  return fetchSbxHeapAddress(this);
}

CallInst *IRBuilderBase::FetchSbxHeapBound(Module* M){
  //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->
    return fetchSbxHeapBound(this, M);
}

CallInst *IRBuilderBase::Verify_Wasm_ptr_with_optimization(Module* M, Value *Address, Value *MaxIndex){
  //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->
  return Call_Verify_Addr_WASMSBX(this, M, Address, MaxIndex);
}

CallInst *IRBuilderBase::Verify_heap_ptr_with_optimization(Module* M, Value *Address, Value *MaxIndex){
    //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->
    return Call_Verify_Addr_HEAPSBX(this, M, Address, MaxIndex);
}

void
IRBuilderBase::Verify_Wasm_ptr_no_optimization(Module* M, Value *Address, Value *MaxIndex) {
    // Emit the sanity check and get the tuple result (Value* and vector<BasicBlock*>)
    return Call_Check_and_trap_WASMSBX(this, M, Address, MaxIndex);
}

void
IRBuilderBase::Verify_heap_ptr_no_optimization(Module* M, Value *Address, Value *MaxIndex) {
    // Emit the sanity check and get the tuple result (Value* and vector<BasicBlock*>)
    return Call_Check_and_trap_HEAPSBX(this, M, Address, MaxIndex);
}

void
IRBuilderBase::Verify_Wasm_ptr_within_loop(Module* M, llvm::BasicBlock* CurBB,
                                           Value *Address, Value *MaxIndex, Instruction *TargetInstr) {
    // Emit the sanity check within the loop and get the tuple result (Value* and vector<BasicBlock*>)
    return Call_Check_and_trap_WASMSBX_within_loop(this, M, CurBB, Address, MaxIndex, TargetInstr);
}

void
IRBuilderBase::Verify_heap_ptr_within_loop(Module* M, llvm::BasicBlock* CurBB,
                                           Value *Address, Value *MaxIndex, Instruction *TargetInstr) {
    // Emit the sanity check within the loop and get the tuple result (Value* and vector<BasicBlock*>)
    return Call_Check_and_trap_HEAPSBX_within_loop(this, M, CurBB, Address, MaxIndex, TargetInstr);
}

Value *IRBuilderBase::AddWasm_condition(Module* M, Value *Address){
    //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->
    return addWasm_condition(this, M, Address);
}

Value *IRBuilderBase::AddHeap_condition(Module* M, Value *Address){
    //if the parsed Source Value is not a Unsigned int, it must be casted to a Unsigned int -->
    return addHeap_condition(this, M, Address);
}
CallInst *IRBuilderBase::CreateIntMinReduce(Value *Src, bool IsSigned) {
  auto ID =
      IsSigned ? Intrinsic::vector_reduce_smin : Intrinsic::vector_reduce_umin;
  return getReductionIntrinsic(this, ID, Src);
}

CallInst *IRBuilderBase::CreateFPMaxReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_fmax, Src);
}

CallInst *IRBuilderBase::CreateFPMinReduce(Value *Src) {
  return getReductionIntrinsic(this, Intrinsic::vector_reduce_fmin, Src);
}

CallInst *IRBuilderBase::CreateLifetimeStart(Value *Ptr, ConstantInt *Size) {
  assert(isa<PointerType>(Ptr->getType()) &&
         "lifetime.start only applies to pointers.");
  Ptr = getCastedInt8PtrValue(Ptr);
  if (!Size)
    Size = getInt64(-1);
  else
    assert(Size->getType() == getInt64Ty() &&
           "lifetime.start requires the size to be an i64");
  Value *Ops[] = { Size, Ptr };
  Module *M = BB->getParent()->getParent();
  Function *TheFn =
      Intrinsic::getDeclaration(M, Intrinsic::lifetime_start, {Ptr->getType()});
  return createCallHelper(TheFn, Ops, this);
}

CallInst *IRBuilderBase::CreateLifetimeEnd(Value *Ptr, ConstantInt *Size) {
  assert(isa<PointerType>(Ptr->getType()) &&
         "lifetime.end only applies to pointers.");
  Ptr = getCastedInt8PtrValue(Ptr);
  if (!Size)
    Size = getInt64(-1);
  else
    assert(Size->getType() == getInt64Ty() &&
           "lifetime.end requires the size to be an i64");
  Value *Ops[] = { Size, Ptr };
  Module *M = BB->getParent()->getParent();
  Function *TheFn =
      Intrinsic::getDeclaration(M, Intrinsic::lifetime_end, {Ptr->getType()});
  return createCallHelper(TheFn, Ops, this);
}

CallInst *IRBuilderBase::CreateInvariantStart(Value *Ptr, ConstantInt *Size) {

  assert(isa<PointerType>(Ptr->getType()) &&
         "invariant.start only applies to pointers.");
  Ptr = getCastedInt8PtrValue(Ptr);
  if (!Size)
    Size = getInt64(-1);
  else
    assert(Size->getType() == getInt64Ty() &&
           "invariant.start requires the size to be an i64");

  Value *Ops[] = {Size, Ptr};
  // Fill in the single overloaded type: memory object type.
  Type *ObjectPtr[1] = {Ptr->getType()};
  Module *M = BB->getParent()->getParent();
  Function *TheFn =
      Intrinsic::getDeclaration(M, Intrinsic::invariant_start, ObjectPtr);
  return createCallHelper(TheFn, Ops, this);
}

CallInst *
IRBuilderBase::CreateAssumption(Value *Cond,
                                ArrayRef<OperandBundleDef> OpBundles) {
  assert(Cond->getType() == getInt1Ty() &&
         "an assumption condition must be of type i1");

  Value *Ops[] = { Cond };
  Module *M = BB->getParent()->getParent();
  Function *FnAssume = Intrinsic::getDeclaration(M, Intrinsic::assume);
  return createCallHelper(FnAssume, Ops, this, "", nullptr, OpBundles);
}

Instruction *IRBuilderBase::CreateNoAliasScopeDeclaration(Value *Scope) {
  Module *M = BB->getModule();
  auto *FnIntrinsic = Intrinsic::getDeclaration(
      M, Intrinsic::experimental_noalias_scope_decl, {});
  return createCallHelper(FnIntrinsic, {Scope}, this);
}

/// Create a call to a Masked Load intrinsic.
/// \p Ptr       - base pointer for the load
/// \p Alignment - alignment of the source location
/// \p Mask      - vector of booleans which indicates what vector lanes should
///                be accessed in memory
/// \p PassThru  - pass-through value that is used to fill the masked-off lanes
///                of the result
/// \p Name      - name of the result variable
CallInst *IRBuilderBase::CreateMaskedLoad(Value *Ptr, Align Alignment,
                                          Value *Mask, Value *PassThru,
                                          const Twine &Name) {
  auto *PtrTy = cast<PointerType>(Ptr->getType());
  Type *DataTy = PtrTy->getElementType();
  assert(DataTy->isVectorTy() && "Ptr should point to a vector");
  assert(Mask && "Mask should not be all-ones (null)");
  if (!PassThru)
    PassThru = UndefValue::get(DataTy);
  Type *OverloadedTypes[] = { DataTy, PtrTy };
  Value *Ops[] = {Ptr, getInt32(Alignment.value()), Mask, PassThru};
  return CreateMaskedIntrinsic(Intrinsic::masked_load, Ops,
                               OverloadedTypes, Name);
}

/// Create a call to a Masked Store intrinsic.
/// \p Val       - data to be stored,
/// \p Ptr       - base pointer for the store
/// \p Alignment - alignment of the destination location
/// \p Mask      - vector of booleans which indicates what vector lanes should
///                be accessed in memory
CallInst *IRBuilderBase::CreateMaskedStore(Value *Val, Value *Ptr,
                                           Align Alignment, Value *Mask) {
  auto *PtrTy = cast<PointerType>(Ptr->getType());
  Type *DataTy = PtrTy->getElementType();
  assert(DataTy->isVectorTy() && "Ptr should point to a vector");
  assert(Mask && "Mask should not be all-ones (null)");
  Type *OverloadedTypes[] = { DataTy, PtrTy };
  Value *Ops[] = {Val, Ptr, getInt32(Alignment.value()), Mask};
  return CreateMaskedIntrinsic(Intrinsic::masked_store, Ops, OverloadedTypes);
}

/// Create a call to a Masked intrinsic, with given intrinsic Id,
/// an array of operands - Ops, and an array of overloaded types -
/// OverloadedTypes.
CallInst *IRBuilderBase::CreateMaskedIntrinsic(Intrinsic::ID Id,
                                               ArrayRef<Value *> Ops,
                                               ArrayRef<Type *> OverloadedTypes,
                                               const Twine &Name) {
  Module *M = BB->getParent()->getParent();
  Function *TheFn = Intrinsic::getDeclaration(M, Id, OverloadedTypes);
  return createCallHelper(TheFn, Ops, this, Name);
}

/// Create a call to a Masked Gather intrinsic.
/// \p Ptrs     - vector of pointers for loading
/// \p Align    - alignment for one element
/// \p Mask     - vector of booleans which indicates what vector lanes should
///               be accessed in memory
/// \p PassThru - pass-through value that is used to fill the masked-off lanes
///               of the result
/// \p Name     - name of the result variable
CallInst *IRBuilderBase::CreateMaskedGather(Value *Ptrs, Align Alignment,
                                            Value *Mask, Value *PassThru,
                                            const Twine &Name) {
  auto *PtrsTy = cast<FixedVectorType>(Ptrs->getType());
  auto *PtrTy = cast<PointerType>(PtrsTy->getElementType());
  unsigned NumElts = PtrsTy->getNumElements();
  auto *DataTy = FixedVectorType::get(PtrTy->getElementType(), NumElts);

  if (!Mask)
    Mask = Constant::getAllOnesValue(
        FixedVectorType::get(Type::getInt1Ty(Context), NumElts));

  if (!PassThru)
    PassThru = UndefValue::get(DataTy);

  Type *OverloadedTypes[] = {DataTy, PtrsTy};
  Value *Ops[] = {Ptrs, getInt32(Alignment.value()), Mask, PassThru};

  // We specify only one type when we create this intrinsic. Types of other
  // arguments are derived from this type.
  return CreateMaskedIntrinsic(Intrinsic::masked_gather, Ops, OverloadedTypes,
                               Name);
}

/// Create a call to a Masked Scatter intrinsic.
/// \p Data  - data to be stored,
/// \p Ptrs  - the vector of pointers, where the \p Data elements should be
///            stored
/// \p Align - alignment for one element
/// \p Mask  - vector of booleans which indicates what vector lanes should
///            be accessed in memory
CallInst *IRBuilderBase::CreateMaskedScatter(Value *Data, Value *Ptrs,
                                             Align Alignment, Value *Mask) {
  auto *PtrsTy = cast<FixedVectorType>(Ptrs->getType());
  auto *DataTy = cast<FixedVectorType>(Data->getType());
  unsigned NumElts = PtrsTy->getNumElements();

#ifndef NDEBUG
  auto PtrTy = cast<PointerType>(PtrsTy->getElementType());
  assert(NumElts == DataTy->getNumElements() &&
         PtrTy->getElementType() == DataTy->getElementType() &&
         "Incompatible pointer and data types");
#endif

  if (!Mask)
    Mask = Constant::getAllOnesValue(
        FixedVectorType::get(Type::getInt1Ty(Context), NumElts));

  Type *OverloadedTypes[] = {DataTy, PtrsTy};
  Value *Ops[] = {Data, Ptrs, getInt32(Alignment.value()), Mask};

  // We specify only one type when we create this intrinsic. Types of other
  // arguments are derived from this type.
  return CreateMaskedIntrinsic(Intrinsic::masked_scatter, Ops, OverloadedTypes);
}

template <typename T0>
static std::vector<Value *>
getStatepointArgs(IRBuilderBase &B, uint64_t ID, uint32_t NumPatchBytes,
                  Value *ActualCallee, uint32_t Flags, ArrayRef<T0> CallArgs) {
  std::vector<Value *> Args;
  Args.push_back(B.getInt64(ID));
  Args.push_back(B.getInt32(NumPatchBytes));
  Args.push_back(ActualCallee);
  Args.push_back(B.getInt32(CallArgs.size()));
  Args.push_back(B.getInt32(Flags));
  llvm::append_range(Args, CallArgs);
  // GC Transition and Deopt args are now always handled via operand bundle.
  // They will be removed from the signature of gc.statepoint shortly.
  Args.push_back(B.getInt32(0));
  Args.push_back(B.getInt32(0));
  // GC args are now encoded in the gc-live operand bundle
  return Args;
}

template<typename T1, typename T2, typename T3>
static std::vector<OperandBundleDef>
getStatepointBundles(Optional<ArrayRef<T1>> TransitionArgs,
                     Optional<ArrayRef<T2>> DeoptArgs,
                     ArrayRef<T3> GCArgs) {
  std::vector<OperandBundleDef> Rval;
  if (DeoptArgs) {
    SmallVector<Value*, 16> DeoptValues;
    llvm::append_range(DeoptValues, *DeoptArgs);
    Rval.emplace_back("deopt", DeoptValues);
  }
  if (TransitionArgs) {
    SmallVector<Value*, 16> TransitionValues;
    llvm::append_range(TransitionValues, *TransitionArgs);
    Rval.emplace_back("gc-transition", TransitionValues);
  }
  if (GCArgs.size()) {
    SmallVector<Value*, 16> LiveValues;
    llvm::append_range(LiveValues, GCArgs);
    Rval.emplace_back("gc-live", LiveValues);
  }
  return Rval;
}

template <typename T0, typename T1, typename T2, typename T3>
static CallInst *CreateGCStatepointCallCommon(
    IRBuilderBase *Builder, uint64_t ID, uint32_t NumPatchBytes,
    Value *ActualCallee, uint32_t Flags, ArrayRef<T0> CallArgs,
    Optional<ArrayRef<T1>> TransitionArgs,
    Optional<ArrayRef<T2>> DeoptArgs, ArrayRef<T3> GCArgs,
    const Twine &Name) {
  // Extract out the type of the callee.
  auto *FuncPtrType = cast<PointerType>(ActualCallee->getType());
  assert(isa<FunctionType>(FuncPtrType->getElementType()) &&
         "actual callee must be a callable value");

  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  // Fill in the one generic type'd argument (the function is also vararg)
  Type *ArgTypes[] = { FuncPtrType };
  Function *FnStatepoint =
    Intrinsic::getDeclaration(M, Intrinsic::experimental_gc_statepoint,
                              ArgTypes);

  std::vector<Value *> Args =
      getStatepointArgs(*Builder, ID, NumPatchBytes, ActualCallee, Flags,
                        CallArgs);

  return Builder->CreateCall(FnStatepoint, Args,
                             getStatepointBundles(TransitionArgs, DeoptArgs,
                                                  GCArgs),
                             Name);
}

CallInst *IRBuilderBase::CreateGCStatepointCall(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualCallee,
    ArrayRef<Value *> CallArgs, Optional<ArrayRef<Value *>> DeoptArgs,
    ArrayRef<Value *> GCArgs, const Twine &Name) {
  return CreateGCStatepointCallCommon<Value *, Value *, Value *, Value *>(
      this, ID, NumPatchBytes, ActualCallee, uint32_t(StatepointFlags::None),
      CallArgs, None /* No Transition Args */, DeoptArgs, GCArgs, Name);
}

CallInst *IRBuilderBase::CreateGCStatepointCall(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualCallee, uint32_t Flags,
    ArrayRef<Value *> CallArgs, Optional<ArrayRef<Use>> TransitionArgs,
    Optional<ArrayRef<Use>> DeoptArgs, ArrayRef<Value *> GCArgs,
    const Twine &Name) {
  return CreateGCStatepointCallCommon<Value *, Use, Use, Value *>(
      this, ID, NumPatchBytes, ActualCallee, Flags, CallArgs, TransitionArgs,
      DeoptArgs, GCArgs, Name);
}

CallInst *IRBuilderBase::CreateGCStatepointCall(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualCallee,
    ArrayRef<Use> CallArgs, Optional<ArrayRef<Value *>> DeoptArgs,
    ArrayRef<Value *> GCArgs, const Twine &Name) {
  return CreateGCStatepointCallCommon<Use, Value *, Value *, Value *>(
      this, ID, NumPatchBytes, ActualCallee, uint32_t(StatepointFlags::None),
      CallArgs, None, DeoptArgs, GCArgs, Name);
}

template <typename T0, typename T1, typename T2, typename T3>
static InvokeInst *CreateGCStatepointInvokeCommon(
    IRBuilderBase *Builder, uint64_t ID, uint32_t NumPatchBytes,
    Value *ActualInvokee, BasicBlock *NormalDest, BasicBlock *UnwindDest,
    uint32_t Flags, ArrayRef<T0> InvokeArgs,
    Optional<ArrayRef<T1>> TransitionArgs, Optional<ArrayRef<T2>> DeoptArgs,
    ArrayRef<T3> GCArgs, const Twine &Name) {
  // Extract out the type of the callee.
  auto *FuncPtrType = cast<PointerType>(ActualInvokee->getType());
  assert(isa<FunctionType>(FuncPtrType->getElementType()) &&
         "actual callee must be a callable value");

  Module *M = Builder->GetInsertBlock()->getParent()->getParent();
  // Fill in the one generic type'd argument (the function is also vararg)
  Function *FnStatepoint = Intrinsic::getDeclaration(
      M, Intrinsic::experimental_gc_statepoint, {FuncPtrType});

  std::vector<Value *> Args =
      getStatepointArgs(*Builder, ID, NumPatchBytes, ActualInvokee, Flags,
                        InvokeArgs);

  return Builder->CreateInvoke(FnStatepoint, NormalDest, UnwindDest, Args,
                               getStatepointBundles(TransitionArgs, DeoptArgs,
                                                    GCArgs),
                               Name);
}

InvokeInst *IRBuilderBase::CreateGCStatepointInvoke(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualInvokee,
    BasicBlock *NormalDest, BasicBlock *UnwindDest,
    ArrayRef<Value *> InvokeArgs, Optional<ArrayRef<Value *>> DeoptArgs,
    ArrayRef<Value *> GCArgs, const Twine &Name) {
  return CreateGCStatepointInvokeCommon<Value *, Value *, Value *, Value *>(
      this, ID, NumPatchBytes, ActualInvokee, NormalDest, UnwindDest,
      uint32_t(StatepointFlags::None), InvokeArgs, None /* No Transition Args*/,
      DeoptArgs, GCArgs, Name);
}

InvokeInst *IRBuilderBase::CreateGCStatepointInvoke(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualInvokee,
    BasicBlock *NormalDest, BasicBlock *UnwindDest, uint32_t Flags,
    ArrayRef<Value *> InvokeArgs, Optional<ArrayRef<Use>> TransitionArgs,
    Optional<ArrayRef<Use>> DeoptArgs, ArrayRef<Value *> GCArgs, const Twine &Name) {
  return CreateGCStatepointInvokeCommon<Value *, Use, Use, Value *>(
      this, ID, NumPatchBytes, ActualInvokee, NormalDest, UnwindDest, Flags,
      InvokeArgs, TransitionArgs, DeoptArgs, GCArgs, Name);
}

InvokeInst *IRBuilderBase::CreateGCStatepointInvoke(
    uint64_t ID, uint32_t NumPatchBytes, Value *ActualInvokee,
    BasicBlock *NormalDest, BasicBlock *UnwindDest, ArrayRef<Use> InvokeArgs,
    Optional<ArrayRef<Value *>> DeoptArgs, ArrayRef<Value *> GCArgs, const Twine &Name) {
  return CreateGCStatepointInvokeCommon<Use, Value *, Value *, Value *>(
      this, ID, NumPatchBytes, ActualInvokee, NormalDest, UnwindDest,
      uint32_t(StatepointFlags::None), InvokeArgs, None, DeoptArgs, GCArgs,
      Name);
}

CallInst *IRBuilderBase::CreateGCResult(Instruction *Statepoint,
                                       Type *ResultType,
                                       const Twine &Name) {
 Intrinsic::ID ID = Intrinsic::experimental_gc_result;
 Module *M = BB->getParent()->getParent();
 Type *Types[] = {ResultType};
 Function *FnGCResult = Intrinsic::getDeclaration(M, ID, Types);

 Value *Args[] = {Statepoint};
 return createCallHelper(FnGCResult, Args, this, Name);
}

CallInst *IRBuilderBase::CreateGCRelocate(Instruction *Statepoint,
                                         int BaseOffset,
                                         int DerivedOffset,
                                         Type *ResultType,
                                         const Twine &Name) {
 Module *M = BB->getParent()->getParent();
 Type *Types[] = {ResultType};
 Function *FnGCRelocate =
     Intrinsic::getDeclaration(M, Intrinsic::experimental_gc_relocate, Types);

 Value *Args[] = {Statepoint,
                  getInt32(BaseOffset),
                  getInt32(DerivedOffset)};
 return createCallHelper(FnGCRelocate, Args, this, Name);
}

CallInst *IRBuilderBase::CreateUnaryIntrinsic(Intrinsic::ID ID, Value *V,
                                              Instruction *FMFSource,
                                              const Twine &Name) {
  Module *M = BB->getModule();
  Function *Fn = Intrinsic::getDeclaration(M, ID, {V->getType()});
  return createCallHelper(Fn, {V}, this, Name, FMFSource);
}

CallInst *IRBuilderBase::CreateBinaryIntrinsic(Intrinsic::ID ID, Value *LHS,
                                               Value *RHS,
                                               Instruction *FMFSource,
                                               const Twine &Name) {
  Module *M = BB->getModule();
  Function *Fn = Intrinsic::getDeclaration(M, ID, { LHS->getType() });
  return createCallHelper(Fn, {LHS, RHS}, this, Name, FMFSource);
}

CallInst *IRBuilderBase::CreateIntrinsic(Intrinsic::ID ID,
                                         ArrayRef<Type *> Types,
                                         ArrayRef<Value *> Args,
                                         Instruction *FMFSource,
                                         const Twine &Name) {
  Module *M = BB->getModule();
  Function *Fn = Intrinsic::getDeclaration(M, ID, Types);
  return createCallHelper(Fn, Args, this, Name, FMFSource);
}

CallInst *IRBuilderBase::CreateConstrainedFPBinOp(
    Intrinsic::ID ID, Value *L, Value *R, Instruction *FMFSource,
    const Twine &Name, MDNode *FPMathTag,
    Optional<RoundingMode> Rounding,
    Optional<fp::ExceptionBehavior> Except) {
  Value *RoundingV = getConstrainedFPRounding(Rounding);
  Value *ExceptV = getConstrainedFPExcept(Except);

  FastMathFlags UseFMF = FMF;
  if (FMFSource)
    UseFMF = FMFSource->getFastMathFlags();

  CallInst *C = CreateIntrinsic(ID, {L->getType()},
                                {L, R, RoundingV, ExceptV}, nullptr, Name);
  setConstrainedFPCallAttr(C);
  setFPAttrs(C, FPMathTag, UseFMF);
  return C;
}

Value *IRBuilderBase::CreateNAryOp(unsigned Opc, ArrayRef<Value *> Ops,
                                   const Twine &Name, MDNode *FPMathTag) {
  if (Instruction::isBinaryOp(Opc)) {
    assert(Ops.size() == 2 && "Invalid number of operands!");
    return CreateBinOp(static_cast<Instruction::BinaryOps>(Opc),
                       Ops[0], Ops[1], Name, FPMathTag);
  }
  if (Instruction::isUnaryOp(Opc)) {
    assert(Ops.size() == 1 && "Invalid number of operands!");
    return CreateUnOp(static_cast<Instruction::UnaryOps>(Opc),
                      Ops[0], Name, FPMathTag);
  }
  llvm_unreachable("Unexpected opcode!");
}

CallInst *IRBuilderBase::CreateConstrainedFPCast(
    Intrinsic::ID ID, Value *V, Type *DestTy,
    Instruction *FMFSource, const Twine &Name, MDNode *FPMathTag,
    Optional<RoundingMode> Rounding,
    Optional<fp::ExceptionBehavior> Except) {
  Value *ExceptV = getConstrainedFPExcept(Except);

  FastMathFlags UseFMF = FMF;
  if (FMFSource)
    UseFMF = FMFSource->getFastMathFlags();

  CallInst *C;
  bool HasRoundingMD = false;
  switch (ID) {
  default:
    break;
#define INSTRUCTION(NAME, NARG, ROUND_MODE, INTRINSIC)        \
  case Intrinsic::INTRINSIC:                                \
    HasRoundingMD = ROUND_MODE;                             \
    break;
#include "llvm/IR/ConstrainedOps.def"
  }
  if (HasRoundingMD) {
    Value *RoundingV = getConstrainedFPRounding(Rounding);
    C = CreateIntrinsic(ID, {DestTy, V->getType()}, {V, RoundingV, ExceptV},
                        nullptr, Name);
  } else
    C = CreateIntrinsic(ID, {DestTy, V->getType()}, {V, ExceptV}, nullptr,
                        Name);

  setConstrainedFPCallAttr(C);

  if (isa<FPMathOperator>(C))
    setFPAttrs(C, FPMathTag, UseFMF);
  return C;
}

Value *IRBuilderBase::CreateFCmpHelper(
    CmpInst::Predicate P, Value *LHS, Value *RHS, const Twine &Name,
    MDNode *FPMathTag, bool IsSignaling) {
  if (IsFPConstrained) {
    auto ID = IsSignaling ? Intrinsic::experimental_constrained_fcmps
                          : Intrinsic::experimental_constrained_fcmp;
    return CreateConstrainedFPCmp(ID, P, LHS, RHS, Name);
  }

  if (auto *LC = dyn_cast<Constant>(LHS))
    if (auto *RC = dyn_cast<Constant>(RHS))
      return Insert(Folder.CreateFCmp(P, LC, RC), Name);
  return Insert(setFPAttrs(new FCmpInst(P, LHS, RHS), FPMathTag, FMF), Name);
}

CallInst *IRBuilderBase::CreateConstrainedFPCmp(
    Intrinsic::ID ID, CmpInst::Predicate P, Value *L, Value *R,
    const Twine &Name, Optional<fp::ExceptionBehavior> Except) {
  Value *PredicateV = getConstrainedFPPredicate(P);
  Value *ExceptV = getConstrainedFPExcept(Except);

  CallInst *C = CreateIntrinsic(ID, {L->getType()},
                                {L, R, PredicateV, ExceptV}, nullptr, Name);
  setConstrainedFPCallAttr(C);
  return C;
}

CallInst *IRBuilderBase::CreateConstrainedFPCall(
    Function *Callee, ArrayRef<Value *> Args, const Twine &Name,
    Optional<RoundingMode> Rounding,
    Optional<fp::ExceptionBehavior> Except) {
  llvm::SmallVector<Value *, 6> UseArgs;

  for (auto *OneArg : Args)
    UseArgs.push_back(OneArg);
  bool HasRoundingMD = false;
  switch (Callee->getIntrinsicID()) {
  default:
    break;
#define INSTRUCTION(NAME, NARG, ROUND_MODE, INTRINSIC)        \
  case Intrinsic::INTRINSIC:                                \
    HasRoundingMD = ROUND_MODE;                             \
    break;
#include "llvm/IR/ConstrainedOps.def"
  }
  if (HasRoundingMD)
    UseArgs.push_back(getConstrainedFPRounding(Rounding));
  UseArgs.push_back(getConstrainedFPExcept(Except));

  CallInst *C = CreateCall(Callee, UseArgs, Name);
  setConstrainedFPCallAttr(C);
  return C;
}

Value *IRBuilderBase::CreateSelect(Value *C, Value *True, Value *False,
                                   const Twine &Name, Instruction *MDFrom) {
  if (auto *CC = dyn_cast<Constant>(C))
    if (auto *TC = dyn_cast<Constant>(True))
      if (auto *FC = dyn_cast<Constant>(False))
        return Insert(Folder.CreateSelect(CC, TC, FC), Name);

  SelectInst *Sel = SelectInst::Create(C, True, False);
  if (MDFrom) {
    MDNode *Prof = MDFrom->getMetadata(LLVMContext::MD_prof);
    MDNode *Unpred = MDFrom->getMetadata(LLVMContext::MD_unpredictable);
    Sel = addBranchMetadata(Sel, Prof, Unpred);
  }
  if (isa<FPMathOperator>(Sel))
    setFPAttrs(Sel, nullptr /* MDNode* */, FMF);
  return Insert(Sel, Name);
}

Value *IRBuilderBase::CreatePtrDiff(Value *LHS, Value *RHS,
                                    const Twine &Name) {
  assert(LHS->getType() == RHS->getType() &&
         "Pointer subtraction operand types must match!");
  auto *ArgType = cast<PointerType>(LHS->getType());
  Value *LHS_int = CreatePtrToInt(LHS, Type::getInt64Ty(Context));
  Value *RHS_int = CreatePtrToInt(RHS, Type::getInt64Ty(Context));
  Value *Difference = CreateSub(LHS_int, RHS_int);
  return CreateExactSDiv(Difference,
                         ConstantExpr::getSizeOf(ArgType->getElementType()),
                         Name);
}

Value *IRBuilderBase::CreateLaunderInvariantGroup(Value *Ptr) {
  assert(isa<PointerType>(Ptr->getType()) &&
         "launder.invariant.group only applies to pointers.");
  // FIXME: we could potentially avoid casts to/from i8*.
  auto *PtrType = Ptr->getType();
  auto *Int8PtrTy = getInt8PtrTy(PtrType->getPointerAddressSpace());
  if (PtrType != Int8PtrTy)
    Ptr = CreateBitCast(Ptr, Int8PtrTy);
  Module *M = BB->getParent()->getParent();
  Function *FnLaunderInvariantGroup = Intrinsic::getDeclaration(
      M, Intrinsic::launder_invariant_group, {Int8PtrTy});

  assert(FnLaunderInvariantGroup->getReturnType() == Int8PtrTy &&
         FnLaunderInvariantGroup->getFunctionType()->getParamType(0) ==
             Int8PtrTy &&
         "LaunderInvariantGroup should take and return the same type");

  CallInst *Fn = CreateCall(FnLaunderInvariantGroup, {Ptr});

  if (PtrType != Int8PtrTy)
    return CreateBitCast(Fn, PtrType);
  return Fn;
}

Value *IRBuilderBase::CreateStripInvariantGroup(Value *Ptr) {
  assert(isa<PointerType>(Ptr->getType()) &&
         "strip.invariant.group only applies to pointers.");

  // FIXME: we could potentially avoid casts to/from i8*.
  auto *PtrType = Ptr->getType();
  auto *Int8PtrTy = getInt8PtrTy(PtrType->getPointerAddressSpace());
  if (PtrType != Int8PtrTy)
    Ptr = CreateBitCast(Ptr, Int8PtrTy);
  Module *M = BB->getParent()->getParent();
  Function *FnStripInvariantGroup = Intrinsic::getDeclaration(
      M, Intrinsic::strip_invariant_group, {Int8PtrTy});

  assert(FnStripInvariantGroup->getReturnType() == Int8PtrTy &&
         FnStripInvariantGroup->getFunctionType()->getParamType(0) ==
             Int8PtrTy &&
         "StripInvariantGroup should take and return the same type");

  CallInst *Fn = CreateCall(FnStripInvariantGroup, {Ptr});

  if (PtrType != Int8PtrTy)
    return CreateBitCast(Fn, PtrType);
  return Fn;
}

Value *IRBuilderBase::CreateVectorSplat(unsigned NumElts, Value *V,
                                        const Twine &Name) {
  auto EC = ElementCount::getFixed(NumElts);
  return CreateVectorSplat(EC, V, Name);
}

Value *IRBuilderBase::CreateVectorSplat(ElementCount EC, Value *V,
                                        const Twine &Name) {
  assert(EC.isNonZero() && "Cannot splat to an empty vector!");

  // First insert it into a poison vector so we can shuffle it.
  Type *I32Ty = getInt32Ty();
  Value *Poison = PoisonValue::get(VectorType::get(V->getType(), EC));
  V = CreateInsertElement(Poison, V, ConstantInt::get(I32Ty, 0),
                          Name + ".splatinsert");

  // Shuffle the value across the desired number of elements.
  SmallVector<int, 16> Zeros;
  Zeros.resize(EC.getKnownMinValue());
  return CreateShuffleVector(V, Zeros, Name + ".splat");
}

Value *IRBuilderBase::CreateExtractInteger(
    const DataLayout &DL, Value *From, IntegerType *ExtractedTy,
    uint64_t Offset, const Twine &Name) {
  auto *IntTy = cast<IntegerType>(From->getType());
  assert(DL.getTypeStoreSize(ExtractedTy) + Offset <=
             DL.getTypeStoreSize(IntTy) &&
         "Element extends past full value");
  uint64_t ShAmt = 8 * Offset;
  Value *V = From;
  if (DL.isBigEndian())
    ShAmt = 8 * (DL.getTypeStoreSize(IntTy) -
                 DL.getTypeStoreSize(ExtractedTy) - Offset);
  if (ShAmt) {
    V = CreateLShr(V, ShAmt, Name + ".shift");
  }
  assert(ExtractedTy->getBitWidth() <= IntTy->getBitWidth() &&
         "Cannot extract to a larger integer!");
  if (ExtractedTy != IntTy) {
    V = CreateTrunc(V, ExtractedTy, Name + ".trunc");
  }
  return V;
}

Value *IRBuilderBase::CreatePreserveArrayAccessIndex(
    Type *ElTy, Value *Base, unsigned Dimension, unsigned LastIndex,
    MDNode *DbgInfo) {
  assert(isa<PointerType>(Base->getType()) &&
         "Invalid Base ptr type for preserve.array.access.index.");
  auto *BaseType = Base->getType();

  Value *LastIndexV = getInt32(LastIndex);
  Constant *Zero = ConstantInt::get(Type::getInt32Ty(Context), 0);
  SmallVector<Value *, 4> IdxList(Dimension, Zero);
  IdxList.push_back(LastIndexV);

  Type *ResultType =
      GetElementPtrInst::getGEPReturnType(ElTy, Base, IdxList);

  Module *M = BB->getParent()->getParent();
  Function *FnPreserveArrayAccessIndex = Intrinsic::getDeclaration(
      M, Intrinsic::preserve_array_access_index, {ResultType, BaseType});

  Value *DimV = getInt32(Dimension);
  CallInst *Fn =
      CreateCall(FnPreserveArrayAccessIndex, {Base, DimV, LastIndexV});
  if (DbgInfo)
    Fn->setMetadata(LLVMContext::MD_preserve_access_index, DbgInfo);

  return Fn;
}

Value *IRBuilderBase::CreatePreserveUnionAccessIndex(
    Value *Base, unsigned FieldIndex, MDNode *DbgInfo) {
  assert(isa<PointerType>(Base->getType()) &&
         "Invalid Base ptr type for preserve.union.access.index.");
  auto *BaseType = Base->getType();

  Module *M = BB->getParent()->getParent();
  Function *FnPreserveUnionAccessIndex = Intrinsic::getDeclaration(
      M, Intrinsic::preserve_union_access_index, {BaseType, BaseType});

  Value *DIIndex = getInt32(FieldIndex);
  CallInst *Fn =
      CreateCall(FnPreserveUnionAccessIndex, {Base, DIIndex});
  if (DbgInfo)
    Fn->setMetadata(LLVMContext::MD_preserve_access_index, DbgInfo);

  return Fn;
}

Value *IRBuilderBase::CreatePreserveStructAccessIndex(
    Type *ElTy, Value *Base, unsigned Index, unsigned FieldIndex,
    MDNode *DbgInfo) {
  assert(isa<PointerType>(Base->getType()) &&
         "Invalid Base ptr type for preserve.struct.access.index.");
  auto *BaseType = Base->getType();

  Value *GEPIndex = getInt32(Index);
  Constant *Zero = ConstantInt::get(Type::getInt32Ty(Context), 0);
  Type *ResultType =
      GetElementPtrInst::getGEPReturnType(ElTy, Base, {Zero, GEPIndex});

  Module *M = BB->getParent()->getParent();
  Function *FnPreserveStructAccessIndex = Intrinsic::getDeclaration(
      M, Intrinsic::preserve_struct_access_index, {ResultType, BaseType});

  Value *DIIndex = getInt32(FieldIndex);
  CallInst *Fn = CreateCall(FnPreserveStructAccessIndex,
                            {Base, GEPIndex, DIIndex});
  if (DbgInfo)
    Fn->setMetadata(LLVMContext::MD_preserve_access_index, DbgInfo);

  return Fn;
}

CallInst *IRBuilderBase::CreateAlignmentAssumptionHelper(const DataLayout &DL,
                                                         Value *PtrValue,
                                                         Value *AlignValue,
                                                         Value *OffsetValue) {
  SmallVector<Value *, 4> Vals({PtrValue, AlignValue});
  if (OffsetValue)
    Vals.push_back(OffsetValue);
  OperandBundleDefT<Value *> AlignOpB("align", Vals);
  return CreateAssumption(ConstantInt::getTrue(getContext()), {AlignOpB});
}

CallInst *IRBuilderBase::CreateAlignmentAssumption(const DataLayout &DL,
                                                   Value *PtrValue,
                                                   unsigned Alignment,
                                                   Value *OffsetValue) {
  assert(isa<PointerType>(PtrValue->getType()) &&
         "trying to create an alignment assumption on a non-pointer?");
  assert(Alignment != 0 && "Invalid Alignment");
  auto *PtrTy = cast<PointerType>(PtrValue->getType());
  Type *IntPtrTy = getIntPtrTy(DL, PtrTy->getAddressSpace());
  Value *AlignValue = ConstantInt::get(IntPtrTy, Alignment);
  return CreateAlignmentAssumptionHelper(DL, PtrValue, AlignValue, OffsetValue);
}

CallInst *IRBuilderBase::CreateAlignmentAssumption(const DataLayout &DL,
                                                   Value *PtrValue,
                                                   Value *Alignment,
                                                   Value *OffsetValue) {
  assert(isa<PointerType>(PtrValue->getType()) &&
         "trying to create an alignment assumption on a non-pointer?");
  return CreateAlignmentAssumptionHelper(DL, PtrValue, Alignment, OffsetValue);
}
Value *IRBuilderBase::CreateCondlTaintedO2Ptr(Value *pValue) {
  return CreateCondlTaintedO2PtrInternal(this, pValue);
}

Value *IRBuilderBase::CreatePToO(Value *pValue) {
  if(!pValue->getType()->getCoreElementType()->isVoidTy())
  {
    //cast it to void* pointer
    pValue = CreateBitCast(pValue,Type::getInt8PtrTy(this->getContext()));
  }
  return CreateCondlTaintedPToOInternal(this, pValue);
}


IRBuilderDefaultInserter::~IRBuilderDefaultInserter() {}
IRBuilderCallbackInserter::~IRBuilderCallbackInserter() {}
IRBuilderFolder::~IRBuilderFolder() {}
void ConstantFolder::anchor() {}
void NoFolder::anchor() {}
