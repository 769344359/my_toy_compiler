#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include "types.h"
#include <stdarg.h>

using namespace std;

static const StructType *ObjectType;
static const StructType *StringType;
static const PointerType *ObjectType_p;
static const PointerType *StringType_p;

static const PointerType *GenericPointerType = Type::getInt8PtrTy(getGlobalContext(), 0); // PointerType::get(Type::getVoidTy(getGlobalContext()), 0);

object (*objalloc)() = NULL;

const StructType* CodeGenContext::addStructType(std::string name, size_t numArgs, ...)
{
	std::vector<const Type*> fields;
	PATypeHolder fwdType = OpaqueType::get(getGlobalContext());
	fields.push_back(PointerType::get(fwdType, 0));
	fields.push_back(GenericPointerType);

	va_list list;
	va_start(list, numArgs);
	for (int i = 0; i < numArgs; i++) {
		fields.push_back(va_arg(list, const Type*));
	}
	va_end(list);

	StructType *stype = StructType::get(getGlobalContext(), fields, false);
	cast<OpaqueType>(fwdType.get())->refineAbstractTypeTo(stype);
	module->addTypeName(name, stype);
	return cast<StructType>(fwdType.get());
}

FunctionType* CodeGenContext::functionType(const Type* retType, bool varargs, size_t numArgs, ...)
{
	std::vector<const Type*> args;
	va_list list;
	va_start(list, numArgs);
	for (int i = 0; i < numArgs; i++) {
		args.push_back(va_arg(list, const Type*));
	}
	va_end(list);
	
	return FunctionType::get(retType, args, varargs);
}

Function* CodeGenContext::addExternalFunction(char *name, FunctionType *fType)
{
	Function *f = Function::Create(fType, GlobalValue::ExternalLinkage, name, module);
	f->setCallingConv(CallingConv::C);
	return f;
}

Function* CodeGenContext::addFunction(char *name, FunctionType *fType, void (^block)(BasicBlock *))
{
	Function *f = Function::Create(fType, GlobalValue::InternalLinkage, name, module);
	f->setCallingConv(CallingConv::C);
	if (block) block(BasicBlock::Create(getGlobalContext(), "entry", f, 0));
	return f;
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	ObjectType = addStructType("object", 1, GenericPointerType);
	ObjectType_p = PointerType::get(ObjectType, 0);
	StringType = addStructType("string", 3, GenericPointerType, GenericPointerType, Type::getInt64Ty(getGlobalContext()));
	StringType_p = PointerType::get(ObjectType, 0);
	
	/* Create objalloc function */
	objallocFunction = addFunction("objalloc", functionType(ObjectType_p, false, 0), ^(BasicBlock *blk) {
    ConstantInt *val_mem = ConstantInt::get(getGlobalContext(), APInt(32, 1));
    const Type* IntPtrTy = IntegerType::getInt32Ty(getGlobalContext());
    Constant* allocsize = ConstantExpr::getSizeOf(ObjectType);
    allocsize = ConstantExpr::getTruncOrBitCast(allocsize, ObjectType);
		ReturnInst::Create(getGlobalContext(), CallInst::CreateMalloc(blk, IntPtrTy, ObjectType, allocsize, val_mem, 
              NULL, "obj"), blk);
	});
	
	/* Create refs to putSlot, getSlot and newobj */
	putSlotFunction = addExternalFunction("putSlot", 
		functionType(Type::getVoidTy(getGlobalContext()), false, 3, ObjectType_p, GenericPointerType, ObjectType_p));
	getSlotFunction = addExternalFunction("getSlot", 
		functionType(ObjectType_p, false, 3, ObjectType_p, GenericPointerType, Type::getInt32Ty(getGlobalContext())));
	newobjFunction = addExternalFunction("newobj", 
		functionType(ObjectType_p, false, 1, ObjectType_p));
	
	/* Create the top level interpreter function to call as entry */
	vector<const Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(getGlobalContext()), argTypes, false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	cObject = new GlobalVariable(ObjectType, true, 
		GlobalValue::InternalLinkage, 0, "class.Object", module);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(getGlobalContext(), bblock);
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager pm;
	pm.add(createPrintModulePass(&outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	//ExistingModuleProvider *mp = new ExistingModuleProvider(module);
	ExecutionEngine *ee = ExecutionEngine::create(module, false);
	objalloc = (object (*)())ee->getPointerToFunction(objallocFunction);
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static const Type *typeOf(const NIdentifier& type) 
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(getGlobalContext());
	}
	else if (type.name.compare("double") == 0) {
		return Type::getFP128Ty(getGlobalContext());
	}
	else if (type.name.compare("object") == 0) {
		return ObjectType_p;
	}
	return Type::getVoidTy(getGlobalContext());
}

static Value* getStringConstant(const string& str, CodeGenContext& context)
{
	Constant *n = ConstantArray::get(getGlobalContext(), str.c_str(), true);
	GlobalVariable *g = new GlobalVariable(n->getType(), true, 
		GlobalValue::InternalLinkage, 0, str.c_str(), context.module);
	g->setInitializer(n);
	std::vector<Constant*> ptr;
	ptr.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
	ptr.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
	return ConstantExpr::getGetElementPtr(g, &ptr[0], ptr.size());
}

static Value* resolveReference(NReference& ref, CodeGenContext& context, bool ignoreLast = false)
{
	Value *curValue = ref.refs.front()->codeGen(context);
	IdentifierList::const_iterator it;
	for (it = ref.refs.begin() + 1; it != ref.refs.end(); it++) {
		NIdentifier& ident = **it;
		if (ignoreLast && it == ref.refs.end() - 1) return curValue;
		
		std::cout << "Next ident " << ident.name << endl;
		Value *ch = getStringConstant(ident.name, context);
		
		std::vector<Value*> params;
		params.push_back(curValue);
		params.push_back(ch);
		params.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1));
		std::cout << "About to call " << typeid(curValue).name() << " :: " << typeid(ch).name() << std::endl;
		CallInst *call = CallInst::Create(context.getSlotFunction, 
			params.begin(), params.end(), "", context.currentBlock());
		call->setCallingConv(CallingConv::C);
		curValue = call;
	}
	return curValue;
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(getGlobalContext()), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getFP128Ty(getGlobalContext()), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	std::cout << "Creating identifier: " << name << endl;
	if (name.compare("null") == 0) {
		return ConstantPointerNull::get(ObjectType_p);
	}
	if (context.locals().find(name) == context.locals().end()) {
		std::cout << "Instantiating object " << name << endl;
		std::vector<Value*> args;
		args.push_back(ConstantPointerNull::get(ObjectType_p));
		CallInst *call = CallInst::Create(context.newobjFunction, args.begin(), 
			args.end(), "", context.currentBlock());
		return context.locals()[name] = call;
	}
	return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NString::codeGen(CodeGenContext& context)
{
	std::cout << "Creating string: " << value << endl;
	std::vector<Value*> args;
	args.push_back(ConstantPointerNull::get(ObjectType_p));
	return CallInst::Create(context.newobjFunction, args.begin(), 
		args.end(), "", context.currentBlock());
}

Value* NReference::codeGen(CodeGenContext& context)
{
	if (refs.size() == 1) {
		NIdentifier *ident = refs.front();
		std::cout << "Creating reference: " << ident->name << endl;
		return ident->codeGen(context);
	}

	std::cout << "Creating reference" << endl;
	return resolveReference(*this, context);
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	NIdentifier& id = *ref.refs.front();
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, args.begin(), args.end(), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;
				
		/* TODO comparison */
	}

	return NULL;
math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	std::cout << "Creating assignment " << endl;
	if (lhs.refs.size() == 1) {
		return new StoreInst(rhs.codeGen(context), context.locals()[lhs.refs.front()->name], false, context.currentBlock());
	}
	else {
		Value *value = resolveReference(lhs, context, true);
		Value *sym = getStringConstant(lhs.refs.back()->name, context);
		
		std::vector<Value*> params;
		params.push_back(value);
		params.push_back(sym);
		params.push_back(rhs.codeGen(context));
		CallInst *call = CallInst::Create(context.putSlotFunction, 
			params.begin(), params.end(), "", context.currentBlock());
		call->setCallingConv(CallingConv::C);
		return call;
	}
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
	AllocaInst *alloc = new AllocaInst(typeOf(type), id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;
	if (assignmentExpr != NULL) {
		NReference ref(id);
		NAssignment assn(ref, *assignmentExpr);
		assn.codeGen(context);
	}
	return alloc;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	std::cout << id.name << endl;
	vector<const Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type), argTypes, false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", function, 0);

	context.pushBlock(bblock);

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
	}
	
	block.codeGen(context);
	ReturnInst::Create(getGlobalContext(), bblock);

	context.popBlock();
	std::cout << "Creating function: " << id.name << endl;
	return function;
}

