
#include "llvm/ADT/SmallString.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileUtilities.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/SubtargetFeature.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Scalar.h"

#include <memory>
#include <string>

#include "Tolerator.h"

#include "config.h"

using namespace llvm;
using std::string;
using std::unique_ptr;
using std::vector;
using llvm::sys::ExecuteAndWait;
using llvm::sys::findProgramByName;
using llvm::legacy::PassManager;
using tolerator::AnalysisType;


static cl::OptionCategory toleratorCategory{"tolerator options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{toleratorCategory}};

static cl::opt<AnalysisType> analysisType{
    cl::desc{"Select analyis type:"},
    cl::values(clEnumValN(AnalysisType::LOGGING,
                          "log",
                          "Log failures and terminate."),
               clEnumValN(AnalysisType::IGNORING,
                          "ignore",
                          "Ignore failures without values."),
               clEnumValN(AnalysisType::DEFAULTING,
                          "defaults",
                          "Yield default values for failures"),
               clEnumValN(AnalysisType::BYPASSING,
                          "returns",
                          "Returns from functions with failures")
               ),
    cl::init(AnalysisType::LOGGING),
    cl::cat{toleratorCategory}};

static cl::opt<string> outFile{"o",
                               cl::desc{"Filename of the instrumented program"},
                               cl::value_desc{"filename"},
                               cl::init(""),
                               cl::cat{toleratorCategory}};

static cl::opt<char> optLevel{
    "O",
    cl::desc{"Optimization level. [-O0, -O1, -O2, or -O3] (default = '-O2')"},
    cl::Prefix,
    cl::ZeroOrMore,
    cl::init('2'),
    cl::cat{toleratorCategory}};

static cl::list<string> libPaths{"L",
                                 cl::Prefix,
                                 cl::desc{"Specify a library search path"},
                                 cl::value_desc{"directory"},
                                 cl::cat{toleratorCategory}};

static cl::list<string> libraries{"l",
                                  cl::Prefix,
                                  cl::desc{"Specify libraries to link against"},
                                  cl::value_desc{"library prefix"},
                                  cl::cat{toleratorCategory}};


static cl::list<string> extras{"e",
                               cl::Prefix,
                               cl::desc{"Specify extra object files to link in"},
                               cl::value_desc{"object files"},
                               cl::cat{toleratorCategory}};


// Make sure that compilation options are enabled when the program loads.
static codegen::RegisterCodeGenFlags cfg;


static void
compile(Module& m, StringRef outputPath) {
  string err;

  Triple triple        = Triple(m.getTargetTriple());
  Target const* target = TargetRegistry::lookupTarget(codegen::getMArch(), triple, err);
  if (!target) {
    report_fatal_error(Twine{"Unable to find target:\n " + err});
  }

  CodeGenOptLevel level = CodeGenOptLevel::Default;
  switch (optLevel) {
    default:
      report_fatal_error("Invalid optimization level.\n");
    // No fall through
    case '0': level = CodeGenOptLevel::None; break;
    case '1': level = CodeGenOptLevel::Less; break;
    case '2': level = CodeGenOptLevel::Default; break;
    case '3': level = CodeGenOptLevel::Aggressive; break;
  }

  string FeaturesStr;
  TargetOptions options = codegen::InitTargetOptionsFromCodeGenFlags(triple);
  auto relocationModel = codegen::getExplicitRelocModel();
  auto codeModel = llvm::codegen::getExplicitCodeModel();
  if (!relocationModel) {
    // Modern distriutions default to PIC, so override if not set.
    // TODO: Recheck defaults in next LLVM version.
    relocationModel = llvm::Reloc::Model::PIC_;
  }

  unique_ptr<TargetMachine> machine(
      target->createTargetMachine(triple.getTriple(),
                                  codegen::getCPUStr(),
                                  codegen::getFeaturesStr(),
                                  options,
                                  relocationModel,
                                  codeModel,
                                  level));
  assert(machine && "Could not allocate target machine!");

  if (auto floatABI = codegen::getFloatABIForCalls(); floatABI != FloatABI::Default) {
    options.FloatABIType = floatABI;
  }

  std::error_code errc;
  auto out =
      std::make_unique<ToolOutputFile>(outputPath, errc, sys::fs::OF_None);
  if (!out) {
    report_fatal_error(Twine{"Unable to create file:\n " + errc.message()});
  }

  // Build up all of the passes that we want to do to the module.
  legacy::PassManager pm;

  // Add target specific info and transforms
  TargetLibraryInfoImpl tlii(triple);
  pm.add(new TargetLibraryInfoWrapperPass(tlii));

  m.setDataLayout(machine->createDataLayout());

  {  // Bound this scope
    raw_pwrite_stream* os(&out->os());

    std::unique_ptr<buffer_ostream> bos;
    if (!out->os().supportsSeeking()) {
      bos = std::make_unique<buffer_ostream>(*os);
      os  = bos.get();
    }

    // Ask the target to add backend passes as necessary.
    if (machine->addPassesToEmitFile(pm, *os, nullptr, CodeGenFileType::ObjectFile)) {
      report_fatal_error("target does not support generation "
                         "of this file type!\n");
    }

    // Before executing passes, print the final values of the LLVM options.
    cl::PrintOptionValues();

    pm.run(m);
  }

  // Keep the output binary if we've been successful to this point.
  out->keep();
}


static void
link(StringRef objectFile, StringRef outputFile) {
  auto clang = findProgramByName("clang++");
  string opt("-O");
  opt += optLevel;

  if (!clang) {
    report_fatal_error("Unable to find clang.");
  }
  vector<string> args{clang.get(), opt, "-o", outputFile.str(), objectFile.str()};

  for (auto& extra : extras) {
    args.push_back(extra);
  }

  for (auto& libPath : libPaths) {
    args.push_back("-L" + libPath);
  }

  for (auto& library : libraries) {
    args.push_back("-l" + library);
  }

  vector<llvm::StringRef> charArgs;
  charArgs.reserve(args.size());
  for (auto& arg : args) {
    charArgs.emplace_back(arg);
  }

  for (auto& arg : args) {
    outs() << arg.c_str() << " ";
  }
  outs() << "\n";

  string err;
  auto result = ExecuteAndWait(
      clang.get(),
      charArgs,
      std::nullopt,
      {},
      0,
      0,
      &err
    );
  if (-1 == result) {
    report_fatal_error("Unable to link output file.");
  }
}


static void
generateBinary(Module& m, StringRef outputFilename) {
  // Compiling to native should allow things to keep working even when the
  // version of clang on the system and the version of LLVM used to compile
  // the tool don't quite match up.
  string objectFile = outputFilename.str() + ".o";
  compile(m, objectFile);
  link(objectFile, outputFilename);
}


static void
saveModule(Module const& m, StringRef filename) {
  std::error_code errc;
  raw_fd_ostream out(filename.data(), errc, sys::fs::OF_None);

  if (errc) {
    report_fatal_error("error saving llvm module to '" + filename + "': \n"
                       + errc.message());
  }
  WriteBitcodeToFile(m, out);
}


static void
prepareLinkingPaths(SmallString<32> invocationPath) {
  // First search the directory of the binary for the library, in case it is
  // all bundled together.
  sys::path::remove_filename(invocationPath);
  if (!invocationPath.empty()) {
    libPaths.push_back(invocationPath.str().str());
  }
// If the builder doesn't plan on installing it, we still need to get to the
// runtime library somehow, so just build in the path to the temporary one.
#ifdef CMAKE_INSTALL_PREFIX
  libPaths.push_back(CMAKE_INSTALL_PREFIX "/lib");
#elif defined(CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  libPaths.push_back(CMAKE_ARCHIVE_OUTPUT_DIRECTORY);
#elif defined(TEMP_LIBRARY_PATH)
  // This is a bit of a hack from the old LLVM build system
  libPaths.push_back(TEMP_LIBRARY_PATH "/Debug+Asserts/lib/");
  libPaths.push_back(TEMP_LIBRARY_PATH "/Release+Asserts/lib/");
  libPaths.push_back(TEMP_LIBRARY_PATH "/Debug/lib/");
  libPaths.push_back(TEMP_LIBRARY_PATH "/Release/lib/");
#endif
  libraries.push_back(RUNTIME_LIB);
}


static void
instrument(Module& m, AnalysisType analysisType) {
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  if (outFile.getValue().empty()) {
    errs() << "-o command line option must be specified.\n";
    exit(-1);
  }

  // Build up all of the passes that we want to run on the module.
  legacy::PassManager pm;
  pm.add(new tolerator::Tolerator(analysisType));
  pm.add(createVerifierPass());
  pm.run(m);

  generateBinary(m, outFile);
  saveModule(m, outFile + ".tolerator.bc");
}


int
main(int argc, char** argv) {
  // This boilerplate provides convenient stack traces and clean LLVM exit
  // handling. It also initializes the built in support for convenient
  // command line option handling.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj shutdown;
  cl::HideUnrelatedOptions(toleratorCategory);
  cl::ParseCommandLineOptions(argc, argv);

  // Construct an IR file from the filename passed on the command line.
  SMDiagnostic err;
  LLVMContext context;
  unique_ptr<Module> module = parseIRFile(inPath.getValue(), err, context);

  if (!module.get()) {
    errs() << "Error reading bitcode file: " << inPath << "\n";
    err.print(argv[0], errs());
    return -1;
  }

  prepareLinkingPaths(StringRef(argv[0]));
  instrument(*module, analysisType);

  return 0;
}
