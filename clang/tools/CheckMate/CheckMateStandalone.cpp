//=--CheckMateStandalone.cpp---------------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// CheckMate tool
//
//===----------------------------------------------------------------------===//

#include "clang/CheckMate/CheckMate.h"
#include "clang/CheckMate/CheckMateGlobalOptions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"

using namespace clang::driver;
using namespace clang::tooling;
using namespace clang;
using namespace llvm;
// See clang/docs/checkedc/CheckMate/clang-tidy.md#_CheckMate-name-prefix
// NOLINTNEXTLINE(readability-identifier-naming)
static cl::OptionCategory _CheckMateCategory("CheckMate options");
static const char *HelpOverview =
    "CheckMate: Automatically Generate Sandbox-Specific Instrumentation required to call "
    "Sandbox Specific Definitions)\n";
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// Use a raw string to reduce the clutter of escape sequences and make it easier
// to line-wrap the message using a text editor. We actually want the final
// blank line in the message; the initial one just helps the editor know what
// text it is supposed to wrap.
//
// XXX: The first two paragraphs are common to all Clang LibTooling-based
// tools and would ideally go in CommonOptionsParser::HelpMessage or
// somewhere else that users will find. But unless/until we pursue that, we
// document that information here for CheckMate.
static const char MoreHelpStr[] = R"(

By default, CheckMate (like any Clang LibTooling-based tool) automatically searches for
a compilation database based on the -p option or (if that option is not
specified) the path of the first source file. If no compilation database is
found, CheckMate prints a warning. If one is found, CheckMate looks up each source file
specified on the command line in the compilation database to find the compiler
options to use for that file. Thus, if you give CheckMate a compilation database
generated by your build system, it will use the same compiler options as your
build system (which may be different for each file). You can add options via
-extra-arg and -extra-arg-before. If you want to run CheckMate on all source files in
your compilation database, you must pass them on the command line; CheckMate will not
take the list automatically from the compilation database. If you specify a
source file that is not in the database, CheckMate will use the compiler options from
the most "similar looking" file in the database according to a set of
heuristics.

If you _do not_ want to use a compilation database, pass "--" after all other CheckMate
arguments. This is important to ensure that CheckMate doesn't automatically detect a
compilation database and use compiler options you do not want from a "similar
looking" file in the database. The "--" may be followed by compiler options that
you want to use for all source files (this is equivalent to specifying those
options via -extra-arg before the "--").

You can use either -output-dir or -output-postfix to control the paths at which
CheckMate writes the new versions of your files. With either of these options, if CheckMate
does not write a new version of a given file, that means the file needs no
changes. If you use neither -output-dir nor -output-postfix, then you can only
pass one source file on the command line and the new version of that file is
written to stdout regardless of whether it differs from the original ("stdout
mode"), but CheckMate still solves for changes to all files under the -base-dir that
are "#include"-d by that file and it is an error if any other file changes.

)";

// Skip the 2 initial newlines.
static cl::extrahelp MoreHelp(MoreHelpStr + 2);

// Letting clang-format reflow these declarations gives very inconsistent
// formatting between options.
// clang-format off

static cl::opt<bool> OptDumpIntermediate(
    "dump-intermediate",
    cl::desc("Dump intermediate information"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptVerbose(
    "verbose",
    cl::desc("Print verbose information"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptOutputPostfix(
    "output-postfix",
    cl::desc("String to insert into the names of updated files just before the "
             "extension (e.g., with -output-postfix=checked, foo.c -> "
             "foo.checked.c)"),
    cl::init("-"), cl::cat(_CheckMateCategory));

static cl::opt<std::string> TaintedDefDir(
    "tainted-dir",
    cl::desc("Directory into which tainted files bearing tainted definitions"
    "would be inserted for compiler directed sandbox enforcement"),
    cl::init(""), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptOutputDir(
    "output-dir",
    cl::desc("Directory under which updated files will be written at the same "
             "relative paths as the originals under the -base-dir"),
    cl::init(""), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptMalloc(
    "use-malloc",
    cl::desc("Allows for the usage of user-specified versions of function "
             "allocators"),
    cl::init(""), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptConstraintOutputJson(
    "constraint-output",
    cl::desc("Path to the file where all the analysis information will be "
             "dumped as json"),
    cl::init("constraint_output.json"), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptStatsOutputJson(
    "stats-output",
    cl::desc("Path to the file where all the stats will be dumped as json"),
    cl::init("TotalConstraintStats.json"), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptWildPtrInfoJson(
    "wildptrstats-output",
    cl::desc("Path to the file where all the info related to WILD ptr grouped "
             "by reason will be dumped as json"),
    cl::init("WildPtrStats.json"), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptPerPtrWILDInfoJson(
    "perptrstats-output",
    cl::desc("Path to the file where all the info related to each WILD ptr "
             "will be dumped as json"),
    cl::init("PerWildPtrStats.json"), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptDumpStats(
    "dump-stats",
    cl::desc("Dump statistics"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptHandleVARARGS(
    "handle-varargs",
    cl::desc("Enable handling of varargs in a sound manner"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptAllTypes(
    "alltypes",
    cl::desc("Consider all Checked C types for conversion"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptAddCheckedRegions(
    "addcr",
    cl::desc("Add Checked Regions"),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptSandboxType(
    "sbx",
    cl::desc("Sandbox Type: (0)->WASM, (1..)->Not Yet Supported"),
    cl::init("wasm"), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptEnableCCTypeChecker(
    "enccty",
    cl::desc(
      "Enable the Checked C type checker. CheckMate normally disables it (via the "
      "equivalent of `clang -fCheckMate-tool`) so that CheckMate can operate on partially "
      "converted programs that may have Checked C type errors."),
    cl::init(false), cl::cat(_CheckMateCategory));

static cl::opt<std::string> OptBaseDir(
    "base-dir",
    cl::desc(
      "Ancestor directory defining the set of files that CheckMate "
      "is allowed to modify (default: the working "
      "directory). All source files specified on the command line must be "
      "under this directory. You can use "
      "this option to let CheckMate modify your project's own header files but not "
      "those of libraries outside your control."),
    cl::init(""), cl::cat(_CheckMateCategory));

static cl::opt<bool> OptAllowSourcesOutsideBaseDir(
    "allow-sources-outside-base-dir",
    cl::desc("When a source file is outside the base directory, issue a "
             "warning instead of an error. This option is intended to be used "
             "temporarily until you fix your project setup and may be removed "
             "in the future."),
    cl::init(false), cl::cat(_CheckMateCategory));

//static cl::opt<bool> OptWarnRootCause(
//    "warn-root-cause",
//    cl::desc("Emit warnings indicating root causes of unchecked pointers."),
//    cl::init(false), cl::cat(_CheckMateCategory));
//
//static cl::opt<bool> OptWarnAllRootCause(
//    "warn-all-root-cause",
//    cl::desc("Emit warnings for all root causes, even those unlikely to be "
//             "interesting."),
//    cl::init(false), cl::cat(_CheckMateCategory));

// In the future, we may enhance this to write the output to individual files.
// For now, the user has to copy and paste the correct portions of stderr.
static cl::opt<bool> OptDumpUnwritableChanges(
    "dump-unwritable-changes",
    cl::desc("When CheckMate generates changes to a file it cannot write (due to "
             "stdout mode or implementation limitations), dump the new version "
             "of the file to stderr for troubleshooting."),
    cl::init(false), cl::cat(_CheckMateCategory));

int main(int argc, const char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);

  // Initialize targets for clang module support.
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  // The following code is based on clangTidyMain in
  // clang-tools-extra/clang-tidy/tool/ClangTidyMain.cpp. Apparently every
  // LibTooling-based tool is supposed to duplicate it??
  llvm::Expected<CommonOptionsParser> ExpectedOptionsParser =
      CommonOptionsParser::create(argc, (const char **)(argv), _CheckMateCategory,
                                  cl::ZeroOrMore, HelpOverview);
  if (!ExpectedOptionsParser) {
    llvm::errs() << "CheckMate: Error(s) parsing command-line arguments:\n"
                 << llvm::toString(ExpectedOptionsParser.takeError());
    return 1;
  }
  CommonOptionsParser &OptionsParser = *ExpectedOptionsParser;
  // Specifying cl::ZeroOrMore rather than cl::OneOrMore and then checking this
  // here lets us give a better error message than the default "Must specify at
  // least 1 positional argument".
  if (OptionsParser.getSourcePathList().empty()) {
    llvm::errs() << "CheckMate: Error: No source files specified.\n"
                 << "See: " << argv[0] << " --help\n";
    return 1;
  }


  // Setup options.
  struct _CheckMateOptions CcOptions;
  CcOptions.BaseDir = OptBaseDir.getValue();
  CcOptions.AllowSourcesOutsideBaseDir = OptAllowSourcesOutsideBaseDir;
  //CcOptions.HandleVARARGS = OptHandleVARARGS;
  CcOptions.DumpStats = OptDumpStats;
  CcOptions.OutputPostfix = OptOutputPostfix.getValue();
  CcOptions.OutputDir = OptOutputDir.getValue();
  CcOptions.TaintedDefDir = TaintedDefDir.getValue();
  CcOptions.Verbose = OptVerbose;
  CcOptions.DumpIntermediate = OptDumpIntermediate;
  CcOptions.ConstraintOutputJson = OptConstraintOutputJson.getValue();
  CcOptions.StatsOutputJson = OptStatsOutputJson.getValue();
  CcOptions.WildPtrInfoJson = OptWildPtrInfoJson.getValue();
  CcOptions.PerWildPtrInfoJson = OptPerPtrWILDInfoJson.getValue();
  CcOptions.AddSandbox = OptSandboxType.getValue();
  CcOptions.EnableCCTypeChecker = OptEnableCCTypeChecker;
  CcOptions.DumpUnwritableChanges = OptDumpUnwritableChanges;



  //Add user specified function allocators
  std::string Malloc = OptMalloc.getValue();
  if (!Malloc.empty()) {
    std::string Delimiter = ",";
    size_t Pos = 0;
    std::string Token;
    while ((Pos = Malloc.find(Delimiter)) != std::string::npos) {
      Token = Malloc.substr(0, Pos);
      CcOptions.AllocatorFunctions.push_back(Token);
      Malloc.erase(0, Pos + Delimiter.length());
    }
    Token = Malloc;
    CcOptions.AllocatorFunctions.push_back(Token);
  } else
    CcOptions.AllocatorFunctions = {};

  // Create CheckMate Interface.
  //
  // See clang/docs/checkedc/CheckMate/clang-tidy.md#_CheckMate-name-prefix
  // NOLINTNEXTLINE(readability-identifier-naming)
  std::unique_ptr<_CheckMateInterface> _CheckMateInterfacePtr(
      _CheckMateInterface::create(CcOptions, OptionsParser.getSourcePathList(),
                           &(OptionsParser.getCompilations())));
  if (!_CheckMateInterfacePtr) {
    // _CheckMateInterface::create has already printed an error message. Just exit.
    return 1;
  }
  // See clang/docs/checkedc/CheckMate/clang-tidy.md#_CheckMate-name-prefix
  // NOLINTNEXTLINE(readability-identifier-naming)
  _CheckMateInterface &_CheckMateInterface = *_CheckMateInterfacePtr;

  //if (OptVerbose)
    errs() << "Parsing source files.\n";

  // Build AST from source.
  if (!_CheckMateInterface.parseASTs()) {
    errs() << "Failure occurred while parsing source files. Exiting.\n";
    return _CheckMateInterface.determineExitCode();
  }

  if (OptVerbose) {
    errs() << "Finished parsing sources.\n";
    errs() << "Adding Top-level Constraint Variables.\n";
  }

  /*
   * Create a new directory at the location of source files called "Tainted"
   * This directory will hold c files of tainted definitions
   * that can be compiled for WASM Sandbox
   */


  // Add variables.
  if (!_CheckMateInterface.addVariables()) {
    errs() << "Failure occurred while trying to add variables. Exiting.\n";
    return _CheckMateInterface.determineExitCode();
  }

  if (OptVerbose) {
    errs() << "Finished adding variables.\n";
    errs() << "Calling Library to build Constraints.\n";
  }

  // Build constraints.
  if (!_CheckMateInterface.buildInitialConstraints()) {
    errs() << "Failure occurred while trying to build constraints. Exiting.\n";
    //return _CheckMateInterface.determineExitCode();
  }

  if (OptVerbose) {
    errs() << "Finished Building Constraints.\n";
    errs() << "Trying to solve Constraints.\n";
  }

  if (OptVerbose) {
    errs() << "Finished solving constraints.\n";
    errs() << "Trying to rewrite the converted files back.\n";
  }

  // Write all the converted files back.
  if ( !_CheckMateInterface.writeAllConvertedFilesToDisk()) {
    errs() << "Failure occurred while trying to rewrite converted files back. "
              "Exiting.\n";
    return _CheckMateInterface.determineExitCode();
  }

  //Run C4 on all the converted files
  if ( !_CheckMateInterface.PlaceC4Charges()) {
    errs() << "Failure occurred while detonating C4 on converted files.\n";
    return _CheckMateInterface.determineExitCode();
  }

  // Even if all passes succeeded, we could still have a diagnostic verification
  // failure.
  return _CheckMateInterface.determineExitCode();
}
