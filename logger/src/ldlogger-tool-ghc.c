#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <wordexp.h>
#include <string.h>
#include <ctype.h>

#include "ldlogger-tool.h"
#include "ldlogger-util.h"

const char* show_stoppers[] = 
  { "--frontend"
  , "--help", "-?"
  , "--info"
  , "--interactive"
  , "--mk-dll"
  , "--numeric-version"
  , "--print-libdir"
  , "--show-iface"
  , "--show-options"
  , "--supported-extensions"
  , "--supported-languages"
  , "--version", "-V"
  , "-e"
  , "-M" 
  };

const char* flags_with_parameter[] = 
  { "-dep-makefile"
  , "-dep-suffix"
  , "-dumpdir"
  , "-hcsuf"
  , "-hidir"
  , "-hisuf"
  , "-dynosuf"
  , "-dynhisuf"
  , "-odir"
  , "-ohi"
  , "-osuf"
  , "-outputdir"
  , "-stubdir"
  , "-tmpdir"
  , "-pgma"
  , "-pgmc"
  , "-pgmdll"
  , "-pgmF"
  , "-pgmi"
  , "-pgmL"
  , "-pgmlc"
  , "-pgmlibtool"
  , "-pgmlo"
  , "-pgmP"
  , "-pgms"
  , "-pgmwindres"
  , "-pgml"
  , "-opta"
  , "-optc"
  , "-optdll"
  , "-optF"
  , "-opti"
  , "-optL"
  , "-optlc"
  , "-optlo"
  , "-optl"
  , "-optP"
  , "-optwindres"
  , "--show-iface"
  , "-distrust"
  , "-hide-package"
  , "-ignore-package"
  , "-package-db"
  , "-package-env"
  , "-package-id"
  , "-package"
  , "-this-unit-id"
  , "-trust"
  , "-dylib-install-name"
  , "-framework-path"
  , "-framework"
  , "-main-is"
  , "-plugin-package-id"
  , "-plugin-package"
  , "-ghcversion-file"
  , "-x"
  , "-H"
  , "-o"
  , "-L"
  , "-l"

  };

int in_list(char* const arg, const char* ls[], int list_length) {
	int i;
	for (i = 0; i < list_length; ++i) {
		if (strcmp(ls[i], arg) == 0) {
			return 1;
		}
	}
	return 0;
}

int processArguments(char* const args[], LoggerAction* action) {
	loggerVectorAdd(&action->arguments, loggerStrDup(args[0]));
	int i;
	for (i = 1; args[i]; ++i) {
		int ss_size = sizeof(show_stoppers)/sizeof(char*);
		if (in_list(args[i], show_stoppers, ss_size)) {
			return 0;
		}
		
		loggerVectorAdd(&action->arguments, loggerStrDup(args[i]));
		// -i<dir>[:<dir>]*
		
		int fwp_size = sizeof(flags_with_parameter)/sizeof(char*);
		if (in_list(args[i], flags_with_parameter, fwp_size)) {
			++i;
			loggerVectorAdd(&action->arguments, loggerStrDup(args[i]));
			continue;
		}
		
		if (args[i][0] != '-') {
			//loggerVectorAdd(&action->sources, loggerStrDup(args[i]));
		}
	}
	loggerVectorAdd(&action->sources, loggerStrDup("<no-info-on-file>"));
	
	return 1;
}

int loggerGhcParserCollectActions(
  const char* prog_,
  const char* toolName_,
  const char* const argv_[],
  LoggerVector* actions_)
{

	const char* end = strrchr(prog_, '/');
	if (end == NULL || (strcmp(end, "/ghc") != 0)) {
		return 1;
	}

	//FILE* pfile = fopen("/home/nboldi/Desktop/hello.txt","a");
	//fprintf(pfile, "trying to store record: %s\n", prog_);
	
	LoggerAction* action = loggerActionNew(toolName_);
	if (action) {
		
		loggerVectorInit(&action->arguments);
		loggerVectorInit(&action->sources);
		
		int store = processArguments(argv_, action);
		
		if (store) {
			loggerVectorAdd(actions_, action);
		} else {
			loggerActionFree(action);
		}
	}
	
	//fclose(pfile);
	
  /*
  printf("loggerGhcParserCollectActions: %s %s", prog_, toolName_);
  int i;
  for (i = 1; argv_[i]; ++i) {
	printf("%s, ", argv_[i]);
  }*/
  //LoggerAction* action = loggerActionNew(toolName_);
  //if (action) {
  //loggerVectorAdd(&action->arguments, "-bzz");
  //loggerVectorAdd(&action->sources, "foobar.hs");
  //loggerFileInitFromPath(&action->output, "foobar.o");
  //loggerVectorAdd(actions_, action);
  //}
  return 1;
}

