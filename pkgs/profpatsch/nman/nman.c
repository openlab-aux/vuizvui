#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>

const char* USAGE =
	"nman drvAttr [section|page] [page]\n"
	"\n"
	"Open man pages in a temporary nix-shell.\n"
	"1 If one argument is given, the drvAttr & page have the same name.\n"
	"2 If two arguments are given and the second arg is\n"
	"    a <number>) like 1, but in man section <number>\n"
	"    a <page>  ) like 1, but open the page <page>\n"
	"3 If three arguments are given, the order is <drvAttr> <sect> <page>\n";

void execNixShell(char const* drvAttr, int manSection, const char* manPage) {
	assert(manSection >= -1);
	char* manCmd;
	// holy debug printf
	//printf("attr: %s, sect: %d, page: %s\n", drvAttr, manSection, manPage);
	int ret;
	if (-1 == manSection) {
		ret = asprintf(&manCmd, "man %s", manPage);
	} else {
	 	ret = asprintf(&manCmd, "man %d %s", manSection, manPage);
	}
	assert(ret != -1);

	if (-1 == execlp("nix-shell", "nix-shell", "-p", drvAttr, "--run", manCmd, NULL)) {
		fprintf(stderr, "%s\n", strerror(errno));
		exit(-1);
	}
	free(manCmd);
}

int main(int argc, char** argv) {
	int manSection = -1;
	char* drvAttr;
	char* manPage;
	if (argc >= 3) {
		// man section or -1 if no man section
		int i = strtol(argv[2], NULL, 10);
		if (i > 1) {
			manSection = i;
		}
	}
	// the first argument is always a derivation attribute
	drvAttr = argv[1];

	// nman does a convenient selection based on the number
	// of attributes given, in order to do what the user means.
	switch (argc) {
	case 1:
		fprintf(stderr, "%s", USAGE);
		exit(-1);
		break;
	case 2:
		// arg is package and drv attr
		manPage = argv[1];
		break;
	case 3:
		if (manSection == -1) {
			// like 2:, but arg 2 is package
			manPage = argv[2];
		} else {
			// man section given, page is arg 1
			manPage = argv[1];
		}
		break;
	case 4:
		// arg 2 is manSection, arg 3 is package
		manPage = argv[3];
		break;
	}

	execNixShell(drvAttr, manSection, manPage);
}
