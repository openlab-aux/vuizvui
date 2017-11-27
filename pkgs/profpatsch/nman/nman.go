package main

import (
	"fmt"
	"os"
	"strconv"
	"log"
	"bytes"
	"os/exec"
	"io/ioutil"
	"syscall"
)

var usage = `nman drvAttr [section|page] [page]

Open man pages in a temporary nix-shell.
1 If one argument is given, the drvAttr & page have the same name.
2 If two arguments are given and the second arg is
    a <number>) like 1, but in the man section <number>
    a <page>  ) like 1, but open the page <page>
3 If three arguments are given, the order is <drvAttr> <sect> <page>
`

func lookupManPage(manpath string, manSection int64, manPage string) (int, error) {
	if manSection < -1 {
		panic("manSection must not be < -1")
	}
	// holy debug printf
	// fmt.Printf("attr: %s, sect: %d, page: %s\n", drvAttr, manSection, manPage)

	manBin, err := exec.LookPath("man");
	if err != nil {
		return 0, fmt.Errorf("man executable not in PATH")
	}

	var manArgs []string
	if (manSection == -1) {
		manArgs = []string{manPage}
	} else {
		manArgs = []string{strconv.FormatInt(manSection, 10), manPage}
	}

	man := exec.Command(manBin, manArgs...)

        man.Env = append(
		os.Environ(),
		fmt.Sprintf("MANPATH=%s", manpath))
	man.Stderr = os.Stderr
	man.Stdout = os.Stdout
	err = man.Run()
	if exiterr, ok := err.(*exec.ExitError); ok {
		ws := exiterr.Sys().(syscall.WaitStatus)
		return ws.ExitStatus(), nil
	} else {
		return 0, err
	}

}

func buildManOutput(drvAttr string) (string, error) {
	nixExpr := fmt.Sprintf(
		`with import <nixpkgs> {}; %s.man or %s.doc or %s.out or %s`,
		drvAttr, drvAttr, drvAttr, drvAttr)

	nixBuild := exec.Command("nix-build", "-E", nixExpr)
	nixBuild.Stderr = os.Stderr
	pipe, err := nixBuild.StdoutPipe()
	if err != nil { return "", fmt.Errorf("could not access stdout of nix-build: %s", err) }

	err = nixBuild.Start()
	if err != nil { return "", fmt.Errorf("could not start nix-build: %s", err) }

	out, err := ioutil.ReadAll(pipe)
	if err != nil { return "", fmt.Errorf("could not read from nix-build: %s", err) }

	lines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
	err = nixBuild.Wait()
	if err != nil { return "", fmt.Errorf("nix-build process died: %s", err) }

	// last line ouf output looks like: /nix/store/abc…-foobar.drv!output
	drvPath := bytes.Split(lines[len(lines)-1], []byte("!"))[0]
	if _, err := os.Stat(string(drvPath)); err != nil {
		return "", fmt.Errorf("%s doesn’t look like an output path: %s", drvPath, err)
	}

	return string(drvPath), nil
}

func main() {
	var manPage string
	var drvAttr string

	args := os.Args

	// man section or -1 if no man section
        var manSection int64 = -1
	if (len(args) >= 3) {
		i, err := strconv.ParseUint(args[2], 10, 64)
		if err == nil && i >= 0 {
			manSection = int64(i)
		}
	}

	// the first argument is always a derivation attribute
	switch len(args) {
	case 2:
		// arg is package and drv attr
		manPage = args[1]
	case 3:
		if (manSection == -1) {
			// like 2, but arg 2 is package
			manPage = args[2]
		} else {
			// man section given, page is arg 1
			manPage = args[1]
		}
	case 4:
		// arg 2 is manSection, arg 3 is package
		manPage = args[3]
	default:
		fmt.Print(usage)
		os.Exit(-1)
	}

	drvAttr = args[1]
	drvPath, err := buildManOutput(drvAttr)
	if err != nil {
		log.Fatal(err)
	}
	exitStatus, err := lookupManPage(drvPath + "/share/man", manSection, manPage)
	if err != nil {
		log.Fatal(err)
	}

	os.Exit(exitStatus)

}
