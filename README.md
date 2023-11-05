<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Issues][issues-shield]][issues-url]
[![LinkedIn][linkedin-shield]][linkedin-url]
[![Typeflexer Tests](https://github.com/TypeFlexer/TypeFlexer-Clang/actions/workflows/main-branch-sync.yml/badge.svg)](https://github.com/TypeFlexer/TypeFlexer-Clang/actions/workflows/main-branch-sync.yml)
[![Nightly Typeflexer Tests](https://github.com/TypeFlexer/TypeFlexer-Clang/actions/workflows/nightly.yml/badge.svg)](https://github.com/TypeFlexer/TypeFlexer-Clang/actions/workflows/nightly.yml)
<p align="center">
  <img src="./images/CheckBox.gif" alt="animated" />
</p>

  <h3 align="center">Type-based program partitioning</h3>

  <p align="center">
    This repo contains a version of the LLVM/Clang toolchain that is being modified to support Check-C-Box which from the fork of Checked C
    <br />
    <a href="https://sites.google.com/view/TypeFlexer/home?authuser=0"><strong>Concept»</strong></a>
    <br />
    <br />
    <a href="https://github.com/purs3lab/CheckC-Box">View Examples</a>
    ·
    <a href="https://github.com/arunkumarbhattar/TypeFlexer_Compiler/issues">Report Bug</a>
    ·
    <a href="https://github.com/arunkumarbhattar/TypeFlexer_Compiler/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#what-is-rlbox">What is RLBOX?</a></li>
      </ul>
      <ul>
        <li><a href="#what-is-checked-c">What is Checked-C?</a></li>
      </ul>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

## Background
### What is RLBOX?

[More Details](docs/background/rlbox.md)

### What is Checked-C?
Checked C extends C with checked pointer types which are restricted by the compiler to spatially safe uses.
Such pointers have one of three possible types, `ptr<T>` , `array_ptr<T>` ,
or `nt_array_ptr<T>` representing a pointer to a single element, array
of elements, or null-terminated array of elements of type `T`, respectively. The latter two have an
associated bounds annotation; e.g., a declaration `array_ptr<int> p : count(n)` says that `p` is a pointer
to an int array whose size is `n` . Checked C's Clang/LLVM-based compiler represents checked
pointers as system-level memory words, i.e., without “fattening” metadata ensuring backward
compatibility. 
**The bounds annotations are used to add dynamic checks on corresponding pointer
accesses to prevent spatial safety violations. Furthermore, these run-time checks are optimized
by LLVM, yielding good performance.**

[More Details](docs/background/checkedc.md)

## Our Idea

<!-- CONTACT -->
## Contact

* Professor/Lead: [Dr. Aravind Machiry](https://machiry.github.io/)

* Lab: [PurS3](https://purs3lab.github.io/)

Researcher(s):
 * Arun Kumar Bhattar

## Testing out TypeFlexer

Programmers are welcome to use TypeFlexer as it is being implemented.  You will
have to build your own copy of the compiler. 
Below are the directions on how to do it.

Step 1: Switch to work Directory
```
cd <WORK_DIR>
```

Step 2: Clone the repository
```
git clone https://github.com/TypeFlexer/TypeFlexer-Clang.git
```

Step 3: Now create a build directory generate the Cmake files 

```
cd <WORK_DIR>/Typeflexer-Clang/llvm/
mkdir build
cd build 
cmake -G Ninja -DLLVM_ENABLE_PROJECTS=clang -DLLVM_ENABLE_RUNTIMES=compiler-rt -DCMAKE_LINKER=/usr/bin/gold DCMAKE_BUILD_TYPE=Debug -DLLVM_LIT_ARGS=-v -DLLVM_PARALLEL_LINK_JOBS=1 ../
```

Step 6: Now you are all set to build the target 
Execute any of the following command (based on your requirement) in the build directory 
```
ninja clang //this command will only build the compiler
ninja checkcbox-headers // This command will run all the sanity test cases for TypeFlexer project 
```

Step 5 (Optional) Sanitizers --> 
```
First compile target llvm-config
cd <compiler-rt> // Switch to compiler-rt directory
cmake .
make install
export ASAN_SYMBOLIZER_PATH=<pathToLLVMSymbolizer> // For meaningful ASAN backtrace
```
## Testing Playground for TypeFlexer

```
cd <WORK_DIR>/Typeflexer-Clang/tests
```

## Sandbox Libraries

```
cd <WORK_DIR>/Typeflexer-Clang/sandboxLib
```
<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/arunkumarbhattar/TypeFlexer_Compiler
[contributors-url]: https://github.com/arunkumarbhattar/TypeFlexer_Compiler/pulse
[forks-shield]: https://img.shields.io/github/forks/arunkumarbhattar/TypeFlexer_Compiler?style=social
[forks-url]: https://github.com/arunkumarbhattar/TypeFlexer_Compiler/network/members
[issues-shield]: https://img.shields.io/bitbucket/issues/arunkumarbhattar/TypeFlexer_Compiler
[issues-url]: https://github.com/arunkumarbhattar/TypeFlexer_Compiler/issues
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/arun-b-093693148
[product-screenshot]: images/screenshot.png

# The TypeFlexer clang repo

This repo contains a version of the LLVM/Clang toolchain that is being modified
to support the TypeFlexer fork of Checked C. Checked C extends
C with checking to detect or prevent common programming errors such as out-of-bounds memory accesses.
The SSDP fork of the Checked
C specification is available at the
[SSDP Checked C repo](https://github.com/secure-sw-dev/checkedc).


