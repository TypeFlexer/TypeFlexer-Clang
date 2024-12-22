# Welcome to TypeFlexer

TypeFlexer is a Type Directed Flexible Program Partitioning system that fundamentally improves the way software can manage and isolate parts of a program deemed vulnerable. Traditional program partitioning combines policy (what to partition) with mechanism (how to partition), often leading to inflexible and high-overhead isolation mechanisms. TypeFlexer stands out by separating policy from mechanism, driven by a specialized type system that uses tainted types to distinguish between trusted and untrusted regions of a program. The flexibility of our type system allows for the implementation of various isolation mechanisms, each offering unique guarantees and performance trade-offs.

TypeFlexer provides an efficient method for program partitioning, demonstrating overheads significantly lower than traditional approaches—ranging from 3.7% to 21.28%, compared to 34.8% in related work. It effectively isolates security vulnerabilities, thereby minimizing their impact on the overall program.

## Getting Started with TypeFlexer

Below are the steps to compile and set up the TypeFlexer environment on your local machine. Ensure you are in your preferred working directory before you start.

### Step 1: Set Up Your Working Directory
```
cd <WORK_DIR>
```

Step 2: Clone the repository
```
git clone https://github.com/TypeFlexer/TypeFlexer-Clang.git
```

Step 3: Generate Cmake Files

Create a build directory and generate CMake files:
```

cd <WORK_DIR>/Typeflexer-Clang/llvm/
mkdir build
cd build
cmake -G Ninja -DLLVM_ENABLE_PROJECTS=clang -DLLVM_ENABLE_RUNTIMES=compiler-rt -DCMAKE_LINKER=/usr/bin/gold -DCMAKE_BUILD_TYPE=Debug -DLLVM_LIT_ARGS=-v -DLLVM_PARALLEL_LINK_JOBS=1 ../
```

Step 4: Build the Compiler

Choose the appropriate build command based on your requirements:

```
ninja clang # Build only the compiler
ninja checkcbox-headers # Run all the sanity test cases for TypeFlexer project
```

Step 5 (Optional): Configure Sanitizers

For meaningful ASAN backtraces, set up the sanitizers as follows:

```
cd <compiler-rt> # Switch to compiler-rt directory
cmake .
make install
export ASAN_SYMBOLIZER_PATH=<pathToLLVMSymbolizer>
```

## Writing and Testing Your First TypeFlexer Program

Here, you will find the necessary tools and examples to get started with writing and compiling simple programs using TypeFlexer.

### Source Code
Consider a simple program, in Vanilla-C (generic-C) below: 

```
#include <stdio.h>
#include <stdlib.h>

#define MAX_PLAYERS 5  // Maximum number of players the array can hold

int main() {
    int scores[MAX_PLAYERS] = {0};
    int numPlayers;

    printf("Enter the number of players: ");
    scanf("%d", &numPlayers);

    // Initialize scores for each player (up to the array limit)
    for (int i = 0; i < numPlayers && i < MAX_PLAYERS; i++) {
        scores[i] = i * 100;  // Each player starts with points multiplied by 100.
    }

    // Update scores with potential buffer overrun if numPlayers > MAX_PLAYERS
    for (int i = 0; i < numPlayers; i++) {  // Unsafe update that may cause buffer overrun
        if (i < MAX_PLAYERS) {
            scores[i] += 50;  // Safely add 50 points
        } else {
            printf("Buffer overrun detected at index %d! Stopping further execution.\n", i);
            break;  // Stop the loop to prevent actual memory corruption
        }
    }

    // Print scores safely only for the initialized range
    for (int i = 0; i < MAX_PLAYERS; i++) {
        printf("Player %d: %d\n", i + 1, scores[i]);
    }

    return 0;
}

```

### Modified Source Code
Now, the developer notices a pointer being indexed un-confidently, in a loop,
and decides to mark scores pointer as tainted.

The modified code below would be as follows 

```
#include <stdio.h>
#include <stdlib.h>

#define MAX_PLAYERS 5  // Maximum number of players the array can hold

int main() {
    _TPtr<int> scores;
    int numPlayers;

    scores = (_TPtr<int>) t_calloc(MAX_PLAYERS, sizeof(int));
    
    printf("Enter the number of players: ");
    scanf("%d", &numPlayers);

    for (int i = 0; i < numPlayers; i++) {  
        scores[i] += 50;  // Safely add 50 points
    }

    // Print scores safely only for the initialized range
    for (int i = 0; i < MAX_PLAYERS; i++) {
        printf("Player %d: %d\n", i + 1, scores[i]);
    }

    t_free(scores);
    return 0;
}

```
### Steps to compile (WASM SBX)

```
clang -fwasmsbx -linksbx modified.c
```
### Steps to compile (HEAP SBX)

```
clang -fheapsbx -linksbx modified.c
```
