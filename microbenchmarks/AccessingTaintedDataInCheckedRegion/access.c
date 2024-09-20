#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <stdlib_tainted.h>
#include <checkcbox_extensions.h>
#include "access.h"

double ReadingAndWritingToCheckedPtr100ktimes()
{
    clock_t start, end;
    double cpu_time_used;
    size_t size_val = 1000000;
    start = clock();
    double final = 0;
    volatile int external_var = 1;
    int* pVal = (int*)malloc(size_val* sizeof(int));
    *pVal = 0;
    for (int i = 0; i < size_val; i++)
    {
        // Add some dependency on external_var to make the loop harder to optimize
        pVal[i] = pVal[i] + external_var % (i + 1);
    }

    end = clock();
    cpu_time_used = ((double) (end-start)) / CLOCKS_PER_SEC;
    return cpu_time_used;


}
double ReadingAndWritingToTaintedPtr100ktimes()
{
    clock_t start, end;
    double cpu_time_used;
    size_t size_val = 1000000;
    start = clock();
    double final = 0;
    volatile int external_var = 1;
#ifdef WASM_SBX
    _TPtr<int> pVal = (_TPtr<int>)t_malloc(size_val*sizeof(int));
#elif HEAP_SBX
    _TPtr<int> pVal = (_TPtr<int>)hoard_malloc(size_val* sizeof(int));
#endif
    for (int i = 0; i < size_val; i++)
    {
        // Add some dependency on external_var to make the loop harder to optimize
        pVal[i] = pVal[i] + external_var % (i + 1);
    }

    end = clock();
    cpu_time_used = ((double) (end-start)) / CLOCKS_PER_SEC;
    return cpu_time_used;
}

int main() {
    double checkedPtrSum = 0.0, taintedPtrSum = 0.0;
    int iterations = 10;

    for (int i = 0; i < iterations; i++) {
        printf("******************Iteration: %d **************************\n", i + 1);

        double checkedPtrTime = ReadingAndWritingToCheckedPtr100ktimes();
        double taintedPtrTime = ReadingAndWritingToTaintedPtr100ktimes();

        // Accumulate the times for both functions
        checkedPtrSum += checkedPtrTime;
        taintedPtrSum += taintedPtrTime;

        printf("ReadingAndWritingToCheckedPtr100ktimes takes %f seconds\n", checkedPtrTime);
        printf("ReadingAndWritingToTaintedPtr100ktimes takes %f seconds\n", taintedPtrTime);
        printf("**********************************************************\n");
    }

    // Compute the mean
    double meanCheckedPtrTime = checkedPtrSum / iterations;
    double meanTaintedPtrTime = taintedPtrSum / iterations;

    // Print the mean values
    printf("Mean time for ReadingAndWritingToCheckedPtr100ktimes: %f seconds\n", meanCheckedPtrTime);
    printf("Mean time for ReadingAndWritingToTaintedPtr100ktimes: %f seconds\n", meanTaintedPtrTime);

    return 0;
}