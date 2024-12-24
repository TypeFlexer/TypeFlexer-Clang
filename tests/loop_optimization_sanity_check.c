#include <stdio.h>
#include <stdlib_tainted.h>

// A small utility to prevent the compiler from optimizing certain operations away.
// The asm volatile acts as a compiler barrier.
void do_not_optimize(double x) {
  asm volatile("" : : "r"(x) : "memory");
}

// Another overload for int, if needed
void do_not_optimize_int(int x) {
  asm volatile("" : : "r"(x) : "memory");
}

typedef struct {
  int id;
  double value;
} MyStruct;

/***********************************************************************
 * TEST 1: Simple for-loop with a constant bound
 * We expect the pass might try to hoist checks outside the loop because
 * the bound is constant. We'll see if the loop modifies anything that
 * prevents hoisting or not.
 **********************************************************************/
void test_forloop_constant(void) {
  printf("=== test_forloop_constant ===\n");

  // Allocate a tainted pointer to double
  _TPtr<double> arr = t_malloc<double>(100);
  if (!arr) {
    printf("[Error] Allocation failed in test_forloop_constant.\n");
    return;
  }

  // Fill array with squares from index 0..49
  // The loop bound is 50 => we expect up to arr[49] = 49^2
  for (int i = 0; i < 50; ++i) {
    arr[i] = (double)i * i;
    do_not_optimize(arr[i]);
  }

  // Print first few
  for (int i = 0; i < 5; ++i) {
    printf("arr[%d]=%.2f ", i, arr[i]);
  }
  printf("\n");

  t_free(arr);
}


/***********************************************************************
 * TEST 2: For-loop with a runtime bound
 * Here 'bound' is not known at compile-time, so the pass might or might
 * not hoist checks out.
 **********************************************************************/
void test_forloop_runtime(void) {
  printf("=== test_forloop_runtime ===\n");

  int runtimeBound = 40 + (rand() % 10); // e.g., 40..49
  _TPtr<double> arr = t_malloc<double>(128); // big enough
  if (!arr) {
    printf("[Error] Allocation failed in test_forloop_runtime.\n");
    return;
  }

  // Fill up to runtimeBound
  for (int i = 0; i < runtimeBound; i++) {
    arr[i] = (double)(i + 0.1);
    do_not_optimize(arr[i]);
  }

  // Print a few
  for (int i = 0; i < (runtimeBound < 5 ? runtimeBound : 5); i++) {
    printf("arr[%d]=%.2f ", i, arr[i]);
  }
  printf("\n");

  t_free(arr);
}


/***********************************************************************
 * TEST 3: While-loop example
 * The loop bound is partially modified in the loop.
 **********************************************************************/
void test_whileloop(void) {
  printf("=== test_whileloop ===\n");

  _TPtr<double> arr = t_malloc<double>(100);
  if (!arr) {
    printf("[Error] Allocation failed in test_whileloop.\n");
    return;
  }

  // We'll fill arr while i < 30
  int i = 0;
  while (i < 30) {
    arr[i] = 2.0 * i;
    do_not_optimize(arr[i]);
    i++;
  }

  // Print a few
  for (int k = 0; k < 5; ++k) {
    printf("arr[%d]=%.2f ", k, arr[k]);
  }
  printf("\n");

  t_free(arr);
}


/***********************************************************************
 * TEST 4: Do-while-loop example
 * The pointer is used in a do-while, which might complicate hoisting
 * because the loop runs at least once.
 **********************************************************************/
void test_dowhileloop(void) {
  printf("=== test_dowhileloop ===\n");

  _TPtr<double> arr = t_malloc<double>(64);
  if (!arr) {
    printf("[Error] Allocation failed in test_dowhileloop.\n");
    return;
  }

  int i = 0;
  do {
    arr[i] = (double)(i + 0.5);
    do_not_optimize(arr[i]);
    i++;
  } while (i < 20);

  // Print some
  for (int k = 0; k < 5; ++k) {
    printf("arr[%d]=%.2f ", k, arr[k]);
  }
  printf("\n");

  t_free(arr);
}


/***********************************************************************
 * Helper function that modifies the pointer inside
 * We'll call this from a loop to test pointer changes in the loop
 **********************************************************************/
void shiftPointer(_TPtr<double>* pArr, int shift) {
  // Let's artificially shift the pointer forward.
  // We do so by reinterpreting the TPtr with an offset
  // For demonstration, just do pointer arithmetic if allowed by the sandbox
  // This might be unsafe in a real environment but helps test the pass
  *pArr = *pArr + shift;
}


/***********************************************************************
 * TEST 5: A loop that modifies the tainted pointer each iteration
 **********************************************************************/
void test_modified_tainted_ptr(void) {
  printf("=== test_modified_tainted_ptr ===\n");

  _TPtr<double> base = t_malloc<double>(200);
  if (!base) {
    printf("[Error] Allocation failed in test_modified_tainted_ptr.\n");
    return;
  }

  // We'll keep a 'current' pointer we shift
  // This can confound a pass that tries to hoist checks out of the loop
  _TPtr<double> current = base;

  // Fill 5 elements, but each time shift the pointer by 1
  // The pass might or might not be able to detect that 'current' changes each iteration
  for (int i = 0; i < 5; i++) {
    *current = (double)i * 1.1;
    do_not_optimize(*current);

    // shift pointer by 1
    shiftPointer(&current, 1);
  }

  // Print the base elements
  for (int k = 0; k < 5; ++k) {
    printf("[base+%d]=%.2f ", k, base[k]);
  }
  printf("\n");

  t_free(base);
}


/***********************************************************************
 * TEST 6: Nested loops
 * We do a nested for loop that writes i*j.
 * The pass might try to hoist checks for the outer/inner loop.
 **********************************************************************/
void test_nested_loops(void) {
  printf("=== test_nested_loops ===\n");

  _TPtr<double> arr = t_malloc<double>(100);
  if (!arr) {
    printf("[Error] Allocation failed in test_nested_loops.\n");
    return;
  }

  int size = 10;
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      int index = i*size + j;
      arr[index] = i * j * 1.0;
      do_not_optimize(arr[index]);
    }
  }

  // Print top-left corner of NxN
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 2; j++) {
      int index = i*size + j;
      printf("arr[%d,%d]=%.2f ", i, j, arr[index]);
    }
    printf("\n");
  }

  t_free(arr);
}


/***********************************************************************
 * TEST 7: A loop that modifies the loop bound inside the loop
 * We'll do an artificially changing 'bound' which might confound hoisting
 **********************************************************************/
void test_modified_loop_bound(void) {
  printf("=== test_modified_loop_bound ===\n");

  _TPtr<double> arr = t_malloc<double>(100);
  if (!arr) {
    printf("[Error] Allocation failed.\n");
    return;
  }

  int bound = 10;
  int i = 0;

  while (i < bound) {
    arr[i] = (double)(i + 0.3);
    do_not_optimize(arr[i]);
    i++;

    // artificially change bound if i hits 5
    if (i == 5) {
      bound = 8; // shorten the loop
    }
  }

  // Print a few
  for (int k = 0; k < i; ++k) {
    printf("arr[%d]=%.2f ", k, arr[k]);
  }
  printf("\n");

  t_free(arr);
}

// Test 8
// Test function working with an array of structures
void test_whileloop_struct(void) {
  printf("=== test_whileloop_struct ===\n");

  // Allocate memory for an array of structures
  _TPtr<MyStruct> arr = (_TPtr<MyStruct>)t_malloc(100 * sizeof(MyStruct));
  if (!arr) {
    printf("[Error] Allocation failed in test_whileloop_struct.\n");
    return;
  }

  // Fill the array while i < 30
  int i = 0;
  while (i < 30) {
    arr[i].id = i;
    arr[i].value = 2.0 * i;
    do_not_optimize(arr[i].value);
    i++;
  }

  // Print a few elements
  for (int k = 0; k < 10; ++k) {
    printf("arr[%d] = { id: %d, value: %.2f }\n", k, arr[k].id, arr[k].value);
  }

  // Free the allocated memory
  t_free(arr);
}


/***********************************************************************
 * TEST 9: A big function that just runs all tests
 **********************************************************************/
int main(void) {
  printf("Running various loop optimization tests with Tainted Pointers...\n\n");

  // 1. For-loop, constant bound
  test_forloop_constant();

  // 2. For-loop, runtime bound
  test_forloop_runtime();

  // 3. While-loop
  test_whileloop();

  // 4. Do-While-loop
  test_dowhileloop();

  // 5. Modified Tainted Pointer in the loop
  test_modified_tainted_ptr();

  // 6. Nested loops
  test_nested_loops();

  // 7. Modified loop bound
  test_modified_loop_bound();

  test_whileloop_struct();
  printf("\nAll tests complete.\n");
  return 0;
}
