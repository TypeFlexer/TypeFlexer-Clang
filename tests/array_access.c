#include <stdio.h>
#include <stdlib_tainted.h>

// Dummy function to prevent compiler optimizations.
// The `asm volatile` statement acts as a compiler barrier.
void do_not_optimize(int x) {
  asm volatile("" : : "r"(x) : "memory");
}

int main() {
  _TPtr<double> arr = t_malloc<double>(128);
  double bound = 129;
  // Initialize the array with squares of indices
  for(int i = 0; i < bound; ++i) {
    arr[i] = i * i;
    do_not_optimize(arr[i]); // Prevent loop from being optimized away
  }

  // Print the array elements
  for(int i = 0; i < 5; ++i) {
    printf("%f", arr[i]);
  }
  printf("\n");

  t_free(arr);
  return 0;
}
