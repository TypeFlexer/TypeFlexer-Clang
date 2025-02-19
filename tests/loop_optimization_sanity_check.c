#include <stdio.h>
#include <stdlib_tainted.h>


/***********************************************************************
 * TEST 1: Simple for-loop with a constant bound
 * We expect the pass might try to hoist checks outside the loop because
 * the bound is constant. We'll see if the loop modifies anything that
 * prevents hoisting or not.
 **********************************************************************/
void test_forloop_constant(void) {
  // Allocate a tainted pointer to double
  _TPtr<double> arr = t_malloc<double>(100);
  
  // Fill array with squares from index 0..49
  // The loop bound is 50 => we expect up to arr[49] = 49^2
  for (int i = 0; i < 50; ++i) {
    arr[i] = (double)i * arr[i];
  }
}

int main(void) {
  printf("Running various loop optimization tests with Tainted Pointers...\n\n");

  // 1. For-loop, constant bound
  test_forloop_constant();

  return 0;
}
