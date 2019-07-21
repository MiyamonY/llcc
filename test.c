#include <stdio.h>
/* gcc -c test.c */
int test0(void) { return 10; }
int test1(long a) { return a + 3; }

int test6(long a, long b, long c, long d, long e, long f) {
  return a + b + c + d + e + f;
}

#if defined(TEST)
void main(void) {
  int x = 0;
  test6(x, 1, 2, 3, 4, 5);
  printf("%d", x);
}
#endif
