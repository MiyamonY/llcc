#include <stdio.h>
/* gcc -c test.c */
void test0(void) { printf("ok\n"); }

void test6(int a0, int a1, int a2, int a3, int a4, int a5) {
  printf("%d %d %d %d %d %d\n", a0, a1, a2, a3, a4, a5);
}

#if defined(TEST)
void main(void) { test6(0, 1, 2, 3, 4, 5); }
#endif
