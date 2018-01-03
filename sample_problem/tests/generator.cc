#include <cstdio>
#include <cstdlib>
#include "testlib.h"

const int MaxT[] = {100, 1000};
const int MaxN[] = {1000, (int) 1e5};

// rnd.next(1, 10)
// rnd.next("[a-zA-Z0-9]{1,1000}")
int main(int argc, char** argv) {
  registerGen(argc, argv, 1);

  int diff = atoi(argv[2]);
  
  int T = MaxT[diff];
  printf("%d\n", T);
  for (int cas = 1; cas <= T; cas++) {
    printf("%d %d\n", rnd.next(1, MaxN[diff]), rnd.next(1, MaxN[diff]));
  }
  return 0;
}
