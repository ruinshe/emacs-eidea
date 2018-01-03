#include <cstdio>
#include "testlib.h"

int main(int argc, char** argv) {
  registerValidation(argc, argv);
  int T = inf.readInt(1, 1000, "T");
  inf.readEoln();
  for (int cas = 1; cas <= T; cas++) {
    inf.readInt(1, (int) 1e5, "a");
    inf.readSpace();
    inf.readInt(1, (int) 1e5, "b");
    inf.readEoln();
  }
  inf.readEof();
  inf.close();
  return 0;
}
