#include <iostream>

int main() {
  int T, a, b;

  std::cin >> T;
  for (int cas = 1; cas <= T; cas++) {
    std::cin >> a >> b;
    std::cout << "Case #" << cas << ": " << a + b << std::endl;
  }
  
  return 0;
}
