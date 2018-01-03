import java.io.*;
import java.util.*;

public class Main {
  public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    int T = scanner.nextInt();
    for (int cas = 1; cas <= T; cas++) {
      int a = scanner.nextInt();
      int b = scanner.nextInt();
      System.out.printf("Case #%d: %d\n", cas, a + b);
    }
  }
}
