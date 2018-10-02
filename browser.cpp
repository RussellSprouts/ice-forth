
#include "6502.h"
#include "out/bootstrap.bin.h"
#include <iostream>

int main(int argc, char **argv) {
  initOpcodes();

  for (int i = 0; i < bootstrap_bin_len; i++) {
    m.memory[i] = bootstrap_bin[i];
  }

  m.ip = m.memory[0xFFFC] + (m.memory[0xFFFD] << 8) - 1;

  while (emulate()) {
    lastLineInput.clear();
    std::getline(std::cin, lastLineInput);
    lastLineInput += "\n";
    lineIndex = 0;
  }
}
