
#include "6502.h"
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <exception>

int main(int argc, char **argv) {
  trace = false;

  initOpcodes();

  FILE *bootstrap = fopen("out/bootstrap.bin", "r");
  if (!fread(&m.memory, 1, sizeof(m.memory), bootstrap)) {
    printf("Couldn't open bootstrap.bin");
    return 1;
  }

	opsFile = fopen("out/ops.out", "w+");

  for (int i = 1; i < argc; i++) {
    if (argv[i] == std::string("-b")) {
      std::string binary = argv[i+1];
      int splitPoint = binary.find(":");
      std::string name = binary.substr(0, splitPoint);
      std::string file = binary.substr(splitPoint+1);
      std::ifstream in(file.c_str());
      std::string fileContents(
        (std::istreambuf_iterator<char>(in)),
        std::istreambuf_iterator<char>());
      std::stringstream ss;
      ss << " " << std::hex << fileContents.size() << " heredoc " << name << " " << fileContents << " ";
      lastLineInput += ss.str();
      i++;
    } else {
      std::ifstream in(argv[i]);
      std::string fileContents(
        (std::istreambuf_iterator<char>(in)),
        std::istreambuf_iterator<char>());
      lastLineInput.append(fileContents);
    }
  }

  m.ip = m.memory[0xFFFC] + (m.memory[0xFFFD] << 8) - 1;

  while(emulate()) {
    lastLineInput.clear();
    std::getline(std::cin, lastLineInput);
    lastLineInput += "\n";
    lineIndex = 0;
  }

  FILE *romout = fopen("out/game.nes", "w+");
  FILE *ramout = fopen("out/ram.out", "w+");


  fwrite("NES\x1A\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", 1, 16, romout);
  fwrite(&m.memory[0x8000], 1, 0x8000, romout);

  fwrite(&m.memory, 1, 0x800, ramout);
}
