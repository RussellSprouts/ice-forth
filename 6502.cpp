
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <exception>

#define IO_PORT 0x401C
#define MEMORY_SIZE (64*1024)

typedef struct Machine {
  uint8_t a = 0;
  uint8_t x = 0;
  uint8_t y = 0;
  uint8_t sp = 0xFF;
  uint16_t ip = 0xFFFF;
  struct {
    bool n = false;
    bool v = false;
    bool z = false;
    bool s = false;
    bool c = false;
    bool d = false;
    bool i = false;
  } status;
  uint8_t memory[MEMORY_SIZE];
} Machine;

static Machine m;
static bool trace;

static std::string lastLineInput;
static size_t lineIndex = 0;

typedef struct OutOfInputException
  : public std::exception
  {} OutOfInputException;

static uint8_t nextChar() {
  if (lineIndex == lastLineInput.size()) {
    throw OutOfInputException();
  }
  uint8_t result = lastLineInput[lineIndex];
  lineIndex++;
  return result;
}

static uint8_t read(uint16_t addr) {
  if (addr == IO_PORT) {
    return nextChar();
  } else {
    return m.memory[addr];
  }
}

static void set(uint16_t addr, uint8_t val) {
  if (addr == IO_PORT) {
    putchar(val);
  } else {
    m.memory[addr] = val;
  }
}

static uint8_t pop() {
  m.sp = m.sp + 1;
  return read(0x100 + m.sp);
}

static void push(uint8_t val) {
  set(0x100 + m.sp, val);
  m.sp = m.sp - 1;
}

static uint16_t getAbsolute() {
  m.ip = m.ip + 2;
  uint8_t low = read(m.ip - 1);
  return low + (read(m.ip) << 8);
}

static uint8_t readImmediate() {
  m.ip = m.ip + 1;
  return read(m.ip);
}

static uint8_t readZeroPage() {
  m.ip = m.ip + 1;
  return read(read(m.ip));
}

static void storeZeroPage(uint8_t val) {
  m.ip = m.ip + 1;
  set(read(m.ip), val);
}

static uint8_t readZeroPageX() {
  m.ip = m.ip + 1;
  return read(0xFF & (read(m.ip) + m.x));
}

static void storeZeroPageX(uint8_t val) {
  m.ip = m.ip + 1;
  set(0xFF & (read(m.ip) + m.x), val);
}

static uint8_t readZeroPageY() {
  m.ip = m.ip + 1;
  return read(0xFF & (read(m.ip) + m.y));
}

static void storeZeroPageY(uint8_t val) {
  m.ip = m.ip + 1;
  uint16_t addr = read(m.ip) + m.y;
  set(0xFF & addr, val);
}

static uint8_t readAbsolute() {
  return read(getAbsolute());
}

static void storeAbsolute(uint8_t val) {
  set(getAbsolute(), val);
}

static uint8_t readAbsoluteX() {
  return read(m.x + getAbsolute());
}

static void storeAbsoluteX(uint8_t val) {
  set(m.x + getAbsolute(), val);
}

static uint8_t readAbsoluteY() {
  return read(m.y + getAbsolute());
}

static void storeAbsoluteY(uint8_t val) {
  set(m.y + getAbsolute(), val);
}

static uint16_t readIndirect() {
  uint16_t addr = getAbsolute();
  uint16_t addrPlus1 = addr + 1;
  // adjust for jmp indirect bug
  if ((addrPlus1 & 0xFF) == 0) {
    addrPlus1 -= 0x100;
  }
  uint8_t low = read(addr);
  return low + (read(addrPlus1) << 8);
}

static uint8_t readIndirectX() {
  uint8_t zpAddr = readImmediate() + m.x;
  uint8_t addrPlus1 = zpAddr + 1;
  uint8_t low = read(zpAddr);
  return read(low + (read(addrPlus1) << 8));
}

static void storeIndirectX(uint8_t val) {
  uint8_t zpAddr = readImmediate() + m.x;
  uint8_t addrPlus1 = zpAddr + 1;
  uint8_t low = read(zpAddr);
  set(low + (read(addrPlus1) << 8), val);
}

static uint8_t readIndirectY() {
  uint8_t addr = readImmediate();
  uint8_t addrPlus1 = addr + 1;
  uint8_t low = read(addr);
  return read(low + (read(addrPlus1) << 8) + m.y);
}

static void storeIndirectY(uint8_t val) {
  uint8_t addr = readImmediate();
  uint8_t addrPlus1 = addr + 1;
  uint8_t low = read(addr);
  set(low + (read(addrPlus1) << 8) + m.y, val);
}

static uint8_t setSZ(uint8_t val) {
  m.status.s = val >= 0x80;
  m.status.z = val == 0;
  return val;
}

static void BIT(uint8_t read) {
  m.status.z = m.a & read;
  m.status.n = read & 0x80;
  m.status.v = read & 0x40;
}

static void LDA(uint8_t value) {
  m.a = setSZ(value);
}

static void LDX(uint8_t value) {
  m.x = setSZ(value);
}

static void LDY(uint8_t value) {
  m.y = setSZ(value);
}

static void INC(uint16_t addr) {
  set(addr, setSZ(read(addr) + 1));
}

static void DEC(uint16_t addr) {
  set(addr, setSZ(read(addr) - 1));
}

static void AND(uint8_t value) {
  m.a = setSZ(m.a & value);
}

static void ORA(uint8_t value) {
  m.a = setSZ(m.a | value);
}

static void EOR(uint8_t value) {
  m.a = setSZ(m.a ^ value);
}

static void ASL(uint16_t addr) {
  uint8_t value = read(addr);
  m.status.c = value >= 0x80;
  set(addr, setSZ(value << 1));
}

static void ROL(uint16_t addr) {
  uint8_t value = read(addr);
  uint8_t c = m.status.c ? 1 : 0;
  m.status.c = value >= 0x80;
  set(addr, setSZ((value << 1) + c));
}

static void LSR(uint16_t addr) {
  uint8_t value = read(addr);
  m.status.c = value & 1;
  set(addr, setSZ((value >> 1) & 0x7F));
}

static void ROR(uint16_t addr) {
  uint8_t value = read(addr);
  uint8_t c = m.status.c ? 0x80 : 0;
  m.status.c = value & 1;
  set(addr, setSZ(((value >> 1) & 0x7F) + c));
}

static void BRANCH(uint8_t condition) {
  uint8_t disp = readImmediate();
  if (condition) {
    if (disp >= 0x80) {
      m.ip = m.ip - 0x100;
    }
    m.ip = m.ip + disp;
  }
}

static void ADC(uint8_t value) {
  uint16_t newValue = m.a + value + !!m.status.c;
  m.status.c = newValue > 0xFF;
  m.status.v = !((m.a ^ value) & 0x80) && ((m.a ^ newValue) & 0x80);
  m.a = setSZ((uint8_t)newValue);
}

static void SBC(uint8_t value) {
  ADC(~value);
}

static void CMP(uint8_t a, uint8_t b) {
  uint8_t comparison = a - b;
  m.status.z = comparison == 0;
  m.status.c = a >= b;
  m.status.n = comparison >= 0x80;
}

static void SET_P(uint8_t a) {
  m.status.n = a & 0x80;
  m.status.v = a & 0x40;
  m.status.d = a & 0x08;
  m.status.i = a & 0x04;
  m.status.z = a & 0x02;
  m.status.c = a & 0x01;
}

typedef struct Handler {
  const char *name;
  void (*handler)();
} Handler;

int main(int argc, char **argv) {
  trace = false;

  Handler opcodes[256] = { 0 };

  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};
  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};
  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};
  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};
  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};

  // bit
  opcodes[0x24] = {"BIT ZP", []{ BIT(readZeroPage()); }};
  opcodes[0x2C] = {"BIT ABS", []{ BIT(readAbsolute()); }};
  // lda
  opcodes[0xA9] = {"LDA IMM", []{ LDA(readImmediate()); }};
  opcodes[0xA5] = {"LDA ZP", []{ LDA(readZeroPage()); }};
  opcodes[0xB5] = {"LDA ZPX", []{ LDA(readZeroPageX()); }};
  opcodes[0xAD] = {"LDA ABS", []{ LDA(readAbsolute()); }};
  opcodes[0xBD] = {"LDA ABSX", []{ LDA(readAbsoluteX()); }};
  opcodes[0xB9] = {"LDA ABSY", []{ LDA(readAbsoluteY()); }};
  opcodes[0xA1] = {"LDA INDX", []{ LDA(readIndirectX()); }};
  opcodes[0xB1] = {"LDA INDY", []{ LDA(readIndirectY()); }};
  // sta
  opcodes[0x85] = {"STA", []{ storeZeroPage(m.a); }};
  opcodes[0x95] = {"STA", []{ storeZeroPageX(m.a); }};
  opcodes[0x8D] = {"STA", []{ storeAbsolute(m.a); }};
  opcodes[0x9D] = {"STA", []{ storeAbsoluteX(m.a); }};
  opcodes[0x99] = {"STA", []{ storeAbsoluteY(m.a); }};
  opcodes[0x81] = {"STA", []{ storeIndirectX(m.a); }};
  opcodes[0x91] = {"STA", []{ storeIndirectY(m.a); }};

  // ldx
  opcodes[0xA2] = {"LDX", []{ LDX(readImmediate()); }};
  opcodes[0xA6] = {"LDX", []{ LDX(readZeroPage()); }};
  opcodes[0xB6] = {"LDX", []{ LDX(readZeroPageY()); }};
  opcodes[0xAE] = {"LDX", []{ LDX(readAbsolute()); }};
  opcodes[0xBE] = {"LDX", []{ LDX(readAbsoluteY()); }};
  // stx
  opcodes[0x86] = {"STX", []{ storeZeroPage(m.x); }};
  opcodes[0x96] = {"STX", []{ storeZeroPageY(m.x); }};
  opcodes[0x8E] = {"STX", []{ storeAbsolute(m.x); }};

  // ldy
  opcodes[0xA0] = {"LDY", []{ LDY(readImmediate()); }};
  opcodes[0xA4] = {"LDY", []{ LDY(readZeroPage()); }};
  opcodes[0xB4] = {"LDY", []{ LDY(readZeroPageX()); }};
  opcodes[0xAC] = {"LDY", []{ LDY(readAbsolute()); }};
  opcodes[0xBC] = {"LDY", []{ LDY(readAbsoluteX()); }};
  // sty
  opcodes[0x84] = {"STY", []{ storeZeroPage(m.y); }};
  opcodes[0x94] = {"STY", []{ storeZeroPageX(m.y); }};
  opcodes[0x8C] = {"STY", []{ storeAbsolute(m.y); }};

  // tsx/txs
  opcodes[0xBA] = {"TSX", []{ m.x = setSZ(m.sp); }};
  opcodes[0x9A] = {"TXS", []{ m.sp = m.x; }};

  // pha/pla
  opcodes[0x48] = {"PHA", []{ push(m.a); }};
  opcodes[0x68] = {"PLA", []{ m.a = pop(); }};

  opcodes[0x08] = {"PHP", []{
    push(
      (m.status.n ? 0x80 : 0)
      | (m.status.v ? 0x40 : 0)
      | 0x20
      | 0x10
      | (m.status.d ? 0x08 : 0)
      | (m.status.i ? 0x04 : 0)
      | (m.status.z ? 0x02 : 0)
      | (m.status.c ? 0x01 : 0));
  }};
  opcodes[0x28] = {"PLP", []{ SET_P(pop()); }};

  // tax/txa
  opcodes[0xAA] = {"TAX", []{ m.x = setSZ(m.a); }};
  opcodes[0x8A] = {"TXA", []{ m.a = setSZ(m.x); }};

  // dex/inx
  opcodes[0xCA] = {"DEX", []{ m.x = setSZ(m.x - 1); }};
  opcodes[0xE8] = {"INX", []{ m.x = setSZ(m.x + 1); }};

  // tay/tya
  opcodes[0xA8] = {"TAY", []{ m.y = setSZ(m.a); }};
  opcodes[0x98] = {"TYA", []{ m.a = setSZ(m.y); }};

  // dey/iny
  opcodes[0x88] = {"DEY", []{ m.y = setSZ(m.y - 1); }};
  opcodes[0xC8] = {"INY", []{ m.y = setSZ(m.y + 1); }};

  // nop
  opcodes[0xEA] = {"NOP", []{; }};

  // inc
  opcodes[0xE6] = {"INC", []{ INC(readImmediate()); }};
  opcodes[0xF6] = {"INC", []{ INC((readImmediate() + m.x) & 0xFF); }};
  opcodes[0xEE] = {"INC", []{ INC(getAbsolute()); }};
  opcodes[0xFE] = {"INC", []{ INC(getAbsolute() + m.x); }};

  // dec
  opcodes[0xC6] = {"DEC", []{ DEC(readImmediate()); }};
  opcodes[0xD6] = {"DEC", []{ DEC((readImmediate() + m.x) & 0xFF); }};
  opcodes[0xCE] = {"DEC", []{ DEC(getAbsolute()); }};
  opcodes[0xDE] = {"DEC", []{ DEC(getAbsolute() + m.x); }};

  // jmp
  opcodes[0x4C] = {"JMP", []{ m.ip = getAbsolute() - 1; }};
  opcodes[0x6C] = {"JMPI", []{ m.ip = readIndirect() - 1; }};

  // flag instructions
  opcodes[0x18] = {"CLC", []{ m.status.c = 0; }};
  opcodes[0x38] = {"SEC", []{ m.status.c = 1; }};
  opcodes[0x58] = {"CLI", []{ m.status.i = 0; }};
  opcodes[0x78] = {"SEI", []{ m.status.i = 1; }};
  opcodes[0xB8] = {"CLV", []{ m.status.v = 0; }};
  opcodes[0xD8] = {"CLC", []{ m.status.c = 0; }};
  opcodes[0xF8] = {"SEC", []{ m.status.c = 1; }};

  // and
  opcodes[0x29] = {"AND", []{ AND(readImmediate()); }};
  opcodes[0x25] = {"AND", []{ AND(readZeroPage()); }};
  opcodes[0x35] = {"AND", []{ AND(readZeroPageX()); }};
  opcodes[0x2D] = {"AND", []{ AND(readAbsolute()); }};
  opcodes[0x3D] = {"AND", []{ AND(readAbsoluteX()); }};
  opcodes[0x39] = {"AND", []{ AND(readAbsoluteY()); }};
  opcodes[0x21] = {"AND", []{ AND(readIndirectX()); }};
  opcodes[0x31] = {"AND", []{ AND(readIndirectY()); }};

  // ora
  opcodes[0x09] = {"ORA", []{ ORA(readImmediate()); }};
  opcodes[0x05] = {"ORA", []{ ORA(readZeroPage()); }};
  opcodes[0x15] = {"ORA", []{ ORA(readZeroPageX()); }};
  opcodes[0x0D] = {"ORA", []{ ORA(readAbsolute()); }};
  opcodes[0x1D] = {"ORA", []{ ORA(readAbsoluteX()); }};
  opcodes[0x19] = {"ORA", []{ ORA(readAbsoluteY()); }};
  opcodes[0x01] = {"ORA", []{ ORA(readIndirectX()); }};
  opcodes[0x11] = {"ORA", []{ ORA(readIndirectY()); }};

  // eor
  opcodes[0x49] = {"EOR", []{ EOR(readImmediate()); }};
  opcodes[0x45] = {"EOR", []{ EOR(readZeroPage()); }};
  opcodes[0x55] = {"EOR", []{ EOR(readZeroPageX()); }};
  opcodes[0x4D] = {"EOR", []{ EOR(readAbsolute()); }};
  opcodes[0x5D] = {"EOR", []{ EOR(readAbsoluteX()); }};
  opcodes[0x59] = {"EOR", []{ EOR(readAbsoluteY()); }};
  opcodes[0x41] = {"EOR", []{ EOR(readIndirectX()); }};
  opcodes[0x51] = {"EOR", []{ EOR(readIndirectY()); }};

  // asl
  opcodes[0x0A] = {"ASL", []{
    m.status.c = m.a & 0x80;
    m.a = setSZ(m.a << 1);
  }};
  opcodes[0x06] = {"ASL", []{ ASL(readImmediate()); }};
  opcodes[0x16] = {"ASL", []{ ASL((readImmediate() + m.x) & 0xFF); }};
  opcodes[0x0E] = {"ASL", []{ ASL(getAbsolute()); }};
  opcodes[0x1E] = {"ASL", []{ ASL(getAbsolute() + m.x); }};

  // lsr
  opcodes[0x4A] = {"LSR", []{
    m.status.c = m.a & 1;
    m.a = setSZ((m.a >> 1) & 0x7F);
  }};
  opcodes[0x46] = {"LSR", []{ LSR(readImmediate()); }};
  opcodes[0x56] = {"LSR", []{ LSR((readImmediate() + m.x) & 0xFF); }};
  opcodes[0x4E] = {"LSR", []{ LSR(getAbsolute()); }};
  opcodes[0x5E] = {"LSR", []{ LSR(getAbsolute() + m.x); }};

  // rol
  opcodes[0x2A] = {"ROL", []{
    bool c = m.status.c;
    m.status.c = m.a & 0x80;
    m.a = setSZ((m.a << 1) + c);
  }};
  opcodes[0x26] = {"ROL", []{ ROL(readImmediate()); }};
  opcodes[0x36] = {"ROL", []{ ROL((readImmediate() + m.x) & 0xFF); }};
  opcodes[0x2E] = {"ROL", []{ ROL(getAbsolute()); }};
  opcodes[0x3E] = {"ROL", []{ ROL(getAbsolute() + m.x); }};

  // ror
  opcodes[0x6A] = {"ROR", []{
    uint8_t c = m.status.c ? 0x80 : 0;
    m.status.c = m.a & 1;
    m.a = setSZ(((m.a >> 1) & 0x7F) + c);
  }};
  opcodes[0x66] = {"ROR", []{ ROR(readImmediate()); }};
  opcodes[0x76] = {"ROR", []{ ROR((readImmediate() + m.x) & 0xFF); }};
  opcodes[0x6E] = {"ROR", []{ ROR(getAbsolute()); }};
  opcodes[0x7E] = {"ROR", []{ ROR(getAbsolute() + m.x); }};

  // jsr
  opcodes[0x20] = {"JSR", []{
    uint16_t target = getAbsolute();
    //print(string.format("JSR %04x from %04x", target, m.ip))
    push(m.ip >> 8);
    push(m.ip);
    m.ip = target - 1;
  }};

  // rts
  opcodes[0x60] = {"RTS", []{
    uint8_t lo = pop();
    m.ip = lo + (pop() << 8);
  }};

  opcodes[0x40] = {"RTI", []{
    SET_P(pop());
    uint8_t lo = pop();
    m.ip = lo + (pop() << 8) - 1;
  }};

  // branch
  opcodes[0x10] = {"BPL", []{ BRANCH(!m.status.s); }};
  opcodes[0x30] = {"BMI", []{ BRANCH(m.status.s); }};
  opcodes[0x50] = {"BVC", []{ BRANCH(!m.status.v); }};
  opcodes[0x70] = {"BVS", []{ BRANCH(m.status.v); }};
  opcodes[0x90] = {"BCC", []{ BRANCH(!m.status.c); }};
  opcodes[0xB0] = {"BCS", []{ BRANCH(m.status.c); }};
  opcodes[0xD0] = {"BNE", []{ BRANCH(!m.status.z); }};
  opcodes[0xF0] = {"BEQ", []{ BRANCH(m.status.z); }};

  opcodes[0x69] = {"ADC", []{ ADC(readImmediate()); }};
  opcodes[0x65] = {"ADC", []{ ADC(readZeroPage()); }};
  opcodes[0x75] = {"ADC", []{ ADC(readZeroPageX()); }};
  opcodes[0x6D] = {"ADC", []{ ADC(readAbsolute()); }};
  opcodes[0x7D] = {"ADC", []{ ADC(readAbsoluteX()); }};
  opcodes[0x79] = {"ADC", []{ ADC(readAbsoluteY()); }};
  opcodes[0x61] = {"ADC", []{ ADC(readIndirectX()); }};
  opcodes[0x71] = {"ADC", []{ ADC(readIndirectY()); }};

  opcodes[0xE9] = {"SBC", []{ SBC(readImmediate()); }};
  opcodes[0xE5] = {"SBC", []{ SBC(readZeroPage()); }};
  opcodes[0xF5] = {"SBC", []{ SBC(readZeroPageX()); }};
  opcodes[0xED] = {"SBC", []{ SBC(readAbsolute()); }};
  opcodes[0xFD] = {"SBC", []{ SBC(readAbsoluteX()); }};
  opcodes[0xF9] = {"SBC", []{ SBC(readAbsoluteY()); }};
  opcodes[0xE1] = {"SBC", []{ SBC(readIndirectX()); }};
  opcodes[0xF1] = {"SBC", []{ SBC(readIndirectY()); }};

  opcodes[0xC9] = {"CMP", []{ CMP(m.a, readImmediate()); }};
  opcodes[0xC5] = {"CMP", []{ CMP(m.a, readZeroPage()); }};
  opcodes[0xD5] = {"CMP", []{ CMP(m.a, readZeroPageX()); }};
  opcodes[0xCD] = {"CMP", []{ CMP(m.a, readAbsolute()); }};
  opcodes[0xDD] = {"CMP", []{ CMP(m.a, readAbsoluteX()); }};
  opcodes[0xD9] = {"CMP", []{ CMP(m.a, readAbsoluteY()); }};
  opcodes[0xC1] = {"CMP", []{ CMP(m.a, readIndirectX()); }};
  opcodes[0xD1] = {"CMP", []{ CMP(m.a, readIndirectY()); }};

  opcodes[0xE0] = {"CPX", []{ CMP(m.x, readImmediate()); }};
  opcodes[0xE4] = {"CPX", []{ CMP(m.x, readZeroPage()); }};
  opcodes[0xEC] = {"CPX", []{ CMP(m.x, readAbsolute()); }};

  opcodes[0xC0] = {"CPY", []{ CMP(m.y, readImmediate()); }};
  opcodes[0xC4] = {"CPY", []{ CMP(m.y, readZeroPage()); }};
  opcodes[0xCC] = {"CPY", []{ CMP(m.y, readAbsolute()); }};

  opcodes[0xFF] = {"DBG_START", []{
    printf("DEBUGGER STARTED");
    trace = true;
  }};
  opcodes[0xEF] = {"DBG_END", []{
    printf("DEBUGGER ENDED");
    trace = false;
  }};
  opcodes[0xDF] = {"DBG_TRACE", []{
    m.ip = m.ip + 1;
    while (m.memory[m.ip]) {
      putchar(m.memory[m.ip]);
      m.ip = m.ip + 1;
    }
    printf("\n");
  }};

  FILE *bootstrap = fopen("bootstrap.bin", "r");
  if (!fread(&m.memory, 1, sizeof(m.memory), bootstrap)) {
    printf("Couldn't open bootstrap.bin");
    return 1;
  }

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

  while (true) {
    if (m.ip == 0xFFFF) { break; }

    uint16_t ip = m.ip;
    uint8_t opcode = readImmediate();
    if (opcodes[opcode].handler == nullptr) {
      printf("\n\nUnkown opcode $%02x\n", opcode);
      printf("IP:\t%04x\t%02x\ta:%02x\tx:%02x\ty:%02x", m.ip, opcode, m.a, m.x, m.y);
      printf("\n");
      for (int i = 0; i < 256; i++) {
        printf("%02x ", m.memory[0x100+i]);
      }
      break;
    } else {
      if (trace) {
        for (int i = 0; i < (0xFF - m.sp)/2; i++) {
          putchar(' ');
        }
        printf("IP:\t%04x\t%02x\t%s\ta:%02x\tx:%02x\ty:%02x\tv:%01x\n",
          m.ip,
          opcode,
          opcodes[opcode].name,
          m.a,
          m.x,
          m.y,
          m.status.v);
      }
      try {
        opcodes[opcode].handler();
      } catch (OutOfInputException &e) {
        // The instruction ran, but didn't have
        // enough user input to finish. Reset the
        // instruction pointer, get more input, and
        // try again.
        m.ip = ip;
        lastLineInput.clear();
        std::getline(std::cin, lastLineInput);
        lastLineInput += "\n";
        lineIndex = 0;
      }
    }
  }

  FILE *romout = fopen("out.nes", "w+");
  FILE *ramout = fopen("ram.out", "w+");

  fwrite("NES\x1A\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", 1, 16, romout);
  fwrite(&m.memory[0x8000], 1, 0x8000, romout);

  fwrite(&m.memory, 1, 0x800, ramout);
}
