#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"
#include <string>

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

static uint16_t popAddr() {
  uint8_t lo = pop();
  uint16_t hi = pop() << 8;
  return hi + lo;
}

static void push(uint8_t val) {
  set(0x100 + m.sp, val);
  m.sp = m.sp - 1;
}

static void pushAddr(uint16_t addr) {
  push(addr >> 8);
  push(addr);
}

static uint8_t immediate() {
  m.ip = m.ip + 1;
  return read(m.ip);
}

// Gets the next two bytes from the instruction stream
// and returns them as a little-endian address.
static uint16_t absolute() {
  uint8_t low = immediate();
  uint16_t hi = immediate() << 8;
  return hi + low;
}

static uint8_t zeroPage() { return immediate(); }
static uint8_t zeroPageX() { return zeroPage() + m.x; }
static uint8_t zeroPageY() { return zeroPage() + m.y; }
static uint16_t absoluteX() { return absolute() + m.x; }
static uint16_t absoluteY() { return absolute() + m.y; }

static uint16_t indirect() {
  uint16_t addr = absolute();
  uint16_t addrPlus1 = addr + 1;
  // adjust for jmp indirect bug
  if ((addrPlus1 & 0xFF) == 0) {
    addrPlus1 -= 0x100;
  }
  uint8_t low = read(addr);
  uint16_t hi = read(addrPlus1) << 8;
  return hi + low;
}

static uint16_t indirectX() {
  uint8_t zpAddr = zeroPageX();
  uint8_t addrPlus1 = zpAddr + 1;
  uint8_t low = read(zpAddr);
  uint16_t hi = read(addrPlus1) << 8;
  return hi + low;
}

static uint16_t indirectY() {
  uint8_t addr = zeroPage();
  uint8_t addrPlus1 = addr + 1;
  uint8_t low = read(addr);
  uint16_t hi = read(addrPlus1) << 8;
  return hi + low + m.y;
}

static uint8_t setSZ(uint8_t val) {
  m.status.n = val >= 0x80;
  m.status.z = val == 0;
  return val;
}
static void BIT(uint8_t read) {
  m.status.z = m.a & read;
  m.status.n = read & 0x80;
  m.status.v = read & 0x40;
}

static void LDA(uint8_t value) { m.a = setSZ(value); }
static void LDX(uint8_t value) { m.x = setSZ(value); }
static void LDY(uint8_t value) { m.y = setSZ(value); }
static void INC(uint16_t addr) { set(addr, setSZ(read(addr) + 1)); }
static void DEC(uint16_t addr) { set(addr, setSZ(read(addr) - 1)); }
static void AND(uint8_t value) { m.a = setSZ(m.a & value); }
static void ORA(uint8_t value) { m.a = setSZ(m.a | value); }
static void EOR(uint8_t value) { m.a = setSZ(m.a ^ value); }

static void ASL_A() {
  m.status.c = m.a >= 0x80;
  m.a = setSZ(m.a << 1);
}

static void ASL(uint16_t addr) {
  uint8_t value = read(addr);
  m.status.c = value >= 0x80;
  set(addr, setSZ(value << 1));
}

static void ROL_A() {
  uint8_t c = m.status.c;
  m.status.c = m.a >= 0x80;
  m.a = setSZ((m.a << 1) + c);
}

static void ROL(uint16_t addr) {
  uint8_t value = read(addr);
  uint8_t c = m.status.c;
  m.status.c = value >= 0x80;
  set(addr, setSZ((value << 1) + c));
}

static void LSR_A() {
  m.status.c = m.a & 1;
  m.a = setSZ(m.a >> 1);
}

static void LSR(uint16_t addr) {
  uint8_t value = read(addr);
  m.status.c = value & 1;
  set(addr, setSZ(value >> 1));
}

static void ROR_A() {
  uint8_t c = m.status.c ? 0x80 : 0;
  m.status.c = m.a & 1;
  m.a = setSZ((m.a >> 1) + c);
}

static void ROR(uint16_t addr) {
  uint8_t value = read(addr);
  uint8_t c = m.status.c ? 0x80 : 0;
  m.status.c = value & 1;
  set(addr, setSZ((value >> 1) + c));
}

static void BRANCH(uint8_t condition) {
  uint8_t disp = immediate();
  if (condition) {
    if (disp >= 0x80) {
      m.ip = m.ip - 0x100;
    }
    m.ip = m.ip + disp;
  }
}

static void JMP(uint16_t addr) {
  m.ip = addr - 1;
}

static void ADC(uint8_t value) {
  uint16_t newValue = m.a + value + m.status.c;
  m.status.c = newValue > 0xFF;
  m.status.v = !((m.a ^ value) & 0x80) && ((m.a ^ newValue) & 0x80);
  m.a = setSZ((uint8_t)newValue);
}

static void SBC(uint8_t value) { ADC(~value); }

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

static uint8_t GET_P() {
  return (m.status.n ? 0x80 : 0)
    | (m.status.v ? 0x40 : 0)
    | 0x20
    | 0x10
    | (m.status.d ? 0x08 : 0)
    | (m.status.i ? 0x04 : 0)
    | (m.status.z ? 0x02 : 0)
    | (m.status.c ? 0x01 : 0);
}

typedef struct Handler {
  const char *name;
  void (*handler)();
} Handler;

static Handler opcodes[256] = { 0 };

void initOpcodes() {
  // bit
  opcodes[0x24] = {"BIT ZP", []{ BIT(read(zeroPage())); }};
  opcodes[0x2C] = {"BIT ABS", []{ BIT(read(absolute())); }};
  // lda
  opcodes[0xA9] = {"LDA IMM", []{ LDA(immediate()); }};
  opcodes[0xA5] = {"LDA ZP", []{ LDA(read(zeroPage())); }};
  opcodes[0xB5] = {"LDA ZPX", []{ LDA(read(zeroPageX())); }};
  opcodes[0xAD] = {"LDA ABS", []{ LDA(read(absolute())); }};
  opcodes[0xBD] = {"LDA ABSX", []{ LDA(read(absoluteX())); }};
  opcodes[0xB9] = {"LDA ABSY", []{ LDA(read(absoluteY())); }};
  opcodes[0xA1] = {"LDA INDX", []{ LDA(read(indirectX())); }};
  opcodes[0xB1] = {"LDA INDY", []{ LDA(read(indirectY())); }};

  opcodes[0x8A] = {"TXA", []{ LDA(m.x); }};
  opcodes[0x98] = {"TYA", []{ LDA(m.y); }};

  // sta
  opcodes[0x85] = {"STA", []{ set(zeroPage(), m.a); }};
  opcodes[0x95] = {"STA", []{ set(zeroPageX(), m.a); }};
  opcodes[0x8D] = {"STA", []{ set(absolute(), m.a); }};
  opcodes[0x9D] = {"STA", []{ set(absoluteX(), m.a); }};
  opcodes[0x99] = {"STA", []{ set(absoluteY(), m.a); }};
  opcodes[0x81] = {"STA", []{ set(indirectX(), m.a); }};
  opcodes[0x91] = {"STA", []{ set(indirectY(), m.a); }};

  // ldx
  opcodes[0xA2] = {"LDX", []{ LDX(immediate()); }};
  opcodes[0xA6] = {"LDX", []{ LDX(read(zeroPage())); }};
  opcodes[0xB6] = {"LDX", []{ LDX(read(zeroPageY())); }};
  opcodes[0xAE] = {"LDX", []{ LDX(read(absolute())); }};
  opcodes[0xBE] = {"LDX", []{ LDX(read(absoluteY())); }};
  opcodes[0xAA] = {"TAX", []{ LDX(m.a); }};
  opcodes[0xCA] = {"DEX", []{ LDX(m.x - 1); }};
  opcodes[0xE8] = {"INX", []{ LDX(m.x + 1); }};
  opcodes[0xBA] = {"TSX", []{ LDX(m.sp); }};

  // stx
  opcodes[0x86] = {"STX", []{ set(zeroPage(), m.x); }};
  opcodes[0x96] = {"STX", []{ set(zeroPageY(), m.x); }};
  opcodes[0x8E] = {"STX", []{ set(absolute(), m.x); }};
  opcodes[0x9A] = {"TXS", []{ m.sp = m.x; }};

  // ldy
  opcodes[0xA0] = {"LDY", []{ LDY(immediate()); }};
  opcodes[0xA4] = {"LDY", []{ LDY(read(zeroPage())); }};
  opcodes[0xB4] = {"LDY", []{ LDY(read(zeroPageX())); }};
  opcodes[0xAC] = {"LDY", []{ LDY(read(absolute())); }};
  opcodes[0xBC] = {"LDY", []{ LDY(read(absoluteX())); }};

  opcodes[0xA8] = {"TAY", []{ LDY(m.a); }};
  opcodes[0x88] = {"DEY", []{ LDY(m.y - 1); }};
  opcodes[0xC8] = {"INY", []{ LDY(m.y + 1); }};

  // sty
  opcodes[0x84] = {"STY", []{ set(zeroPage(), m.y); }};
  opcodes[0x94] = {"STY", []{ set(zeroPageX(), m.y); }};
  opcodes[0x8C] = {"STY", []{ set(absolute(), m.y); }};

  // pha/pla
  opcodes[0x48] = {"PHA", []{ push(m.a); }};
  opcodes[0x68] = {"PLA", []{ LDA(pop()); }};

  opcodes[0x08] = {"PHP", []{ push(GET_P()); }};
  opcodes[0x28] = {"PLP", []{ SET_P(pop()); }};

  // official and unofficial nops
  opcodes[0x1A] = {"NOP1A", []{ }};
  opcodes[0x3A] = {"NOP3A", []{ }};
  opcodes[0x5A] = {"NOP5A", []{ }};
  opcodes[0x7A] = {"NOP7A", []{ }};
  opcodes[0xDA] = {"NOPDA", []{ }};
  opcodes[0xEA] = {"NOP", []{ }};
  opcodes[0xFA] = {"NOPFA", []{ }};

  opcodes[0x80] = {"NOP80 #", []{ immediate(); }};
  opcodes[0x82] = {"NOP82 #", []{ immediate(); }};
  opcodes[0x89] = {"NOP89 #", []{ immediate(); }};
  opcodes[0xC2] = {"NOPC2 #", []{ immediate(); }};
  opcodes[0xE2] = {"NOPE2 #", []{ immediate(); }};

  // inc
  opcodes[0xE6] = {"INC", []{ INC(zeroPage()); }};
  opcodes[0xF6] = {"INC", []{ INC(zeroPageX()); }};
  opcodes[0xEE] = {"INC", []{ INC(absolute()); }};
  opcodes[0xFE] = {"INC", []{ INC(absoluteX()); }};

  // dec
  opcodes[0xC6] = {"DEC", []{ DEC(zeroPage()); }};
  opcodes[0xD6] = {"DEC", []{ DEC(zeroPageX()); }};
  opcodes[0xCE] = {"DEC", []{ DEC(absolute()); }};
  opcodes[0xDE] = {"DEC", []{ DEC(absoluteX()); }};

  // jmp
  opcodes[0x4C] = {"JMP", []{ JMP(absolute()); }};
  opcodes[0x6C] = {"JMPI", []{ JMP(indirect()); }};

  // flag instructions
  opcodes[0x18] = {"CLC", []{ m.status.c = 0; }};
  opcodes[0x38] = {"SEC", []{ m.status.c = 1; }};
  opcodes[0x58] = {"CLI", []{ m.status.i = 0; }};
  opcodes[0x78] = {"SEI", []{ m.status.i = 1; }};
  opcodes[0xB8] = {"CLV", []{ m.status.v = 0; }};
  opcodes[0xD8] = {"CLC", []{ m.status.c = 0; }};
  opcodes[0xF8] = {"SEC", []{ m.status.c = 1; }};

  // and
  opcodes[0x29] = {"AND", []{ AND(immediate()); }};
  opcodes[0x25] = {"AND", []{ AND(read(zeroPage())); }};
  opcodes[0x35] = {"AND", []{ AND(read(zeroPageX())); }};
  opcodes[0x2D] = {"AND", []{ AND(read(absolute())); }};
  opcodes[0x3D] = {"AND", []{ AND(read(absoluteX())); }};
  opcodes[0x39] = {"AND", []{ AND(read(absoluteY())); }};
  opcodes[0x21] = {"AND", []{ AND(read(indirectX())); }};
  opcodes[0x31] = {"AND", []{ AND(read(indirectY())); }};

  // ora
  opcodes[0x09] = {"ORA", []{ ORA(immediate()); }};
  opcodes[0x05] = {"ORA", []{ ORA(read(zeroPage())); }};
  opcodes[0x15] = {"ORA", []{ ORA(read(zeroPageX())); }};
  opcodes[0x0D] = {"ORA", []{ ORA(read(absolute())); }};
  opcodes[0x1D] = {"ORA", []{ ORA(read(absoluteX())); }};
  opcodes[0x19] = {"ORA", []{ ORA(read(absoluteY())); }};
  opcodes[0x01] = {"ORA", []{ ORA(read(indirectX())); }};
  opcodes[0x11] = {"ORA", []{ ORA(read(indirectY())); }};

  // eor
  opcodes[0x49] = {"EOR", []{ EOR(immediate()); }};
  opcodes[0x45] = {"EOR", []{ EOR(read(zeroPage())); }};
  opcodes[0x55] = {"EOR", []{ EOR(read(zeroPageX())); }};
  opcodes[0x4D] = {"EOR", []{ EOR(read(absolute())); }};
  opcodes[0x5D] = {"EOR", []{ EOR(read(absoluteX())); }};
  opcodes[0x59] = {"EOR", []{ EOR(read(absoluteY())); }};
  opcodes[0x41] = {"EOR", []{ EOR(read(indirectX())); }};
  opcodes[0x51] = {"EOR", []{ EOR(read(indirectY())); }};

  // asl
  opcodes[0x0A] = {"ASL", []{ ASL_A(); }};
  opcodes[0x06] = {"ASL", []{ ASL(zeroPage()); }};
  opcodes[0x16] = {"ASL", []{ ASL(zeroPageX()); }};
  opcodes[0x0E] = {"ASL", []{ ASL(absolute()); }};
  opcodes[0x1E] = {"ASL", []{ ASL(absoluteX()); }};

  // lsr
  opcodes[0x4A] = {"LSR", []{ LSR_A(); }};
  opcodes[0x46] = {"LSR", []{ LSR(zeroPage()); }};
  opcodes[0x56] = {"LSR", []{ LSR(zeroPageX()); }};
  opcodes[0x4E] = {"LSR", []{ LSR(absolute()); }};
  opcodes[0x5E] = {"LSR", []{ LSR(absoluteX()); }};

  // rol
  opcodes[0x2A] = {"ROL", []{ ROL_A(); }};
  opcodes[0x26] = {"ROL", []{ ROL(zeroPage()); }};
  opcodes[0x36] = {"ROL", []{ ROL(zeroPageX()); }};
  opcodes[0x2E] = {"ROL", []{ ROL(absolute()); }};
  opcodes[0x3E] = {"ROL", []{ ROL(absoluteX()); }};

  // ror
  opcodes[0x6A] = {"ROR", []{ ROR_A(); }};
  opcodes[0x66] = {"ROR", []{ ROR(zeroPage()); }};
  opcodes[0x76] = {"ROR", []{ ROR(zeroPageX()); }};
  opcodes[0x6E] = {"ROR", []{ ROR(absolute()); }};
  opcodes[0x7E] = {"ROR", []{ ROR(absoluteX()); }};

  // jsr/rts/rti
  opcodes[0x20] = {"JSR", []{ pushAddr(m.ip + 2); JMP(absolute()); }};
  opcodes[0x60] = {"RTS", []{ JMP(popAddr() + 1); }};
  opcodes[0x40] = {"RTI", []{ SET_P(pop()); JMP(popAddr()); }};

  // branch
  opcodes[0x10] = {"BPL", []{ BRANCH(!m.status.n); }};
  opcodes[0x30] = {"BMI", []{ BRANCH(m.status.n); }};
  opcodes[0x50] = {"BVC", []{ BRANCH(!m.status.v); }};
  opcodes[0x70] = {"BVS", []{ BRANCH(m.status.v); }};
  opcodes[0x90] = {"BCC", []{ BRANCH(!m.status.c); }};
  opcodes[0xB0] = {"BCS", []{ BRANCH(m.status.c); }};
  opcodes[0xD0] = {"BNE", []{ BRANCH(!m.status.z); }};
  opcodes[0xF0] = {"BEQ", []{ BRANCH(m.status.z); }};

  opcodes[0x69] = {"ADC", []{ ADC(immediate()); }};
  opcodes[0x65] = {"ADC", []{ ADC(read(zeroPage())); }};
  opcodes[0x75] = {"ADC", []{ ADC(read(zeroPageX())); }};
  opcodes[0x6D] = {"ADC", []{ ADC(read(absolute())); }};
  opcodes[0x7D] = {"ADC", []{ ADC(read(absoluteX())); }};
  opcodes[0x79] = {"ADC", []{ ADC(read(absoluteY())); }};
  opcodes[0x61] = {"ADC", []{ ADC(read(indirectX())); }};
  opcodes[0x71] = {"ADC", []{ ADC(read(indirectY())); }};

  opcodes[0xE9] = {"SBC", []{ SBC(immediate()); }};
  opcodes[0xE5] = {"SBC", []{ SBC(read(zeroPage())); }};
  opcodes[0xF5] = {"SBC", []{ SBC(read(zeroPageX())); }};
  opcodes[0xED] = {"SBC", []{ SBC(read(absolute())); }};
  opcodes[0xFD] = {"SBC", []{ SBC(read(absoluteX())); }};
  opcodes[0xF9] = {"SBC", []{ SBC(read(absoluteY())); }};
  opcodes[0xE1] = {"SBC", []{ SBC(read(indirectX())); }};
  opcodes[0xF1] = {"SBC", []{ SBC(read(indirectY())); }};

  opcodes[0xC9] = {"CMP", []{ CMP(m.a, immediate()); }};
  opcodes[0xC5] = {"CMP", []{ CMP(m.a, read(zeroPage())); }};
  opcodes[0xD5] = {"CMP", []{ CMP(m.a, read(zeroPageX())); }};
  opcodes[0xCD] = {"CMP", []{ CMP(m.a, read(absolute())); }};
  opcodes[0xDD] = {"CMP", []{ CMP(m.a, read(absoluteX())); }};
  opcodes[0xD9] = {"CMP", []{ CMP(m.a, read(absoluteY())); }};
  opcodes[0xC1] = {"CMP", []{ CMP(m.a, read(indirectX())); }};
  opcodes[0xD1] = {"CMP", []{ CMP(m.a, read(indirectY())); }};

  opcodes[0xE0] = {"CPX", []{ CMP(m.x, immediate()); }};
  opcodes[0xE4] = {"CPX", []{ CMP(m.x, read(zeroPage())); }};
  opcodes[0xEC] = {"CPX", []{ CMP(m.x, read(absolute())); }};

  opcodes[0xC0] = {"CPY", []{ CMP(m.y, immediate()); }};
  opcodes[0xC4] = {"CPY", []{ CMP(m.y, read(zeroPage())); }};
  opcodes[0xCC] = {"CPY", []{ CMP(m.y, read(absolute())); }};

  // Debugging opcodes (not used on real processors)
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
}

// Runs the emulator until it needs more input or
// stops. Returns true if it needs more input, and
// false if emulation is done.
bool emulate() {
  while (true) {
    if (m.ip == 0xFFFF) { return false; }

    uint16_t ip = m.ip;
    uint8_t opcode = immediate();
    if (opcodes[opcode].handler == nullptr) {
      printf("\n\nUnknown opcode $%02x\n", opcode);
      printf("IP:\t%04x\t%02x\ta:%02x\tx:%02x\ty:%02x", m.ip, opcode, m.a, m.x, m.y);
      printf("\n");
      for (int i = 0; i < 256; i++) {
        printf("%02x ", m.memory[0x100+i]);
      }
      return false;
    } else {
      if (trace) {
        // Output some spaces to visualize the call stack depth.
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
        // instruction pointer, and return a request
        // for more input.
        m.ip = ip;
        return true;
      }
    }
  }
  return false;
}

