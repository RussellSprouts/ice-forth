
local bit = require 'bit'
local ffi = require 'ffi'

local IO_PORT = 0x401C -- pick a register which is not used on a real NES
local MEMORY_SIZE = 512 * 1024

local m
m = {
  a = 0, x = 0, y = 0, sp = 0, ip = -1,
  status = {
    n = 0, v = 0, z = 0, s = 0, c = 0
  },
  memory = ffi.new("unsigned char[?]", MEMORY_SIZE),
  read = function(addr)
    --print(string.format("read %04x: %02x", addr, m.memory[addr]))
    return m.memory[addr]
  end,
  set = function(addr, val)
    --print(string.format("set %04x to %02x", addr, val))
    if addr == IO_PORT then
      --print("Output: [")
      val = bit.band(val, 0xFF)
      io.write(string.char(val))
      --io.write("]")
    else
      m.memory[addr] = bit.band(val, 0xFF)
    end
  end,
  getAbsolute = function()
    m.ip = m.ip + 2
    return m.read(m.ip - 1) + m.read(m.ip) * 256
  end,
  readImmediate = function()
    m.ip = m.ip + 1
    return m.read(m.ip)
  end,
  readZeroPage = function()
    m.ip = m.ip + 1
    return m.read(m.read(m.ip))
  end,
  storeZeroPage = function(val)
    m.ip = m.ip + 1
    m.set(m.read(m.ip), val)
  end,
  readZeroPageX = function()
    m.ip = m.ip + 1
    return m.read(bit.band(m.read(m.ip) + m.x, 0xFF))
  end,
  storeZeroPageX = function(val)
    m.ip = m.ip + 1
    return m.store(bit.band(m.read(m.ip) + m.x, 0xFF), val)
  end,
  readZeroPageY = function()
    m.ip = m.ip + 1
    return m.read(bit.band(m.read(m.ip) + m.y, 0xFF))
  end,
  storeZeroPageY = function()
    m.ip = m.ip + 1
    return m.store(bit.band(m.read(m.ip) + m.y, 0xFF), val)
  end,
  readAbsolute = function()
    return m.read(m.getAbsolute())
  end,
  storeAbsolute = function(val)
    m.set(m.getAbsolute(), val)
  end,
  readAbsoluteX = function()
    return m.read(m.x + m.getAbsolute())
  end,
  storeAbsoluteX = function(val)
    return m.store(m.x + m.getAbsolute(), val)
  end,
  readAbsoluteY = function()
    return m.read(m.y + m.getAbsolute())
  end,
  storeAbsoluteY = function(val)
    return m.store(m.y + m.getAbsolute(), val)
  end,
  readIndirect = function()
    local addr = m.getAbsolute()
    local addrPlus1 = addr + 1
    -- adjust for jump indirect bug
    if bit.band(addrPlus1, 0xFF) == 0 then
      addrPlus1 = addrPlus1 - 256
    end
    return m.read(m.read(addr) + m.read(addrPlus1) * 256)
  end,
  readIndirectX = function()
    m.ip = m.ip + 1
    local addr = m.read(m.ip) + m.x
    local addrPlus1 = bit.band(addr + 1, 0xFF)
    return m.read(m.read(addr) + m.read(addrPlus1) * 256)
  end,
  storeIndirectX = function(val)
    m.ip = m.ip + 1
    local addr = m.read(m.ip) + m.x
    local addrPlus1 = bit.band(addr + 1, 0xFF)
    return m.store(m.read(addr) + m.read(addrPlus1) * 256, val)
  end,
  readIndirectY = function()
    m.ip = m.ip + 1
    local addr = m.read(m.ip)
    local addrPlus1 = bit.band(addr + 1, 0xFF)
    return m.read(m.read(addr) + m.read(addrPlus1) * 256 + m.y)
  end,
  storeIndirectY = function(val)
    m.ip = m.ip + 1
    local addr = m.read(m.ip)
    local addrPlus1 = bit.band(addr + 1, 0xFF)
    return m.set(m.read(addr) + m.read(addrPlus1) * 256 + m.y, val)
  end,

  setSZ = function(val)
    val = bit.band(val, 0xFF)
    if val >= 0x80 then
      m.status.s = 1
    else
      m.status.s = 0
    end
    if val == 0 then
      m.status.z = 1
    else
      m.status.z = 0
    end
    return val
  end
}

local function BIT(addr)
  local read = m.read(addr)
  m.status.z = bit.band(m.a, read)
  m.status.n = bit.band(read, 0x80)
  m.status.v = bit.band(read, 0x40)
end

local function LDA(value)
  m.a = m.setSZ(value)
end

local function LDX(value)
  m.x = m.setSZ(value)
end

local function LDY(value)
  m.y = m.setSZ(value)
end

local function INC(addr)
  m.set(addr, m.setSZ(m.read(addr) + 1))
end

local function DEC(addr)
  m.set(addr, m.setSZ(m.read(addr) - 1))
end

local function AND(value)
  m.a = m.setSZ(bit.band(m.a, value))
end

local function ORA(value)
  m.a = m.setSZ(bit.bor(m.a, value))
end

local function EOR(value)
  m.a = m.setSZ(bit.bxor(m.a, value))
end

local function ASL(addr)
  local value = m.read(addr)
  m.status.c = value >= 0x80 and 1 or 0
  m.set(addr, m.setSZ(bit.lshift(value, 1)))
end

local function ROL(addr)
  local value = m.read(addr)
  local c = m.status.c
  m.status.c = value >= 0x80 and 1 or 0
  m.set(addr, m.setSZ(bit.lshift(value, 1) + c))
end

local function LSR(addr)
  local value = m.read(addr)
  m.status.c = value % 2
  m.set(addr, m.setSZ(bit.rshift(value, 1)))
end

local function ROR(addr)
  local value = m.read(addr)
  local c = m.status.c and 0x80 or 0
  m.status.c = value % 2
  m.set(addr, m.setSZ(bit.rshift(value, 1) + c))
end

local function BRANCH(condition)
  local disp = m.readImmediate()
  if condition then
    if disp >= 0x80 then
      disp = -0x100 + disp
    end
    m.ip = m.ip + disp
  end
end

local function ADC(value)
  local newValue = m.a + value + m.status.c
  if newValue > 0xFF then
    m.status.c = 1
  else
    m.status.c = 0
  end
  m.status.v = bit.band(bit.band(bit.bnot(bit.bxor(m.a, value)), bit.bxor(m.a, newValue)), 0x80) == 0x80 and 1 or 0
  m.a = m.setSZ(newValue)
end

local function SBC(value)
  return ADC(bit.band(bit.bnot(value), 0xFF))
end

local function CMP(a, b)
  local comparison = bit.band(a - b, 0xFF)
  m.status.z = comparison == 0 and 1 or 0
  m.status.c = a >= b and 1 or 0
  m.status.n = comparison > 0x7F and 1 or 0
end

local opcodes = {
  -- bit
  [0x24] = function() BIT(m.readZeroPage()) end,
  [0x2C] = function() BIT(m.readAbsolute()) end,
  -- lda
  [0xA9] = function() LDA(m.readImmediate()) end,
  [0xA5] = function() LDA(m.readZeroPage()) end,
  [0xB5] = function() LDA(m.readZeroPageX()) end,
  [0xAD] = function() LDA(m.readAbsolute()) end,
  [0xBD] = function() LDA(m.readAbsoluteX()) end,
  [0xB9] = function() LDA(m.readAbsoluteY()) end,
  [0xA1] = function() LDA(m.readIndirectX()) end,
  [0xB1] = function() LDA(m.readIndirectY()) end,
  -- sta
  [0x85] = function() m.storeZeroPage(m.a) end,
  [0x95] = function() m.storeZeroPageX(m.a) end,
  [0x8D] = function() m.storeAbsolute(m.a) end,
  [0x9D] = function() m.storeAbsoluteX(m.a) end,
  [0x99] = function() m.storeAbsoluteY(m.a) end,
  [0x81] = function() m.storeIndirectX(m.a) end,
  [0x91] = function() m.storeIndirectY(m.a) end,

  -- ldx
  [0xA2] = function() LDX(m.readImmediate()) end,
  [0xA6] = function() LDX(m.readZeroPage()) end,
  [0xB6] = function() LDX(m.readZeroPageY()) end,
  [0xAE] = function() LDX(m.readAbsolute()) end,
  [0xBE] = function() LDX(m.readAbsoluteY()) end,
  -- stx
  [0x86] = function() m.storeZeroPage(m.x) end,
  [0x96] = function() m.storeZeroPageY(m.x) end,
  [0x8E] = function() m.storeAbsolute(m.x) end,

  -- ldy
  [0xA0] = function() LDY(m.readImmediate()) end,
  [0xA4] = function() LDY(m.readZeroPage()) end,
  [0xB4] = function() LDY(m.readZeroPageX()) end,
  [0xAC] = function() LDY(m.readAbsolute()) end,
  [0xBC] = function() LDY(m.readAbsoluteX()) end,
  -- sty
  [0x84] = function() m.storeZeroPage(m.y) end,
  [0x94] = function() m.storeZeroPageX(m.y) end,
  [0x8C] = function() m.storeAbsolute(m.y) end,

  -- tsx/txs
  [0x9A] = function() m.x = m.sp end,
  [0xBA] = function() m.sp = m.x end,

  -- pha/pla
  [0x48] = function() m.sp = m.sp - 1; m.set(0x100 + m.sp, m.a) end,
  [0x68] = function() m.a = m.read(0x100 + m.sp); m.sp = m.sp + 1 end,

  -- tax/txa
  [0xAA] = function() m.x = m.setSZ(m.a) end,
  [0x8A] = function() m.a = m.setSZ(m.x) end,

  -- dex/inx
  [0xCA] = function() m.x = m.setSZ(m.x - 1) end,
  [0xE8] = function() m.x = m.setSZ(m.x + 1) end,

  -- tay/tya
  [0xA8] = function() m.y = m.setSZ(m.a) end,
  [0x98] = function() m.a = m.setSZ(m.y) end,

  -- dey/iny
  [0x88] = function() m.y = m.setSZ(m.y - 1) end,
  [0xC8] = function() m.y = m.setSZ(m.y + 1) end,

  -- nop
  [0xEA] = function() end,

  -- inc
  [0xE6] = function() INC(m.readImmediate()) end,
  [0xF6] = function() INC(bit.band(m.readImmediate() + m.x), 0xFF) end,
  [0xEE] = function() INC(m.getAbsolute()) end,
  [0xFE] = function() INC(m.getAbsolute() + m.x) end,

  -- dec
  [0xC6] = function() DEC(m.readImmediate()) end,
  [0xD6] = function() DEC(bit.band(m.readImmediate() + m.x), 0xFF) end,
  [0xCE] = function() DEC(m.getAbsolute()) end,
  [0xDE] = function() DEC(m.getAbsolute() + m.x) end,

  -- jmp
  [0x4C] = function() m.ip = m.getAbsolute() - 1 end,
  [0x6C] = function() m.ip = m.readIndirect() - 1 end,

  -- flag instructions
  [0x18] = function() m.status.c = 0 end,
  [0x38] = function() m.status.c = 1 end,
  [0x58] = function() m.status.i = 0 end,
  [0x78] = function() m.status.i = 1 end,
  [0xB8] = function() m.status.v = 0 end,
  [0xD8] = function() m.status.c = 0 end,
  [0xF8] = function() m.status.c = 1 end,

  -- and
  [0x29] = function() AND(m.readImmediate()) end,
  [0x25] = function() AND(m.readZeroPage()) end,
  [0x35] = function() AND(m.readZeroPageX()) end,
  [0x2D] = function() AND(m.readAbsolute()) end,
  [0x3D] = function() AND(m.readAbsoluteX()) end,
  [0x39] = function() AND(m.readAbsoluteY()) end,
  [0x21] = function() AND(m.readIndirectX()) end,
  [0x31] = function() AND(m.readIndirectY()) end,

  -- ora
  [0x09] = function() ORA(m.readImmediate()) end,
  [0x05] = function() ORA(m.readZeroPage()) end,
  [0x15] = function() ORA(m.readZeroPageX()) end,
  [0x0D] = function() ORA(m.readAbsolute()) end,
  [0x1D] = function() ORA(m.readAbsoluteX()) end,
  [0x19] = function() ORA(m.readAbsoluteY()) end,
  [0x01] = function() ORA(m.readIndirectX()) end,
  [0x11] = function() ORA(m.readIndirectY()) end,

  -- eor
  [0x49] = function() ORA(m.readImmediate()) end,
  [0x45] = function() ORA(m.readZeroPage()) end,
  [0x55] = function() ORA(m.readZeroPageX()) end,
  [0x4D] = function() ORA(m.readAbsolute()) end,
  [0x5D] = function() ORA(m.readAbsoluteX()) end,
  [0x59] = function() ORA(m.readAbsoluteY()) end,
  [0x41] = function() ORA(m.readIndirectX()) end,
  [0x51] = function() ORA(m.readIndirectY()) end,

  -- asl
  [0x0A] = function() m.status.c = bit.band(m.a, 0x80) ~= 0 and 1 or 0; m.a = m.setSZ(bit.lshift(m.a, 1)) end,
  [0x06] = function() ASL(m.readImmediate()) end,
  [0x16] = function() ASL(bit.band(m.readImmediate() + m.x, 0xFF)) end,
  [0x0E] = function() ASL(m.getAbsolute()) end,
  [0x1E] = function() ASL(m.getAbsolute() + m.x) end,

  -- lsr
  [0x4A] = function() m.status.c = m.a % 2; m.a = m.setSZ(bit.rshift(m.a, 1)) end,
  [0x46] = function() LSR(m.readImmediate()) end,
  [0x56] = function() LSR(bit.band(m.readImmediate() + m.x, 0xFF)) end,
  [0x4E] = function() LSR(m.getAbsolute()) end,
  [0x5E] = function() LSR(m.getAbsolute() + m.x) end,

  -- rol
  [0x2A] = function()
    local c = m.status.c
    m.status.c = bit.band(m.a, 0x80) ~= 0 and 1 or 0
    m.a = m.setSZ(bit.lshift(m.a, 1) + c)
  end,
  [0x26] = function() ROL(m.readImmediate()) end,
  [0x36] = function() ROL(bit.band(m.readImmediate() + m.x, 0xFF)) end,
  [0x2E] = function() ROL(m.getAbsolute()) end,
  [0x3E] = function() ROL(m.getAbsolute() + m.x) end,

  -- ror
  [0x6A] = function()
    local c = m.status.c == 1 and 0x80 or 0
    m.status.c = m.a % 2
    m.a = m.setSZ(bit.rshift(m.a, 1) + c)
  end,
  [0x66] = function() ROR(m.readImmediate()) end,
  [0x76] = function() ROR(bit.band(m.readImmediate() + m.x, 0xFF)) end,
  [0x6E] = function() ROR(m.getAbsolute()) end,
  [0x7E] = function() ROR(m.getAbsolute() + m.x) end,

  -- jsr
  [0x20] = function()
    local target = m.getAbsolute()
    m.sp = m.sp - 2
    m.set(0x100 + m.sp, m.ip)
    m.set(0x101 + m.sp, bit.rshift(m.ip, 8))
    m.ip = target - 1
  end,

  -- rts
  [0x60] = function()
    m.ip = m.read(0x100 + m.sp) + m.read(0x101 + m.sp) * 256
    m.sp = m.sp + 2
  end,

  -- branch
  [0x10] = function() BRANCH(m.status.s == 1) end,
  [0x30] = function() BRANCH(m.status.s == 0) end,
  [0x50] = function() BRANCH(m.status.v == 0) end,
  [0x70] = function() BRANCH(m.status.v == 1) end,
  [0x90] = function() BRANCH(m.status.c == 0) end,
  [0xB0] = function() BRANCH(m.status.c == 1) end,
  [0xD0] = function() BRANCH(m.status.z == 0) end,
  [0xF0] = function() BRANCH(m.status.z == 1) end,

  [0x69] = function() ADC(m.readImmediate()) end,
  [0x65] = function() ADC(m.readZeroPage()) end,
  [0x75] = function() ADC(m.readZeroPageX()) end,
  [0x6D] = function() ADC(m.readAbsolute()) end,
  [0x7D] = function() ADC(m.readAbsoluteX()) end,
  [0x79] = function() ADC(m.readAbsoluteY()) end,
  [0x61] = function() ADC(m.readIndirectX()) end,
  [0x71] = function() ADC(m.readIndirectY()) end,

  [0xE9] = function() SBC(m.readImmediate()) end,
  [0xE5] = function() SBC(m.readZeroPage()) end,
  [0xF5] = function() SBC(m.readZeroPageX()) end,
  [0xED] = function() SBC(m.readAbsolute()) end,
  [0xFD] = function() SBC(m.readAbsoluteX()) end,
  [0xF9] = function() SBC(m.readAbsoluteY()) end,
  [0xE1] = function() SBC(m.readIndirectX()) end,
  [0xF1] = function() SBC(m.readIndirectY()) end,

  [0xC9] = function() CMP(m.a, m.readImmediate()) end,
  [0xC5] = function() CMP(m.a, m.readZeroPage()) end,
  [0xD5] = function() CMP(m.a, m.readZeroPageX()) end,
  [0xCD] = function() CMP(m.a, m.readAbsolute()) end,
  [0xDD] = function() CMP(m.a, m.readAbsoluteX()) end,
  [0xD9] = function() CMP(m.a, m.readAbsoluteY()) end,
  [0xC1] = function() CMP(m.a, m.readIndirectX()) end,
  [0xD1] = function() CMP(m.a, m.readIndirectY()) end,

  [0xE0] = function() CMP(m.x, m.readImmediate()) end,
  [0xE4] = function() CMP(m.x, m.readZeroPage()) end,
  [0xEC] = function() CMP(m.x, m.readAbsolute()) end,

  [0xC0] = function() CMP(m.x, m.readImmediate()) end,
  [0xC4] = function() CMP(m.x, m.readZeroPage()) end,
  [0xCC] = function() CMP(m.x, m.readAbsolute()) end,
}

-- lda #
m.memory[0] = 0xA9
m.memory[1] = string.byte 'A'
-- sta IO_PORT
m.memory[2] = 0x8D
m.memory[3] = 0x1C
m.memory[4] = 0x40
-- ldx #
m.memory[5] = 0xA2
m.memory[6] = string.byte 'B'
-- stx IO_PORT
m.memory[7] = 0x8E
m.memory[8] = 0x1C
m.memory[9] = 0x40
-- jmp
m.memory[10] = 0x4C
m.memory[11] = 0x02
m.memory[12] = 0x00

function emulate()
  while true do
    local opcode = m.readImmediate()
    if not opcodes[opcode] then
      print(string.format("\n\nUnknown opcode $%02x", opcode))
      return
    else
      opcodes[opcode]()
    end
  end
end

local fig = io.open('fig', 'r')
local rom = fig:read('*a')
for i = 0, 0x7FFF do
  --print(i, rom:byte(i+1, i+2), rom:len())
  m.memory[0x8000 + i] = rom:byte(i+1, i+2)
end
m.ip = 0x8001

emulate()
