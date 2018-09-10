
local bit = require 'bit'
local ffi = require 'ffi'

local IO_PORT = 0x401C -- pick a register which is not used on a real NES
local MEMORY_SIZE = 512 * 1024

local forthCode = io.open('bootstrap.f', 'r')
local forthCode2 = io.open('game.f', 'r')
local lastLineInput = forthCode:read'*a' .. forthCode2:read'*a'

local trace = false

local m
m = {
  a = 0, x = 0, y = 0, sp = 0xFF, ip = -1,
  status = {
    n = 0, v = 0, z = 0, s = 0, c = 0
  },
  memory = ffi.new("unsigned char[?]", MEMORY_SIZE),
  read = function(addr)
    if addr == IO_PORT then
      if lastLineInput:len() == 0 then
        lastLineInput = io.read("*l") .. '\n'
      end
      local result = lastLineInput:byte()
      lastLineInput = lastLineInput:sub(2)
      return result
    else
      --print(string.format("read %04x: %02x", addr, m.memory[addr]))
      return m.memory[addr]
    end
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
  pop = function()
    m.sp = bit.band(m.sp + 1, 0xFF)
    return m.read(0x100 + m.sp)
  end,
  push = function(v)
    m.set(0x100 + m.sp, v)
    m.sp = bit.band(m.sp - 1, 0xFF)
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
    return m.set(bit.band(m.read(m.ip) + m.x, 0xFF), val)
  end,
  readZeroPageY = function()
    m.ip = m.ip + 1
    return m.read(bit.band(m.read(m.ip) + m.y, 0xFF))
  end,
  storeZeroPageY = function()
    m.ip = m.ip + 1
    return m.set(bit.band(m.read(m.ip) + m.y, 0xFF), val)
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
    return m.set(m.x + m.getAbsolute(), val)
  end,
  readAbsoluteY = function()
    return m.read(m.y + m.getAbsolute())
  end,
  storeAbsoluteY = function(val)
    return m.set(m.y + m.getAbsolute(), val)
  end,
  readIndirect = function()
    local addr = m.getAbsolute()
    local addrPlus1 = addr + 1
    -- adjust for jump indirect bug
    if bit.band(addrPlus1, 0xFF) == 0 then
      addrPlus1 = addrPlus1 - 256
    end
    return m.read(addr) + m.read(addrPlus1) * 256
  end,
  readIndirectX = function()
    local zpAddr = m.readImmediate()
    zpAddr = bit.band(zpAddr + m.x, 0xFF)
    local addrPlus1 = bit.band(zpAddr + 1, 0xFF)
    return m.read(m.read(zpAddr) + m.read(addrPlus1) * 256)
  end,
  storeIndirectX = function(val)
    m.ip = m.ip + 1
    local addr = bit.band(m.read(m.ip) + m.x, 0xFF)
    local addrPlus1 = bit.band(addr + 1, 0xFF)
    return m.set(m.read(addr) + m.read(addrPlus1) * 256, val)
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

local function printStack()
  io.write(string.format('STACK: sp:%02x ', m.sp))
  for i = 0xFF, m.sp+1, -1 do
    io.write(string.format('%02x ', m.memory[0x100 + i]))
  end
  io.write('\n')
end

local function BIT(read)
  m.status.z = bit.band(m.a, read)
  m.status.n = bit.band(read, 0x80) == 0x80 and 1 or 0
  m.status.v = bit.band(read, 0x40) == 0x40 and 1 or 0
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
  local c = m.status.c ~= 0 and 0x80 or 0
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
  [0x24] = {'BIT ZP', function() BIT(m.readZeroPage()) end},
  [0x2C] = {'BIT ABS', function() BIT(m.readAbsolute()) end},
  -- lda
  [0xA9] = {'LDA IMM', function() LDA(m.readImmediate()) end},
  [0xA5] = {'LDA ZP', function() LDA(m.readZeroPage()) end},
  [0xB5] = {'LDA ZPX', function() LDA(m.readZeroPageX()) end},
  [0xAD] = {'LDA ABS', function() LDA(m.readAbsolute()) end},
  [0xBD] = {'LDA ABSX', function() LDA(m.readAbsoluteX()) end},
  [0xB9] = {'LDA ABSY', function() LDA(m.readAbsoluteY()) end},
  [0xA1] = {'LDA INDX', function() LDA(m.readIndirectX()) end},
  [0xB1] = {'LDA INDY', function() LDA(m.readIndirectY()) end},
  -- sta
  [0x85] = {'STA', function() m.storeZeroPage(m.a) end},
  [0x95] = {'STA', function() m.storeZeroPageX(m.a) end},
  [0x8D] = {'STA', function() m.storeAbsolute(m.a) end},
  [0x9D] = {'STA', function() m.storeAbsoluteX(m.a) end},
  [0x99] = {'STA', function() m.storeAbsoluteY(m.a) end},
  [0x81] = {'STA', function() m.storeIndirectX(m.a) end},
  [0x91] = {'STA', function() m.storeIndirectY(m.a) end},

  -- ldx
  [0xA2] = {'LDX', function() LDX(m.readImmediate()) end},
  [0xA6] = {'LDX', function() LDX(m.readZeroPage()) end},
  [0xB6] = {'LDX', function() LDX(m.readZeroPageY()) end},
  [0xAE] = {'LDX', function() LDX(m.readAbsolute()) end},
  [0xBE] = {'LDX', function() LDX(m.readAbsoluteY()) end},
  -- stx
  [0x86] = {'STX', function() m.storeZeroPage(m.x) end},
  [0x96] = {'STX', function() m.storeZeroPageY(m.x) end},
  [0x8E] = {'STX', function() m.storeAbsolute(m.x) end},

  -- ldy
  [0xA0] = {'LDY', function() LDY(m.readImmediate()) end},
  [0xA4] = {'LDY', function() LDY(m.readZeroPage()) end},
  [0xB4] = {'LDY', function() LDY(m.readZeroPageX()) end},
  [0xAC] = {'LDY', function() LDY(m.readAbsolute()) end},
  [0xBC] = {'LDY', function() LDY(m.readAbsoluteX()) end},
  -- sty
  [0x84] = {'STY', function() m.storeZeroPage(m.y) end},
  [0x94] = {'STY', function() m.storeZeroPageX(m.y) end},
  [0x8C] = {'STY', function() m.storeAbsolute(m.y) end},

  -- tsx/txs
  [0xBA] = {'TSX', function() m.x = m.sp end},
  [0x9A] = {'TXS', function() m.sp = m.x end},

  -- pha/pla
  [0x48] = {'PHA', function() m.push(m.a) end},
  [0x68] = {'PLA', function() m.a = m.pop() end},

  -- php/plp
  [0x08] = {'PHP', function()
    m.push(
      (m.status.n ~= 0 and 0x80 or 0)
      + (m.status.v ~= 0 and 0x40 or 0)
      + 0x20
      + 0x10
      + (m.status.d ~= 0 and 0x08 or 0)
      + (m.status.i ~= 0 and 0x04 or 0)
      + (m.status.z ~= 0 and 0x02 or 0)
      + (m.status.c ~= 0 and 0x01 or 0))
  end},
  [0x28] = {'PLP', function()
    local val = m.pop()
    m.status.n = bit.band(m.status, 0x80) and 1 or 0
    m.status.v = bit.band(m.status, 0x40) and 1 or 0
    m.status.d = bit.band(m.status, 0x08) and 1 or 0
    m.status.i = bit.band(m.status, 0x04) and 1 or 0
    m.status.z = bit.band(m.status, 0x02) and 1 or 0
    m.status.c = bit.band(m.status, 0x01) and 1 or 0
  end},

  -- tax/txa
  [0xAA] = {'TAX', function() m.x = m.setSZ(m.a) end},
  [0x8A] = {'TXA', function() m.a = m.setSZ(m.x) end},

  -- dex/inx
  [0xCA] = {'DEX', function() m.x = m.setSZ(m.x - 1) end},
  [0xE8] = {'INX', function() m.x = m.setSZ(m.x + 1) end},

  -- tay/tya
  [0xA8] = {'TAY', function() m.y = m.setSZ(m.a) end},
  [0x98] = {'TYA', function() m.a = m.setSZ(m.y) end},

  -- dey/iny
  [0x88] = {'DEY', function() m.y = m.setSZ(m.y - 1) end},
  [0xC8] = {'INY', function() m.y = m.setSZ(m.y + 1) end},

  -- nop
  [0xEA] = {'NOP', function() end},

  -- inc
  [0xE6] = {'INC', function() INC(m.readImmediate()) end},
  [0xF6] = {'INC', function() INC(bit.band(m.readImmediate() + m.x), 0xFF) end},
  [0xEE] = {'INC', function() INC(m.getAbsolute()) end},
  [0xFE] = {'INC', function() INC(m.getAbsolute() + m.x) end},

  -- dec
  [0xC6] = {'DEC', function() DEC(m.readImmediate()) end},
  [0xD6] = {'DEC', function() DEC(bit.band(m.readImmediate() + m.x), 0xFF) end},
  [0xCE] = {'DEC', function() DEC(m.getAbsolute()) end},
  [0xDE] = {'DEC', function() DEC(m.getAbsolute() + m.x) end},

  -- jmp
  [0x4C] = {'JMP', function() m.ip = m.getAbsolute() - 1 end},
  [0x6C] = {'JMPI', function() m.ip = m.readIndirect() - 1 end},

  -- flag instructions
  [0x18] = {'CLC', function() m.status.c = 0 end},
  [0x38] = {'SEC', function() m.status.c = 1 end},
  [0x58] = {'CLI', function() m.status.i = 0 end},
  [0x78] = {'SEI', function() m.status.i = 1 end},
  [0xB8] = {'CLV', function() m.status.v = 0 end},
  [0xD8] = {'CLC', function() m.status.c = 0 end},
  [0xF8] = {'SEC', function() m.status.c = 1 end},

  -- and
  [0x29] = {'AND', function() AND(m.readImmediate()) end},
  [0x25] = {'AND', function() AND(m.readZeroPage()) end},
  [0x35] = {'AND', function() AND(m.readZeroPageX()) end},
  [0x2D] = {'AND', function() AND(m.readAbsolute()) end},
  [0x3D] = {'AND', function() AND(m.readAbsoluteX()) end},
  [0x39] = {'AND', function() AND(m.readAbsoluteY()) end},
  [0x21] = {'AND', function() AND(m.readIndirectX()) end},
  [0x31] = {'AND', function() AND(m.readIndirectY()) end},

  -- ora
  [0x09] = {'ORA', function() ORA(m.readImmediate()) end},
  [0x05] = {'ORA', function() ORA(m.readZeroPage()) end},
  [0x15] = {'ORA', function() ORA(m.readZeroPageX()) end},
  [0x0D] = {'ORA', function() ORA(m.readAbsolute()) end},
  [0x1D] = {'ORA', function() ORA(m.readAbsoluteX()) end},
  [0x19] = {'ORA', function() ORA(m.readAbsoluteY()) end},
  [0x01] = {'ORA', function() ORA(m.readIndirectX()) end},
  [0x11] = {'ORA', function() ORA(m.readIndirectY()) end},

  -- eor
  [0x49] = {'EOR', function() EOR(m.readImmediate()) end},
  [0x45] = {'EOR', function() EOR(m.readZeroPage()) end},
  [0x55] = {'EOR', function() EOR(m.readZeroPageX()) end},
  [0x4D] = {'EOR', function() EOR(m.readAbsolute()) end},
  [0x5D] = {'EOR', function() EOR(m.readAbsoluteX()) end},
  [0x59] = {'EOR', function() EOR(m.readAbsoluteY()) end},
  [0x41] = {'EOR', function() EOR(m.readIndirectX()) end},
  [0x51] = {'EOR', function() EOR(m.readIndirectY()) end},

  -- asl
  [0x0A] = {'ASL', function() m.status.c = bit.band(m.a, 0x80) ~= 0 and 1 or 0; m.a = m.setSZ(bit.lshift(m.a, 1)) end},
  [0x06] = {'ASL', function() ASL(m.readImmediate()) end},
  [0x16] = {'ASL', function() ASL(bit.band(m.readImmediate() + m.x, 0xFF)) end},
  [0x0E] = {'ASL', function() ASL(m.getAbsolute()) end},
  [0x1E] = {'ASL', function() ASL(m.getAbsolute() + m.x) end},

  -- lsr
  [0x4A] = {'LSR', function() m.status.c = m.a % 2; m.a = m.setSZ(bit.rshift(m.a, 1)) end},
  [0x46] = {'LSR', function() LSR(m.readImmediate()) end},
  [0x56] = {'LSR', function() LSR(bit.band(m.readImmediate() + m.x, 0xFF)) end},
  [0x4E] = {'LSR', function() LSR(m.getAbsolute()) end},
  [0x5E] = {'LSR', function() LSR(m.getAbsolute() + m.x) end},

  -- rol
  [0x2A] = {'ROL', function()
    local c = m.status.c
    m.status.c = bit.band(m.a, 0x80) ~= 0 and 1 or 0
    m.a = m.setSZ(bit.lshift(m.a, 1) + c)
  end},
  [0x26] = {'ROL', function() ROL(m.readImmediate()) end},
  [0x36] = {'ROL', function() ROL(bit.band(m.readImmediate() + m.x, 0xFF)) end},
  [0x2E] = {'ROL', function() ROL(m.getAbsolute()) end},
  [0x3E] = {'ROL', function() ROL(m.getAbsolute() + m.x) end},

  -- ror
  [0x6A] = {'ROR', function()
    local c = m.status.c ~= 0 and 0x80 or 0
    m.status.c = m.a % 2
    m.a = m.setSZ(bit.rshift(m.a, 1) + c)
  end},
  [0x66] = {'ROR', function() ROR(m.readImmediate()) end},
  [0x76] = {'ROR', function() ROR(bit.band(m.readImmediate() + m.x, 0xFF)) end},
  [0x6E] = {'ROR', function() ROR(m.getAbsolute()) end},
  [0x7E] = {'ROR', function() ROR(m.getAbsolute() + m.x) end},

  -- jsr
  [0x20] = {'JSR', function()
    local target = m.getAbsolute()
    --print(string.format("JSR %04x from %04x", target, m.ip))
    m.push(bit.rshift(m.ip, 8))
    m.push(m.ip)
    m.ip = target - 1
  end},

  -- rts
  [0x60] = {'RTS', function()
    m.ip = m.pop() + 256*m.pop()
    --print(string.format("NEW IP %04x", m.ip))
  end},

  -- branch
  [0x10] = {'BPL', function() BRANCH(m.status.s == 0) end},
  [0x30] = {'BMI', function() BRANCH(m.status.s ~= 0) end},
  [0x50] = {'BVC', function() BRANCH(m.status.v == 0) end},
  [0x70] = {'BVS', function() BRANCH(m.status.v ~= 0) end},
  [0x90] = {'BCC', function() BRANCH(m.status.c == 0) end},
  [0xB0] = {'BCS', function() BRANCH(m.status.c ~= 0) end},
  [0xD0] = {'BNE', function() BRANCH(m.status.z == 0) end},
  [0xF0] = {'BEQ', function() BRANCH(m.status.z ~= 0) end},

  [0x69] = {'ADC', function() ADC(m.readImmediate()) end},
  [0x65] = {'ADC', function() ADC(m.readZeroPage()) end},
  [0x75] = {'ADC', function() ADC(m.readZeroPageX()) end},
  [0x6D] = {'ADC', function() ADC(m.readAbsolute()) end},
  [0x7D] = {'ADC', function() ADC(m.readAbsoluteX()) end},
  [0x79] = {'ADC', function() ADC(m.readAbsoluteY()) end},
  [0x61] = {'ADC', function() ADC(m.readIndirectX()) end},
  [0x71] = {'ADC', function() ADC(m.readIndirectY()) end},

  [0xE9] = {'SBC', function() SBC(m.readImmediate()) end},
  [0xE5] = {'SBC', function() SBC(m.readZeroPage()) end},
  [0xF5] = {'SBC', function() SBC(m.readZeroPageX()) end},
  [0xED] = {'SBC', function() SBC(m.readAbsolute()) end},
  [0xFD] = {'SBC', function() SBC(m.readAbsoluteX()) end},
  [0xF9] = {'SBC', function() SBC(m.readAbsoluteY()) end},
  [0xE1] = {'SBC', function() SBC(m.readIndirectX()) end},
  [0xF1] = {'SBC', function() SBC(m.readIndirectY()) end},

  [0xC9] = {'CMP', function() CMP(m.a, m.readImmediate()) end},
  [0xC5] = {'CMP', function() CMP(m.a, m.readZeroPage()) end},
  [0xD5] = {'CMP', function() CMP(m.a, m.readZeroPageX()) end},
  [0xCD] = {'CMP', function() CMP(m.a, m.readAbsolute()) end},
  [0xDD] = {'CMP', function() CMP(m.a, m.readAbsoluteX()) end},
  [0xD9] = {'CMP', function() CMP(m.a, m.readAbsoluteY()) end},
  [0xC1] = {'CMP', function() CMP(m.a, m.readIndirectX()) end},
  [0xD1] = {'CMP', function() CMP(m.a, m.readIndirectY()) end},

  [0xE0] = {'CPX', function() CMP(m.x, m.readImmediate()) end},
  [0xE4] = {'CPX', function() CMP(m.x, m.readZeroPage()) end},
  [0xEC] = {'CPX', function() CMP(m.x, m.readAbsolute()) end},

  [0xC0] = {'CPY', function() CMP(m.y, m.readImmediate()) end},
  [0xC4] = {'CPY', function() CMP(m.y, m.readZeroPage()) end},
  [0xCC] = {'CPY', function() CMP(m.y, m.readAbsolute()) end},

  -- illegal opcodes
  [0xE3] = {'ISC', function() INC(m.readIndirectX()) SBC(m.readIndirectX()) end},
  [0xE7] = {'ISC', function() INC(m.readZeroPage()) SBC(m.readZeroPage()) end},
  [0xEF] = {'ISC', function() INC(m.readAbsolute()) SBC(m.readAbsolute()) end},
  [0xF3] = {'ISC', function() INC(m.readIndirectY()) SBC(m.readIndirectY()) end},
  [0xF7] = {'ISC', function() INC(m.readZeroPageX()) SBC(m.readZeroPageX()) end},
  [0xFB] = {'ISC', function() INC(m.readAbsoluteY()) SBC(m.readAbsoluteY()) end},
  [0xFF] = {'ISC', function() INC(m.readAbsoluteX()) SBC(m.readAbsoluteX()) end},

  [0xFF] = {'DBG_START', function()
    print"DEBUGGER STARTED"
    trace = true
  end},
  [0xEF] = {'DBG_END', function()
    print "DEBUGGER ENDED"
    trace = false
  end},
  [0xDF] = {'DBG_TRACE', function()
    m.ip = m.ip + 1
    while m.memory[m.ip] ~= 0 do
      io.write(string.char(m.memory[m.ip]))
      m.ip = m.ip + 1
    end
    print()
  end}
}

function emulate()
  while true do
    if m.ip == -1 then
      return
    end
    local opcode = m.readImmediate()
    if not opcodes[opcode] then
      print(string.format("\n\nUnknown opcode $%02x", opcode))
      print(string.format("IP:\t%04x\t%02x\ta:%02x\tx:%02x\ty:%02x", m.ip, opcode, m.a, m.x, m.y))
      io.write('\n')
      for i = 0, 256 do
        io.write(string.format("%02x ", m.memory[0x200+i]))
      end
      return
    else
      if trace then
        print(string.format("IP:\t%04x\t%02x\t%s\ta:%02x\tx:%02x\ty:%02x\tv:%01x", m.ip, opcode, opcodes[opcode][1], m.a, m.x, m.y, m.status.v))
      end
      --printStack()
      opcodes[opcode][2]()
    end
  end
end

local bootstrap = io.open('bootstrap.bin', 'r')
local rom = bootstrap:read('*a')
for i = 0, 0xFFFF do
  -- print(i, rom:byte(i+1, i+2), rom:len())
  m.memory[i] = rom:byte(i+1, i+2)
end

m.ip = m.memory[0xFFFC] + bit.lshift(m.memory[0xFFFD], 8) - 1
emulate()

print [[
==============================
Execution halted. Freezing ROM
==============================
]]

local romout = io.open('out.nes', 'w')
local ramout = io.open('ram.out', 'w')

romout:write("NES\x1A\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")

for i = 0, 0x7FFF do
  romout:write(string.char(m.memory[i + 0x8000]))
end

for i = 0, 0x7FF do
  ramout:write(string.char(m.memory[i]))
end
