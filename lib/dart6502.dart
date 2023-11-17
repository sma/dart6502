import 'dart:io';

class CPU {
  CPU(this.memory);

  final Memory memory;
  var a = 0; // 8-bit accumulator
  var x = 0; // 8-bit index register
  var y = 0; // 8-bit index register
  var sp = 0; // 8-bit stack pointer, relative to $100
  var pc = 0; // 16-bit program counter
  var n = false; // negative flag (bit 7 of SR)
  var v = false; // overflow flag (bit 6 of SR)
  var b = false; // break flag (bit 4 of SR)
  var d = false; // decimal flag (bit 3 of SR)
  var i = false; // interrupt flag (bit 2 of SR)
  var z = false; // zero flag (bit 1 of SR)
  var c = false; // carry flag (bit 0 of SR)

  void reset() {
    a = x = y = 0;
    sp = 0x1FD;
    pc = getWord(0xFFFC);
    n = v = b = d = i = z = c = false;
  }

  int getWord(int address) => memory[address] | (memory[address + 1] << 8);

  void run() {
    while (!b) {
      step();
    }
  }

  void step() {
    // print(pc.toHex(4));
    switch (memory[pc++]) {
      case 0x00: // BRK
        b = true;
      case 0x06: // ASL zp
        final address = memory[pc++];
        final value = memory[address];
        c = value & 0x80 != 0;
        _zn(memory[address] = (value << 1) & 0xff);
      case 0x09: // ORA #nn
        _zn(a |= memory[pc++]);
      case 0x0a: // ASL A
        c = a & 0x80 != 0;
        _zn(a = (a << 1) & 0xff);
      case 0x10: // BPL rel
        _rel(!n);
      case 0x11: // ORA (zp),Y
        final value = memory[getWord(memory[pc++]) + y];
        _zn(a |= value);
      case 0x15: // ORA zp,X
        _zn(a |= memory[(memory[pc++] + x) & 0xff]);
      case 0x18: // CLC
        c = false;
      case 0x20: // JSR abs
        memory[sp--] = (pc + 2) >> 8;
        memory[sp--] = (pc + 2) & 0xff;
        pc = getWord(pc);
      case 0x24: // BIT zp
        final value = memory[memory[pc++]];
        n = value & 0x80 != 0;
        v = value & 0x40 != 0;
        z = (value & a) == 0;
      case 0x26: // ROL zp
        final address = memory[pc++];
        final value = memory[address];
        final inc = c ? 1 : 0;
        c = value & 0x80 != 0;
        _zn(memory[address] = ((value << 1) | inc) & 0xff);
      case 0x28: // PLP
        final value = memory[++sp];
        n = value & 0x80 != 0;
        v = value & 0x40 != 0;
        b = value & 0x10 != 0;
        d = value & 0x08 != 0;
        i = value & 0x04 != 0;
        z = value & 0x02 != 0;
        c = value & 0x01 != 0;
      case 0x29: // AND #nn
        _zn(a &= memory[pc++]);
      case 0x2a: // ROL A
        final inc = c ? 1 : 0;
        c = a & 0x80 != 0;
        _zn(a = ((a << 1) | inc) & 0xff);
      case 0x2c: // BIT abs
        final value = memory[getWord(pc)];
        pc += 2;
        n = value & 0x80 != 0;
        v = value & 0x40 != 0;
        z = value == 0;
      case 0x30: // BMI rel
        _rel(n);
      case 0x35: // AND zp,X
        _zn(a &= memory[(memory[pc++] + x) & 0xff]);
      case 0x38: // SEC
        c = true;
      case 0x46: // LSR zp
        final address = memory[pc++];
        final value = memory[address];
        c = value & 0x01 != 0;
        _zn(memory[address] = value >> 1);
      case 0x48: // PHA
        memory[sp--] = a;
      case 0x49: // EOR #nn
        _zn(a ^= memory[pc++]);
      case 0x4a: // LSR A
        c = a & 0x01 != 0;
        _zn(a = a >> 1);
      case 0x4c: // JMP abs
        pc = getWord(pc);
      case 0x50: // BVC rel
        _rel(!v);
      case 0x56: // LSR zp,X
        final address = memory[pc++] + x;
        final value = memory[address];
        c = value & 0x01 != 0;
        _zn(memory[address] = value >> 1);
      case 0x58: // CLI
        i = false;
      case 0x59: // EOR abs,Y
        final address = getWord(pc) + y;
        pc += 2;
        _zn(a ^= memory[address]);
      case 0x60: // RTS
        pc = memory[++sp] | (memory[++sp] << 8);
      case 0x65: // ADC zp
        final value = memory[memory[pc++]];
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0x68: // PLA
        _zn(a = memory[++sp]);
      case 0x69: // ADC #nn
        final value = memory[pc++];
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0x6c: // JMP (abs)
        pc = getWord(getWord(pc));
      case 0x6d: // ADC abs
        final value = memory[getWord(pc)];
        pc += 2;
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0x6e: // ROR zp
        final address = memory[pc++];
        final value = memory[address];
        final inc = c ? 0x80 : 0;
        c = value & 0x01 != 0;
        _zn(memory[address] = ((value >> 1) | inc) & 0xff);
      case 0x70: // BVS rel
        _rel(v);
      case 0x71: // ADC (zp),Y
        final value = memory[getWord(memory[pc++]) + y];
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0x75: // ADC zp,X
        final address = memory[pc++] + x;
        final value = memory[address];
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0x81: // STA (zp,X)
        memory[getWord(memory[pc++] + x)] = a;
      case 0x84: // STY zp
        memory[memory[pc++]] = y;
      case 0x85: // STA zp
        memory[memory[pc++]] = a;
      case 0x86: // STX zp
        memory[memory[pc++]] = x;
      case 0x88: // DEY
        _zn(y = (y - 1) & 0xff);
      case 0x8a: // TXA
        _zn(a = x);
      case 0x8c: // STY abs
        memory[getWord(pc)] = y;
        pc += 2;
      case 0x8d: // STA abs
        memory[getWord(pc)] = a;
        pc += 2;
      case 0x90: // BCC rel
        _rel(!c);
      case 0x91: // STA (zp),Y
        memory[getWord(memory[pc++]) + y] = a;
      case 0x94: // STY zp,X
        memory[(memory[pc++] + x) & 0xff] = y;
      case 0x95: // STA zp,X
        memory[(memory[pc++] + x) & 0xff] = a;
      case 0x98: // TYA
        _zn(a = y);
      case 0x99: // STA abs,Y
        memory[getWord(pc) + y] = a;
        pc += 2;
      case 0x9a: // TXS
        sp = x + 0x100;
      case 0x9d: // STA abs,X
        memory[getWord(pc) + x] = a;
        pc += 2;
      case 0xa0: // LDY #nn
        _zn(y = memory[pc++]);
      case 0xa1: // LDA (zp,X)
        _zn(a = memory[getWord(memory[pc++] + x)]);
      case 0xa2: // LDX #nn
        _zn(x = memory[pc++]);
      case 0xa4: // LDY zp
        _zn(y = memory[memory[pc++]]);
      case 0xa5: // LDA zp
        _zn(a = memory[memory[pc++]]);
      case 0xa6: // LDX zp
        _zn(x = memory[memory[pc++]]);
      case 0xa8: // TAY
        _zn(y = a);
      case 0xa9: // LDA #nn
        _zn(a = memory[pc++]);
      case 0xaa: // TAX
        _zn(x = a);
      case 0xad: // LDA abs
        _zn(a = memory[getWord(pc)]);
        pc += 2;
      case 0xb0: // BCS rel
        _rel(c);
      case 0xb1: // LDA (zp),Y
        _zn(a = memory[getWord(memory[pc++]) + y]);
      case 0xb4: // LDY zp,X
        _zn(y = memory[(memory[pc++] + x) & 0xff]);
      case 0xb5: // LDA zp,X
        _zn(a = memory[(memory[pc++] + x) & 0xff]);
      case 0xb9: // LDA abs,Y
        _zn(a = memory[getWord(pc) + y]);
        pc += 2;
      case 0xba: // TSX
        _zn(x = sp - 0x100);
      case 0xbd: // LDA abs,X
        _zn(a = memory[getWord(pc) + x]);
        pc += 2;
      case 0xbe: // LDX abs,Y
        _zn(x = memory[getWord(pc) + y]);
        pc += 2;
      case 0xc0: // CPY #nn
        _cmp(y, memory[pc++]);
      case 0xc4: // CPY zp
        _cmp(y, memory[memory[pc++]]);
      case 0xc5: // CMP zp
        _cmp(a, memory[memory[pc++]]);
      case 0xc6: // DEC zp
        final address = memory[pc++];
        final value = memory[address];
        _zn(memory[address] = (value - 1) & 0xff);
      case 0xc8: // INY
        _zn(y = (y + 1) & 0xff);
      case 0xc9: // CMP #nn
        _cmp(a, memory[pc++]);
      case 0xca: // DEX
        _zn(x = (x - 1) & 0xff);
      case 0xd0: // BNE rel
        _rel(!z);
      case 0xd1: // CMP (zp),Y
        _cmp(a, memory[getWord(memory[pc++]) + y]);
      case 0xd5: // CMP zp,X
        _cmp(a, memory[(memory[pc++] + x) & 0xff]);
      case 0xd8: // CLD
        d = false;
      case 0xd9: // CMP abs,Y
        _cmp(a, memory[getWord(pc) + y]);
        pc += 2;
      case 0xdd: // CMP abs,X
        _cmp(a, memory[getWord(pc) + x]);
        pc += 2;
      case 0xe0: // CPX #nn
        _cmp(x, memory[pc++]);
      case 0xe5: // SBC zp
        final value = memory[memory[pc++]];
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0xe6: // INC zp
        final address = memory[pc++];
        final value = memory[address];
        _zn(memory[address] = (value + 1) & 0xff);
      case 0xe8: // INX
        _zn(x = (x + 1) & 0xff);
      case 0xe9: // SBC #nn
        final value = memory[pc++];
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0xea: // NOP
        break;
      case 0xf0: // BEQ rel
        _rel(z);
      case 0xf1: // SBC (zp),Y
        final value = memory[getWord(memory[pc++]) + y];
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0xf5: // SBC zp,X
        final value = memory[(memory[pc++] + x) & 0xff];
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        _zn(a = result & 0xff);
      case 0xf6: // INC zp,X
        final address = (memory[pc++] + x) & 0xff;
        final value = memory[address];
        _zn(memory[address] = (value + 1) & 0xff);
      case 0xfd: // SBC abs,X
        final value = memory[getWord(pc) + x];
        pc += 2;
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        _zn(a = result & 0xff);
      default:
        throw UnimplementedError('opcode ${memory[--pc].toHex(2)} at ${pc.toHex(4)} not implemented');
    }
  }

  void _zn(int value) {
    z = value == 0;
    n = value & 0x80 != 0;
  }

  void _rel(bool f) {
    if (f) {
      final offset = memory[pc++];
      pc += offset < 0x80 ? offset : offset - 0x100;
    } else {
      pc++;
    }
  }

  void _cmp(int value, int other) {
    c = value >= other;
    z = value == other;
    n = (value - other) & 0x80 != 0;
  }

  void loadHex(String hex) {
    for (final line in hex.split('\n').map((line) => line.trim()).where((line) => line.isNotEmpty)) {
      var address = int.parse(line.split(':').first, radix: 16);
      for (final byte in line.split(':').last.trim().split(' ').map((byte) => int.parse(byte, radix: 16))) {
        memory[address++] = byte;
      }
    }
  }
}

class Memory {
  int operator [](int address) {
    switch (address) {
      case 0xd011: // keyboard control
        final ch = stdin.readByteSync() & 0x7f;
        _data[0xd010] = (ch == 10 ? 13 : ch) | 0x80;
        return 0x80;
      default:
        return _data[address];
    }
  }

  void operator []=(int address, int value) {
    switch (address) {
      case 0xd012: // display output
      case 0xd0f2: // display output
        final ch = value & 0x7f;
        stdout.writeCharCode(ch == 13 ? 10 : ch);
      default:
        _data[address] = value;
    }
  }

  void load(int address, Iterable<int> data) {
    _data.setRange(address, address + data.length, data);
  }

  final _data = List.filled(65536, 0);

  void dump(int address, [int? end]) {
    end ??= address + 7;
    var addr = true;
    while (address <= end) {
      if (addr) {
        stdout.writeln();
        stdout.write(address.toHex(4));
        stdout.write(':');
        addr = false;
      }
      stdout.write(' ');
      stdout.write(this[address++].toHex(2));
      if (address % 8 == 0) addr = true;
    }
    stdout.writeln();
  }
}

extension on int {
  String toHex(int length) => toRadixString(16).padLeft(length, '0').toUpperCase();
}
