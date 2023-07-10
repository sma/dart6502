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
      case 0x09: // ORA #nn
        a |= memory[pc++];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x0a: // ASL A
        c = a & 0x80 != 0;
        a = (a << 1) & 0xff;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x10: // BPL rel
        if (!n) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
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
        memory[address] = ((value << 1) | inc) & 0xff;
        z = memory[address] == 0;
        n = memory[address] & 0x80 != 0;
      case 0x29: // AND #nn
        a &= memory[pc++];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x2c: // BIT abs
        final value = memory[getWord(pc)];
        pc += 2;
        n = value & 0x80 != 0;
        v = value & 0x40 != 0;
        z = (value & a) == 0;
      case 0x30: // BMI rel
        if (n) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      case 0x48: // PHA
        memory[sp--] = a;
      case 0x49: // EOR #nn
        a ^= memory[pc++];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x4a: // LSR A
        c = a & 0x01 != 0;
        a = a >> 1;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x4c: // JMP abs
        pc = getWord(pc);
      case 0x50: // BVC rel
        if (!v) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      case 0x58: // CLI
        i = false;
      case 0x60: // RTS
        pc = memory[++sp] | (memory[++sp] << 8);
      case 0x68: // PLA
        a = memory[++sp];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x69: // ADC #nn
        final value = memory[pc++];
        final result = a + value + (c ? 1 : 0);
        c = result > 0xff;
        v = ((a ^ result) & (value ^ result) & 0x80) != 0;
        a = result & 0xff;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0x6c: // JMP (abs)
        pc = getWord(getWord(pc));
      case 0x81: // STA (zp, X)
        memory[getWord(memory[pc++] + x)] = a;
      case 0x84: // STY zp
        memory[memory[pc++]] = y;
      case 0x85: // STA zp
        memory[memory[pc++]] = a;
      case 0x86: // STX zp
        memory[memory[pc++]] = x;
      case 0x88: // DEY
        y = (y - 1) & 0xff;
        z = y == 0;
        n = y & 0x80 != 0;
      case 0x8c: // STY abs
        memory[getWord(pc)] = y;
        pc += 2;
      case 0x8d: // STA abs
        memory[getWord(pc)] = a;
        pc += 2;
      case 0x90: // BCC rel
        if (!c) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      case 0x95: // STA zp,X
        memory[(memory[pc++] + x) & 0xff] = a;
      case 0x99: // STA abs,Y
        memory[getWord(pc) + y] = a;
        pc += 2;
      case 0xa0: // LDY #nn
        y = memory[pc++];
        z = y == 0;
        n = y & 0x80 != 0;
      case 0xa1: // LDA (zp,X)
        a = memory[getWord(memory[pc++]) + x];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xa2: // LDX #nn
        x = memory[pc++];
        z = x == 0;
        n = x & 0x80 != 0;
      case 0xa5: // LDA zp
        a = memory[memory[pc++]];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xa9: // LDA #nn
        a = memory[pc++];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xaa: // TAX
        x = a;
        z = x == 0;
        n = x & 0x80 != 0;
      case 0xad: // LDA abs
        a = memory[getWord(pc)];
        pc += 2;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xb0: // BCS rel
        if (c) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      case 0xb5: // LDA zp,X
        a = memory[(memory[pc++] + x) & 0xff];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xb9: // LDA abs,Y
        a = memory[getWord(pc) + y];
        pc += 2;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xc4: // CPY zp
        final value = memory[memory[pc++]];
        c = y >= value;
        z = y == value;
        n = (y - value) & 0x80 != 0;
      case 0xc5: // CMP zp
        final value = memory[memory[pc++]];
        c = a >= value;
        z = a == value;
        n = (a - value) & 0x80 != 0;
      case 0xc8: // INY
        y = (y + 1) & 0xff;
        z = y == 0;
        n = y & 0x80 != 0;
      case 0xc9: // CMP #nn
        final nn = memory[pc++];
        c = a >= nn;
        z = a == nn;
        n = (a - nn) & 0x80 != 0;
      case 0xca: // DEX
        x = (x - 1) & 0xff;
        z = x == 0;
        n = x & 0x80 != 0;
      case 0xd0: // BNE rel
        if (!z) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      case 0xd8: // CLD
        d = false;
      case 0xe5: // SBC zp
        final value = memory[memory[pc++]];
        final result = a - value - (c ? 0 : 1);
        c = result >= 0;
        v = ((a ^ result) & (a ^ value) & 0x80) != 0;
        a = result & 0xff;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xe6: // INC zp
        final address = memory[pc++];
        final value = memory[address];
        memory[address] = (value + 1) & 0xff;
        z = memory[address] == 0;
        n = memory[address] & 0x80 != 0;
      case 0xf0: // BEQ rel
        if (z) {
          final offset = memory[pc++];
          pc += offset < 0x80 ? offset : offset - 0x100;
        } else {
          pc++;
        }
      default:
        throw UnimplementedError('opcode ${memory[--pc].toHex(2)} at ${pc.toHex(4)} not implemented');
    }
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
        final ch = value & 0x7f;
        stdout.writeCharCode(ch == 13 ? 10 : ch);
      default:
        _data[address] = value;
    }
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
