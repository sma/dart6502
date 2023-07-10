Apple 1 Simulator
=================
The "operation system" of the **Apple 1** is a system monitor cramped into 256 bytes ($FF00-$FFFF). It is known as **Wozmon** because it was written by Steve "Woz" Wozniak in a very compact way. The Apple 1 boots into this minimal command prompt which can be used to view and edit memory and run code.

How difficult can be to write a 6502 CPU simulator to run Wozmon?

Preparation
-----------
Luckily, we only need a minimal interface to the rest of the world: Reading a single keystroke and emitting a character to the terminal screen.

Both operation are performed using memory mapped registers:

* If bit 7 of $D012 is zero, you may store an ASCII character (with bit 7 set to 1) to that address and it will be shown on the terminal screen (upon the next screen refresh). As long as bit 7 is set, you may not overwrite that value and must wait until bit 7 is zero again.

* If bit 7 of $D011 is zero, no key was typed. Otherwise, bit 7 denotes that a key was typed and you can read the ASCII value (with bit 7 set to 1) of that key from $D010.

Memory
------
Let's create a class `Memory` to abstract memory access. We begin with a simple wrapper of a list of integers (actually bytes, so I could also use a `Uint8List`):

```dart
class Memory {
  int operator [](int address) {
    return _data[address];
  }

  void operator []=(int address, int value) {
    _data[address] = value;
  }

  final _data = List.filled(65536, 0);
}
```

To read a character typed on the keyboard, Wozmon waits until $D011 has bit 7 set to 1 and then reads $D010 to get its ASCII value + $80. As it doesn't matter for my simulation whether the CPU does some busy waiting or not, I'll read a character from `stdin` (converting LF into CR) when the bit is tested and store it in $D010 like so:

```dart
class Memory {
  int operator [](int address) {
    switch (address) {
      case 0xd011:
        final ch = stdin.readByteSync() & 0x7f;
        _data[0xd010] = (ch == 10 ? 13 : ch) | 0x80;
        return 0x80;
      default:
        return _data[address];
    }
  }
  ...
```

To emit a character, we never write anything to $D012 so it is always 0 and therefore bit 7 always denotes that the terminal is ready to emit another character. All we have to do is stripping the seventh bit and making sure that CR is actually LF (which then gets translated to CRLF by Dart internally).

```dart
class Memory {
  ...
  void operator []=(int address, int value) {
    switch (address) {
      case 0xd012:
        final ch = value & 0x7f;
        stdout.writeCharCode(ch == 13 ? 10 : ch);
      default:
        _data[address] = value;
    }
  }
  ...
```

Note that the Apple 1 only knows uppercase letters and no lowercase letters, so I recommend to use capslock when you interact with Wozmon.

CPU
---
Next, let's create a class `CPU` which uses the memory and stores all registers (A, X, Y, SP, PC) and flags (N, V, B, D, I, Z, C) of a 6502 processor. If the CPU is `reset`, it will initialize its stack pointer (SP) to $1FD and its program counter (PC) to the 16-LSB address stored in $FFFC (and $FFFD). All other registers are set to zero (I think) which also sets all flags to false.

```dart
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
}
```

Next, we need a `run` method which executes the next instruction at `pc` and then increments that register and repeats this until the CPU is halted. A `step` method will execute a single instruction and we need to build a giant switch-case statement for all 180ish instructions of a 6502.

```dart
class CPU {
  ...
  void run() {
    while (!b) {
      step();
    }
  }

  void step() {
    switch (memory[pc++]) {
      default:
        throw UnimplementedError('opcode ${memory[--pc].toHex(2)} at ${pc.toHex(4)} not implemented');
    }
  }
  ...
}
```

Instead of implementing every instruction, let's try to drive this process by the actual Wozmon code. It looks like this:

```yaml
FF00: D8 58 A0 7F 8C 12 D0 A9 
FF08: A7 8D 11 D0 8D 13 D0 C9 
FF10: DF F0 13 C9 9B F0 03 C8 
FF18: 10 0F A9 DC 20 EF FF A9 
...
FFF8: 00 00 00 0F 00 FF 00 00 
```

I add a method `load` to load this hex dump into the CPU's memory:

```dart
class CPU {
  ...
  void load(String hex) {
    for (final line in hex.split('\n')
        .map((line) => line.trim())
        .where((line) => line.isNotEmpty)) {
      var address = int.parse(line.split(':').first, radix: 16);
      for (final byte in line.split(':').last.trim().split(' ').map((byte) => int.parse(byte, radix: 16))) {
        memory[address++] = byte;
      }
    }
  }
  ...
}
```

Let's start the simulation:

```dart
void main() {
  final cpu = CPU(Memory());
  cpu.load(File('wozmon.txt').readAsStringSync());
  cpu.reset();
  cpu.run();
}
```

This should print something like

    UnimplementedError: opcode D8 at FF00 not implemented

This is the first instruction we need to implement. `D8` is called `CLD` for "clear decimal flag" which resets the `d` flag.

```dart
class CPU {
  ...
  void step() {
    switch (memory[pc++]) {
      case 0xd8: // CLD
        d = false;
      default:
        ...
  }
  ...
}
```

The next instruction is `58` or `CLI` which resets the `i` flag.

The funny thing is: Copilot "knows" that I'm creating a 6502 simulator and can provide the correct mnemonic and implemention upon prompting just `case 0x58:`.

### Time passes…

With a little help from my friend (Copilot), I implemented the following instructions and reached $FF31 after the `\` for reset was printed to the terminal and a key was read. There's some potential for refactorings, for sure. But I had to fix only one instruction – RTS – where Copilot wanted to add 1 to the `pc` register for unknown reasons. Everything else seems to follow the 6502 specification (I know, I don't check for `sp` underflow).

```dart
  void step() {
    switch (memory[pc++]) {
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
      case 0x58: // CLI
        i = false;
      case 0x60: // RTS
        pc = memory[++sp] | (memory[++sp] << 8);
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
      case 0xa0: // LDY #nn
        y = memory[pc++];
        z = y == 0;
        n = y & 0x80 != 0;
      case 0xa9: // LDA #nn
        a = memory[pc++];
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xad: // LDA abs
        a = memory[getWord(pc)];
        pc += 2;
        z = a == 0;
        n = a & 0x80 != 0;
      case 0xc8: // INY
        y = (y + 1) & 0xff;
        z = y == 0;
        n = y & 0x80 != 0;
      case 0xc9: // CMP #nn
        final nn = memory[pc++];
        c = a >= nn;
        z = a == nn;
        n = (a - nn) & 0x80 != 0;
      case 0xd8: // CLD
        d = false;
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
```

I continue by implementing instructions and eventually reach $FF75 which is page 2 of the Wozmon disassembly listing from the official Apple 1 handbook and I'm guessing that half of the work is done. This took me about an hour so far. I had previous experiences with the 6502 so far, but I know how CPUs works, can understand assembler, even 6502 assembler good enough and of course, I know Dart.

A bit later, I can successfully enter `200` and get back `0000: 00` which is wrong but still impressive as I can simulate enough instructions to implement one of the three modi of Wozmon: Examining a single memory location.

I'm not sure why my input isn't correctly parsed. The relevant code is at $FF5F, according to the handbook. I'm guessing, `ROL` isn't correctly implemented as I already verified that EOR and ADC correctly extract the hex nibble value. But nothing ends up in the H and L memory locations.

Here's the code:

```dart
      case 0x26: // ROL zp
        final address = memory[pc++];
        final value = memory[address];
        c = value & 0x80 != 0;
        memory[address] = ((value << 1) | (c ? 1 : 0)) & 0xff;
        z = memory[address] == 0;
        n = memory[address] & 0x80 != 0;
```

And then it's obivous, isn't it? The `c = ...` line will overwrite the current carry flag that is needed in the next line. So this is the fix to the bug:

```dart
        final inc = c ? 1 : 0;
        c = value & 0x80 != 0;
        memory[address] = ((value << 1) | inc) & 0xff;
```

Upon entering `200`, I now get back `0200: 00`. That's better but still incorrect, as there should be a value != 0 because that's the input buffer which contains my entered `200`. Reading the byte to display happens at $FFBF, which is a `LDA (zp,X)` instruction.

Let's have a look:

```dart
      case 0xa1: // LDA (zp,X)
        a = memory[(memory[pc++] + x) & 0xff];
        z = a == 0;
        n = a & 0x80 != 0;
```

I can verify that $24 (XAML) is accessed, but instead of adding X to $24 and returning a single byte, we need to get the word at $24/$25 and add X to that word and then get A from that address. Copilot failed on me by not understanding the indirect mode :(

Here's the fix:

```dart
        a = memory[getWord(memory[pc++]) + x];
```

Now `200` correctly returns `0200: B2`. Yeah! However, testing `24.25` doesn't work yet. I need to implement the `INC zp` instruction to make it work, too. Yeah!

Testing `300:AA` to set a byte fails because `STA (zp, X)` is missing, another indirect address mode instruction. Let's hope Copilot will manage it this time. It does and after implementing this instruction, it seems to work just fine.

The last function of Wozmon is running code by using `R`. I have to implement one last instruction, an indirect jump. Let's add it. After some two hours I've a working simulator!

### Testing …

To write a tiny program that will print a `@` I enter:

    0300: A9 C0 20 EF FF R

It will correctly print `@` before it crashes because there are no more instructions after the `JSR` instruction. The memory is filled with zeros and zero happens to be the `BRK` instruction, let's implement it, too.

```dart
      case 0x00: // BRK
        b = true;
```

This way, my `run` method will stop and my simuator will terminate as intended.

### Summary

I had to implement 50 instructions to make Wozmon run. I'm not sure whether I saved much time compared to systhematically implementing everything as I had to run the code 100 or so times to get the next exception that guided me to which instruction to implement next. However, everything is sort of tested and I feel good.

Next Steps
----------
The next step would obviously be to also make Apple Basic a.k.a. Integer Basic run, a 4 KB module that could be loaded from cassette to $E000.$EFFF and then run by entering `E000 R` in Wozmon.

Instead of using the normal terminal, one could also write a tiny Flutter application to simulate a 40x24 character screen, even using the original 5x7 character set and a blinking `@` as a cursor.