import 'package:dart6502/dart6502.dart';
import 'package:test/test.dart';

void main() {
  test('reset', () {
    final cpu = CPU(Memory());
    cpu.memory[0xFFFC] = 0x12;
    cpu.memory[0xFFFD] = 0x34;
    cpu.reset();
    expect(cpu.pc, 0x3412);
    expect(cpu.sp, 0x1fd);
  });

  group('instructions', () {
    final cpu = CPU(Memory());
    setUp(() {
      cpu.reset();
      cpu.memory.load(0x1000, [0, 0, 0, 0]);
      cpu.pc = 0x1000;
    });

    test('brk', () {
      cpu.memory.load(0x1000, [0x00]);
      cpu.step();
      expect(cpu.pc, 0x1001);
      expect(cpu.b, true);
    });

    test('ora #nn', () {
      cpu.memory.load(0x1000, [0x09, 0x45, 0x09, 0x80, 0x09, 0x00]);
      cpu.a = 0x12;
      cpu.step();
      expect(cpu.pc, 0x1002);
      expect(cpu.a, 0x57);
      expect(cpu.n, false);
      expect(cpu.z, false);
      cpu.a = 0x12;
      cpu.step();
      expect(cpu.pc, 0x1004);
      expect(cpu.a, 0x92);
      expect(cpu.n, true);
      expect(cpu.z, false);
      cpu.a = 0x00;
      cpu.step();
      expect(cpu.pc, 0x1006);
      expect(cpu.a, 0x00);
      expect(cpu.n, false);
      expect(cpu.z, true);
    });

    group('bpl', () {
      setUp(() => cpu.memory.load(0x1000, [0x10, 0x04]));
      test('do branch', () {
        cpu.n = false;
        cpu.step();
        expect(cpu.pc, 0x1006);
      });
      test('no branch', () {
        cpu.n = true;
        cpu.step();
        expect(cpu.pc, 0x1002);
      });
    });

    test('jsr', () {
      cpu.memory.load(0x1000, [0x20, 0x34, 0x12]);
      cpu.step();
      expect(cpu.pc, 0x1234);
      expect(cpu.sp, 0x1fb);
      expect(cpu.getWord(cpu.sp + 1), 0x1003);
    });

    group('bmi', () {
      setUp(() => cpu.memory.load(0x1000, [0x30, 0x04]));
      test('do branch', () {
        cpu.n = true;
        cpu.step();
        expect(cpu.pc, 0x1006);
      });
      test('no branch', () {
        cpu.n = false;
        cpu.step();
        expect(cpu.pc, 0x1002);
      });
    });
  });
}
