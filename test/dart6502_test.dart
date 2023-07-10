import 'package:dart6502/dart6502.dart';
import 'package:test/test.dart';

void main() {
  test('reset', () {
    final cpu = CPU(Memory());
    cpu.memory[0xFFFC] = 0x12;
    cpu.memory[0xFFFD] = 0x34;
    cpu.reset();
    expect(cpu.pc, 0x3412);
  });
}
