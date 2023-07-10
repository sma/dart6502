import 'dart:io';

import 'package:dart6502/dart6502.dart';

void main() {
  final cpu = CPU(Memory());
  cpu.loadHex(File('data/wozmon.txt').readAsStringSync());
  // cpu.memory.dump(0xFF00, 0xFFFD);
  cpu.reset();
  cpu.run();
}
