import 'dart:io';

import 'package:dart6502/dart6502.dart';

void main() {
  stdin.echoMode = false;
  stdin.lineMode = false;

  final cpu = CPU(Memory());
  cpu.loadHex(File('data/wozmon.txt').readAsStringSync());
  cpu.reset();

  final basic = File('data/a1basic.bin');
  if (basic.existsSync()) {
    final data = basic.readAsBytesSync();
    for (var i = 0; i < data.length; i++) {
      cpu.memory[0xE000 + i] = data[i];
    }
    cpu.pc = 0xE000;
  }

  cpu.run();
}
