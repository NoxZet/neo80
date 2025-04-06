# neo80

Currently developing a Z80 emulator. Eventual plan is to make a fantasy PC with virtual chips and peripherals, and expand the Z80 instruction set to support a memory mapper and privilege levels.

The core is targetting GCC on Windows and Linux.

To compile and run Z80 core tests, run `run_test.ps1` or `run_test.sh`.

## Roadmap

- Finish Z80 core
- Design graphical output interface
- Design keyboard input interface
- Design reading external data (virtual floppy?)
- Create an assembler/compiler
- Write an operating system

## License

GPL 3. Application and peripherals developed for neo80 can have any license. An exception clause will apply to all headers and the extended instruction set to permit any proprietary software.