#include "z80.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define assert(condition, ...) if (!(condition)) {\
    printf(__VA_ARGS__);\
    goto fail;\
}

#define resetInstructions(...) {\
    const uint8_t arr[] = { __VA_ARGS__ };\
    memcpy(memory, arr, sizeof arr);\
    pc = 0;\
}
static inline void resetGPregisters() {
    memset(regs, 0, sizeof regs);
    memset(shadowRegs, 0, sizeof shadowRegs);
}
static inline void resetIIregisters() {
    memset(indexRegs, 0, sizeof indexRegs);
}
static inline void resetAllRegisters() {
    resetGPregisters();
    resetIIregisters();
    sp=0;
}
static inline void resetMemory() {
    memset(memory, 0, sizeof memory);
}

#define BINARY_PATTERN "%c%c%c%c%c%c%c%c"
#define BINARY_BYTE(byte)  \
  ((byte) & 0x80 ? '1' : '0'), \
  ((byte) & 0x40 ? '1' : '0'), \
  ((byte) & 0x20 ? '1' : '0'), \
  ((byte) & 0x10 ? '1' : '0'), \
  ((byte) & 0x08 ? '1' : '0'), \
  ((byte) & 0x04 ? '1' : '0'), \
  ((byte) & 0x02 ? '1' : '0'), \
  ((byte) & 0x01 ? '1' : '0') 

int main() {
    memory = calloc(0x10000, sizeof(uint8_t));
    initializeCore();

    // LD B-L,A
    resetInstructions(0x47, 0x4f, 0x57, 0x5f, 0x67, 0x6f);
    REG_A = 0x76;
    tick();
    assert(REG_B == 0x76, "LD B,A ------ B is %x\n", REG_B);
    tick();
    assert(REG_C == 0x76, "LD C,A ------ C is %x\n", REG_C);
    tick();
    assert(REG_D == 0x76, "LD D,A ------ D is %x\n", REG_D);
    assert(REG_E == 0, "after LD into B,C,D ------ E is %x\n", REG_E);
    assert(REG_H == 0, "after LD into B,C,D ------ H is %x\n", REG_H);
    assert(REG_L == 0, "after LD into B,C,D ------ L is %x\n", REG_L);
    tick();
    assert(REG_E == 0x76, "LD E,A ------ E is %x\n", REG_E);
    tick();
    assert(REG_H == 0x76, "LD H,A ------ H is %x\n", REG_H);
    tick();
    assert(REG_L == 0x76, "LD L,A ------ L is %x\n", REG_L);

    // LD A,(BC) from 0, then load BC with 1523 and load 57 from 1523
    resetInstructions(0x0a, 0x06, 0x15, 0x0e, 0x23, 0x0a);
    memory[0x1523] = 0x57;
    resetGPregisters();
    tick();
    assert(REG_A == 0x0a, "LD A,(BC) @BC=0 ------ A is %x\n", REG_A);
    tick();
    assert(REG_B == 0x15, "LD B,15 ------ B is %x\n", REG_B);
    tick();
    assert(REG_C == 0x23, "LD C,23 ------ C is %x\n", REG_C);
    tick();
    assert(REG_A == 0x57, "LD A,(BC) @BC=1523 ------ A is %x\n", REG_A);

    // LD B,(HL) and LD C,(HL) from 0, then load HL with 1620 and load 44 from 1620
    resetInstructions(0x46, 0x4e, 0x26, 0x16, 0x2e, 0x20, 0x46, 0x4e);
    memory[0x1620] = 0x44;
    resetGPregisters();
    tick();
    assert(REG_B == 0x46, "LD B,(HL) @HL=0 ------ B is %x\n", REG_B);
    assert(REG_C == 0, "after LD B,(HL) @HL=0 ------ C is %x\n", REG_C);
    tick();
    assert(REG_C == 0x46, "LD C,(HL) @HL=0 ------ C is %x\n", REG_C);
    tick(); tick(); tick(); tick();
    assert(REG_B == 0x44, "LD B,(HL) @HL=1620 ------ B is %x\n", REG_B);
    assert(REG_C == 0x44, "LD C,(HL) @HL=1620 ------ C is %x\n", REG_C);

    // LD D,81;   LD (HL),D;   LD E,31;   LD (HL),E
    resetInstructions(0x16, 0x81, 0x72, 0x1e, 0x31, 0x73);
    // H=16, L=20
    tick(); tick();
    assert(memory[0x1620] == 0x81, "LD (HL),D ------ (1620) is %x, HL is %x, D is %x\n", memory[0x1620], REG_HL, REG_D);
    tick(); tick();
    assert(memory[0x1620] == 0x31, "LD (HL),E ------ (1620) is %x, HL is %x, E is %x\n", memory[0x1620], REG_HL, REG_E);
    
    // LD (HL),64
    resetInstructions(0x36, 0x64);
    REG_H = 0x23; REG_L = 0x04;
    tick();
    assert(memory[0x2304] == 0x64, "LD (HL),64 ------ (2304) is %x\n", memory[0x2304]);

    // LD A,(DE)
    resetInstructions(0x1a);
    REG_D = 0x61; REG_E = 0x62; memory[0x6162] = 0xfc;
    tick();
    assert(REG_A == 0xfc, "LD A,(DE) ------ A is %x\n", REG_A);

    // LD A,(nn);   LD (nn),A
    resetInstructions(0x3a, 0x22, 0x11, 0x32, 0xa0, 0x18);
    memory[0x1122] = 0x55;
    tick();
    assert(REG_A == 0x55, "LD A,(1122) ------ A is %x\n", REG_A);
    tick();
    assert(memory[0x18a0] == 0x55, "LD (18a0) ------ (18a0) is %x\n", memory[0x18a0]);

    // load 5 into H, load H into A;   LD (BC),A
    resetMemory(); resetInstructions(0x26, 0x05, 0x7c, 0x02);
    resetAllRegisters();
    REG_B = 0x7a; REG_C = 0x9b;
    tick(); tick(); tick();
    assert(memory[0x7a9b] == 0x05, "LD (BC),A ------ (BC) is %x, A is %x\n", memory[0x7a9b], REG_A);

    // LD dd,nn (all 4)
    resetInstructions(0x01, 0x51, 0xa1, 0x11, 0x52, 0xa2, 0x21, 0x53, 0xa3, 0x31, 0x54, 0xa4);
    tick();
    assert(REG_B == 0xa1, "LD BC,nn ------ B is %x\n", REG_B);
    assert(REG_C == 0x51, "LD BC,nn ------ B is %x\n", REG_C);
    tick();
    assert(REG_D == 0xa2, "LD DE,nn ------ B is %x\n", REG_D);
    assert(REG_E == 0x52, "LD DE,nn ------ B is %x\n", REG_E);
    tick();
    assert(REG_H == 0xa3, "LD HL,nn ------ B is %x\n", REG_H);
    assert(REG_L == 0x53, "LD HL,nn ------ B is %x\n", REG_L);
    tick();
    assert(sp == 0xa454, "LD SP,nn ------ SP is %x\n", sp);

    // LD IX,nn;   LD IY,nn
    resetInstructions(0xdd, 0x21, 0x40, 0x41, 0xfd, 0x21, 0x50, 0x63);
    tick();
    assert(REG_IX == 0x4140, "LD IX,nn ------ IX is %x\n", REG_IX);
    tick();
    assert(REG_IY == 0x6350, "LD IY,nn ------ IY is %x\n", REG_IY);

    // LD HL,(nn);   LD (nn),HL;    LD E,H
    resetInstructions(0x2a, 0x25, 0x45, 0x22, 0x03, 0x46, 0x5c);
    resetAllRegisters();
    memory[0x4525] = 0x77; memory[0x4526] = 0x1f;
    tick();
    assert(REG_H == 0x1f, "LD HL,(nn) ------ H is %x\n", REG_H);
    assert(REG_L == 0x77, "LD HL,(nn) ------ L is %x\n", REG_L);
    tick(); tick();
    assert(memory[0x4603] == 0x77, "LD (nn),HL ------ (4603) is %x\n", memory[0x4603]);
    assert(memory[0x4604] == 0x1f, "LD (nn),HL ------ (4604) is %x\n", memory[0x4604]);
    assert(REG_E == 0x1f, "after LD HL,(nn) and LD (nn),HL;  LD E,H ------ E is %x\n", REG_E);

    // LD DE,(nn);   LD (nn),DE;    LD C,D   - Z80 ED instructions
    resetInstructions(0xed, 0x5b, 0x26, 0x47, 0xed, 0x53, 0x13, 0x48, 0x4a);
    resetAllRegisters();
    memory[0x4726] = 0xbc; memory[0x4727] = 0xab;
    tick();
    assert(REG_D == 0xab && REG_E == 0xbc, "LD DE,(nn) ------ D is %x, E is %x\n", REG_D, REG_E);
    assert(REG_E == 0xbc, "LD DE,(nn) ------ E is %x\n", REG_E);
    tick(); tick();
    assert(memory[0x4813] == 0xbc, "LD (nn),DE ------ (4813) is %x\n", memory[0x4813]);
    assert(memory[0x4814] == 0xab, "LD (nn),DE ------ (4814) is %x\n", memory[0x4814]);
    assert(REG_C == 0xab, "after LD DE,(nn) and LD (nn),DE;  LD C,D ------ C is %x\n", REG_C);
    
    // LD SP,(nn);   LD (nn),SP;   - Z80 ED instructions
    resetInstructions(0xed, 0x7b, 0xa6, 0x57, 0xed, 0x73, 0xa3, 0x58);
    resetAllRegisters();
    memory[0x57a6] = 0x10; memory[0x57a7] = 0x8e;
    tick();
    assert(sp == 0x8e10, "LD SP,(nn) ------ SP is %x\n", sp);
    tick();
    assert(memory[0x58a3] == 0x10, "LD (nn),DE ------ (58a3) is %x\n", memory[0x58a3]);
    assert(memory[0x58a4] == 0x8e, "LD (nn),DE ------ (58a4) is %x\n", memory[0x58a4]);

    // LD SP,HL;   LD SP,IX;   LD SP,IY
    resetInstructions(0xf9, 0xdd, 0xf9, 0xfd, 0xf9);
    resetAllRegisters();
    REG_H = 0x2a; REG_L = 0x30;
    tick();
    assert(sp == 0x2a30, "LD SP,HL ------ SP is %x\n", sp);
    REG_IX = 0x2b31;
    tick();
    assert(sp == 0x2b31, "LD SP,IX ------ SP is %x\n", sp);
    REG_IY = 0x2c32;
    tick();
    assert(sp == 0x2c32, "LD SP,IY ------ SP is %x\n", sp);

    // LD SP, PUSH BC, PUSH DE, POP BC, PUSH HL, PUSH AF, POP DE, POP AF, POP HL
    resetInstructions(0x31, 0x10, 0x40, /*PUSH-POPs*/0xc5, 0xd5, 0xc1, 0xe5, 0xf5, 0xd1, 0xf1, 0xe1);
    resetAllRegisters();
    REG_A = 0x60; REG_F = 0x70; REG_B = 0x80; REG_C = 0x90; REG_D = 0xa0; REG_E = 0xb0; REG_H = 0xc0; REG_L = 0xd0;
    tick(); tick(); tick(); tick(); tick(); tick();
    assert(memory[0x400a] == 0x70 && memory[0x400b] == 0x60, "PUSH AF ------ (400a) is %02x%02x\n", memory[0x400a], memory[0x400b]);
    assert(memory[0x400c] == 0xd0 && memory[0x400d] == 0xc0, "PUSH HL ------ (400c) is %02x%02x\n", memory[0x400c], memory[0x400d]);
    assert(memory[0x400e] == 0x90 && memory[0x400f] == 0x80, "PUSH BC ------ (400e) is %02x%02x\n", memory[0x400e], memory[0x400f]);
    assert(sp == 0x400a, "PUSH AFHLBC ------ pc is %x\n", pc);
    assert(REG_A == 0x60 && REG_F == 0x70 && REG_B == 0xa0 && REG_C == 0xb0 && REG_D == 0xa0 && REG_E == 0xb0 && REG_H == 0xc0 && REG_L == 0xd0, "POP BC regs not as expected %x %x\n", REG_B, REG_C);
    tick(); tick(); tick();
    assert(sp == 0x4010, "POP AFHLBC ------ pc is %x\n", pc);
    assert(REG_A == 0xc0 && REG_F == 0xd0 && REG_B == 0xa0 && REG_C == 0xb0 && REG_D == 0x60 && REG_E == 0x70 && REG_H == 0x80 && REG_L == 0x90,
        "POP AFHLBC all regs not as expected AF: %x %x, BC: %x %x, DE: %x %x, HL: %x %x\n", REG_A, REG_F, REG_B, REG_C, REG_D, REG_E, REG_H, REG_L);

    // PUSH IX, PUSH IY, POP IX, POP IY
    resetInstructions(0xdd, 0xe5, 0xfd, 0xe5, 0xdd, 0xe1, 0xfd, 0xe1);
    resetAllRegisters();
    sp = 0x5001; REG_IX = 0x714d; REG_IY = 0x8d8e;
    tick(); tick();
    assert(memory[0x5000] == 0x71, "PUSH IX ------ (5000) is %x\n", memory[0x5000]);
    assert(memory[0x4fff] == 0x4d, "PUSH IX ------ (4fff) is %x\n", memory[0x4fff]);
    assert(memory[0x4ffe] == 0x8d, "PUSH IY ------ (4ffe) is %x\n", memory[0x4ffe]);
    assert(memory[0x4ffd] == 0x8e, "PUSH IY ------ (4ffd) is %x\n", memory[0x4ffd]);
    assert(sp == 0x5001 - 4, "after PUSH IX, PUSH IY ------ sp is %x\n", sp);
    tick(); tick();
    assert(REG_IX == 0x8d8e, "POP IX ------ IX is %x\n", REG_IX);
    assert(REG_IY == 0x714d, "POP IY ------ IY is %x\n", REG_IY);
    assert(sp == 0x5001, "after POP IX, POP IY ------ sp is %x\n", sp);

    // EXX;   EX AF,AF';   LD BC,nn;   LD DE,nn;   LD HL,nn;   LD A,n;   EX DE,HL;   EXX;   EX AF,AF';
    resetInstructions(0xd9, 0x08, /*LD BC,nn*/0x01, 0x55, 0x66, /*LD DE,nn*/0x11, 0x77, 0x88, /*LD HL,nn*/0x21, 0x99, 0xaa, /*LD A,n*/0x3e, 0xbb, /*EX DE,HL*/0xeb, 0xd9, 0x08);
    resetAllRegisters();
    REG_B = 0x14; REG_C = 0x21; REG_D = 0x32; REG_E = 0x43; REG_H = 0x54; REG_L = 0x65; REG_F = 0xff; REG_A = 0xa0;
    tick();
    assert(REG_B == 0 && REG_C == 0 && REG_D == 0 && REG_E == 0 && REG_H == 0 && REG_L == 0 && REG_F == 0xff && REG_A == 0xa0, "EXX ------ some variable is not zero or AF wrong");
    tick();
    assert(REG_B == 0 && REG_C == 0 && REG_D == 0 && REG_E == 0 && REG_H == 0 && REG_L == 0 && REG_F == 0x00 && REG_A == 0x00, "EX AF,AF' ------ some variable is not zero");
    tick(); tick(); tick(); tick(); tick(); // 4 loads and EX DE,HL
    assert(REG_H == 0x88 && REG_L == 0x77, "EX DE,HL ------ H is %x, L is %x\n", REG_H, REG_L);
    assert(REG_D == 0xaa && REG_E == 0x99, "EX DE,HL ------ D is %x, E is %x\n", REG_D, REG_E);
    tick(); // EXX
    assert(REG_B == 0x14 && REG_C == 0x21 && REG_D == 0x32 && REG_E == 0x43 && REG_H == 0x54 && REG_L == 0x65 && REG_F == 0x00 && REG_A == 0xbb, "EXX ------ some variable is not back");
    tick(); // EX AF,AF'
    assert(REG_B == 0x14 && REG_C == 0x21 && REG_D == 0x32 && REG_E == 0x43 && REG_H == 0x54 && REG_L == 0x65 && REG_F == 0xff && REG_A == 0xa0, "EX AF,AF' ------ some variable broke or AF is not back");

    // EX (SP),HL;   EX (SP),IX;   EX (SP),IY
    resetInstructions(0xe3, 0xdd, 0xe3, 0xfd, 0xe3);
    resetAllRegisters();
    REG_H = 0x11; REG_L = 0x82; REG_IX = 0x9514; REG_IY = 0x6432;
    sp = 0x5031;
    memory[0x5031] = 0x8c; memory[0x5032] = 0x7b;
    tick();
    assert(memory[sp] == 0x82 && memory[sp + 1] == 0x11, "EX (SP),HL ------ memory[0x5031] is %02x%02x\n", memory[0x5032], memory[0x5031]);
    assert(REG_H == 0x7b && REG_L == 0x8c, "EX (SP),HL ------ H: %x, L: %x\n", REG_H, REG_L);
    tick();
    assert(memory[sp] == 0x14 && memory[sp + 1] == 0x95, "EX (SP),IX ------ memory[0x5031] is %02x%02x\n", memory[0x5032], memory[0x5031]);
    assert(REG_IX == 0x1182, "EX (SP),IX ------ IX: %02x\n", REG_IX);
    assert(REG_H == 0x7b && REG_L == 0x8c, "EX (SP),IX ------ H: %x, L: %x\n", REG_H, REG_L);
    tick();
    assert(memory[sp] == 0x32 && memory[sp + 1] == 0x64, "EX (SP),IX ------ memory[0x5031] is %02x%02x\n", memory[0x5032], memory[0x5031]);
    assert(REG_IY == 0x9514, "EX (SP),IY ------ IY: %02x\n", REG_IY);
    assert(REG_IX == 0x1182, "EX (SP),IY ------ IX: %02x\n", REG_IX);
    assert(REG_H == 0x7b && REG_L == 0x8c, "EX (SP),IY ------ H: %x, L: %x\n", REG_H, REG_L);

    // ADD A,B 2+3
    REG_A = 0x2; REG_B = 0x3; REG_F = 0;
    resetInstructions(0x80); tick();
    assert(REG_A == 0x5, "ADD A,B 2+3 ------ A: %x\n", REG_A);
    assert(REG_F == 0b00000000, "ADD A,B 2+3 ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    // ADD A,C 9+7
    REG_A = 0x9; REG_C = 0x7; REG_F = 0;
    resetInstructions(0x81); tick();
    assert(REG_A == 0x10, "ADD A,C 9+7 ------ A: %x\n", REG_A);
    assert(REG_F == 0b00010000, "ADD A,C 9+7 ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    // ADD A,D 7e+1
    REG_A = 0x7e; REG_D = 0x1; REG_F = 0;
    resetInstructions(0x82); tick();
    assert(REG_A == 0x7f, "ADD A,D fe+1 ------ A: %x\n", REG_A);
    assert(REG_F == 0b00000000, "ADD A,D fe+1 ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    // ADD A,E 7e+2
    REG_A = 0x7e; REG_E = 0x2; REG_F = 0xff;
    resetInstructions(0x83); tick();
    assert(REG_A == 0x80, "ADD A,E 7e+2 ------ A: %x\n", REG_A);
    assert(REG_F == 0b10010100, "ADD A,E 7e+2 ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    // ADC A,H 30+cf, no carry
    REG_A = 0x30; REG_H = 0xcf; REG_F = 0;
    resetInstructions(0x8c); tick();
    assert(REG_A == 0xff, "ADC A,H 30+cf ------ A: %x\n", REG_A);
    assert(REG_F == 0b10000000, "ADC A,H 30+cf ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    // ADC A,L 30+cf, with carry
    REG_A = 0x30; REG_H = 0; REG_L = 0xcf; REG_F = FMASK_CARRY;
    resetInstructions(0x8d); tick();
    assert(REG_A == 0x00, "ADC A,L 30+cf w carry ------ A: %x\n", REG_A);
    assert(REG_F == 0b01010001, "ADC A,L 30+cf w carry ------ F: "BINARY_PATTERN"\n", BINARY_BYTE(REG_F));

    free(memory);
    printf("\033[47mAll tests passed!\033[0m\n");
    return 0;
    fail:
    free(memory);
    return 1;
}
