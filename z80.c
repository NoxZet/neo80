#include "z80.h"
#include <stdio.h>
#include <string.h>

#define memory_nn(h, l) memory[VAL_nn(h, l)]

static inline uint8_t regValue(uint8_t r) {
    if (r == ID_INDIRECT_HL) {
        return memory[REG_HL];
    } else {
        return regs[OPCODE_REG_POSITION[r]];
    }
}
static inline uint8_t* regPointer(uint8_t r) {
    if (r == ID_INDIRECT_HL) {
        return &memory[REG_HL];
    } else {
        return &regs[OPCODE_REG_POSITION[r]];
    }
}

uint8_t regs[8] = { 0 };
uint8_t shadowRegs[8] = { 0 };
uint16_t indexRegs[2] = { 0 };
uint16_t pc = 0, sp = 0;
uint8_t* memory;
uint8_t parityMap[256] = { 0 };

void initializeCore() {
    for (int i = 0; i < 256; i++) {
        int ones = 0;
        for (int j = 0; j < 8; j++) {
            ones += (ones >> j) & 0x1;
        }
        parityMap[i] = (1 - ones % 2) * FMASK_PARITY;
    }
}

void add(uint8_t operand, uint8_t carry = 0) {
    const uint16_t sum16 = (uint16_t)REG_A + operand + carry;
    const uint8_t sum8 = (uint8_t)sum16;
    REG_F = (
        (sum16 >= 0x100 ? FMASK_CARRY : 0)
        // SUB is 0
        | ((REG_A & 0x80) == (operand & 0x80) && (sum8 & 0x80) != (REG_A & 0x80) ? FMASK_PARITY : 0)
        | ((REG_A & 0xf) + (operand & 0xf) + carry >= 0x10 ? FMASK_HALFCARRY : 0)
        | (sum8 == 0 ? FMASK_ZERO : 0)
        | (sum8 & 0x80 ? FMASK_SIGN : 0)
    );
    REG_A = sum8;
}

inline static void subOptions(uint8_t operand, bool saveResult) {
    uint8_t dif = REG_A - operand;
    REG_F = (
        (operand > REG_A ? FMASK_CARRY : 0)
        | FMASK_SUB
        | ((REG_A & 0x80) != (operand & 0x80) && (dif & 0x80) != (REG_A & 0x80) ? FMASK_PARITY : 0)
        | ((operand & 0xf) > (REG_A & 0xf) ? FMASK_HALFCARRY : 0)
        | (dif == 0 ? FMASK_ZERO : 0)
        | (dif & 0x80 ? FMASK_SIGN : 0)
    );
    if (saveResult)
        REG_A = dif;
}
void sub(uint8_t operand) {
    return subOptions(operand, true);
}
void cp(uint8_t operand) {
    return subOptions(operand, false);
}

void inc(uint8_t* ptr) {
    (*ptr)++;
    REG_F = (
        (REG_F & FMASK_CARRY)
        // SUB is 0
        | (*ptr == 0x80 ? FMASK_PARITY : 0)
        | ((*ptr & 0x0f) == 0x0 ? FMASK_HALFCARRY : 0)
        | (*ptr == 0 ? FMASK_ZERO : 0)
        | (*ptr & 0x80 ? FMASK_SIGN : 0)
    );
}

void dec(uint8_t* ptr) {
    (*ptr)--;
    REG_F = (
        (REG_F & FMASK_CARRY)
        | FMASK_SUB
        | (*ptr == 0x7f ? FMASK_PARITY : 0)
        | ((*ptr & 0x0f) == 0xf ? FMASK_HALFCARRY : 0)
        | (*ptr == 0 ? FMASK_ZERO : 0)
        | (*ptr & 0x80 ? FMASK_SIGN : 0)
    );
}

static inline void neg() {
    REG_A = 0 - REG_A;
    REG_F = (
        (REG_A != 0 ? FMASK_CARRY : 0)
        | FMASK_SUB
        | (REG_A == 0x80 ? FMASK_PARITY : 0)
        | ((REG_A & 0xf) != 0 ? FMASK_HALFCARRY : 0)
        | (REG_A == 0 ? FMASK_ZERO : 0)
        | (REG_A & 0x80 ? FMASK_SIGN : 0)
    );
}

static inline void add16(uint16_t* target, uint16_t operand) {
    uint32_t sum = *target + operand;
    REG_F = (
        (sum & 0x10000 ? FMASK_CARRY : 0)
        // SUB 0
        | ((REG_HL & 0x0fff) + (operand & 0x0fff) & 0x1000 ? FMASK_HALFCARRY : 0)
        | (REG_F & (FMASK_PARITY | FMASK_ZERO | FMASK_SIGN))
    );
    *target = (uint16_t)sum;
}

static inline void adcHL(uint16_t operand) {
    uint32_t sum32 = REG_HL + operand + (REG_F & FMASK_CARRY);
    uint16_t sum16 = (uint16_t)sum16;
    REG_F = (
        (sum32 & 0x10000 ? FMASK_CARRY : 0)
        // SUB 0
        | ((REG_HL & 0x8000) == (operand & 0x8000) && (sum32 & 0x8000) != (REG_HL & 0x8000) ? FMASK_PARITY : 0)
        | ((REG_HL & 0x0fff) + (operand & 0x0fff) & 0x1000 ? FMASK_HALFCARRY : 0)
        | (sum16 == 0 ? FMASK_ZERO : 0)
        | (sum16 & 0x8000 ? FMASK_SIGN : 0)
    );
    REG_HL = sum16;
}

static inline void sbcHL(uint16_t operand) {
    operand += REG_F & FMASK_CARRY;
    uint16_t sum16 = REG_HL - operand;
    REG_F = (
        (operand > REG_HL ? FMASK_CARRY : 0)
        // SUB 0
        | ((REG_HL & 0x8000) != (operand & 0x8000) && (sum16 & 0x8000) != (REG_HL & 0x8000) ? FMASK_PARITY : 0)
        | ((operand & 0x0fff) > (REG_HL & 0x0fff) ? FMASK_HALFCARRY : 0)
        | (sum16 == 0 ? FMASK_ZERO : 0)
        | (sum16 & 0x8000 ? FMASK_SIGN : 0)
    );
    REG_HL = sum16;
}

static inline void setLogicalFlags() {
    REG_F = parityMap[REG_A] + (REG_A & 0x80 ? FMASK_SIGN : 0) + (REG_A == 0 ? FMASK_ZERO : 0);
}

void and(uint8_t operand) {
    REG_A &= operand;
    setLogicalFlags();
}

void or(uint8_t operand) {
    REG_A |= operand;
    setLogicalFlags();
}

void xor(uint8_t operand) {
    REG_A ^= operand;
    setLogicalFlags();
}

// based on
// http://www.z80.info/zip/z80-documented.pdf
// https://stackoverflow.com/a/45246967
static inline void daa() {
    uint16_t sum = REG_A;
    if (REG_F & FMASK_SUB) {
        if (REG_F & FMASK_HALFCARRY)
            sum -= 6;
        if (REG_F & FMASK_CARRY)
            sum -= 60;
        REG_F = REG_F & (FMASK_CARRY | FMASK_SUB)
            | (REG_F & FMASK_HALFCARRY && (REG_A & 0xf) <= 0x5 ? FMASK_HALFCARRY : 0);
    }
    else {
        if (REG_F & FMASK_HALFCARRY || (sum & 0xf) >= 0xa)
            sum += 6;
        if (REG_F & FMASK_CARRY || sum >= 0xa0)
            sum += 60;
        REG_F = REG_F & FMASK_CARRY | ((REG_A & 0xf) >= 0xa ? FMASK_HALFCARRY : 0);
    }
    uint8_t sum8 = (uint8_t)sum;
    REG_F = REG_F | (
        (REG_F & FMASK_CARRY || REG_A >= 0x9a ? FMASK_CARRY : 0)
        // SUB is 0
        + parityMap[sum8]
        // HALFCARRY is decided in blocks above
        + (sum8 == 0 ? FMASK_ZERO : 0)
        + (sum8 & 0x80 ? FMASK_SIGN : 0)
    );
    REG_A = sum8;
}

int tick() {
    uint8_t opcode1 = memory[pc];
    uint8_t opcode2 = memory[pc + 1];
    uint8_t opcode3 = memory[pc + 2];
    uint8_t opcode4 = memory[pc + 3];
    uint8_t opcodeNoffset = opcode2; // n for instructions that use (HL) and n - if using Index, this is offset by 1 because of +d
    uint8_t* lPtr = &REG_L;
    uint16_t hl = REG_HL;
    uint16_t hlIndex = hl;
    uint16_t indirectPC = 0; // to increment PC more if using (II+d)

    restartPrefix:
    uint16_t address;
    uint16_t temp[3] = { 0 };
    uint8_t* tempL = (uint8_t*)temp;
    uint8_t* tempH = (uint8_t*)temp + sizeof(uint8_t);
    switch (opcode1) {
        // ED - Z80 special instructions
        case 0xed:
            switch (opcode2) {
                // #z80 LD A,I; LD A,R; LD I,A; LD R,A
                // #z80 LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR
                // #z80 ADD A,(II+d), ADC,
                // LD dd,(nn)       01xx1011
                case 0x4b: case 0x5b: case 0x6b:
                    address = VAL_nn(opcode4, opcode3);
                    regs[((opcode2 & 0x30) >> 3) + 1] = memory[address + 1]; // high
                    regs[((opcode2 & 0x30) >> 3)] = memory[address]; // low
                    pc += 4; return 6;
                // LD SP,(nn)       01111011
                case 0x7b:
                    address = VAL_nn(opcode4, opcode3);
                    sp = ((uint16_t)memory[address + 1] << 8) + memory[address];
                    pc += 4; return 6;
                // LD (nn),dd       01xx0011
                case 0x43: case 0x53: case 0x63:
                    address = VAL_nn(opcode4, opcode3);
                    memory[address + 1] = regs[((opcode2 & 0x30) >> 3) + 1]; // high
                    memory[address] = regs[((opcode2 & 0x30) >> 3)]; // low
                    pc += 4; return 6;
                // LD (nn),SP       01110011
                case 0x73:
                    address = VAL_nn(opcode4, opcode3);
                    memory[address + 1] = SLICE_HIGH(sp);
                    memory[address] = SLICE_LOW(sp);
                    pc += 4; return 6;
                // NEG (A = 0 - A)
                case 0x44: neg(); pc += 2; return 2;
                // ADC HL, dd
                case 0x4a: adcHL(REG_BC); pc++; return 4;
                case 0x5a: adcHL(REG_DE); pc++; return 4;
                case 0x6a: adcHL(REG_HL); pc++; return 4;
                case 0x7a: adcHL(sp); pc++; return 4;
                // SBC HL, dd
                case 0x42: adcHL(REG_BC); pc++; return 4;
                case 0x52: adcHL(REG_DE); pc++; return 4;
                case 0x62: adcHL(REG_HL); pc++; return 4;
                case 0x72: adcHL(sp); pc++; return 4;
            }
            return -1;
        // DD/FD - Z80 IX/IY instructions
        // HL operations are done with II, (HL) operations are done with (II+d)
        case 0xdd: case 0xfd:
            lPtr = opcode1 == 0xdd ? (uint8_t*)&REG_IX : (uint8_t*)&REG_IY;
            hl = *(uint16_t*)lPtr;
            hlIndex = hl + opcode3;
            indirectPC = 1;
            opcodeNoffset = opcode4; // from format (i n) to (DD i d n)
            opcode1 = opcode2;
            opcode2 = opcode3;
            opcode3 = opcode4;
            pc++;
            goto restartPrefix;
        // **** LOAD INSTRUCTIONS ****
        // LD r,r' and LD r,(HL)
        // LD B,r;    LD B,(HL)
        case 0x40: REG_B = REG_B; pc++; return 1;           case 0x41: REG_B = REG_C; pc++; return 1;
        case 0x42: REG_B = REG_D; pc++; return 1;           case 0x43: REG_B = REG_E; pc++; return 1;
        case 0x44: REG_B = *(lPtr + 1); pc++; return 1;     case 0x45: REG_B = *lPtr; pc++; return 1;
        case 0x46: REG_B = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x47: REG_B = REG_A; pc++; return 1;
        // LD C,r;    LD C,(HL)
        case 0x48: REG_C = REG_B; pc++; return 1;           case 0x49: REG_C = REG_C; pc++; return 1;
        case 0x4a: REG_C = REG_D; pc++; return 1;           case 0x4b: REG_C = REG_E; pc++; return 1;
        case 0x4c: REG_C = *(lPtr + 1); pc++; return 1;     case 0x4d: REG_C = *lPtr; pc++; return 1;
        case 0x4e: REG_C = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x4f: REG_C = REG_A; pc++; return 1;
        // LD D,r;    LD D,(HL)
        case 0x50: REG_D = REG_B; pc++; return 1;           case 0x51: REG_D = REG_C; pc++; return 1;
        case 0x52: REG_D = REG_D; pc++; return 1;           case 0x53: REG_D = REG_E; pc++; return 1;
        case 0x54: REG_D = *(lPtr + 1); pc++; return 1;     case 0x55: REG_D = *lPtr; pc++; return 1;
        case 0x56: REG_D = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x57: REG_D = REG_A; pc++; return 1;
        // LD E,r;    LD E,(HL)
        case 0x58: REG_E = REG_B; pc++; return 1;           case 0x59: REG_E = REG_C; pc++; return 1;
        case 0x5a: REG_E = REG_D; pc++; return 1;           case 0x5b: REG_E = REG_E; pc++; return 1;
        case 0x5c: REG_E = *(lPtr + 1); pc++; return 1;     case 0x5d: REG_E = *lPtr; pc++; return 1;
        case 0x5e: REG_E = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x5f: REG_E = REG_A; pc++; return 1;
        // LD H,r;    LD H,(HL)
        case 0x60: *(lPtr + 1) = REG_B; pc++; return 1;    case 0x61: *(lPtr + 1) = REG_C; pc++; return 1;
        case 0x62: *(lPtr + 1) = REG_D; pc++; return 1;    case 0x63: *(lPtr + 1) = REG_E; pc++; return 1;
        case 0x64: *(lPtr + 1) = *(lPtr + 1); pc++; return 1;      case 0x65: *(lPtr + 1) = *lPtr; pc++; return 1;
        case 0x66: REG_H = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x67: *(lPtr + 1) = REG_A; pc++; return 1;
        // LD L,r;    LD L,(HL)
        case 0x68: *lPtr = REG_B; pc++; return 1;          case 0x69: *lPtr = REG_C; pc++; return 1;
        case 0x6a: *lPtr = REG_D; pc++; return 1;          case 0x6b: *lPtr = REG_E; pc++; return 1;
        case 0x6c: *lPtr = *(lPtr + 1); pc++; return 1;    case 0x6d: *lPtr = *lPtr; pc++; return 1;
        case 0x6e: REG_L = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x6f: *lPtr = REG_A; pc++; return 1;
        // LD A,r;    LD A,(HL)
        case 0x78: REG_A = REG_B; pc++; return 1;           case 0x79: REG_A = REG_C; pc++; return 1;
        case 0x7a: REG_A = REG_D; pc++; return 1;           case 0x7b: REG_A = REG_E; pc++; return 1;
        case 0x7c: REG_A = *(lPtr + 1); pc++; return 1;     case 0x7d: REG_A = *lPtr; pc++; return 1;
        case 0x7e: REG_A = memory[hlIndex]; pc += 1+indirectPC; return 2;
        case 0x7f: REG_A = REG_A; pc++; return 1;
        // LD (HL),r;
        case 0x70: memory[hlIndex] = REG_B; pc += 1+indirectPC; return 2;   case 0x71: memory[hlIndex] = REG_C; pc += 1+indirectPC; return 2;
        case 0x72: memory[hlIndex] = REG_D; pc += 1+indirectPC; return 2;   case 0x73: memory[hlIndex] = REG_E; pc += 1+indirectPC; return 2;
        case 0x74: memory[hlIndex] = REG_H; pc += 1+indirectPC; return 2;   case 0x75: memory[hlIndex] = REG_L; pc += 1+indirectPC; return 2;
        case 0x77: memory[hlIndex] = REG_A; pc += 1+indirectPC; return 2;
        // LD r,n
        case 0x06: REG_B = opcode2; pc += 2; return 2;      case 0x0e: REG_C = opcode2; pc += 2; return 2;
        case 0x16: REG_D = opcode2; pc += 2; return 2;      case 0x1e: REG_E = opcode2; pc += 2; return 2;
        case 0x26: *(lPtr + 1) = opcode2; pc += 2; return 2;    case 0x2e: *lPtr = opcode2; pc += 2; return 2;
        case 0x36: memory[hlIndex] = opcodeNoffset; pc += 2+indirectPC; return 2;
        case 0x3e: REG_A = opcode2; pc += 2; return 2;
        // LD A,(BC); LD A,(DE)     000x1010
        case 0x0a: REG_A = memory[REG_BC]; pc++; return 2;
        case 0x1a: REG_A = memory[REG_DE]; pc++; return 2;
        // LD A,(nn)                00111010 n n
        case 0x3a: REG_A = memory_nn(opcode3, opcode2); pc += 3; return 4;
        // LD (BC),A; LD (DE),A     000x0010
        case 0x02: memory[REG_BC] = REG_A; pc++; return 2;
        case 0x12: memory[REG_DE] = REG_A; pc++; return 2;
        // LD (nn),A                00110010 n n
        case 0x32: memory_nn(opcode3, opcode2) = REG_A; pc += 3; return 4;
        // LD dd,nn                 00xx0001 n n
        case 0x01: REG_C = opcode2; REG_B = opcode3; pc += 3; return 2;
        case 0x11: REG_E = opcode2; REG_D = opcode3; pc += 3; return 2;
        case 0x21: *lPtr = opcode2; *(lPtr + 1) = opcode3; REG_L = opcode2; pc += 3; return 2;
        case 0x31: sp = VAL_nn(opcode3, opcode2); pc += 3; return 2;
        // LD HL,(nn)               00101010
        case 0x2a:
            address = VAL_nn(opcode3, opcode2);
            *lPtr = memory[address]; *(lPtr + 1) = memory[address + 1];
            pc += 3; return 5;
        // LD (nn),HL               00100010
        case 0x22:
            address = VAL_nn(opcode3, opcode2);
            memory[address] = *lPtr; memory[address + 1] = *(lPtr + 1);
            pc += 3; return 5;
        // LD SP,HL                 11111001
        case 0xf9: sp = hl; pc++; return 1;
        // PUSH dd
        case 0xc5: sp--; memory[sp] = REG_B; sp--; memory[sp] = REG_C; pc++; return 3;
        case 0xd5: sp--; memory[sp] = REG_D; sp--; memory[sp] = REG_E; pc++; return 3;
        case 0xe5: sp--; memory[sp] = *(lPtr + 1); sp--; memory[sp] = *lPtr; pc++; return 3;
        case 0xf5: sp--; memory[sp] = REG_A; sp--; memory[sp] = REG_F; pc++; return 3;
        // POP dd
        case 0xc1: REG_C = memory[sp]; sp++; REG_B = memory[sp]; sp++; pc++; return 3;
        case 0xd1: REG_E = memory[sp]; sp++; REG_D = memory[sp]; sp++; pc++; return 3;
        case 0xe1: *lPtr = memory[sp]; sp++; *(lPtr + 1) = memory[sp]; sp++; pc++; return 3;
        case 0xf1: REG_F = memory[sp]; sp++; REG_A = memory[sp]; sp++; pc++; return 3;
        // EX DE,HL
        case 0xeb:
            temp[0] = *(uint16_t*)&REG_E;
            *(uint16_t*)&REG_E = *(uint16_t*)&REG_L;
            *(uint16_t*)&REG_L = temp[0];
            pc++; return 1;
        // EX AF,AF'
        case 0x08:
            temp[0] = REG_AF;
            REG_AF = *(uint16_t*)&shadowRegs[ID_F];
            *(uint16_t*)&shadowRegs[ID_F] = temp[0];
            pc++; return 1;
        // EXX (swap BC, DE, HL with shadow registers)
        case 0xd9: 
            memcpy(&temp, &REG_C, 6 * sizeof(uint8_t));
            memcpy(&REG_C, &shadowRegs[ID_C], 6 * sizeof(uint8_t));
            memcpy(&shadowRegs[ID_C], &temp, 6 * sizeof(uint8_t));
            pc++; return 1;
        // EX (SP),HL
        case 0xe3:
            temp[0] = hl;
            *(lPtr + 1) = memory[sp + 1]; *lPtr = memory[sp];
            memory[sp + 1] = *tempH; memory[sp] = *tempL;
            pc++; return 5;

        // **** 8 BIT ARITHMETIC ****
        // ADD A,r;   ADD A,(HL)
        case 0x80: add(REG_B); pc++; return 1; case 0x81: add(REG_C); pc++; return 1;
        case 0x82: add(REG_D); pc++; return 1; case 0x83: add(REG_E); pc++; return 1;
        case 0x84: add(*(lPtr + 1)); pc++; return 1; case 0x85: add(*lPtr); pc++; return 1;
        case 0x86: add(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0x87: add(REG_A); pc++; return 1;
        // ADC A,r;   ADC A,(HL)
        case 0x88: add(REG_B, REG_F & FMASK_CARRY); pc++; return 1; case 0x89: add(REG_C + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x8a: add(REG_D, REG_F & FMASK_CARRY); pc++; return 1; case 0x8b: add(REG_E + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x8c: add(*(lPtr + 1), REG_F & FMASK_CARRY); pc++; return 1; case 0x8d: add(*lPtr + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x8e: add(memory[hlIndex], REG_F & FMASK_CARRY); pc += 1+indirectPC; return 2;
        case 0x8f: add(REG_A, REG_F & FMASK_CARRY); pc++; return 1;
        // SUB A,r;   SUB A,(HL)
        case 0x90: sub(REG_B); pc++; return 1; case 0x91: sub(REG_C); pc++; return 1;
        case 0x92: sub(REG_D); pc++; return 1; case 0x93: sub(REG_E); pc++; return 1;
        case 0x94: sub(*(lPtr + 1)); pc++; return 1; case 0x95: sub(*lPtr); pc++; return 1;
        case 0x96: sub(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0x97: sub(REG_A); pc++; return 1;
        // SBC A,r;   SBC A,(HL)
        case 0x98: sub(REG_B + (REG_F & FMASK_CARRY)); pc++; return 1; case 0x99: sub(REG_C + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x9a: sub(REG_D + (REG_F & FMASK_CARRY)); pc++; return 1; case 0x9b: sub(REG_E + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x9c: sub(*(lPtr + 1) + (REG_F & FMASK_CARRY)); pc++; return 1; case 0x9d: sub(*lPtr + (REG_F & FMASK_CARRY)); pc++; return 1;
        case 0x9e: sub(memory[hlIndex] + (REG_F & FMASK_CARRY)); pc += 1+indirectPC; return 2;
        case 0x9f: sub(REG_A + (REG_F & FMASK_CARRY)); pc++; return 1;
        // AND A,r;   AND A,(HL)
        case 0xa0: and(REG_B); pc++; return 1; case 0xa1: and(REG_C); pc++; return 1;
        case 0xa2: and(REG_D); pc++; return 1; case 0xa3: and(REG_E); pc++; return 1;
        case 0xa4: and(*(lPtr + 1)); pc++; return 1; case 0xa5: and(*lPtr); pc++; return 1;
        case 0xa6: and(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0xa7: and(REG_A); pc++; return 1;
        // XOR A,r;   XOR A,(HL)
        case 0xa8: xor(REG_B); pc++; return 1; case 0xa9: xor(REG_C); pc++; return 1;
        case 0xaa: xor(REG_D); pc++; return 1; case 0xab: xor(REG_E); pc++; return 1;
        case 0xac: xor(*(lPtr + 1)); pc++; return 1; case 0xad: xor(*lPtr); pc++; return 1;
        case 0xae: xor(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0xaf: xor(REG_A); pc++; return 1;
        // OR A,r;   OR A,(HL)
        case 0xb0: or(REG_B); pc++; return 1; case 0xb1: or(REG_C); pc++; return 1;
        case 0xb2: or(REG_D); pc++; return 1; case 0xb3: or(REG_E); pc++; return 1;
        case 0xb4: or(*(lPtr + 1)); pc++; return 1; case 0xb5: or(*lPtr); pc++; return 1;
        case 0xb6: or(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0xb7: or(REG_A); pc++; return 1;
        // CP A,r;   CP A,(HL)
        case 0xb8: cp(REG_B); pc++; return 1; case 0xb9: cp(REG_C); pc++; return 1;
        case 0xba: cp(REG_D); pc++; return 1; case 0xbb: cp(REG_E); pc++; return 1;
        case 0xbc: cp(*(lPtr + 1)); pc++; return 1; case 0xbd: cp(*lPtr); pc++; return 1;
        case 0xbe: cp(memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0xbf: cp(REG_A); pc++; return 1;
        // ADD A,n
        case 0xc6: add(opcode2); pc += 2; return 2;
        // ADC A,n
        case 0xce: add(opcode2 + (REG_F & FMASK_CARRY)); pc += 2; return 2;
        // SUB A,n
        case 0xd6: sub(opcode2); pc += 2; return 2;
        // SBC A,n
        case 0xde: sub(opcode2 + (REG_F & FMASK_CARRY)); pc += 2; return 2;
        // AND A,n
        case 0xe6: and(opcode2); pc += 2; return 2;
        // XOR A,n
        case 0xee: xor(opcode2); pc += 2; return 2;
        // OR A,n
        case 0xf6: or(opcode2); pc += 2; return 2;
        // CP A,n
        case 0xfe: cp(opcode2); pc += 2; return 2;
        // INC r;   INC (HL)
        case 0x04: inc(&REG_B); pc++; return 1; case 0x0c: inc(&REG_C); pc++; return 1;
        case 0x14: inc(&REG_D); pc++; return 1; case 0x1c: inc(&REG_E); pc++; return 1;
        case 0x24: inc(lPtr + 1); pc++; return 1; case 0x2c: inc(lPtr); pc++; return 1;
        case 0x34: inc(&memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0x3c: inc(&REG_A); pc++; return 1;
        // DEC r;   DEC (HL)
        case 0x05: dec(&REG_B); pc++; return 1; case 0x0d: dec(&REG_C); pc++; return 1;
        case 0x15: dec(&REG_D); pc++; return 1; case 0x1d: dec(&REG_E); pc++; return 1;
        case 0x25: dec(lPtr + 1); pc++; return 1; case 0x2d: dec(lPtr); pc++; return 1;
        case 0x35: dec(&memory[hlIndex]); pc += 1+indirectPC; return 2;
        case 0x3d: dec(&REG_A); pc++; return 1;
        // DAA
        case 0x27: daa(); pc++; return 1;
        // CPL (complement)
        case 0x2f: REG_A = ~REG_A; REG_F = REG_F | FMASK_HALFCARRY | FMASK_SUB; pc++; return 1;
        // CCF (complement carry flag)
        case 0x3f:
            REG_F = (REG_F & (FMASK_SIGN | FMASK_ZERO | FMASK_PARITY) | (REG_F & FMASK_CARRY) << 4)
                ^ (REG_F & FMASK_CARRY);
            pc++; return 1;
        // SCF (set carry flag)
        case 0x37:
            REG_F = REG_F & (FMASK_SIGN | FMASK_ZERO | FMASK_PARITY) | FMASK_CARRY;
            pc++; return 1;
        // NOP
        case 0x00: pc++; return 1;
        // HALT
        case 0x76: return 1;
        // #TODO interrupts
        // ADD HL, dd
        case 0x09: add16((uint16_t*)lPtr, REG_BC); pc++; return 3;
        case 0x19: add16((uint16_t*)lPtr, REG_DE); pc++; return 3;
        case 0x29: add16((uint16_t*)lPtr, REG_HL); pc++; return 3;
        case 0x39: add16((uint16_t*)lPtr, sp); pc++; return 3;
        // INC dd
        case 0x03: REG_BC++; pc++; return 2;
        case 0x13: REG_DE++; pc++; return 2;
        case 0x23: (*lPtr)++; pc++; return 2;
        case 0x33: sp++; pc++; return 2;
        // DEC dd
        case 0x0b: REG_BC--; pc++; return 2;
        case 0x1b: REG_DE--; pc++; return 2;
        case 0x2b: (*lPtr)--; pc++; return 2;
        case 0x3b: sp--; pc++; return 2;
    }
    return 1;
}
