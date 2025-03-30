#ifndef Z80_HEADER
#define Z80_HEADER

#include <stdbool.h>
#include <stdint.h>

#define ID_B 1
#define ID_C 0
#define ID_D 3
#define ID_E 2
#define ID_H 5
#define ID_L 4
#define ID_INDIRECT_HL 6
#define ID_A 7
#define ID_F 6

static const uint8_t OPCODE_REG_POSITION[] = {1, 0, 3, 2, 5, 4, 6, 7};

#define FMASK_SIGN 0x80
#define FMASK_ZERO 0x40
#define FMASK_HALFCARRY 0x10
#define FMASK_PARITY 0x4
#define FMASK_SUB 0x2 // N
#define FMASK_CARRY 0x1

#define REG_B regs[ID_B]
#define REG_C regs[ID_C]
#define REG_D regs[ID_D]
#define REG_E regs[ID_E]
#define REG_H regs[ID_H]
#define REG_L regs[ID_L]
#define REG_F regs[ID_F]
#define REG_A regs[ID_A]
#define REG_IX indexRegs[0]
#define REG_IY indexRegs[1]

#define VAL_nn(h, l) (((uint16_t)(h) << 8) + l)
#define VAL_dd(hId) VAL_nn(regs[hId], regs[hId + 1])

#define REG_BC (*(uint16_t*)&REG_C)
#define REG_DE (*(uint16_t*)&REG_E)
#define REG_HL (*(uint16_t*)&REG_L)
#define REG_AF (*(uint16_t*)&REG_F)

#define SLICE_LOW(reg) ((reg) & 0xff)
#define SLICE_HIGH(reg) ((reg) >> 8)

extern uint8_t regs[8];
extern uint8_t shadowRegs[8];
extern uint16_t indexRegs[2];
extern uint16_t pc, sp;
extern uint8_t* memory;
extern uint8_t parityMap[256];

void initializeCore();
int tick();

#endif
