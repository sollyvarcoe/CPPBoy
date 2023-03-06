#pragma once

#include "memory_unit.h"
#include "test.h"
#include <functional>
#include <unordered_map>

static std::unordered_map<uint8_t, std::string> assembly_string_map{
    {{0x00, "nop"},         {0x01, "ld bc,d16"},   {0x02, "ld [bc],a"},
     {0x03, "inc bc"},      {0x04, "inc b"},       {0x05, "dec b"},
     {0x06, "ld b,d8"},     {0x07, "rlca"},        {0x08, "ld [a16],sp"},
     {0x09, "add hl,bc"},   {0x0a, "ld a,[bc]"},   {0x0b, "dec bc"},
     {0x0c, "inc c"},       {0x0d, "dec c"},       {0x0e, "ld c,d8"},
     {0x0f, "rrca"},

     {0x10, "stop"},        {0x11, "ld de,d16"},   {0x12, "ld [de],a"},
     {0x13, "inc de"},      {0x14, "inc d"},       {0x15, "dec d"},
     {0x16, "ld d,d8"},     {0x17, "rla"},         {0x18, "jr pc+r8"},
     {0x19, "add hl,de"},   {0x1a, "ld a,[de]"},   {0x1b, "dec de"},
     {0x1c, "inc e"},       {0x1d, "dec e"},       {0x1e, "ld e,d8"},
     {0x1f, "rra"},

     {0x20, "jr nz,pc+r8"}, {0x21, "ld hl,d16"},   {0x22, "ld [hl+],a"},
     {0x23, "inc hl"},      {0x24, "inc h"},       {0x25, "dec h"},
     {0x26, "ld h,d8"},     {0x27, "daa"},         {0x28, "jr z,pc+r8"},
     {0x29, "add hl,hl"},   {0x2a, "ld a,[hl+]"},  {0x2b, "dec hl"},
     {0x2c, "inc l"},       {0x2d, "dec l"},       {0x2e, "ld l,d8"},
     {0x2f, "cpl"},

     {0x30, "jr nc,pc+r8"}, {0x31, "ld sp,d16"},   {0x32, "ld [hl-],a"},
     {0x33, "inc sp"},      {0x34, "inc [hl]"},    {0x35, "dec [hl]"},
     {0x36, "ld [hl],d8"},  {0x37, "scf"},         {0x38, "jr c,pc+r8"},
     {0x39, "add hl,sp"},   {0x3a, "ld a,[hl-]"},  {0x3b, "dec sp"},
     {0x3c, "inc a"},       {0x3d, "dec a"},       {0x3e, "ld a,d8"},
     {0x3f, "ccf"},

     {0x40, "ld b,b"},      {0x41, "ld b,c"},      {0x42, "ld b,d"},
     {0x43, "ld b,e"},      {0x44, "ld b,h"},      {0x45, "ld b,l"},
     {0x46, "ld b,[hl]"},   {0x47, "ld b,a"},      {0x48, "ld c,b"},
     {0x49, "ld c,c"},      {0x4a, "ld c,d"},      {0x4b, "ld c,e"},
     {0x4c, "ld c,h"},      {0x4d, "ld c,l"},      {0x4e, "ld c,[hl]"},
     {0x4f, "ld c,a"},

     {0x50, "ld d,b"},      {0x51, "ld d,c"},      {0x52, "ld d,d"},
     {0x53, "ld d,e"},      {0x54, "ld d,h"},      {0x55, "ld d,l"},
     {0x56, "ld d,[hl]"},   {0x57, "ld d,a"},      {0x58, "ld e,b"},
     {0x59, "ld e,c"},      {0x5a, "ld e,d"},      {0x5b, "ld e,e"},
     {0x5c, "ld e,h"},      {0x5d, "ld e,l"},      {0x5e, "ld e,[hl]"},
     {0x5f, "ld e,a"},

     {0x60, "ld h,b"},      {0x61, "ld h,c"},      {0x62, "ld h,d"},
     {0x63, "ld h,e"},      {0x64, "ld h,h"},      {0x65, "ld h,l"},
     {0x66, "ld h,[hl]"},   {0x67, "ld h,a"},      {0x68, "ld l,b"},
     {0x69, "ld l,c"},      {0x6a, "ld l,d"},      {0x6b, "ld l,e"},
     {0x6c, "ld l,h"},      {0x6d, "ld l,l"},      {0x6e, "ld l,[hl]"},
     {0x6f, "ld l,a"},

     {0x70, "ld [hl],b"},   {0x71, "ld [hl],c"},   {0x72, "ld [hl],d"},
     {0x73, "ld [hl],e"},   {0x74, "ld [hl],h"},   {0x75, "ld [hl],l"},
     {0x76, "halt"},        {0x77, "ld [hl],a"},   {0x78, "ld a,b"},
     {0x79, "ld a,c"},      {0x7a, "ld a,d"},      {0x7b, "ld a,e"},
     {0x7c, "ld a,h"},      {0x7d, "ld a,l"},      {0x7e, "ld a,[hl]"},
     {0x7f, "ld a,a"},

     {0x80, "add b"},       {0x81, "add c"},       {0x82, "add d"},
     {0x83, "add e"},       {0x84, "add h"},       {0x85, "add l"},
     {0x86, "add [hl]"},    {0x87, "add a"},       {0x88, "adc b"},
     {0x89, "adc c"},       {0x8a, "adc d"},       {0x8b, "adc e"},
     {0x8c, "adc h"},       {0x8d, "adc l"},       {0x8e, "adc [hl]"},
     {0x8f, "adc a"},

     {0x90, "sub b"},       {0x91, "sub c"},       {0x92, "sub d"},
     {0x93, "sub e"},       {0x94, "sub h"},       {0x95, "sub l"},
     {0x96, "sub [hl]"},    {0x97, "sub a"},       {0x98, "sbc b"},
     {0x99, "sbc c"},       {0x9a, "sbc d"},       {0x9b, "sbc e"},
     {0x9c, "sbc h"},       {0x9d, "sbc l"},       {0x9e, "sbc [hl]"},
     {0x9f, "sbc a"},

     {0xa0, "and b"},       {0xa1, "and c"},       {0xa2, "and d"},
     {0xa3, "and e"},       {0xa4, "and h"},       {0xa5, "and l"},
     {0xa6, "and [hl]"},    {0xa7, "and a"},       {0xa8, "xor b"},
     {0xa9, "xor c"},       {0xaa, "xor d"},       {0xab, "xor e"},
     {0xac, "xor h"},       {0xad, "xor l"},       {0xae, "xor [hl]"},
     {0xaf, "xor a"},

     {0xb0, "or b"},        {0xb1, "or c"},        {0xb2, "or d"},
     {0xb3, "or e"},        {0xb4, "or h"},        {0xb5, "or l"},
     {0xb6, "or [hl]"},     {0xb7, "or a"},        {0xb8, "cp b"},
     {0xb9, "cp c"},        {0xba, "cp d"},        {0xbb, "cp e"},
     {0xbc, "cp h"},        {0xbd, "cp l"},        {0xbe, "cp [hl]"},
     {0xbf, "cp a"},

     {0xc0, "ret nz"},      {0xc1, "pop bc"},      {0xc2, "jp nz,a16"},
     {0xc3, "jp a16"},      {0xc4, "call nz,a16"}, {0xc5, "push bc"},
     {0xc6, "add d8"},      {0xc7, "rst $00"},     {0xc8, "ret z"},
     {0xc9, "ret"},         {0xca, "jp z,a16"},    {0xcb, "CBPREFIX"},
     {0xcc, "call z,a16"},  {0xcd, "call a16"},    {0xce, "adc d8"},
     {0xcf, "rst $08"},

     {0xd0, "ret nc"},      {0xd1, "pop de"},      {0xd2, "jp nc,a16"},
     {0xd3, "db $d3"},      {0xd4, "call nc,a16"}, {0xd5, "push de"},
     {0xd6, "sub d8"},      {0xd7, "rst $10"},     {0xd8, "ret c"},
     {0xd9, "reti"},        {0xda, "jp c,a16"},    {0xdb, "db $db"},
     {0xdc, "call c,a16"},  {0xdd, "db $dd"},      {0xde, "sbc d8"},
     {0xdf, "rst $18"},

     {0xe0, "ldh [a8],a"},  {0xe1, "pop hl"},      {0xe2, "ld [c],a"},
     {0xe3, "db $e3"},      {0xe4, "db $e4"},      {0xe5, "push hl"},
     {0xe6, "and d8"},      {0xe7, "rst $20"},     {0xe8, "add sp,r8"},
     {0xe9, "jp hl"},       {0xea, "ld [a16],a"},  {0xeb, "db $eb"},
     {0xec, "db $ec"},      {0xed, "db $ed"},      {0xee, "xor d8"},
     {0xef, "rst $28"},

     {0xf0, "ldh a,[a8]"},  {0xf1, "pop af"},      {0xf2, "ld a,[c]"},
     {0xf3, "di"},          {0xf4, "db $f4"},      {0xf5, "push af"},
     {0xf6, "or d8"},       {0xf7, "rst $30"},     {0xf8, "ld hl,sp+r8"},
     {0xf9, "ld sp,hl"},    {0xfa, "ld a,[a16]"},  {0xfb, "ei"},
     {0xfc, "db $fc"},      {0xfd, "db $fd"},      {0xfe, "cp d8"},
     {0xff, "rst $38"}}};

static std::unordered_map<uint8_t, std::string> cb_instruction_map{
    {{0x00, "rlc b"},      {0x01, "rlc c"},      {0x02, "rlc d"},
     {0x03, "rlc e"},      {0x04, "rlc h"},      {0x05, "rlc l"},
     {0x06, "rlc [hl]"},   {0x07, "rlc a"},      {0x08, "rrc b"},
     {0x09, "rrc c"},      {0x0a, "rrc d"},      {0x0b, "rrc e"},
     {0x0c, "rrc h"},      {0x0d, "rrc l"},      {0x0e, "rrc [hl]"},
     {0x0f, "rrc a"},      {0x10, "rl b"},       {0x11, "rl c"},
     {0x12, "rl d"},       {0x13, "rl e"},       {0x14, "rl h"},
     {0x15, "rl l"},       {0x16, "rl [hl]"},    {0x17, "rl a"},
     {0x18, "rr b"},       {0x19, "rr c"},       {0x1a, "rr d"},
     {0x1b, "rr e"},       {0x1c, "rr h"},       {0x1d, "rr l"},
     {0x1e, "rr [hl]"},    {0x1f, "rr a"},       {0x20, "sla b"},
     {0x21, "sla c"},      {0x22, "sla d"},      {0x23, "sla e"},
     {0x24, "sla h"},      {0x25, "sla l"},      {0x26, "sla [hl]"},
     {0x27, "sla a"},      {0x28, "sra b"},      {0x29, "sra c"},
     {0x2a, "sra d"},      {0x2b, "sra e"},      {0x2c, "sra h"},
     {0x2d, "sra l"},      {0x2e, "sra [hl]"},   {0x2f, "sra a"},
     {0x30, "swap b"},     {0x31, "swap c"},     {0x32, "swap d"},
     {0x33, "swap e"},     {0x34, "swap h"},     {0x35, "swap l"},
     {0x36, "swap [hl]"},  {0x37, "swap a"},     {0x38, "srl b"},
     {0x39, "srl c"},      {0x3a, "srl d"},      {0x3b, "srl e"},
     {0x3c, "srl h"},      {0x3d, "srl l"},      {0x3e, "srl [hl]"},
     {0x3f, "srl a"},      {0x40, "bit 0,b"},    {0x41, "bit 0,c"},
     {0x42, "bit 0,d"},    {0x43, "bit 0,e"},    {0x44, "bit 0,h"},
     {0x45, "bit 0,l"},    {0x46, "bit 0,[hl]"}, {0x47, "bit 0,a"},
     {0x48, "bit 1,b"},    {0x49, "bit 1,c"},    {0x4a, "bit 1,d"},
     {0x4b, "bit 1,e"},    {0x4c, "bit 1,h"},    {0x4d, "bit 1,l"},
     {0x4e, "bit 1,[hl]"}, {0x4f, "bit 1,a"},    {0x50, "bit 2,b"},
     {0x51, "bit 2,c"},    {0x52, "bit 2,d"},    {0x53, "bit 2,e"},
     {0x54, "bit 2,h"},    {0x55, "bit 2,l"},    {0x56, "bit 2,[hl]"},
     {0x57, "bit 2,a"},    {0x58, "bit 3,b"},    {0x59, "bit 3,c"},
     {0x5a, "bit 3,d"},    {0x5b, "bit 3,e"},    {0x5c, "bit 3,h"},
     {0x5d, "bit 3,l"},    {0x5e, "bit 3,[hl]"}, {0x5f, "bit 3,a"},
     {0x60, "bit 4,b"},    {0x61, "bit 4,c"},    {0x62, "bit 4,d"},
     {0x63, "bit 4,e"},    {0x64, "bit 4,h"},    {0x65, "bit 4,l"},
     {0x66, "bit 4,[hl]"}, {0x67, "bit 4,a"},    {0x68, "bit 5,b"},
     {0x69, "bit 5,c"},    {0x6a, "bit 5,d"},    {0x6b, "bit 5,e"},
     {0x6c, "bit 5,h"},    {0x6d, "bit 5,l"},    {0x6e, "bit 5,[hl]"},
     {0x6f, "bit 5,a"},    {0x70, "bit 6,b"},    {0x71, "bit 6,c"},
     {0x72, "bit 6,d"},    {0x73, "bit 6,e"},    {0x74, "bit 6,h"},
     {0x75, "bit 6,l"},    {0x76, "bit 6,[hl]"}, {0x77, "bit 6,a"},
     {0x78, "bit 7,b"},    {0x79, "bit 7,c"},    {0x7a, "bit 7,d"},
     {0x7b, "bit 7,e"},    {0x7c, "bit 7,h"},    {0x7d, "bit 7,l"},
     {0x7e, "bit 7,[hl]"}, {0x7f, "bit 7,a"},    {0x80, "res 0,b"},
     {0x81, "res 0,c"},    {0x82, "res 0,d"},    {0x83, "res 0,e"},
     {0x84, "res 0,h"},    {0x85, "res 0,l"},    {0x86, "res 0,[hl]"},
     {0x87, "res 0,a"},    {0x88, "res 1,b"},    {0x89, "res 1,c"},
     {0x8a, "res 1,d"},    {0x8b, "res 1,e"},    {0x8c, "res 1,h"},
     {0x8d, "res 1,l"},    {0x8e, "res 1,[hl]"}, {0x8f, "res 1,a"},
     {0x90, "res 2,b"},    {0x91, "res 2,c"},    {0x92, "res 2,d"},
     {0x93, "res 2,e"},    {0x94, "res 2,h"},    {0x95, "res 2,l"},
     {0x96, "res 2,[hl]"}, {0x97, "res 2,a"},    {0x98, "res 3,b"},
     {0x99, "res 3,c"},    {0x9a, "res 3,d"},    {0x9b, "res 3,e"},
     {0x9c, "res 3,h"},    {0x9d, "res 3,l"},    {0x9e, "res 3,[hl]"},
     {0x9f, "res 3,a"},    {0xa0, "res 4,b"},    {0xa1, "res 4,c"},
     {0xa2, "res 4,d"},    {0xa3, "res 4,e"},    {0xa4, "res 4,h"},
     {0xa5, "res 4,l"},    {0xa6, "res 4,[hl]"}, {0xa7, "res 4,a"},
     {0xa8, "res 5,b"},    {0xa9, "res 5,c"},    {0xaa, "res 5,d"},
     {0xab, "res 5,e"},    {0xac, "res 5,h"},    {0xad, "res 5,l"},
     {0xae, "res 5,[hl]"}, {0xaf, "res 5,a"},    {0xb0, "res 6,b"},
     {0xb1, "res 6,c"},    {0xb2, "res 6,d"},    {0xb3, "res 6,e"},
     {0xb4, "res 6,h"},    {0xb5, "res 6,l"},    {0xb6, "res 6,[hl]"},
     {0xb7, "res 6,a"},    {0xb8, "res 7,b"},    {0xb9, "res 7,c"},
     {0xba, "res 7,d"},    {0xbb, "res 7,e"},    {0xbc, "res 7,h"},
     {0xbd, "res 7,l"},    {0xbe, "res 7,[hl]"}, {0xbf, "res 7,a"},
     {0xc0, "set 0,b"},    {0xc1, "set 0,c"},    {0xc2, "set 0,d"},
     {0xc3, "set 0,e"},    {0xc4, "set 0,h"},    {0xc5, "set 0,l"},
     {0xc6, "set 0,[hl]"}, {0xc7, "set 0,a"},    {0xc8, "set 1,b"},
     {0xc9, "set 1,c"},    {0xca, "set 1,d"},    {0xcb, "set 1,e"},
     {0xcc, "set 1,h"},    {0xcd, "set 1,l"},    {0xce, "set 1,[hl]"},
     {0xcf, "set 1,a"},    {0xd0, "set 2,b"},    {0xd1, "set 2,c"},
     {0xd2, "set 2,d"},    {0xd3, "set 2,e"},    {0xd4, "set 2,h"},
     {0xd5, "set 2,l"},    {0xd6, "set 2,[hl]"}, {0xd7, "set 2,a"},
     {0xd8, "set 3,b"},    {0xd9, "set 3,c"},    {0xda, "set 3,d"},
     {0xdb, "set 3,e"},    {0xdc, "set 3,h"},    {0xdd, "set 3,l"},
     {0xde, "set 3,[hl]"}, {0xdf, "set 3,a"},    {0xe0, "set 4,b"},
     {0xe1, "set 4,c"},    {0xe2, "set 4,d"},    {0xe3, "set 4,e"},
     {0xe4, "set 4,h"},    {0xe5, "set 4,l"},    {0xe6, "set 4,[hl]"},
     {0xe7, "set 4,a"},    {0xe8, "set 5,b"},    {0xe9, "set 5,c"},
     {0xea, "set 5,d"},    {0xeb, "set 5,e"},    {0xec, "set 5,h"},
     {0xed, "set 5,l"},    {0xee, "set 5,[hl]"}, {0xef, "set 5,a"},
     {0xf0, "set 6,b"},    {0xf1, "set 6,c"},    {0xf2, "set 6,d"},
     {0xf3, "set 6,e"},    {0xf4, "set 6,h"},    {0xf5, "set 6,l"},
     {0xf6, "set 6,[hl]"}, {0xf7, "set 6,a"},    {0xf8, "set 7,b"},
     {0xf9, "set 7,c"},    {0xfa, "set 7,d"},    {0xfb, "set 7,e"},
     {0xfc, "set 7,h"},    {0xfd, "set 7,l"},    {0xfe, "set 7,[hl]"},
     {0xff, "set 7,a"}}};

using cpuFunc = void (*)(CpuRegisters &, MemoryUnit &);

void test(CpuRegisters &registers, uint8_t *rom) {
  uint8_t lsb = rom[registers.pc.value()];
  registers.pc.set(registers.pc.value() + 1);
  uint8_t msb = rom[registers.pc.value()];
  registers.pc.set(registers.pc.value() + 1);
  registers.bc.set(msb << 8 | lsb);
}

inline bool calc_half_carry(const uint8_t a, const uint8_t b) {
  // Extract first 4 bits of a and b
  // Add resultant values then return bit 4
  uint8_t res = (a & 0x0F) + (b & 0x0F);
  return res & (1 << 4) >> 4;
}

inline bool calc_half_carry(const uint16_t a, const uint16_t b) {
  // Extract first 12 bits of a and b
  // Add resultant values then return bit 12
  uint16_t res = (a & 0xFFF0) + (b & 0xFFF0);
  return res & (1 << 12) >> 12;
}

inline bool calc_carry(const uint8_t a, const uint8_t b) {
  uint16_t res = a + b;
  return res & (1 << 8) >> 8;
}

inline bool calc_carry(const uint16_t a, const uint8_t b) {
  uint32_t res = a + b;
  return res * (1 << 16) >> 16;
}

inline void incrementRegister(Register &r, CpuRegisters &registers) {
  bool half_carry = calc_half_carry(r.value(), 1);
  uint8_t res = r.value() + 1;
  (res == 0) ? registers.set_zero_f() : registers.clear_zero_f();
  (half_carry) ? registers.set_hcarry_f() : registers.clear_hcarry_f();
  registers.clear_sub_f();
}

inline void incrementRegister(DualRegister &r, CpuRegisters &registers) {
  bool half_carry = calc_half_carry(r.value(), 1);
  uint16_t res = r.value() + 1;
  (res == 0) ? registers.set_zero_f() : registers.clear_zero_f();
  (half_carry) ? registers.set_hcarry_f() : registers.clear_hcarry_f();
  registers.clear_sub_f();
}

inline void decrementRegister(Register &r) { r.decrement(); }

inline void decrementRegister(DualRegister &r) { r.decrement(); }

inline void push(Counter &sp, Register &hi, Register &lo, MemoryUnit &mem) {
  sp.decrement();
  mem.write(sp.value(), hi.value());
  sp.decrement();
  mem.write(sp.value(), lo.value());
}

inline void pop(Counter &sp, DualRegister &target, MemoryUnit &mem) {
  uint8_t lsb = mem.read(sp.value());
  sp.increment();
  uint8_t msb = mem.read(sp.value());
  sp.increment();
  uint16_t nn = (msb << 8) | lsb;
  target.set(nn);
}

inline void load(Register &r1, Register &r2) { r1.set(r2.value()); }

static std::unordered_map<uint8_t, cpuFunc> instruction_map {
  {
    {0x00, [](CpuRegisters& registers, MemoryUnit& memory) { 
        // nop
        return;
    }},
    {0x01, [](CpuRegisters& registers, MemoryUnit& memory) {

        // ld bc,d16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.bc.set(msb << 8 | lsb);
    }},
    // {0x02, "ld [bc],a"},
    {0x03, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc bc
        incrementRegister(registers.bc, registers);
    }},    
    {0x04, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc b
        incrementRegister(registers.b, registers);
    }},    
    {0x05, [](CpuRegisters& registers, MemoryUnit& memory) {
        // dec b
        decrementRegister(registers.b);
    }},    
    {0x06, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.b.set(val);
    }},
    // {0x07, "rlca"},
    {0x08, [](CpuRegisters& registers, MemoryUnit& memory){
        // ld [a16], sp
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint16_t addr = (msb << 8) | lsb;
        memory.write(addr, registers.sp.value());
    }},
    {0x09, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,bc
        registers.add_hl(registers.bc.value());
    }},
    // {0x0a, "ld a,[bc]"},
    {0x0b, [](CpuRegisters& registers, MemoryUnit& memory) {
        // dec bc
        decrementRegister(registers.bc);
    }},    
    {0x0c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc c
        incrementRegister(registers.c, registers);
    }},    
    {0x0d, [](CpuRegisters& registers, MemoryUnit& memory) {
        // dec c
        decrementRegister(registers.c);
    }},    
    {0x0e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.c.set(val);
    }},
    // {0x0f, "rrca"},

    {0x10, [](CpuRegisters& registers, MemoryUnit& memory) {
        // stop
        return;
    }},
    {0x11, [](CpuRegisters& registers, MemoryUnit& memory) {
        //ld de,d16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.de.set(msb << 8 | lsb);
    }},
    // {0x12, "ld [de],a"},
    {0x13, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc de
        incrementRegister(registers.de, registers);

    }},
    {0x14, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc d
        incrementRegister(registers.d, registers);

    }},
    // {0x15, "dec d"},
    {0x16, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.d.set(val);
    }},
    // {0x17, "rla"},
    // {0x18, "jr pc+r8"},
    {0x19, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,de
        registers.add_hl(registers.de.value());
    }},
    {0x1a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,[de]
        registers.a.set(memory.read(registers.de.value()));

    }},
    // {0x1b, "dec de"},
    {0x1c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc e
        incrementRegister(registers.e, registers);

    }},
    // {0x1d, "dec e"},
    {0x1e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.e.set(val);
    }},
    // {0x1f, "rra"},
    {0x20, [](CpuRegisters& registers, MemoryUnit& memory) {
        // jr nz,pc+r8
        int8_t e = static_cast<int8_t>(memory.read(registers.pc.value()));
        registers.pc.increment();
        if (!(registers.f.value() >> 7 | 0x01)) {
            registers.pc.set(registers.pc.value() + e);
        }
    }},
    {0x21, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld hl, d16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.hl.set(msb << 8 | lsb);
    }},
    {0x22, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl+],a
        memory.write(registers.hl.value(), registers.a.value());
        registers.hl.increment();
    }},
    {0x23, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc hl
        incrementRegister(registers.hl, registers);

    }},
    {0x24, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc h
        incrementRegister(registers.h, registers);

    }},    // {0x25, "dec h"},
    {0x26, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.h.set(val);
    }},
    // {0x27, "daa"},
    {0x28, [](CpuRegisters& registers, MemoryUnit& memory) {
        // jr z,pc+r8
        int8_t e = static_cast<int8_t>(memory.read(registers.pc.value()));
        registers.pc.increment();
        if (registers.f.value() >> 7 | 0x01) {
            registers.pc.set(registers.pc.value() + e);
        }
    }},
    {0x29, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,hl
        registers.add_hl(registers.hl.value());
    }},
    {0x2a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,[hl+]
        uint8_t val = memory.read(registers.hl.value());
        registers.hl.increment();
        registers.a.set(val);
    }},
    // {0x2b, "dec hl"},
    {0x2c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc l
        incrementRegister(registers.l, registers);

    }},
    // {0x2d, "dec l"},
    {0x2e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.l.set(val);
    }},
    {0x2f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // cpl
        uint8_t val = registers.a.value() ^ 0xFF;
        registers.a.set(val);
        registers.set_hcarry_f();
        registers.set_sub_f();
    }},
    // {0x30, "jr nc,pc+r8"},
    {0x31, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld sp,d16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.sp.set(msb << 8 | lsb);
    }},
    {0x32, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl-],a
        memory.write(registers.hl.value(), registers.a.value());
        registers.hl.decrement();
    }},
    // {0x33, "inc sp"},
    // {0x34, "inc [hl]"},
    // {0x35, "dec [hl]"},
    {0x36, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], d8
        uint8_t d8 = memory.read(registers.pc.value());
        registers.pc.increment();
        memory.write(registers.hl.value(), d8);
    }},
    {0x37, [](CpuRegisters& registers, MemoryUnit& memory) {
        // scf
        registers.set_carry_f();
        registers.clear_sub_f();
        registers.clear_hcarry_f();
    }},
    // {0x38, "jr c,pc+r8"},
    {0x39, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,sp
        registers.add_hl(registers.sp.value());
    }},
    // {0x3a, "ld a,[hl-]"},
    // {0x3b, "dec sp"},
    {0x3c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc a
        incrementRegister(registers.a, registers);

    }},
    // {0x3d, "dec a"},
    {0x3e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,d8
        uint8_t val = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.a.set(val);
    }},
    {0x3f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ccf
        bool carry = registers.carry_f();
        carry ^= 1;
        (carry) ? registers.set_carry_f() : registers.clear_carry_f();
        registers.clear_sub_f();
        registers.clear_hcarry_f();
    }},
    {0x40, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, b 
        uint8_t val = registers.b.value();
        registers.b.set(val);
    }},
    {0x41, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, c
        load(registers.b, registers.c);
    }},
    {0x42, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, d
        load(registers.b, registers.d);
    }},
    {0x43, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, e
        load(registers.b, registers. e);
    }},
    {0x44, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, h
        load(registers.b, registers.h);
    }},
    {0x45, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, l
        load(registers.b, registers.l);
    }},
    {0x46, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.b.set(val);
    }},
    {0x47, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, a
        load(registers.b, registers.a);
    }},
    {0x48, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, b
        load(registers.c,registers.b);
    }},
    {0x49, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, c
        load(registers.c, registers.c);
    }},
    {0x4a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, d
        load(registers.c, registers.d);
    }},
    {0x4b, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, e
        load(registers.c, registers.e);
    }},
    {0x4c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, h
        load(registers.c,registers.h);
    }},
    {0x4d, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, l
        load(registers.c, registers.l);
    }},
    {0x4e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.c.set(val);
    }},
    {0x4f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld c, a
        load(registers.c, registers.a);
    }},
    {0x50, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, b
        load(registers.d,registers.b);
    }},
    {0x51, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, c
        load(registers.d, registers.c);
    }},
    {0x52, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, d
        load(registers.d, registers.d);
    }},
    {0x53, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, e
        load(registers.d, registers.e);
    }},
    {0x54, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, h
        load(registers.d,registers.h);
    }},
    {0x55, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, l
        load(registers.d, registers.l);
    }},
    {0x56, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.d.set(val);
    }},
    {0x57, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld d, a
        load(registers.d, registers.a);
    }},
    {0x58, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, b
        load(registers.e,registers.b);
    }},
    {0x59, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, c
        load(registers.e, registers.c);
    }},
    {0x5a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, d
        load(registers.e, registers.d);
    }},
    {0x5b, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, e
        load(registers.e, registers.e);
    }},
    {0x5c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, h
        load(registers.e,registers.h);
    }},
    {0x5d, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, l
        load(registers.e, registers.l);
    }},
    {0x5e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.e.set(val);
    }},
    {0x5f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld e, a
        load(registers.e, registers.a);
    }},
    {0x60, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, b
        load(registers.h,registers.b);
    }},
    {0x61, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, c
        load(registers.h, registers.c);
    }},
    {0x62, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, d
        load(registers.h, registers.d);
    }},
    {0x63, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, e
        load(registers.h, registers.e);
    }},
    {0x64, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, h
        load(registers.h,registers.h);
    }},
    {0x65, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, l
        load(registers.h, registers.l);
    }},
    {0x66, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.h.set(val);
    }},
    {0x67, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld h, a
        load(registers.h, registers.a);
    }},
    {0x68, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, b
        load(registers.l,registers.b);
    }},
    {0x69, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, c
        load(registers.l, registers.c);
    }},
    {0x6a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, d
        load(registers.l, registers.d);
    }},
    {0x6b, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, e
        load(registers.l, registers.e);
    }},
    {0x6c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, h
        load(registers.l,registers.h);
    }},
    {0x6d, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, l
        load(registers.l, registers.l);
    }},
    {0x6e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.l.set(val);
    }},
    {0x6f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld l, a
        load(registers.l, registers.a);
    }},
    {0x70, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], b
        memory.write(registers.hl.value(), registers.b.value());
    }},
    {0x71, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], c
        memory.write(registers.hl.value(), registers.c.value());
    }},
    {0x72, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], d
        memory.write(registers.hl.value(), registers.d.value());
    }},
    {0x73, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], e
        memory.write(registers.hl.value(), registers.e.value());
    }},
    {0x74, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], h
        memory.write(registers.hl.value(), registers.h.value());
    }},
    {0x75, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], l
        memory.write(registers.hl.value(), registers.l.value());
    }},
    // {0x76, "halt"},
    {0x77, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld [hl], a
        memory.write(registers.hl.value(), registers.a.value());
    }},
    {0x78, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, b
        load(registers.a,registers.b);
    }},
    {0x79, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, c
        load(registers.a, registers.c);
    }},
    {0x7a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, d
        load(registers.a, registers.d);
    }},
    {0x7b, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, e
        load(registers.a, registers.e);
    }},
    {0x7c, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, h
        load(registers.a,registers.h);
    }},
    {0x7d, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,l
        load(registers.a, registers.l);
    }},
    {0x7e, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,[hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.a.set(val);
    }},
    {0x7f, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a, a
        load(registers.a, registers.a);
    }},
    {0x80, [](CpuRegisters& registers, MemoryUnit& memory){
        // add b
        registers.add(registers.b);
    }},
    {0x81, [](CpuRegisters& registers, MemoryUnit& memory){
        // add c
        registers.add(registers.c);
    }},
    {0x82, [](CpuRegisters& registers, MemoryUnit& memory){
        // add d
        registers.add(registers.d);
    }},
    {0x83, [](CpuRegisters& registers, MemoryUnit& memory){
        // add e
        registers.add(registers.e);
    }},
    {0x84, [](CpuRegisters& registers, MemoryUnit& memory){
        // add h
        registers.add(registers.h);
    }},
    {0x85, [](CpuRegisters& registers, MemoryUnit& memory){
        // add l
        registers.add(registers.l);
    }},
    {0x86, [](CpuRegisters& registers, MemoryUnit& memory){
        // add [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.add(val);
    }},
    {0x87, [](CpuRegisters& registers, MemoryUnit& memory){
        // add a
        registers.add(registers.a);
    }},
    {0x88, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh b
        registers.add_carry(registers.b);
    }},
    {0x89, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh c
        registers.add_carry(registers.c);
    }},
    {0x8a, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh d
        registers.add_carry(registers.d);
    }},
    {0x8b, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh e
        registers.add_carry(registers.e);
    }},
    {0x8c, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh h
        registers.add_carry(registers.h);
    }},
    {0x8d, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh l
        registers.add_carry(registers.l);
    }},
    {0x8e, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.add_carry(val);
    }},
    {0x8f, [](CpuRegisters& registers, MemoryUnit& memory){
        // adh a
        registers.add_carry(registers.a);
    }}, 
    {0x90, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub b
        registers.sub(registers.b);
    }},
    {0x91, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub c
        registers.sub(registers.c);
    }},
    {0x92, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub d
        registers.sub(registers.d);
    }},
    {0x93, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub e
        registers.sub(registers.e);
    }},
    {0x94, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub h
        registers.sub(registers.h);
    }},
    {0x95, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub l
        registers.sub(registers.l);
    }},
    {0x96, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.sub(val);
    }},
    {0x97, [](CpuRegisters& registers, MemoryUnit& memory){
        // sub a
        registers.sub(registers.a);
    }},
    {0x98, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh b
        registers.sub_carry(registers.b);
    }},
    {0x99, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh c
        registers.sub_carry(registers.c);
    }},
    {0x9a, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh d
        registers.sub_carry(registers.d);
    }},
    {0x9b, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh e
        registers.sub_carry(registers.e);
    }},
    {0x9c, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh h
        registers.sub_carry(registers.h);
    }},
    {0x9d, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh l
        registers.sub_carry(registers.l);
    }},
    {0x9e, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.sub_carry(val);
    }},
    {0x9f, [](CpuRegisters& registers, MemoryUnit& memory){
        // sbh a
        registers.sub_carry(registers.a);
    }}, 
    {0xa0, [](CpuRegisters& registers, MemoryUnit& memory){
        // and b
        registers.and_instr(registers.b);
    }},
    {0xa1, [](CpuRegisters& registers, MemoryUnit& memory){
        // and c
        registers.and_instr(registers.c);
    }},
    {0xa2, [](CpuRegisters& registers, MemoryUnit& memory){
        // and d
        registers.and_instr(registers.d);
    }},
    {0xa3, [](CpuRegisters& registers, MemoryUnit& memory){
        // and e
        registers.and_instr(registers.e);
    }},
    {0xa4, [](CpuRegisters& registers, MemoryUnit& memory){
        // and h
        registers.and_instr(registers.h);
    }},
    {0xa5, [](CpuRegisters& registers, MemoryUnit& memory){
        // and l
        registers.and_instr(registers.l);
    }},
    {0xa6, [](CpuRegisters& registers, MemoryUnit& memory){
        // and [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.and_instr(val);
    }},
    {0xa7, [](CpuRegisters& registers, MemoryUnit& memory){
        // and a
        registers.and_instr(registers.a);
    }},
    {0xa8, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor b
        registers.xor_instr(registers.b);
    }},
    {0xa9, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor c
        registers.xor_instr(registers.c);
    }},
    {0xaa, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor d
        registers.xor_instr(registers.d);
    }},
    {0xab, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor e
        registers.xor_instr(registers.e);
    }},
    {0xac, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor h
        registers.xor_instr(registers.h);
    }},
    {0xad, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor l
        registers.xor_instr(registers.l);
    }},
    {0xae, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.xor_instr(val);
    }},
    {0xaf, [](CpuRegisters& registers, MemoryUnit& memory){
        // xor a
        registers.xor_instr(registers.a);
    }},
    {0xb0, [](CpuRegisters& registers, MemoryUnit& memory){
        // or b
        registers.or_instr(registers.b);
    }},
    {0xb1, [](CpuRegisters& registers, MemoryUnit& memory){
        // or c
        registers.or_instr(registers.c);
    }},
    {0xb2, [](CpuRegisters& registers, MemoryUnit& memory){
        // or d
        registers.or_instr(registers.d);
    }},
    {0xb3, [](CpuRegisters& registers, MemoryUnit& memory){
        // or e
        registers.or_instr(registers.e);
    }},
    {0xb4, [](CpuRegisters& registers, MemoryUnit& memory){
        // or h
        registers.or_instr(registers.h);
    }},
    {0xb5, [](CpuRegisters& registers, MemoryUnit& memory){
        // or l
        registers.or_instr(registers.l);
    }},
    {0xb6, [](CpuRegisters& registers, MemoryUnit& memory){
        // or [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.or_instr(val);
    }},
    {0xb7, [](CpuRegisters& registers, MemoryUnit& memory){
        // or a
        registers.or_instr(registers.a);
    }},
    {0xb8, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp b
        registers.compare(registers.b);
    }},
    {0xb9, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp c
        registers.compare(registers.c);
    }},
    {0xba, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp d
        registers.compare(registers.d);
    }},
    {0xbb, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp e
        registers.compare(registers.e);
    }},
    {0xbc, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp h
        registers.compare(registers.h);
    }},
    {0xbd, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp l
        registers.compare(registers.l);
    }},
    {0xbe, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.compare(val);
    }},
    {0xbf, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp a
        registers.compare(registers.a);
    }},
    {0xc0, [](CpuRegisters& registers, MemoryUnit& memory){
        // ret nz
        if (!registers.zero_f()) {
            uint8_t lsb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint8_t msb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint16_t nn = (msb << 8) | lsb;
            registers.pc.set(nn);
        }
    }},
    // {0xc1, "pop bc"},
    {0xc1, [](CpuRegisters& registers, MemoryUnit& memory){
        // pop bc
        pop(registers.sp, registers.bc, memory);
    }},
    // {0xc2, "jp nz,a16"},
    {0xc3, [](CpuRegisters& registers, MemoryUnit& memory){
        // jp a16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint16_t nn = (msb << 8) | lsb;
        registers.pc.set(nn);
    }},
    // {0xc4, "call nz,a16"},
    {0xc5, [](CpuRegisters& registers, MemoryUnit& memory){
        // push bc
        push(registers.sp, registers.b, registers.c, memory);
    }},
    // {0xc6, "add d8"},
    // {0xc7, "rst $00"},
    {0xc8, [](CpuRegisters& registers, MemoryUnit& memory){
        // ret z
        if (registers.zero_f()) {
            uint8_t lsb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint8_t msb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint16_t nn = (msb << 8) | lsb;
            registers.pc.set(nn);
        }
    }},
    {0xc9, [](CpuRegisters& registers, MemoryUnit& memory){
        // ret
        uint8_t lsb = memory.read(registers.sp.value());
        registers.sp.increment();
        uint8_t msb = memory.read(registers.sp.value());
        registers.sp.increment();
        uint16_t nn = (msb << 8) | lsb;
        registers.pc.set(nn);
    }},
    // {0xca, "jp z,a16"},
    {0xcd, [](CpuRegisters& registers, MemoryUnit& memory) {
        // CBPREFIX
        // TODO: Implement

        return;
    }},
    // {0xcc, "call z,a16"},
    {0xcd, [](CpuRegisters& registers, MemoryUnit& memory) {

        // call nn 
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.sp.decrement();
        memory.write(registers.sp.value(), registers.pc.value() >> 8);
        registers.sp.decrement();
        memory.write(registers.sp.value(), registers.pc.value() & 0xFF);
        uint16_t nn = (msb << 8) | lsb;
        registers.pc.set(nn);
    }},
    // {0xce, "adc d8"},
    // {0xcf, "rst $08"},
    {0xd0, [](CpuRegisters& registers, MemoryUnit& memory){
        // ret nc
        if (!registers.carry_f()) {
            uint8_t lsb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint8_t msb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint16_t nn = (msb << 8) | lsb;
            registers.pc.set(nn);
        }
    }},
    // {0xd1, "pop de"},
    // {0xd2, "jp nc,a16"},
    // {0xd3, "db $d3"},
    // {0xd4, "call nc,a16"},
    {0xd5, [](CpuRegisters& registers, MemoryUnit& memory){
        // push de
        push(registers.sp, registers.d, registers.e, memory);
    }},
    // {0xd6, "sub d8"},
    // {0xd7, "rst $10"},
    {0xd8, [](CpuRegisters& registers, MemoryUnit& memory){
        // ret c
        if (registers.carry_f()) {
            uint8_t lsb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint8_t msb = memory.read(registers.sp.value());
            registers.sp.increment();
            uint16_t nn = (msb << 8) | lsb;
            registers.pc.set(nn);
        }
    }},
    // {0xd9, "reti"},
    // {0xda, "jp c,a16"},
    // {0xdb, "db $db"},
    // {0xdc, "call c,a16"},
    // {0xdd, "db $dd"},
    // {0xde, "sbc d8"},
    // {0xdf, "rst $18"},
    {0xe0, [](CpuRegisters& registers, MemoryUnit& memory){
        // ldh [a8],a
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint16_t address = (0xFF << 8) | lsb;
        memory.write(address, registers.a.value());

    }},
    {0xe1, [](CpuRegisters& registers, MemoryUnit& memory){
        // pop hl
        pop(registers.sp, registers.hl, memory);

    }},
    {0xe2, [](CpuRegisters& registers, MemoryUnit& memory){
        // ld [c],a
        uint8_t lsb = registers.c.value();
        uint16_t addr = (0xFF << 8) | lsb;
        memory.write(addr, registers.a.value());
    }},
    // {0xe3, "db $e3"},
    // {0xe4, "db $e4"},
    {0xe5, [](CpuRegisters& registers, MemoryUnit& memory){
        // push hl
        push(registers.sp, registers.h, registers.l, memory);
        
    }},
    {0xe6, [](CpuRegisters& registers, MemoryUnit& memory){
        // and d8
        uint8_t d8 = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.or_instr(d8); 
    }},
    // {0xe7, "rst $20"},
    // {0xe8, "add sp,r8"},
    // {0xe9, "jp hl"},
    {0xea, [](CpuRegisters& registers, MemoryUnit& memory){
        // ld [a16],a
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint16_t addr = (msb << 8) | lsb;
        memory.write(addr, registers.a.value());
    }},
    // {0xeb, "db $eb"},
    // {0xec, "db $ec"},
    // {0xed, "db $ed"},
    // {0xee, "xor d8"},
    // {0xef, "rst $28"},

    // {0xf0, "ldh a,[a8]"},
    {0xf0, [](CpuRegisters& registers, MemoryUnit& memory){
        // ldh a,[a8]
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint16_t nn = (0xFF << 8) | lsb;
        uint8_t val = memory.read(nn);
        registers.a.set(val);
    }},
    // {0xf1, "pop af"},
    // {0xf2, "ld a,[c]"},
    {0xf3, [](CpuRegisters& registers, MemoryUnit& memory){
        // di
        memory.ime = 0;

    }},
    // {0xf4, "db $f4"},
    {0xf5, [](CpuRegisters& registers, MemoryUnit& memory){
        // push af
        push(registers.sp, registers.a, registers.f, memory);

    }},
    // {0xf6, "or d8"},
    // {0xf7, "rst $30"},
    // {0xf8, "ld hl,sp+r8"},
    // {0xf9, "ld sp,hl"},
    // {0xfa, "ld a,[a16]"},
    // {0xf3, "ei"},
    {0xfb, [](CpuRegisters& registers, MemoryUnit& memory){
        // ei
        memory.ime = 1;

    }},
    // {0xfc, "db $fc"},
    // {0xfd, "db $fd"},
    {0xfe, [](CpuRegisters& registers, MemoryUnit& memory){
        // cp d8 
        uint8_t d8{memory.read(registers.pc.value())};
        registers.pc.increment();

        uint8_t a{registers.a.value()};
        uint8_t result = registers.a.value() - d8;
        bool carry = calc_carry(a, d8);
        bool half_carry = calc_half_carry(a, d8);

        (result == 0) ? registers.set_zero_f() : registers.clear_zero_f();
        registers.set_sub_f();
        (carry) ? registers.set_carry_f() : registers.clear_carry_f();
        (half_carry) ? registers.set_hcarry_f() : registers.clear_hcarry_f();

    }},
    // {0xff, "rst $38"}
    }
};
