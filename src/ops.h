#pragma once

#include <unordered_map>
#include <functional>
#include "test.h"
#include "memory_unit.h"

static std::unordered_map<uint8_t, std::string> assembly_string_map{
    {
    {0x00, "nop"},
    {0x01, "ld bc,d16"},
    {0x02, "ld [bc],a"},
    {0x03, "inc bc"},
    {0x04, "inc b"},
    {0x05, "dec b"},
    {0x06, "ld b,d8"},
    {0x07, "rlca"},
    {0x08, "ld [a16],sp"},
    {0x09, "add hl,bc"},
    {0x0a, "ld a,[bc]"},
    {0x0b, "dec bc"},
    {0x0c, "inc c"},
    {0x0d, "dec c"},
    {0x0e, "ld c,d8"},
    {0x0f, "rrca"},

    {0x10, "stop"},
    {0x11, "ld de,d16"},
    {0x12, "ld [de],a"},
    {0x13, "inc de"},
    {0x14, "inc d"},
    {0x15, "dec d"},
    {0x16, "ld d,d8"},
    {0x17, "rla"},
    {0x18, "jr pc+r8"},
    {0x19, "add hl,de"},
    {0x1a, "ld a,[de]"},
    {0x1b, "dec de"},
    {0x1c, "inc e"},
    {0x1d, "dec e"},
    {0x1e, "ld e,d8"},
    {0x1f, "rra"},

    {0x20, "jr nz,pc+r8"},
    {0x21, "ld hl,d16"},
    {0x22, "ld [hl+],a"},
    {0x23, "inc hl"},
    {0x24, "inc h"},
    {0x25, "dec h"},
    {0x26, "ld h,d8"},
    {0x27, "daa"},
    {0x28, "jr z,pc+r8"},
    {0x29, "add hl,hl"},
    {0x2a, "ld a,[hl+]"},
    {0x2b, "dec hl"},
    {0x2c, "inc l"},
    {0x2d, "dec l"},
    {0x2e, "ld l,d8"},
    {0x2f, "cpl"},

    {0x30, "jr nc,pc+r8"},
    {0x31, "ld sp,d16"},
    {0x32, "ld [hl-],a"},
    {0x33, "inc sp"},
    {0x34, "inc [hl]"},
    {0x35, "dec [hl]"},
    {0x36, "ld [hl],d8"},
    {0x37, "scf"},
    {0x38, "jr c,pc+r8"},
    {0x39, "add hl,sp"},
    {0x3a, "ld a,[hl-]"},
    {0x3b, "dec sp"},
    {0x3c, "inc a"},
    {0x3d, "dec a"},
    {0x3e, "ld a,d8"},
    {0x3f, "ccf"},

    {0x40, "ld b,b"},
    {0x41, "ld b,c"},
    {0x42, "ld b,d"},
    {0x43, "ld b,e"},
    {0x44, "ld b,h"},
    {0x45, "ld b,l"},
    {0x46, "ld b,[hl]"},
    {0x47, "ld b,a"},
    {0x48, "ld c,b"},
    {0x49, "ld c,c"},
    {0x4a, "ld c,d"},
    {0x4b, "ld c,e"},
    {0x4c, "ld c,h"},
    {0x4d, "ld c,l"},
    {0x4e, "ld c,[hl]"},
    {0x4f, "ld c,a"},

    {0x50, "ld d,b"},
    {0x51, "ld d,c"},
    {0x52, "ld d,d"},
    {0x53, "ld d,e"},
    {0x54, "ld d,h"},
    {0x55, "ld d,l"},
    {0x56, "ld d,[hl]"},
    {0x57, "ld d,a"},
    {0x58, "ld e,b"},
    {0x59, "ld e,c"},
    {0x5a, "ld e,d"},
    {0x5b, "ld e,e"},
    {0x5c, "ld e,h"},
    {0x5d, "ld e,l"},
    {0x5e, "ld e,[hl]"},
    {0x5f, "ld e,a"},

    {0x60, "ld h,b"},
    {0x61, "ld h,c"},
    {0x62, "ld h,d"},
    {0x63, "ld h,e"},
    {0x64, "ld h,h"},
    {0x65, "ld h,l"},
    {0x66, "ld h,[hl]"},
    {0x67, "ld h,a"},
    {0x68, "ld l,b"},
    {0x69, "ld l,c"},
    {0x6a, "ld l,d"},
    {0x6b, "ld l,e"},
    {0x6c, "ld l,h"},
    {0x6d, "ld l,l"},
    {0x6e, "ld l,[hl]"},
    {0x6f, "ld l,a"},

    {0x70, "ld [hl],b"},
    {0x71, "ld [hl],c"},
    {0x72, "ld [hl],d"},
    {0x73, "ld [hl],e"},
    {0x74, "ld [hl],h"},
    {0x75, "ld [hl],l"},
    {0x76, "halt"},
    {0x77, "ld [hl],a"},
    {0x78, "ld a,b"},
    {0x79, "ld a,c"},
    {0x7a, "ld a,d"},
    {0x7b, "ld a,e"},
    {0x7c, "ld a,h"},
    {0x7d, "ld a,l"},
    {0x7e, "ld a,[hl]"},
    {0x7f, "ld a,a"},

    {0x80, "add b"},
    {0x81, "add c"},
    {0x82, "add d"},
    {0x83, "add e"},
    {0x84, "add h"},
    {0x85, "add l"},
    {0x86, "add [hl]"},
    {0x87, "add a"},
    {0x88, "adc b"},
    {0x89, "adc c"},
    {0x8a, "adc d"},
    {0x8b, "adc e"},
    {0x8c, "adc h"},
    {0x8d, "adc l"},
    {0x8e, "adc [hl]"},
    {0x8f, "adc a"},    

    {0x90, "sub b"},
    {0x91, "sub c"},
    {0x92, "sub d"},
    {0x93, "sub e"},
    {0x94, "sub h"},
    {0x95, "sub l"},
    {0x96, "sub [hl]"},
    {0x97, "sub a"},
    {0x98, "sbc b"},
    {0x99, "sbc c"},
    {0x9a, "sbc d"},
    {0x9b, "sbc e"},
    {0x9c, "sbc h"},
    {0x9d, "sbc l"},
    {0x9e, "sbc [hl]"},
    {0x9f, "sbc a"},

    {0xa0, "and b"},
    {0xa1, "and c"},
    {0xa2, "and d"},
    {0xa3, "and e"},
    {0xa4, "and h"},
    {0xa5, "and l"},
    {0xa6, "and [hl]"},
    {0xa7, "and a"},
    {0xa8, "xor b"},
    {0xa9, "xor c"},
    {0xaa, "xor d"},
    {0xab, "xor e"},
    {0xac, "xor h"},
    {0xad, "xor l"},
    {0xae, "xor [hl]"},
    {0xaf, "xor a"},

    {0xb0, "or b"},
    {0xb1, "or c"},
    {0xb2, "or d"},
    {0xb3, "or e"},
    {0xb4, "or h"},
    {0xb5, "or l"},
    {0xb6, "or [hl]"},
    {0xb7, "or a"},
    {0xb8, "cp b"},
    {0xb9, "cp c"},
    {0xba, "cp d"},
    {0xbb, "cp e"},
    {0xbc, "cp h"},
    {0xbd, "cp l"},
    {0xbe, "cp [hl]"},
    {0xbf, "cp a"},

    {0xc0, "ret nz"},
    {0xc1, "pop bc"},
    {0xc2, "jp nz,a16"},
    {0xc3, "jp a16"},
    {0xc4, "call nz,a16"},
    {0xc5, "push bc"},
    {0xc6, "add d8"},
    {0xc7, "rst $00"},
    {0xc8, "ret z"},
    {0xc9, "ret"},
    {0xca, "jp z,a16"},
    {0xcb, "CBPREFIX"},
    {0xcc, "call z,a16"},
    {0xcd, "call a16"},
    {0xce, "adc d8"},
    {0xcf, "rst $08"},

    {0xd0, "ret nc"},
    {0xd1, "pop de"},
    {0xd2, "jp nc,a16"},
    {0xd3, "db $d3"},
    {0xd4, "call nc,a16"},
    {0xd5, "push de"},
    {0xd6, "sub d8"},
    {0xd7, "rst $10"},
    {0xd8, "ret c"},
    {0xd9, "reti"},
    {0xda, "jp c,a16"},
    {0xdb, "db $db"},
    {0xdc, "call c,a16"},
    {0xdd, "db $dd"},
    {0xde, "sbc d8"},
    {0xdf, "rst $18"},

    {0xe0, "ldh [a8],a"},
    {0xe1, "pop hl"},
    {0xe2, "ld [c],a"},
    {0xe3, "db $e3"},
    {0xe4, "db $e4"},
    {0xe5, "push hl"},
    {0xe6, "and d8"},
    {0xe7, "rst $20"},
    {0xe8, "add sp,r8"},
    {0xe9, "jp hl"},
    {0xea, "ld [a16],a"},
    {0xeb, "db $eb"},
    {0xec, "db $ec"},
    {0xed, "db $ed"},
    {0xee, "xor d8"},
    {0xef, "rst $28"},

    {0xf0, "ldh a,[a8]"},
    {0xf1, "pop af"},
    {0xf2, "ld a,[c]"},
    {0xf3, "di"},
    {0xf4, "db $f4"},
    {0xf5, "push af"},
    {0xf6, "or d8"},
    {0xf7, "rst $30"},
    {0xf8, "ld hl,sp+r8"},
    {0xf9, "ld sp,hl"},
    {0xfa, "ld a,[a16]"},
    {0xfb, "ei"},
    {0xfc, "db $fc"},
    {0xfd, "db $fd"},
    {0xfe, "cp d8"},
    {0xff, "rst $38"}
    }
};

using cpuFunc = void (*)(CpuRegisters&, MemoryUnit&);

void test(CpuRegisters& registers, uint8_t* rom) {
    uint8_t lsb = rom[registers.pc.value()];
    registers.pc.set(registers.pc.value() +1);
    uint8_t msb = rom[registers.pc.value()];
    registers.pc.set(registers.pc.value() +1);
    registers.bc.set(msb << 8 | lsb);
}

inline void incrementRegister(Register& r) {
    r.increment();
}

inline void incrementRegister(DualRegister& r) {
    r.increment();
}

inline void decrementRegister(Register& r) {
    r.decrement();
}

inline void decrementRegister(DualRegister& r) {
    r.decrement();
}

inline void push(Counter& sp, Register& hi, Register& lo, MemoryUnit& mem) {
    sp.decrement();
    mem.write(sp.value(), hi.value());
    sp.decrement();
    mem.write(sp.value(), lo.value());
}

inline void pop(Counter& sp, DualRegister& target, MemoryUnit& mem) {
    uint8_t lsb = mem.read(sp.value());
    sp.increment();
    uint8_t msb = mem.read(sp.value());
    sp.increment();
    uint16_t nn = (msb << 8) | lsb;
    target.set(nn);
}

inline void addDualRegisters(DualRegister& first, DualRegister& second) {
    // Value gets stored in first register
    uint16_t value = first.value() + second.value();
    first.set(value);
}

static std::unordered_map<uint8_t, cpuFunc> instruction_map{
    {
    {0x00, [](CpuRegisters& registers, MemoryUnit& memory) {return;}},
    {0x01, [](CpuRegisters& registers, MemoryUnit& memory) {

        // ld bc,d16
        uint8_t lsb = memory.read(registers.pc.value());
        registers.pc.increment();
        uint8_t msb = memory.read(registers.pc.value());
        registers.pc.increment();
        registers.bc.set(msb << 8 | lsb);
    }},
    // {0x02, "ld [bc],a"},
    // {0x03, "inc bc"},
    {0x03, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc bc
        incrementRegister(registers.bc);
    }},    
    {0x04, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc b
        incrementRegister(registers.b);
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
    // {0x08, "ld [a16],sp"},
    {0x09, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,bc
        addDualRegisters(registers.hl, registers.bc);
    }},
    // {0x0a, "ld a,[bc]"},
    // {0x0b, "dec bc"},
    // {0x0c, "inc c"},
    // {0x0d, "dec c"},
    // {0x0e, "ld c,d8"},
    // {0x0f, "rrca"},

    {0x10, [](CpuRegisters& registers, MemoryUnit& memory) {
        // STOP - IME = 0 ???
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
        incrementRegister(registers.de);

    }},
    {0x14, [](CpuRegisters& registers, MemoryUnit& memory) {
        // inc d
        incrementRegister(registers.d);

    }},
    // {0x15, "dec d"},
    // {0x16, "ld d,d8"},
    // {0x17, "rla"},
    // {0x18, "jr pc+r8"},
    // {0x19, "add hl,de"},
    {0x19, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,de
        addDualRegisters(registers.hl, registers.de);
    }},
    {0x1a, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld a,[de]
        registers.a.set(memory.read(registers.de.value()));

    }},
    // {0x1b, "dec de"},
    // {0x1c, "inc e"},
    // {0x1d, "dec e"},
    // {0x1e, "ld e,d8"},
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
    // {0x23, "inc hl"},
    // {0x24, "inc h"},
    // {0x25, "dec h"},
    // {0x26, "ld h,d8"},
    // {0x27, "daa"},
    // {0x28, "jr z,pc+r8"},
    // {0x29, "add hl,hl"},
    {0x29, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,hl
        addDualRegisters(registers.hl, registers.hl);
    }},
    // {0x2a, "ld a,[hl+]"},
    // {0x2b, "dec hl"},
    // {0x2c, "inc l"},
    // {0x2d, "dec l"},
    // {0x2e, "ld l,d8"},
    // {0x2f, "cpl"},

    // {0x30, "jr nc,pc+r8"},
    // {0x31, "ld sp,d16"},
    // {0x32, "ld [hl-],a"},
    // {0x33, "inc sp"},
    // {0x34, "inc [hl]"},
    // {0x35, "dec [hl]"},
    // {0x36, "ld [hl],d8"},
    // {0x37, "scf"},
    // {0x38, "jr c,pc+r8"},
    // {0x39, "add hl,sp"},
    {0x39, [](CpuRegisters& registers, MemoryUnit& memory) {
        // add hl,sp
        // TODO:
        // addDualRegisters(registers.hl, registers.sp);
    }},
    // {0x3a, "ld a,[hl-]"},
    // {0x3b, "dec sp"},
    // {0x3c, "inc a"},
    // {0x3d, "dec a"},
    // {0x3e, "ld a,d8"},
    // {0x3f, "ccf"},

    {0x40, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, b 
        uint8_t val = registers.b.value();
        registers.b.set(val);
    }},
    {0x41, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, c
        uint8_t val = registers.c.value();
        registers.b.set(val);
    }},
    {0x42, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, d
        uint8_t val = registers.d.value();
        registers.b.set(val);
    }},
    {0x43, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, e
        uint8_t val = registers.e.value();
        registers.b.set(val);
    }},
    {0x44, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, h
        uint8_t val = registers.h.value();
        registers.b.set(val);
    }},
    {0x45, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, l
        uint8_t val = registers.l.value();
        registers.b.set(val);
    }},
    {0x46, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, [hl]
        uint8_t val = memory.read(registers.hl.value());
        registers.b.set(val);
    }},
    {0x47, [](CpuRegisters& registers, MemoryUnit& memory) {
        // ld b, a
        uint8_t val = registers.a.value();
        registers.b.set(val);
    }},
    // {0x48, "ld c,b"},
    // {0x49, "ld c,c"},
    // {0x4a, "ld c,d"},
    // {0x4b, "ld c,e"},
    // {0x4c, "ld c,h"},
    // {0x4d, "ld c,l"},
    // {0x4e, "ld c,[hl]"},
    // {0x4f, "ld c,a"},

    // {0x50, "ld d,b"},
    // {0x51, "ld d,c"},
    // {0x52, "ld d,d"},
    // {0x53, "ld d,e"},
    // {0x54, "ld d,h"},
    // {0x55, "ld d,l"},
    // {0x56, "ld d,[hl]"},
    // {0x57, "ld d,a"},
    // {0x58, "ld e,b"},
    // {0x59, "ld e,c"},
    // {0x5a, "ld e,d"},
    // {0x5b, "ld e,e"},
    // {0x5c, "ld e,h"},
    // {0x5d, "ld e,l"},
    // {0x5e, "ld e,[hl]"},
    // {0x5f, "ld e,a"},

    // {0x60, "ld h,b"},
    // {0x61, "ld h,c"},
    // {0x62, "ld h,d"},
    // {0x63, "ld h,e"},
    // {0x64, "ld h,h"},
    // {0x65, "ld h,l"},
    // {0x66, "ld h,[hl]"},
    // {0x67, "ld h,a"},
    // {0x68, "ld l,b"},
    // {0x69, "ld l,c"},
    // {0x6a, "ld l,d"},
    // {0x6b, "ld l,e"},
    // {0x6c, "ld l,h"},
    // {0x6d, "ld l,l"},
    // {0x6e, "ld l,[hl]"},
    // {0x6f, "ld l,a"},

    // {0x70, "ld [hl],b"},
    // {0x71, "ld [hl],c"},
    // {0x72, "ld [hl],d"},
    // {0x73, "ld [hl],e"},
    // {0x74, "ld [hl],h"},
    // {0x75, "ld [hl],l"},
    // {0x76, "halt"},
    // {0x77, "ld [hl],a"},
    // {0x78, "ld a,b"},
    // {0x79, "ld a,c"},
    // {0x7a, "ld a,d"},
    // {0x7b, "ld a,e"},
    // {0x7c, "ld a,h"},
    // {0x7d, "ld a,l"},
    // {0x7e, "ld a,[hl]"},
    // {0x7f, "ld a,a"},

    // {0x80, "add b"},
    // {0x81, "add c"},
    // {0x82, "add d"},
    // {0x83, "add e"},
    // {0x84, "add h"},
    // {0x85, "add l"},
    // {0x86, "add [hl]"},
    // {0x87, "add a"},
    // {0x88, "adc b"},
    // {0x89, "adc c"},
    // {0x8a, "adc d"},
    // {0x8b, "adc e"},
    // {0x8c, "adc h"},
    // {0x8d, "adc l"},
    // {0x8e, "adc [hl]"},
    // {0x8f, "adc a"},    

    // {0x90, "sub b"},
    // {0x91, "sub c"},
    // {0x92, "sub d"},
    // {0x93, "sub e"},
    // {0x94, "sub h"},
    // {0x95, "sub l"},
    // {0x96, "sub [hl]"},
    // {0x97, "sub a"},
    // {0x98, "sbc b"},
    // {0x99, "sbc c"},
    // {0x9a, "sbc d"},
    // {0x9b, "sbc e"},
    // {0x9c, "sbc h"},
    // {0x9d, "sbc l"},
    // {0x9e, "sbc [hl]"},
    // {0x9f, "sbc a"},

    // {0xa0, "and b"},
    // {0xa1, "and c"},
    // {0xa2, "and d"},
    // {0xa3, "and e"},
    // {0xa4, "and h"},
    // {0xa5, "and l"},
    // {0xa6, "and [hl]"},
    // {0xa7, "and a"},
    // {0xa8, "xor b"},
    // {0xa9, "xor c"},
    // {0xaa, "xor d"},
    // {0xab, "xor e"},
    // {0xac, "xor h"},
    // {0xad, "xor l"},
    // {0xae, "xor [hl]"},
    // {0xaf, "xor a"},

    // {0xb0, "or b"},
    // {0xb1, "or c"},
    // {0xb2, "or d"},
    // {0xb3, "or e"},
    // {0xb4, "or h"},
    // {0xb5, "or l"},
    // {0xb6, "or [hl]"},
    // {0xb7, "or a"},
    // {0xb8, "cp b"},
    // {0xb9, "cp c"},
    // {0xba, "cp d"},
    // {0xbb, "cp e"},
    // {0xbc, "cp h"},
    // {0xbd, "cp l"},
    // {0xbe, "cp [hl]"},
    // {0xbf, "cp a"},

    // {0xc0, "ret nz"},
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
    // {0xc8, "ret z"},
    // {0xc9, "ret"},
    // {0xca, "jp z,a16"},
    // {0xcb, "CBPREFIX"},
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

    // {0xd0, "ret nc"},
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
    // {0xd8, "ret c"},
    // {0xd9, "reti"},
    // {0xda, "jp c,a16"},
    // {0xdb, "db $db"},
    // {0xdc, "call c,a16"},
    // {0xdd, "db $dd"},
    // {0xde, "sbc d8"},
    // {0xdf, "rst $18"},

    // {0xe0, "ldh [a8],a"},
    // {0xe1, "pop hl"},
    {0xe1, [](CpuRegisters& registers, MemoryUnit& memory){
        // pop hl
        pop(registers.sp, registers.hl, memory);

    }},
    // {0xe2, "ld [c],a"},
    // {0xe3, "db $e3"},
    // {0xe4, "db $e4"},
    {0xe5, [](CpuRegisters& registers, MemoryUnit& memory){
        // push hl
        push(registers.sp, registers.h, registers.l, memory);
        
    }},
    // {0xe6, "and d8"},
    // {0xe7, "rst $20"},
    // {0xe8, "add sp,r8"},
    // {0xe9, "jp hl"},
    // {0xea, "ld [a16],a"},
    // {0xeb, "db $eb"},
    // {0xec, "db $ec"},
    // {0xed, "db $ed"},
    // {0xee, "xor d8"},
    // {0xef, "rst $28"},

    // {0xf0, "ldh a,[a8]"},
    // {0xf1, "pop af"},
    // {0xf2, "ld a,[c]"},
    // {0xf3, "di"},
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
    // {0xfb, "ei"},
    // {0xfc, "db $fc"},
    // {0xfd, "db $fd"},
    // {0xfe, "cp d8"},
    // {0xff, "rst $38"}
    }
};