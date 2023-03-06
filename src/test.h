#pragma once

#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

struct Flags {
  bool zero, sub, half_carry, carry;
};

class Register {
private:
  uint8_t value_ = 0x0;

public:
  Register() = default;
  ~Register() = default;
  Register(const Register &) = delete;
  Register &operator=(const Register &) = delete;

  void set(uint8_t input) { value_ = input; }

  uint8_t value() const { return value_; };

  void increment() { value_++; }

  void decrement() { value_--; }

  friend std::ostream &operator<<(std::ostream &out, Register const &data) {
    out << +data.value();
    return out;
  }
};

class DualRegister {

private:
  Register &hi;
  Register &lo;

public:
  DualRegister(Register &hi, Register &lo) : hi{hi}, lo{lo} {};
  DualRegister() = delete;
  ~DualRegister() = default;
  DualRegister(const DualRegister &) = delete;
  DualRegister &operator=(const DualRegister &) = delete;

  void set(uint16_t input) {
    hi.set(input >> 8);
    lo.set(input & 0xFF);
  }

  void increment() { set(value() + 1); }

  void decrement() { set(value() - 1); }

  uint16_t value() const { return (hi.value() << 8) | lo.value(); };

  friend std::ostream &operator<<(std::ostream &out, DualRegister const &data) {
    out << +data.value();
    return out;
  }
};

class Counter {
protected:
  uint16_t value_ = 0x0;

private:
public:
  Counter() = default;
  ~Counter() = default;
  Counter(const Counter &) = delete;
  Counter &operator=(const Counter &) = delete;

  void set(uint16_t input) { value_ = input; }

  void increment() { value_++; }

  void decrement() { value_--; }

  uint16_t value() const { return value_; };

  friend std::ostream &operator<<(std::ostream &out, Counter const &data) {
    out << +data.value();
    return out;
  }
};

class CpuRegisters {
public:
  Register a, b, c, d, e, f, h, l;
  DualRegister af, bc, de, hl;
  Counter sp, pc;

  CpuRegisters()
      : a{}, b{}, c{}, d{}, e{}, f{}, h{}, l{}, af{a, f}, bc{b, c}, de{d, e},
        hl{h, l}, sp{}, pc{} {};

  bool zero_f() const { return (f.value() >> 7) & 1; }
  bool sub_f() const { return (f.value() >> 6) & 1; }
  bool hcarry_f() const { return (f.value() >> 5) & 1; }
  bool carry_f() const { return (f.value() >> 4) & 1; }

  void set_zero_f() {
    uint8_t new_val = (1 << 7) | f.value();
    f.set(new_val);
  }
  void set_sub_f() {
    uint8_t new_val = (1 << 6) | f.value();
    f.set(new_val);
  }
  void set_hcarry_f() {
    uint8_t new_val = (1 << 5) | f.value();
    f.set(new_val);
  }
  void set_carry_f() {
    uint8_t new_val = (1 << 4) | f.value();
    f.set(new_val);
  }

  void clear_zero_f() {
    uint8_t new_val = ~(1 << 7) & f.value();
    f.set(new_val);
  }
  void clear_sub_f() {
    uint8_t new_val = ~(1 << 6) & f.value();
    f.set(new_val);
  }
  void clear_hcarry_f() {
    uint8_t new_val = ~(1 << 5) & f.value();
    f.set(new_val);
  }
  void clear_carry_f() {
    uint8_t new_val = ~(1 << 4) & f.value();
    f.set(new_val);
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

  void add(uint8_t n);
  void add(Register &r);
  void add_carry(uint8_t n);
  void add_carry(Register &r);

  void add_hl(uint16_t r_val);

  void sub(uint8_t n);
  void sub(Register &r);
  void sub_carry(uint8_t n);
  void sub_carry(Register &r);

  void and_instr(uint8_t n);
  void and_instr(Register &r);
  void or_instr(uint8_t n);
  void or_instr(Register &r);
  void xor_instr(uint8_t n);
  void xor_instr(Register &r);
  void compare(uint8_t n);
  void compare(Register &r);

  friend std::ostream &operator<<(std::ostream &out,
                                  CpuRegisters const &registers) {
    out << "a: " << +registers.a.value() << " f: " << +registers.f.value()
        << "\n";
    out << "b: " << +registers.b.value() << " c: " << +registers.c.value()
        << "\n";
    out << "d: " << +registers.d.value() << " e: " << +registers.e.value()
        << "\n";
    out << "h: " << +registers.h.value() << " l: " << +registers.l.value()
        << "\n";
    out << "sp:" << +registers.sp.value() << "\n";
    out << "pc:" << +registers.pc.value() << "\n";
    out << "Flags are zero: " << registers.zero_f();
    out << " sub: " << registers.sub_f();
    out << " half_carry: " << registers.hcarry_f();
    out << " carry: " << registers.carry_f() << "\n";

    return out;
  }
};