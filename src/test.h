#pragma once

#include <iostream>
#include <fstream>
#include <vector>
#include <memory>
#include "ops.h"

class Register {
  private:
    uint8_t value_ = 0x0;


  public:
    Register() = default;
    ~Register() = default;
    Register(const Register&) = delete;
    Register& operator=(const Register&) = delete;

    void set(uint8_t input) {
      value_ = input;
    }

    uint8_t value() const {
      return value_;
    };

    void increment() {
        value_++;
    }

    void decrement() {
        value_--;
    }

    friend std::ostream& operator<< (std::ostream &out, Register const& data) {
      out << +data.value();
      return out;
    }
};

class DualRegister {

  private:
    Register& hi;
    Register& lo;

  public:
    DualRegister(Register& hi, Register& lo) : hi{hi}, lo{lo} {};
    DualRegister() = delete;
    ~DualRegister() = default;
    DualRegister(const DualRegister&) = delete;
    DualRegister& operator=(const DualRegister&) = delete;

    void set(uint16_t input) {
      hi.set(input >> 8);
      lo.set(input & 0xFF);
    }

    void increment() {
      set(value() + 1);
    }

    void decrement() {
      set(value() - 1);
    }

    uint16_t value() const {
      return (hi.value() << 8) | lo.value();
    };

    friend std::ostream& operator<< (std::ostream &out, DualRegister const& data) {
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
    Counter(const Counter&) = delete;
    Counter& operator=(const Counter&) = delete;

    void set(uint16_t input) {
      value_ = input;
    }

    void increment() {
        value_++;
    }

    void decrement() {
        value_--;
    }

    uint16_t value() const {
      return value_;
    };

    friend std::ostream& operator<< (std::ostream &out, Counter const& data) {
      out << +data.value();
      return out;
    }
};

class CpuRegisters {

  public:
    Register a, b, c, d, e, f, h, l;
    DualRegister af, bc, de, hl;
    Counter sp, pc;

    CpuRegisters() : a{}, b{}, c{}, d{}, e{}, f{}, h{}, l{}, af{a, f}, bc{b, c}, de{d,e}, hl{h,l}, sp{}, pc{} {};

    friend std::ostream& operator<< (std::ostream &out, CpuRegisters const& registers) {
      out << "a: " << +registers.a.value() << " f: " << +registers.f.value() << "\n";
      out << "b: " << +registers.b.value() << " c: " << +registers.c.value() << "\n";
      out << "d: " << +registers.d.value() << " e: " << +registers.e.value() << "\n";
      out << "h: " << +registers.h.value() << " l: " << +registers.l.value() << "\n";
      out << "sp:" << +registers.sp.value() << "\n";
      out << "pc:" << +registers.pc.value() << "\n";

      return out;
    }
};