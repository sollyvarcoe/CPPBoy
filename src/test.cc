#include "test.h"
#include "memory_unit.h"
#include "ops.h"
#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

namespace cartridge {
constexpr int entrypoint{0x0100};
constexpr int logo{0x0104};
constexpr int title{0x0134};
constexpr int manufacturer_code{0x0100};
constexpr int cgb_flag{0x0100};
constexpr int new_licensee_code{0x0144};
constexpr int sgb_flag{0x0146};
constexpr int cartridge_type{0x0147};
constexpr int rom_size{0x0148};
constexpr int ram_size{0x0149};
constexpr int destination_code{0x014A};
constexpr int old_licensee_code{0x014B};
constexpr int version_number{0x014C};
constexpr int header_checksum{0x014D};
constexpr int global_checksum{0x014E};
} // namespace cartridge

void ProcessOps(MemoryUnit &memory) {

  CpuRegisters registers{};
  registers.pc.set(0x0100);
  registers.sp.set(0xFFFE);
  registers.af.set(0x01B0);
  registers.bc.set(0x0013);
  registers.de.set(0x00D8);
  registers.hl.set(0x014D);

  while (true) {
    uint8_t byte_at_pc = memory.read(registers.pc.value());
    registers.pc.increment();

    std::string assembly_str = assembly_string_map[byte_at_pc];
    std::cout << +byte_at_pc << std::endl;
    std::cout << assembly_str << std::endl;

    instruction_map[byte_at_pc](registers, memory);
    std::cout << registers << std::endl;
  }
}

int main() {
  std::ifstream cartridge_file(
      "/home/sollyvarcoe/Projects/C++Boy/bazel-bin/src/tetris.gb",
      std::ios::binary | std::ios::in);

  size_t length{0};
  if (cartridge_file) {

    cartridge_file.seekg(0, std::ios::end);
    length = cartridge_file.tellg();
    cartridge_file.seekg(0, std::ios::beg);
  } else {
    throw std::runtime_error("Cannot open rom");
  }

  Register a{};
  MemoryUnit mem{};
  std::cout << mem.read(0xC000) << std::endl;
  std::cout << a << std::endl;

  char *buffer = (char *)malloc(sizeof(char) * length);
  cartridge_file.read(buffer, length);
  uint8_t *cartridge_bytes = (uint8_t *)buffer;
  std::cout << length << " " << cartridge::title << std::endl;
  std::string name(&buffer[cartridge::title], 8);
  std::cout << name << std::endl;
  mem.load_cartridge(buffer, length);
  ProcessOps(mem);
  return 0;
}

void CpuRegisters::add(const uint8_t n) {
  const uint8_t a_val = a.value();
  const uint8_t res = a_val + n;
  a.set(res);

  bool carry = calc_carry(a_val, n);
  bool half_carry = calc_half_carry(a_val, n);
  bool zero = (res == 0);

  (zero) ? set_zero_f() : clear_zero_f();
  (half_carry) ? set_hcarry_f() : clear_hcarry_f();
  (carry) ? set_carry_f() : clear_carry_f();
  clear_sub_f();
}
void CpuRegisters::add(Register &r) {
  const uint8_t r_val = r.value();
  add(r_val);
}
void CpuRegisters::add_carry(uint8_t n) {
  uint8_t res = n + carry_f();
  add(res);
}
void CpuRegisters::add_carry(Register &r) {
  uint8_t res = r.value() + carry_f();
  add(res);
}

void CpuRegisters::add_hl(uint16_t r_val) {
  const uint16_t hl_val = hl.value();
  const uint16_t res = hl_val + r_val;
  hl.set(res);

  bool carry = calc_carry(hl_val, r_val);
  bool half_carry = calc_half_carry(hl_val, r_val);

  (half_carry) ? set_hcarry_f() : clear_hcarry_f();
  (carry) ? set_carry_f() : clear_carry_f();
  clear_sub_f();
}

void CpuRegisters::sub(const uint8_t n) {
  const int8_t negative_n = -n;
  add(negative_n);
}
void CpuRegisters::sub(Register &r) { sub(r.value()); }
void CpuRegisters::sub_carry(uint8_t n) { sub(n + carry_f()); }
void CpuRegisters::sub_carry(Register &r) { sub(r.value() + carry_f()); }
void CpuRegisters::and_instr(uint8_t n) {
  const uint8_t a_val = a.value();
  const uint8_t res = a_val & n;

  a.set(res);

  bool zero = (res == 0);
  (zero) ? set_zero_f() : clear_zero_f();
  clear_sub_f();
  clear_carry_f();
  clear_hcarry_f();
}
void CpuRegisters::and_instr(Register &r) { or_instr(r.value()); }
void CpuRegisters::or_instr(uint8_t n) {
  const uint8_t a_val = a.value();
  const uint8_t res = a_val | n;

  a.set(res);

  bool zero = (res == 0);
  (zero) ? set_zero_f() : clear_zero_f();
  clear_sub_f();
  clear_carry_f();
  clear_hcarry_f();
}
void CpuRegisters::or_instr(Register &r) { or_instr(r.value()); }
void CpuRegisters::xor_instr(uint8_t n) {
  const uint8_t a_val = a.value();
  const uint8_t res = a_val ^ n;

  a.set(res);

  bool zero = (res == 0);
  (zero) ? set_zero_f() : clear_zero_f();
  clear_sub_f();
  clear_carry_f();
  clear_hcarry_f();
}
void CpuRegisters::xor_instr(Register &r) { or_instr(r.value()); }
void CpuRegisters::compare(uint8_t n) {
  // Set flags for a subtract but do not set register
  const uint8_t a_val = a.value();
  const uint8_t neg_n = -n;
  const uint8_t res = a_val + neg_n;

  bool carry = calc_carry(a_val, neg_n);
  bool half_carry = calc_half_carry(a_val, neg_n);
  bool zero = (res == 0);

  (zero) ? set_zero_f() : clear_zero_f();
  (half_carry) ? set_hcarry_f() : clear_hcarry_f();
  (carry) ? set_carry_f() : clear_carry_f();
  set_sub_f();
}
void CpuRegisters::compare(Register &r) { compare(r.value()); }