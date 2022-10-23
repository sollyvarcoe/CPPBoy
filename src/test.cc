#include <iostream>
#include <fstream>
#include <vector>
#include <memory>
#include "ops.h"
#include "test.h"
#include "memory_unit.h"


namespace cartridge {
  constexpr int entrypoint{0x0100};
  constexpr int logo{0x0104};
  constexpr int title{0x0134};
  constexpr int manufacturer_code {0x0100};
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
}

void ProcessOps(MemoryUnit& memory) {

  CpuRegisters registers{};
  registers.pc.set(0x1000);
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

int main () {  
  std::ifstream cartridge_file("/home/sollyvarcoe/Projects/C++Boy/bazel-bin/src/tetris.gb", std::ios::binary | std::ios::in);

  size_t length{0};
  if(cartridge_file) {

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

  char * buffer = (char *) malloc(sizeof(char) * length);
  cartridge_file.read(buffer, length);
  uint8_t * cartridge_bytes = (uint8_t *) buffer;
  std::cout << length << " " << cartridge::title << std::endl;
  std::string name(&buffer[cartridge::title], 8);
  std::cout << name << std::endl;
  mem.load_cartridge(buffer, length);
  ProcessOps(mem);
  return 0;
}