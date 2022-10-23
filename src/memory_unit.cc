#include "memory_unit.h"
#include <cstdint>
#include <iostream>
#include <stdexcept>

uint8_t MemoryUnit::read(uint16_t address) const {

    // Read ROM
    if (address >= rom_start_addr && address < rom_end_addr) {
        return rom[address - rom_start_addr];
    }

    // Read VRAM
    if (address >= vram_start_addr && address < vram_end_addr) {
        return vram[address - vram_start_addr];
    }

    // Read working RAM
    if (address >= wram_start_addr && address < wram_end_addr) {
        return wram[address - wram_start_addr];
    }

    // Read Echo RAM
    if (address >= echo_ram_start_addr && address < echo_ram_end_addr) {
        // Just read from working RAM
        return wram[address - echo_ram_start_addr];
    }

    // Read OAM
    if (address >= oam_start_addr && address < oam_end_addr) {
        return oam[address - oam_start_addr];
    }   

    // Read high RAM
    if (address >= hram_start_addr && address < hram_end_addr) {
        return hram[address - hram_start_addr];
    }
    else {
        return 0;
    }
}

void MemoryUnit::write(uint16_t address, uint8_t data) {

    // Write to VRAM 
    if (address >= vram_start_addr && address < vram_end_addr) {
        vram[address - vram_start_addr] = data;
        return;
    }

    // Write to external RAM 
    if (address >= eram_start_addr && address < eram_end_addr) {
        eram[address - eram_start_addr] = data;
        return;
    }

    // Write to working RAM
    if (address >= wram_start_addr && address < wram_end_addr) {
        wram[address - wram_start_addr] = data;
        return;
    }

    // Write to echo RAM
    if (address >= echo_ram_start_addr && address < echo_ram_end_addr) {
        // Just write to working ram
        wram[address - echo_ram_start_addr] = data;
        return;
    }

    // Write to OAM
    if (address >= oam_start_addr && address < oam_end_addr) {
        oam[address - oam_start_addr] = data;
        return;
    }

    // Write to high RAM
    if (address >= hram_start_addr && address < hram_end_addr) {
        hram[address - hram_start_addr] = data;
        return;
    }
    // Writing to illegal memory address
    std::cout << std::hex << +address << std::endl;
}

void MemoryUnit::load_cartridge(char* cartridge_data, size_t size) {
    uint8_t* byte_data = (uint8_t*) cartridge_data;
    std::copy(byte_data, byte_data + size, rom.begin());
}