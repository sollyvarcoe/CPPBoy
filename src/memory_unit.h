#pragma once

#include <vector>
#include <stdint.h>

// ROM constants
constexpr uint16_t rom_start_addr = 0x0000;
constexpr uint16_t rom_end_addr = 0x8000;
constexpr size_t rom_size = rom_end_addr - rom_start_addr;

// Video RAM constans
constexpr uint16_t vram_start_addr = 0x8000;
constexpr uint16_t vram_end_addr = 0xA000;
constexpr size_t vram_size = vram_end_addr - vram_start_addr;

// External RAM constants
constexpr uint16_t eram_start_addr = 0xA000;
constexpr uint16_t eram_end_addr = 0xC000;
constexpr size_t eram_size = eram_end_addr - eram_start_addr;

// Working RAM constants
constexpr uint16_t wram_start_addr = 0xC000;
constexpr uint16_t wram_end_addr = 0xE000;
constexpr size_t wram_size = wram_end_addr - wram_start_addr;

// Echo RAM constants (Do not use)
constexpr uint16_t echo_ram_start_addr = 0xE000;
constexpr uint16_t echo_ram_end_addr = 0xFE00;

// OAM constants
constexpr uint16_t oam_start_addr = 0xFE00;
constexpr uint16_t oam_end_addr = 0xFEA0;
constexpr size_t oam_size = oam_end_addr - oam_start_addr;

// Unusable memory constants
constexpr uint16_t unusable_start_addr = 0xFEA0;
constexpr uint16_t unusable_end_addr = 0xFF00;

// IO registers 
constexpr uint16_t io_start_addr = 0xFF00;
constexpr uint16_t io_end_addr = 0xFF80;

// High RAM constants
constexpr uint16_t hram_start_addr = 0xFF80;
constexpr uint16_t hram_end_addr = 0xFFFF;
constexpr size_t hram_size = hram_end_addr - hram_start_addr;

// Interrupt register
constexpr uint16_t interupt_register_addr = 0xFFFF;


class MemoryUnit { 
    private:
        
        // ROM (i.e. cartridge)
        std::vector<uint8_t> rom{std::vector<uint8_t>(rom_size)};

        // Video RAM
        std::vector<uint8_t> vram{std::vector<uint8_t>(vram_size)};

        // External RAM
        std::vector<uint8_t> eram{std::vector<uint8_t>(eram_size)};

        // Working RAM
        std::vector<uint8_t> wram{std::vector<uint8_t>(wram_size)};

        // Sprite attribute table (OAM)
        std::vector<uint8_t> oam{std::vector<uint8_t>(oam_size)};

        // High RAM
        std::vector<uint8_t> hram{std::vector<uint8_t>(hram_size)};

    public:
    
        uint8_t read(uint16_t address) const;
        void write(uint16_t address, uint8_t data);
        void load_cartridge(char* cartridge_data, size_t size);
        MemoryUnit() = default;
        ~MemoryUnit() = default;

};