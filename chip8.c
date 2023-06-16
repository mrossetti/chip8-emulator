#ifndef CHIP8_INCL
#define CHIP8_INCL (1)

#ifndef CHIP8_API_DECL
#define CHIP8_API_DECL static
#endif

#ifndef CHIP8_API_IMPL
#define CHIP8_API_IMPL static
#endif

// CHIP8 mem is 4 KiB (first 512 bytes are reserved for the interpreter and hex-digits' sprites)
#define CHIP8_MEMOFFSET_PROGRAM 0x200
#define CHIP8_MAX_PROGRAM_SIZE 0xCA0  // MEMOFFSET_STACK (0xEA0) - MEMOFFSET_PROGRAM (0x200)
// CHIP8 sprites for hex-digits (0 to F) are stored in the reserved portion of memory (each is an 5 (h) x 8-bits (w) sprite)
#define CHIP8_MEMOFFSET_SPRITE_HEX 0x1B0  // MEMOFFSET_PROGRAM (0x200) - 16 (#HEX) * CHIP8_SPRITE_HEX_SIZE (5)
#define CHIP8_SPRITE_HEX_SIZE 5
// CHIP8 stack has 16 slots, 16-bits each
#define CHIP8_MEMOFFSET_STACK 0xEA0
#define CHIP8_STACK_SIZE 32  // #bytes = (16 * 16) bits / 8
// CHIP8 display is monochrome (1 bit per pixel)
#define CHIP8_MEMOFFSET_DISPLAY 0xF00
#define CHIP8_DISPLAY_BYTE_WIDTH 8
#define CHIP8_DISPLAY_BIT_WIDTH 64
#define CHIP8_DISPLAY_HEIGHT 32
#define CHIP8_DISPLAY_SIZE 256  // #bytes = (64 * 32) bits / 8

struct CHIP8 {
    // general purpose registers V0 to VF
    unsigned char reg[16];
    // special purpose registers (cannot be accessed directly)
    unsigned short I;   // register for a 12-bits address
    unsigned short SP;  // stack pointer
    unsigned short PC;  // program counter
    unsigned char DT;  // delay timer
    unsigned char ST;  // sound timer
    // key (A to F) state (0: key up, 1: key down)
    unsigned short key;
    // 4 KiB memory
    unsigned char mem[4096];
};

// `chip8_pack16` is a utility function to pack two bytes (high and low) into a single 16-bits word.
CHIP8_API_IMPL inline unsigned short chip8_pack16(unsigned char byte_h, unsigned char byte_l) {
    return ((unsigned short)byte_h << 8) | (unsigned short)byte_l;
}
// `chip8_store16` is a utility functionto to store a single 16-bits word in two contiguous bytes in memory.
CHIP8_API_IMPL inline void chip8_store16(unsigned char* byte_ptr, unsigned short word) {
    *byte_ptr++ = word >> 8;
    *byte_ptr = word & 0xFF;
}

// `chip8_load_program` loads the chip8 program's bytecode in memory, returns the number of bytes successfully read.
CHIP8_API_DECL unsigned chip8_load_program(const unsigned char* src, unsigned num_bytes);
// `chip8_load_program` loads the chip8 program from a file in memory, returns the number of bytes successfully read.
CHIP8_API_DECL unsigned chip8_load_program_file(const char* file_path);

// `chip8_disassemble_instr` disassemble a chip8 instruction printing it to stdout in a human readable form and returning the number of bytes written.
CHIP8_API_DECL unsigned chip8_disassemble_instr(unsigned char instr_h, unsigned char instr_l);
// `chip8_disassemble_program` disassemble a chip8 program (loaded in memory) printing it to stdout in a human readable form.
CHIP8_API_DECL void chip8_disassemble_program(void);

// `chip8_print_bitmap` prints a bitmap (1 bit-per-pixel image) to stdout.
CHIP8_API_DECL void chip8_print_bitmap(const unsigned char* bitmap, unsigned char byte_w, unsigned char byte_h);
// `chip8_draw_bitmap` draws a bitmap onto another at the specified coordinates and returns whether any of the touched destination bits were on.
CHIP8_API_DECL unsigned char chip8_draw_bitmap(unsigned char* onto, unsigned char onto_w, unsigned char onto_h, const unsigned char* bitmap, unsigned char bitmap_hor_mask, unsigned char bitmap_h,unsigned char bit_x, unsigned char byte_y);

// `chip8_run_program` runs a chip8 program (loaded in memory).
CHIP8_API_DECL void chip8_run_program(void);


#ifdef CHIP8_IMPL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CHIP8_API_IMPL struct CHIP8 chip8 = { 0 };

CHIP8_API_IMPL unsigned chip8_load_program(const unsigned char* src, unsigned num_bytes) {
    unsigned num_bytes_read = 0;
    
    if (num_bytes < CHIP8_MAX_PROGRAM_SIZE) {
        unsigned i;
        // copy program's bytecode to mem (start at 0x200)
        for (i = 0; i < num_bytes; ++i)
            chip8.mem[CHIP8_MEMOFFSET_PROGRAM + i] = src[i];
        // zero out the rest (including stack and display)
        for (i = (CHIP8_MEMOFFSET_PROGRAM + num_bytes); i < sizeof(chip8.mem); ++i)
            chip8.mem[i] = 0;
        num_bytes_read = num_bytes;
    }

    return num_bytes_read;
}

CHIP8_API_IMPL unsigned chip8_load_program_file(const char* file_path) {
    FILE* f = fopen(file_path, "rb");
    if (!f) return 0;

    fseek(f, 0, SEEK_END);
    int file_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    unsigned num_bytes_read = 0;

    if (file_size > 0 && file_size < CHIP8_MAX_PROGRAM_SIZE) {
        // copy program's bytecode to mem (start at 0x200)
        num_bytes_read = fread(&chip8.mem[CHIP8_MEMOFFSET_PROGRAM], 1, file_size, f);
        num_bytes_read = (num_bytes_read == file_size) ? num_bytes_read : 0;
        // zero out the rest (including stack and display)
        for (unsigned i = (CHIP8_MEMOFFSET_PROGRAM + num_bytes_read); i < sizeof(chip8.mem); ++i)
            chip8.mem[i] = 0;
    }

    fclose(f);
    return num_bytes_read;
}

CHIP8_API_IMPL unsigned chip8_disassemble_instr(unsigned char instr_h, unsigned char instr_l) {
    //  x  : lowest  4-bits of the high unsigned char of the instruction (register number 0 to 16 excluded)
    //   y : highest 4-bits of the low  unsigned char of the instruction (register number 0 to 16 excluded)
    //    n: lowest  4-bits of the instruction (nibble may be used as 4-bits immediate or register number)
    //   nn: lowest  8-bits of the instruction ( 8-bits immediate)
    //  nnn: lowest 12-bits of the instruction (12-bits address in the 4KiB of memory)

    unsigned char  x   = (instr_h & 0xF);
    unsigned char   y  = (instr_l >> 4);
    unsigned char    n = (instr_l & 0xF);
    unsigned char   nn = (instr_l);
    unsigned short nnn = (((unsigned short)instr_h) << 4) | (unsigned short)instr_l;

    switch (instr_h >> 4) {
        case 0x0: {
            switch (nn) {
                case 0xE0: return printf("CLR"); break;  // clears the display
                case 0xEE: return printf("RET"); break;  // return from subroutine
                default:   return printf("HALT");break;  // halt the program's execution
            }
        } break;
        case 0x1: return printf("JMP  $%d",      nnn); break;  // jump to address $nnn
        case 0x2: return printf("CALL $%d",      nnn); break;  // call subroutine at address $nnn
        case 0x3: return printf("SKEQ v%X, $%d",x,nn); break;  // skip next instr if reg[x] == $nn
        case 0x4: return printf("SKNE v%X, $%d",x,nn); break;  // skip next instr if reg[x] != $nn
        case 0x5: return printf("SKEQ v%X, v%X",x,y ); break;  // skip next instr if reg[x] == reg[y]
        case 0x6: return printf("MOV  v%X, $%d",x,nn); break;  // set reg[x]  = $nn
        case 0x7: return printf("ADD  v%X, $%d",x,nn); break;  // set reg[x] += $nn
        case 0x8: {
            switch (n) {
                case 0x0: return printf("MOV  v%X, v%X",x,y); break;  // set reg[x] = reg[y]
                case 0x1: return printf("OR   v%X, v%X",x,y); break;  // set reg[x] |= reg[y]
                case 0x2: return printf("AND  v%X, v%X",x,y); break;  // set reg[x] &= reg[y]
                case 0x3: return printf("XOR  v%X, v%X",x,y); break;  // set reg[x] ^= reg[y]
                case 0x4: return printf("ADD  v%X, v%X",x,y); break;  // set reg[x] += reg[y] and set reg[0xF] = carry flag
                case 0x5: return printf("SUB  v%X, v%X",x,y); break;  // set reg[x] -= reg[y] and set reg[0xF] = !borrow flag
                case 0x6: return printf("RSH  v%X, v%X",x,y); break;  // set reg[x] >>= reg[y] and set reg[0xF] = old LSB
                case 0x7: return printf("BSUB v%X, v%X",x,y); break;  // set reg[x] = reg[y] - reg[x] (backward subtract)
                case 0xE: return printf("LSH  v%X, v%X",x,y); break;  // set reg[x] <<= reg[y] and set reg[0xF] = old MSB
                default:  return printf("HALT");              break;  // halt the program's execution
            }
        } break;
        case 0x9: return printf("SKNE v%X, v%X",     x,y  ); break;  // skip next instr if reg[x] != reg[y]
        case 0xA: return printf("MOV   I, $%d",        nnn); break;  // set I = $nnn (I is a special purpose register for addresses)
        case 0xB: return printf("JMP0 $%d",            nnn); break;  // jump to address reg[0] + $nnn
        case 0xC: return printf("RAND v%X, $%d",     x, nn); break;  // set reg[x] = random_byte() & $nn
        case 0xD: return printf("DRAW v%X, v%X, $%d",x,y,n); break;  // draw $n by 8-bits width sprite, stored in mem[I : I + $n], onto the display at coords reg[x], reg[y] and set reg[0xF] = collision flag
        case 0xE: {
            switch (nn) {
                case 0x9E: return printf("SKDN v%X",x); break;  // skip next instr if key stored in reg[x] is down
                case 0xA1: return printf("SKUP v%X",x); break;  // skip next instr if key stored in reg[x] is up
                default:   return printf("HALT");       break;  // halt the program's execution
            }
        } break;
        case 0xF: {
            switch (nn) {
                case 0x07: return printf("MOV  v%X, DT",x); break;  // set reg[x] = DT (i.e. read from DELAY TIMER)
                case 0x0A: return printf("WAIT v%X",x);     break;  // wait until key-press and store key-code in reg[x]
                case 0x15: return printf("MOV  DT, v%X",x); break;  // set DT = reg[x] (i.e. write to DELAY TIMER)
                case 0x18: return printf("MOV  ST, v%X",x); break;  // set ST = reg[x] (i.e. write to SOUND TIMER)
                case 0x1E: return printf("ADD   I, v%X",x); break;  // set I += reg[x]
                case 0x29: return printf("LHEX  I, v%X",x); break;  // load hex: set I = mem addr of hex-digit stored in reg[x]
                case 0x33: return printf("SBCD  v%X",x);    break;  // store binary-coded-decimal: mem[I] = hundreds, mem[I+1] = tens, mem[I+2] = ones
                case 0x55: return printf("SAVE v0-v%X",x);  break;  // save registers from v0 to v{x}  to  mem[I : I + {x}]
                case 0x65: return printf("LOAD v0-v%X",x);  break;  // load registers from v0 to v{x} from mem[I : I + {x}]
                default:   return printf("HALT");           break;  // halt the program's execution
            }
        } break;
    }
}

CHIP8_API_IMPL void chip8_disassemble_program(void) {
    unsigned i = CHIP8_MEMOFFSET_PROGRAM;

    while ((i + 1 - CHIP8_MEMOFFSET_PROGRAM) < CHIP8_MAX_PROGRAM_SIZE) {
        if (chip8.mem[i] == 0 && chip8.mem[i + 1] == 0) {
            break;
        }
        printf("%04d  ", i);
        unsigned w = chip8_disassemble_instr(chip8.mem[i], chip8.mem[i + 1]);
        for (unsigned s = 0; s < (20 - w); ++s) {
            putchar(' ');
        }
        printf("; 0x%02X 0x%02X\n", chip8.mem[i], chip8.mem[i + 1]);
        i += 2;
    }
}

CHIP8_API_IMPL void chip8_print_bitmap(const unsigned char* bitmap, unsigned char byte_w, unsigned char byte_h) {
    for (unsigned byte_y = 0; byte_y < byte_h; ++byte_y) {
        for (unsigned byte_x = 0; byte_x < byte_w; ++byte_x) {
            // print byte
            for (unsigned i = 0; i < 8; ++i)
                putchar((bitmap[byte_y * byte_w + byte_x] & (128u >> i)) ? '@' : '.');
            // spacing
            putchar(' ');
        }
        putchar('\n');
    }
}

CHIP8_API_IMPL unsigned char chip8_draw_bitmap(
    unsigned char* onto, unsigned char onto_w, unsigned char onto_h,
    const unsigned char* bitmap, unsigned char bitmap_hor_mask, unsigned char bitmap_h,
    unsigned char bit_x, unsigned char byte_y
) {
    unsigned byte_x = (bit_x >> 3);
    unsigned bit_dx = (bit_x & 3);
    unsigned min_h = (bitmap_h < onto_h) ? bitmap_h : onto_h;

    if (byte_x >= onto_w) {
        return 0;
    }

    unsigned byte_i, i;
    unsigned collid = 0;

    if ((byte_x + 1) < onto_w) {
        for (i = 0; i < min_h; ++i) {
            byte_i = (byte_y + i) * onto_w + byte_x;
            collid = ((onto[byte_i] | onto[byte_i + 1]) & bitmap_hor_mask) != 0;
            
            onto[byte_i + 0] |= (bitmap[i] & bitmap_hor_mask) >> bit_dx;
            onto[byte_i + 1] |= (bitmap[i] & bitmap_hor_mask) << (8 - bit_dx);
        }
    } else {
        for (i = 0; i < min_h; ++i) {
            byte_i = (byte_y + i) * onto_w + byte_x;
            collid = ((onto[byte_i] | onto[byte_i + 1]) & bitmap_hor_mask) != 0;

            onto[byte_i] |= (bitmap[i] & bitmap_hor_mask) >> bit_dx;
        }
    }

    return collid;
}

CHIP8_API_IMPL void chip8_run_program(void) {
    // reset chip8 environment (except for program [0x200 : 0xEA0])
    for (unsigned i = 0; i < 16; ++i) {
        chip8.reg[i] = 0;
    }

    chip8.I  = 0;
    chip8.SP = CHIP8_MEMOFFSET_STACK;
    chip8.PC = CHIP8_MEMOFFSET_PROGRAM;
    chip8.DT = 0;
    chip8.ST = 0;

    for (unsigned i = CHIP8_MEMOFFSET_STACK; i < sizeof(chip8.mem); ++i) {
        chip8.mem[i] = 0;
    }

    // load hex sprites in memory
    #define chip8_load_hex_sprite(hex,b0,b1,b2,b3,b4) do {\
        chip8.mem[CHIP8_MEMOFFSET_SPRITE_HEX + hex * 5 + 0] = b0;\
        chip8.mem[CHIP8_MEMOFFSET_SPRITE_HEX + hex * 5 + 1] = b1;\
        chip8.mem[CHIP8_MEMOFFSET_SPRITE_HEX + hex * 5 + 2] = b2;\
        chip8.mem[CHIP8_MEMOFFSET_SPRITE_HEX + hex * 5 + 3] = b3;\
        chip8.mem[CHIP8_MEMOFFSET_SPRITE_HEX + hex * 5 + 4] = b4;\
    } while (0)

    chip8_load_hex_sprite(0x0,0xF0,0x90,0x90,0x90,0xF0);
    chip8_load_hex_sprite(0x1,0x20,0x60,0x20,0x20,0x70);
    chip8_load_hex_sprite(0x2,0xF0,0x10,0xF0,0x80,0xF0);
    chip8_load_hex_sprite(0x3,0xF0,0x10,0xF0,0x10,0xF0);
    chip8_load_hex_sprite(0x4,0x90,0x90,0xF0,0x10,0x10);
    chip8_load_hex_sprite(0x5,0xF0,0x80,0xF0,0x10,0xF0);
    chip8_load_hex_sprite(0x6,0xF0,0x80,0xF0,0x90,0xF0);
    chip8_load_hex_sprite(0x7,0xF0,0x10,0x20,0x40,0x40);
    chip8_load_hex_sprite(0x8,0xF0,0x90,0xF0,0x90,0xF0);
    chip8_load_hex_sprite(0x9,0xF0,0x90,0xF0,0x10,0xF0);
    chip8_load_hex_sprite(0xA,0xF0,0x90,0xF0,0x90,0x90);
    chip8_load_hex_sprite(0xB,0xE0,0x90,0xE0,0x90,0xE0);
    chip8_load_hex_sprite(0xC,0xF0,0x80,0x80,0x80,0xF0);
    chip8_load_hex_sprite(0xD,0xE0,0x90,0x90,0x90,0xE0);
    chip8_load_hex_sprite(0xE,0xF0,0x80,0xF0,0x80,0xF0);
    chip8_load_hex_sprite(0xF,0xF0,0x80,0xF0,0x80,0x80);

    #undef chip8_load_hex_sprite

    // FIXME:
    // > platform should update timers & key state every frame
    // > platform should redraw (e.g. "\033c" to clear terminal window)
    // > instructions should respect certain #cycles and timings

    while (chip8.PC >= CHIP8_MEMOFFSET_PROGRAM && chip8.PC < CHIP8_MEMOFFSET_STACK) {
        const unsigned char instr_h = chip8.mem[chip8.PC];
        const unsigned char instr_l = chip8.mem[chip8.PC + 1];
        chip8.PC += 2;

        const unsigned char x = instr_h & 0xF;
        const unsigned char y = instr_l >> 4;
        const unsigned char n = instr_l & 0xF;
        const unsigned char imm = instr_l;
        const unsigned short addr = (unsigned)((instr_h & 0xF) << 8) | (unsigned)instr_l;

        switch (instr_h >> 4) {
            case 0x0: {
                switch (instr_l) {
                    case 0xE0: { memset(&chip8.mem[CHIP8_MEMOFFSET_DISPLAY], 0, CHIP8_DISPLAY_SIZE); } break;
                    case 0xEE: { chip8.SP -= 2; chip8.PC = chip8_pack16(chip8.mem[chip8.SP], chip8.mem[chip8.SP + 1]); } break;
                    default: { chip8.PC = 0xFFFF; } break;
                }
            } break;
            case 0x1: { chip8.PC = addr; } break;
            case 0x2: { chip8_store16(&chip8.mem[chip8.SP], chip8.PC); chip8.SP += 2; chip8.PC = addr; } break;
            case 0x3: { chip8.PC += (chip8.reg[x] == imm) << 1;  } break;
            case 0x4: { chip8.PC += (chip8.reg[x] != imm) << 1; } break;
            case 0x5: { chip8.PC += (chip8.reg[x] == chip8.reg[y]) << 1; } break;
            case 0x6: { chip8.reg[x]  = imm; } break;
            case 0x7: { chip8.reg[x] += imm; } break;
            case 0x8: {
                switch (n) {
                    case 0x0: { chip8.reg[x]  = chip8.reg[y]; } break;
                    case 0x1: { chip8.reg[x] |= chip8.reg[y]; } break;
                    case 0x2: { chip8.reg[x] &= chip8.reg[y]; } break;
                    case 0x3: { chip8.reg[x] ^= chip8.reg[y]; } break;
                    case 0x4: { chip8.reg[0xF] = (((unsigned short)chip8.reg[x]+(unsigned short)chip8.reg[y])>0xFF); chip8.reg[x] += chip8.reg[y]; } break;
                    case 0x5: { chip8.reg[0xF] = (chip8.reg[x] > chip8.reg[y]); chip8.reg[x] -= chip8.reg[y]; } break;
                    case 0x6: { chip8.reg[0xF] = (chip8.reg[x] & 0x1); chip8.reg[x] >>= chip8.reg[y]; } break;
                    case 0x7: { chip8.reg[0xF] = (chip8.reg[y] > chip8.reg[x]); chip8.reg[x] = chip8.reg[y] - chip8.reg[x]; } break;
                    case 0xE: { chip8.reg[0xF] = (chip8.reg[x] >>  7); chip8.reg[x] <<= chip8.reg[y]; } break;
                    default: chip8.PC = 0xFFFF; break;
                }
            } break;
            case 0x9: { chip8.PC += ((chip8.reg[x] != chip8.reg[y]) << 1); } break;
            case 0xA: { chip8.I = addr; } break;
            case 0xB: { chip8.PC = (unsigned short)chip8.reg[0] + addr; } break;
            case 0xC: { chip8.reg[x] = rand() & imm; } break;
            case 0xD: { chip8.reg[0xF] = chip8_draw_bitmap(&chip8.mem[CHIP8_MEMOFFSET_DISPLAY], CHIP8_DISPLAY_BYTE_WIDTH, CHIP8_DISPLAY_HEIGHT, &chip8.mem[chip8.I], 0xFF, n, chip8.reg[x] & 0x3F, chip8.reg[y] & 0x1F); } break;
            case 0xE: {
                switch (instr_l) {
                    case 0x9E: { chip8.PC += ((  chip8.key  >> chip8.reg[x]) & 1) << 1; } break;
                    case 0xA1: { chip8.PC += (((~chip8.key) >> chip8.reg[x]) & 1) << 1; } break;
                    default: { chip8.PC = 0xFFFF; } break;
                }
            } break;
            case 0xF: {
                switch (instr_l) {
                    case 0x07: { chip8.reg[x] = chip8.DT; } break;
                    case 0x0A: {
                        char keycode = getc(stdin);
                        if      (keycode >= '0' && keycode <= '9') { chip8.key |= (1 << (keycode - '0')); }
                        else if (keycode >= 'a' && keycode <= 'f') { chip8.key |= (1 << (keycode - 'a')); }
                        else if (keycode >= 'A' && keycode <= 'F') { chip8.key |= (1 << (keycode - 'A')); }
                        else { chip8.PC -= 2; }  // keep waiting by processing this very instruction again!
                    } break;
                    case 0x15: { chip8.DT = chip8.reg[x]; } break;
                    case 0x18: { chip8.ST = chip8.reg[x]; } break;
                    case 0x1E: { chip8.I += chip8.reg[x]; } break;
                    case 0x29: { chip8.I  = CHIP8_MEMOFFSET_SPRITE_HEX + (unsigned)chip8.reg[x] * CHIP8_SPRITE_HEX_SIZE; } break;
                    case 0x33: { chip8.mem[chip8.I] = chip8.reg[x] / 100;  chip8.mem[chip8.I + 1] = (chip8.reg[x] / 10) % 10; chip8.mem[chip8.I + 2] = chip8.reg[x] % 10; } break;
                    case 0x55: { for (unsigned r = 0; r < x; ++r) chip8.mem[(chip8.I + r) & 0xFFF] = chip8.reg[r]; } break;
                    case 0x65: { for (unsigned r = 0; r < x; ++r) chip8.reg[r] = chip8.mem[(chip8.I + r) & 0xFFF]; } break;
                    default:   { chip8.PC = 0xFFFF; } break;
                }
            } break;
        }
    }
}

#endif  // CHIP8_IMPL


#ifdef CHIP8_DEMO

int main (int argc, char* argv[]) {
    // if (argc < 2) {
    //     fprintf(stderr, "[error] missing file!\n");
    //     return 0;
    // }
    // unsigned n = chip8_load_program_file(argv[1]);
    // if (!n) {
    //     fprintf(stderr, "[error] could not load file '%s'!\n", argv[1]);
    //     return 0;
    // }
    // printf("DISSASSEMBLE `%s`\n", argv[1]);
    // chip8_disassemble_program();

    unsigned char demo[] = {
        0x00, 0xE0,  // CLR  ; refresh display
        0x60, 0x03,  // MOV  v0, $3
        0xF0, 0x29,  // LHEX v0 ; I = addr of sprite '3'
        0x60, 0x09,  // MOV  v0, $9
        0x61, 0x02,  // MOV  v1, $2
        0xD0, 0x15,  // DRAW v0, v1, $5  ; draw sprite '3' (5 bytes) at x = reg[0], y = reg[1]
    };

    chip8_load_program(demo, sizeof(demo));
    chip8_disassemble_program();

    chip8_run_program();
    chip8_print_bitmap(&chip8.mem[CHIP8_MEMOFFSET_DISPLAY], CHIP8_DISPLAY_BYTE_WIDTH, CHIP8_DISPLAY_HEIGHT);
    
    return 0;
}

#endif  // CHIP8_DEMO


#endif  // CHIP8_INCL
