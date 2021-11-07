cmake_minimum_required(VERSION 3.10)

set(SRC_DIR ${CMAKE_SOURCE_DIR}/examples/smol)

set(CMAKE_SYSTEM_NAME      Generic)
set(CROSS_COMPILE          riscv64-unknown-elf-)
set(CMAKE_C_COMPILER       "${CROSS_COMPILE}gcc")
set(CMAKE_OBJCOPY          "${CROSS_COMPILE}objcopy")
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_CXX_COMPILER_WORKS TRUE)

set(CMAKE_C_FLAGS_DEBUG     "" CACHE STRING "")
set(CMAKE_CXX_FLAGS_DEBUG   "" CACHE STRING "")
set(CMAKE_C_FLAGS_RELEASE   "" CACHE STRING "")
set(CMAKE_CXX_FLAGS_RELEASE "" CACHE STRING "")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -march=rv32i")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -mabi=ilp32")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -nostartfiles")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -nostdlib")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} -g")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}" CACHE STRING "CFLAGS")
set(CMAKE_ASM_FLAGS "${CMAKE_C_FLAGS}" CACHE STRING "")

set(LINKER_SCRIPT          "${SRC_DIR}/smol_firmware.ld")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -T${LINKER_SCRIPT}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-Map=sections.map")

project(smol_firmware)
add_executable(smol_firmware ${SRC_DIR}/smol_firmware.c)

# Convert the ELF to a readmemh-accepted hex file
add_custom_command(
    TARGET smol_firmware POST_BUILD
    COMMAND ${CMAKE_OBJCOPY} -O verilog smol_firmware smol_ram_init.hex
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "[${CMAKE_OBJCOPY}]: smol_firmware ELF --> smol_ram_init.hex"
)
