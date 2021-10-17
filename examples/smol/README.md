# smol
A small example SoC with an RV32I mipyfive core.

🚧 *Work in Progress* 🚧

## Features
- An RV32I mipyfive core
- Harvard architecture
    - Separate instruction (IMEM) and data (DMEM) memories
- A basic dmem and UART module connected to a peripheral-bus and control unit (PBCU)
- A UART transceiver (RX/TX)
    - Adjustable baud rate and data packet width
    - No parity bits

## Memory map 🗺️
| Address Range            | Description                             |
| ------------------------ | --------------------------------------- |
 ...