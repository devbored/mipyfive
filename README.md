# mipyfive
Another RISC-V core - implemented via [nMigen](https://github.com/m-labs/nmigen)

*Work in Progress*

## Design
- Implements RV32I
- 5 stage pipelined processor
- Static branch prediction (assume not-taken)

## Main Checklist Items:
:heavy_check_mark: Design the main RISC-V RV32I Core

:heavy_check_mark: Unit Testing (via nMigen)

:x: Validate the main RISC-V RV32I Core (RISC-V Conformance Testing)

:x: Design basic SoC-creation utility

:x: Address all the TODO's in the source code

:x: Finish initial documentation

## Supplemental Checklist Items
:x: Provide optional modules (i.e. Vectored Interrupt Controller, Interconnect Bus, Debug HW, etc.)

:x: Explore/Add RISC-V Extensions
