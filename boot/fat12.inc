;---Automatically Generated from template 'bash' wrote by @aliben---
; @Copyright (C) 2021 All rights reserved.
; @file: fat12.in
; @author: aliben.develop@gmail.com
; @created_date: 2021-12-12 00:06:04
; @last_modified_date: 2021-12-15 21:07:03
; @brief: TODO
;---***********************************************---

RootDirSectors equ 14
SectorOfRootDirStart equ 19
SectorOfFAT1Start equ 1
SectorBalance equ 17

; Program beginning
  jmp short Label_Start
  nop
  BootSectorOEMName                         db 'Aliben  '
  BIOSParameterBlockBytesPerSec             dw 512
  BIOSParameterBlockSectorPerCluster        db 1
  BIOSParameterBlockReservedSectorCount     dw 1
  BIOSParameterBlockNumFATTables            db 2
  BIOSParameterBlockNumRootEntry            dw 224
  BIOSParameterBlockTotoalSector16          dw 2880
  BIOSParameterBlockMedia                   db 0xf0
  BIOSParameterBlockSectorOfTableFAT        dw 9
  BIOSParameterBlockSectorPerTrack          dw 18
  BIOSParameterBlockNumHeads                dw 2
  BIOSParameterBlockHiddenSectors           dd 0
  BIOSParameterBlockTotalSector32           dd 0
  BootSectorDriveNum                        db 0
  BootSectorReserved1                       db 0
  BootSectorExtensionFlag                   db 0x29
  BootSectorVolID                           dd 0
  BootSectorVolLab                          db 'boot loader'
  BootSectorFileSystemType                  db 'FAT12   '
