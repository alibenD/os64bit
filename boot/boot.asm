;---Automatically Generated from template 'bash' wrote by @aliben---
; @Copyright (C) 2021 All rights reserved.
; @file: boot.asm
; @author: aliben.develop@gmail.com
; @created_date: 2021-12-12 00:06:04
; @last_modified_date: 2021-12-19 13:22:07
; @brief: TODO
;---***********************************************---
org 0x7c00

; Literal valus definition
BaseAddressOfStack  equ 0x7c00
BaseAddressOfLoader equ 0x1000
OffsetOfLoader      equ 0x0000

%include "fat12.inc"

Label_Start:
  mov ax, cs                          ; cs default: 0000
  mov ds, ax
  mov es, ax
  mov ss, ax
  mov sp, BaseAddressOfStack          ; sp=ip=0x7c00

;======= clear screen
  mov ax, 0600h   ; AH=06, AL:# of lines to scroll, prev. lines are blanked, if 0 or >screen size, all blanked
  mov bx, 0700h   ; Attribute to be used on blank line.
  mov cx, 0       ; CH:row of upper left corner of scroll windows, CL: column of the same
  mov dx, 0184fh  ; DH:row of lower right corner of scroll windows, DL: column of the same
  int 10h         ; INT 10h,06h: Scroll window up, nothing returns

;======= set focus

  mov ax, 0200h
  mov bx, 0000h
  mov dx, 0000h
  int 10h

;=========== display on screen : Start Booting.......
  mov ax, 1301h             ; AH=13h, AL=write mode
                            ; 0:chars only, Attribute in BL, cursor not moved
                            ; 1:chard only, Attribute in BL, cursor moved
                            ; 2:contains chars and attributes, cursor not moved
                            ; 3:contains chars and attributes, cursor moved
                            ; Bit settings for write mode (register AL):
                            ; |7|6|5|4|3|2|1|0|  AL
                            ;  | | | | | | | `---- 0=don't move cursor, 1=move cursor
                            ;  | | | | | | `----- 0=BL has attributes, 1=string has attributes
                            ;  `---------------- unused
  mov bx, 000fh             ; BH: video page number, BL attribute if mode 0 or 1(AL bit 1 or 0)
  mov dx, 0000h             ; DH: row coordinate, DL: column coordinate
  mov cx, 10                ; CX: length of string
  push ax
  mov ax, ds                ; Set ES as DS(Data section)
  mov es, ax
  pop ax
  mov bp, StartBootMessage  ; ES:BP=pointer to string
  int 10h                   ; INT 10h,13h: Write String(BIOS version from 1986/1/10)

;======= reset floppy
  xor ah, ah                ; AH=00h, Reset DiskSystem, reset AH:AL=00??h
  xor dl, dl                ; DL: driver number(0=A, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
  int 13h                   ; INT 13h,00h: Reset DiskSystem

;=========== search loader.bin
  mov word [SectorNumber], SectorOfRootDirStart

Label_Search_In_Root_Dir_Begin:
  cmp word [RootDirSizeForLoop], 0      ; if RootDirSizeForLoop = 0(inited with RootDirSectors = 14)
  jz Label_No_LoaderBin                 ; then jmp to Label_No_LoaderBin
  dec word [RootDirSizeForLoop]         ; else --RootDirSizeForLoop
  ;mov ax, 0000h
  mov ax, 00h
  mov es, ax
  mov bx, 8000h                         ; buffer address es:bx
  mov ax, [SectorNumber]                ; sector to read from
  mov cl, 1                             ; sector# to read
  call Func_ReadOneSector
  mov si, LoaderFileName
  mov di, 8000h
  cld                                   ; clear DF=0 direct +
  mov dx, 10h

Label_Search_For_LoaderBin:
  cmp dx, 0                             ;   |Cond|ZF|CR|
                                        ;   |dx>0|0 |0 |
                                        ;   |dx=0|1 |0 |
                                        ;   |dx<0|0 |1 |
  jz Label_Goto_Next_Sector_In_Root_Dir ; jmp when dx == 0
  dec dx
  mov cx, 11                            ; Filename+Extension = 8 + 3 = 11 Bytes

Label_Cmp_FileName:
  cmp cx, 0
  jz Label_FileName_Found               ; jmp when cx == 0
  dec cx
  lodsb                                 ; Load byte at address DS:(E)SI into AL
  cmp al, byte [es:di]
  jz Label_Go_On                        ; when al = filename[idx], jmp GoOn
  jmp Label_Different                   ; filename different

Label_Go_On:
  inc di
  jmp Label_Cmp_FileName

Label_Different:
  and di, 0ffe0h
  add di, 20h
  mov si, LoaderFileName
  jmp Label_Search_For_LoaderBin

Label_Goto_Next_Sector_In_Root_Dir:
  add word [SectorNumber], 1
  jmp Label_Search_In_Root_Dir_Begin

;===================== display on screen : ERROR: No LOADER Found
Label_No_LoaderBin:
  mov ax, 1301h
  mov bx, 008ch
  mov dx, 0100h
  mov cx, 22                ; string length
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, NoLoaderMessage   ; Set ES:BP to NoLoaderMessage
  int 10h                   ; Writing String
  jmp $                     ; Inifinite loop

;===================== fond loader.bin name in root director struct
Label_FileName_Found:
  mov ax, RootDirSectors
  and di, 0FFE0h                  ; Round down for RootDirEntryBegin
  add di, 01Ah                    ; Offset for DIR_FirstClust
  mov cx, word [es:di]            ; Get the first cluster of loader.bin
  push cx
  add cx, ax
  add cx, SectorBalance           ; Calculate the sector# of loader.bin
  mov ax, BaseAddressOfLoader
  mov es, ax
  mov bx, OffsetOfLoader
  mov ax, cx

Label_Go_On_Loading_File:
  push ax
  push bx
  mov ah, 0eh
  mov al, '.'
  mov bl, 0fh
  int 10h
  pop bx                          ; BX store OffsetOfLoader
  pop ax                          ; AX store sector# of loader.bin to read

  mov cl, 1                       ; Read 1 sector to ES:BX, i.e. to read loader.bin to ES:BX
  call Func_ReadOneSector
  pop ax                          ; AX store the first cluster of loader.bin
  call Func_GetFATEntry
  cmp ax, 0fffh
  jz Label_File_Loaded
  push ax
  mov dx, RootDirSectors
  add ax, dx
  add ax, SectorBalance
  add bx, [BIOSParameterBlockBytesPerSec]
  jmp Label_Go_On_Loading_File

Label_File_Loaded:
  jmp BaseAddressOfLoader:OffsetOfLoader

;======================= read one sector from floppy
Func_ReadOneSector:
  push bp                                       ; save caller stack frame
  mov bp, sp                                    ; create function stack for Func_ReadOneSector
  sub esp, 2                                    ; create a word for cl
  mov byte [bp - 2], cl                         ; save cl regarding sector number to stack
  push bx                                       ; save bx regarding buffer address to load sector
  mov bl, [BIOSParameterBlockSectorPerTrack]
  div bl                                        ; division is stored in EAX, and the remainder in EDX
  inc ah                                        ; section number start from 1
  mov cl, ah                                    ; set sector number
  mov dh, al                                    ; set header number
  shr al, 1
  mov ch, al                                    ; set track/cylinder number
  and dh, 1
  pop bx                                        ; restore buffer address to bx
  mov dl, [BootSectorDriveNum]                  ; set drive number(BootSectorDriveNum = 0)

Label_Go_On_Reading:
  mov ah, 2                     ; AH=02
  mov al, byte [bp - 2]         ; AL=number of sectors to read(1-128 dec.)
                                ; CH=track/cylinder number (0-1023 dec.)
                                ; CL=sector number(1-17 dec.)
                                ; DH=header number(0-15 dec.)
                                ; DL=drive number(0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
                                ; ES:BX=pointer to buffer
                                ; return by AH = status;
                                ;           AL= number of sectors read,
                                ;           CF=0 if succ., 1 if error
  int 13h                       ; INT 13h,02h: Read Disk Sectors
  jc Label_Go_On_Reading        ; If read failed(CF=1), then Go On Reading. i.e. while(read_failed) GoOnReading
  add esp, 2                    ; return
  pop bp                        ; restore caller stack frame
  ret

;======================== get FAT Entry
Func_GetFATEntry:
  push es
  push bx
  push ax
  mov ax, 00
  mov es, ax
  pop ax
  mov byte [Odd], 0
  mov bx, 3
  mul bx
  mov bx, 2
  div bx
  cmp dx, 0
  jz Label_Even
  mov byte [Odd], 1

Label_Even:
  xor dx, dx
  mov bx, [BIOSParameterBlockBytesPerSec]
  div bx
  push dx
  mov bx, 8000h
  add ax, SectorOfFAT1Start
  mov cl, 2
  call Func_ReadOneSector

  pop dx
  add bx, dx
  mov ax, [es:bx]
  cmp byte [Odd], 1
  jnz Label_Even_2
  shr ax, 4

Label_Even_2:
  and ax, 0fffh
  pop bx
  pop es
  ret

;======================= tmp variable
RootDirSizeForLoop  dw RootDirSectors
SectorNumber        dw 0
Odd                 db 0

;======================= display message

StartBootMessage: db "Start Boot"               ; strlen(StartBootMessage) = 10, +\0 = 11
NoLoaderMessage:  db "ERROR: No LOADER Found"
LoaderFileName:   db "LOADER  BIN", 0           ; double space separating name and postfix

;=========== fill zero until whole sector
times 510 - ($ - $$) db 0
dw 0xaa55
