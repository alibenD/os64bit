;---Automatically Generated from template 'bash' wrote by @aliben---
; @Copyright (C) 2021 All rights reserved.
; @file: loader.asm
; @author: aliben.develop@gmail.com
; @created_date: 2021-12-12 00:06:04
; @last_modified_date: 2021-12-19 14:46:28
; @brief: TODO
;---***********************************************---

org 10000h

;======= Start
  jmp Label_Start

%include "fat12.inc"

BaseOfKernelFile    equ   0x00
OffsetOfKernelFile  equ   0x100000

TmpBaseOfKernelAddr     equ   0x00
TmpOffsetOfKernelFile   equ   0x7E00

MemoryStructBufferAddr  equ   0x7E00

[SECTION gdt]

  LABEL_GDT: dd 0,0
  LABEL_DESC_CODE32: dd 0x0000FFFF, 0x00CF9A00        ; Base:0x00000000, Limit:0xFFFFFFFF, STA_X|STA_R
  LABEL_DESC_DATA32: dd 0x0000FFFF, 0x00CF9200        ; Base:0x00000000, Limit:0xFFFFFFFF, STA_W

  GDTLen equ $ - LABEL_GDT
  GDTPtr dw GDTLen - 1
    dd LABEL_GDT

  SelectorCode32 equ LABEL_DESC_CODE32 - LABEL_GDT
  SelectorData32 equ LABEL_DESC_DATA32 - LABEL_GDT

[SECTION gdt64]
  LABEL_GDT64:      dq 0x0000000000000000
  LABEL_DESC_CODE64 dq 0x0020980000000000
  LABEL_DESC_DATA64 dq 0x0000920000000000

  GDTLen64 equ $ - LABEL_GDT64
  GDTPtr64 dw GDTLen64 - 1
    dd LABEL_GDT64

  SelectorCode64 equ LABEL_DESC_CODE64 - LABEL_GDT64
  SelectorData64 equ LABEL_DESC_DATA64 - LABEL_GDT64

[SECTION .s16]
[BITS 16]

;======= Init register ======
Label_Start:
  mov ax, cs        ; CS: 0000
  mov ds, ax
  mov es, ax
  mov ax, 0x00
  mov ss, ax
  mov sp, 0x7c00    ; Boot is not useable anymore, reuse the memory of boot

;======= Display on screen : Start Loader......
  mov ax, 1301h
  mov bx, 000fh
  mov dx, 0200h
  mov cx, 12
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, StringStartLoader
  int 10h

;======= open address A20
  push ax
  in al, 92h
  or al, 00000010b
  out 92h, al
  pop ax
  cli

  db 0x66
  lgdt [GDTPtr]

  mov eax, cr0
  or  eax, 1
  mov cr0, eax

  mov ax, SelectorCode32
  mov fs, ax
  mov eax, cr0
  and al, 11111110b
  mov cr0, eax

  sti

;======= reset floppy
  xor ah, ah
  xor dl, dl
  int 13h

;=========== search loader.bin
  mov word [SectorNumber], SectorOfRootDirStart

Label_Search_In_Root_Dir_Begin:
  cmp word [RootDirSizeForLoop], 0      ; if RootDirSizeForLoop = 0(inited with RootDirSectors = 14)
  jz Label_No_LoaderBin                 ; then jmp to Label_No_LoaderBin
  dec word [RootDirSizeForLoop]         ; else --RootDirSizeForLoop
  mov ax, 00h
  mov es, ax
  mov bx, 8000h                         ; buffer address es:bx
  mov ax, [SectorNumber]                ; sector to read from
  mov cl, 1                             ; sector# to read
  call Func_ReadOneSector
  mov si, KernelFileName
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
  mov si, KernelFileName
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

Label_FileName_Found:
  mov ax, RootDirSectors
  and di, 0FFE0h                  ; Round down for RootDirEntryBegin
  add di, 01Ah                    ; Offset for DIR_FirstClust
  mov cx, word [es:di]            ; Get the first cluster of loader.bin
  push cx
  add cx, ax
  add cx, SectorBalance           ; Calculate the sector# of loader.bin
  mov ax, TmpBaseOfKernelAddr
  mov es, ax
  mov bx, TmpOffsetOfKernelFile
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

  push cx
  push eax
  push fs
  push edi
  push ds
  push esi

  mov cx, 200h
  mov ax, BaseOfKernelFile
  mov fs, ax
  mov edi, dword [OffsetOfKernelFileCount]

  mov ax, TmpBaseOfKernelAddr
  mov ds, ax
  mov esi, TmpOffsetOfKernelFile

Label_Mov_Kernel:
  mov al, byte [ds:esi]
  mov byte [fs:edi], al

  inc esi
  inc edi

  loop Label_Mov_Kernel

  mov eax, 0x10000
  mov ds, eax

  mov dword [OffsetOfKernelFileCount], edi

  pop esi
  pop ds
  pop edi
  pop fs
  pop eax
  pop cx

  call Func_GetFATEntry
  cmp ax, 0fffh
  jz Label_File_Loaded
  push ax
  mov dx, RootDirSectors
  add ax, dx
  add ax, SectorBalance
  jmp Label_Go_On_Loading_File

Label_File_Loaded:
  mov ax, 0B800h
  mov gs, ax
  mov ah, 0Fh
  mov al, 'G'
  mov [gs:((80*0 + 39) *2)], ax

KillMotor:
  push dx
  mov dx, 03F2h
  mov al, 0
  out dx, al
  pop dx

;======== get memory address size type
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0400h
  mov cx, 24
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, StartGetMemStructMessage
  int 10h

  mov ebx, 0
  mov ax, 0x00
  mov es, ax
  mov di, MemoryStructBufferAddr

Label_Get_Mem_Struct:
  mov eax, 0x0E820
  mov ecx, 20
  mov edx, 0x534D4150
  int 15h
  jc Label_Get_Mem_Fail
  add di, 20
  cmp ebx, 0
  jne Label_Get_Mem_Struct
  jmp Label_Get_Mem_OK

Label_Get_Mem_Fail:
  mov ax, 1301h
  mov bx, 008Ch
  mov dx, 0500h
  mov cx, 23
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetMemStructErrMessage
  int 10h
  jmp $

Label_Get_Mem_OK:
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0600h
  mov cx, 29
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetMemStructOKMessage
  int 10h

;========= Get SVGA Information
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0800h
  mov cx, 23
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, StartGetSVGAVBEInfoMessage
  int 10h

  mov ax, 0x00
  mov es, ax
  mov di, 0x8000
  mov ax, 4F00h

  jz .KO

;====== Fail
  mov ax, 1301h
  mov bx, 008Ch
  mov dx, 0900h
  mov cx, 23
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetSVGAVBEInfoErrMessage
  int 10h
  jmp $

.KO:
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0A00h
  mov cx, 29
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetSVGAVBEInfoOKMessage
  int 10h

;====== Get SVGA Mode Info
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0C00h
  mov cx, 24
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, StartGetSVGAVBEInfoMessage
  int 10h

  mov ax, 0x00
  mov es, ax
  mov si, 0x800e

  mov esi, dword [es:si]
  mov edi, 0x8200

Label_SVGA_Mode_Info_Get:
  push ax
  mov ax, 00h
  mov al, ch
  call Label_DispAL

  mov ax, 00h
  mov al, cl
  call Label_DispAL
  pop ax

  cmp cx, 0FFFFh
  jz Label_SVGA_Mode_Info_Finish

  mov ax, 4F01h
  int 10h

  cmp ax, 004Fh

  jnz Label_SVGA_Mode_Info_FAIL

  add esi, 2
  add edi, 0x100

  jmp Label_SVGA_Mode_Info_Get

Label_SVGA_Mode_Info_FAIL:
  mov ax, 1301h
  mov bx, 008Ch
  mov dx, 0D00h
  mov cx, 24
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetSVGAModeInfoErrMessage
  int 10h

Label_SET_SVGA_Mode_VESA_VBE_FAIL:
  jmp $

Label_SVGA_Mode_Info_Finish:
  mov ax, 1301h
  mov bx, 000Fh
  mov dx, 0E00h
  mov cx, 30
  push ax
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, GetSVGAModeInfoOKMessage
  int 10h

;======= set the SVGA mode(VESA VBE)
  mov ax, 4F02h
  mov bx, 4180h
  int 10h

  cmp ax, 004Fh
  jnz Label_SET_SVGA_Mode_VESA_VBE_FAIL

;======= init IDT GDT goto protect mode
  cli
  db 0x66
  lgdt [GDTPtr]

  mov eax, cr0
  or eax, 1
  mov cr0, eax

  jmp dword SelectorCode32:GO_TO_TMP_Protect

[SECTION .s32]
[BITS 32]

GO_TO_TMP_Protect:
;======= go to tmp long mode
  mov ax, 0x10
  mov ds, ax
  mov es, ax
  mov fs, ax
  mov ss, ax
  mov esp, 7E00h

  call support_long_mode
  test eax, eax

  jz no_support

;====== Init temporary page table 0x90000
  mov dword [0x90000], 0x91007
  mov dword [0x90000], 0x91007
  mov dword [0x91000], 0x92007
  mov dword [0x92000], 0x000083
  mov dword [0x92008], 0x200083
  mov dword [0x92010], 0x400083
  mov dword [0x92018], 0x600083
  mov dword [0x92020], 0x800083
  mov dword [0x92028], 0xa00083

;====== Load GDTR
  db 0x66
  lgdt [GDTPtr64]
  mov ax, 0x10
  mov ds, ax
  mov es, ax
  mov fs, ax
  mov gs, ax
  mov ss, ax
  mov esp, 7E00h

;======= Open PAE
  mov eax, cr4
  bts eax, 5
  mov cr4, eax

;======= load cr3
  mov eax, 0x90000
  mov cr3, eax

;====== enable long-mode
  mov ecx, 0C0000080h ; IA32_EFER
  rdmsr

  bts eax, 8
  wrmsr

;====== open PE and paging
  mov eax, cr0
  bts eax, 0
  bts eax, 31
  mov cr0, eax

  jmp SelectorCode64:OffsetOfKernelFile

;====== test support long mode or not
support_long_mode:
  mov eax, 0x80000000
  cpuid
  cmp eax, 0x80000001
  setnb al
  jb support_long_mode_done
  mov eax, 0x80000001
  cpuid
  bt edx, 29
  setc al

support_long_mode_done:
  movzx eax, al
  ret

;======= no support
no_support:
  jmp $

;==== Read one sector from floppy

[SECTION .s16lib]
[BITS 16]

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

;==== Display num in al
Label_DispAL:
  push ecx
  push edx
  push edi
  mov edi, [DisplayPosition]
  mov ah, 0Fh
  mov dl, al
  shr al, 4
  mov ecx, 2

.begin:
  and al, 0Fh
  cmp al, 9
  ja .1
  add al, '0'
  jmp .2

.1:
  sub al, 0Ah
  add al, 'A'

.2:
  mov [gs:edi], ax
  add edi, 2
  mov al, dl
  loop .begin
  mov [DisplayPosition], edi

  pop edi
  pop edx
  pop ecx
  ret

;===== tmp IDT
IDT:
  times 0x50 dq 0
IDT_END:
IDE_POINTER:
  dw IDT_END -IDT -1
  dd IDT


;======= Tmp variables
RootDirSizeForLoop dw RootDirSectors
OffsetOfKernelFileCount dd OffsetOfKernelFile
SectorNumber dw 0
Odd db 0
DisplayPosition dd 0

StringStartLoader: db "Start Loader"
NoLoaderMessage: db "ERROR:No KERNEL Found"
KernelFileName: db "KERNEL  BIN", 0
StartGetMemStructMessage: db "Start Get Memory Struct."
GetMemStructErrMessage: db "Get Memory Struct ERROR"
GetMemStructOKMessage: db "Get Memory Struct SUCCESSFUL!"

StartGetSVGAVBEInfoMessage: db "Start Get SVGA VBE Info"
GetSVGAVBEInfoErrMessage: db "Get SVGA VBE Info Error"
GetSVGAVBEInfoOKMessage: db "Get SVGA VBE Info SUCCESSFUL!"

StartGetSVGAModeInfoMessage: db "Start Get SVGA Mode Info"
GetSVGAModeInfoErrMessage: db "Get SVGA Mode Info ERROR"
GetSVGAModeInfoOKMessage: db "Get SVGA Mode Info SUCCESSFUL"

