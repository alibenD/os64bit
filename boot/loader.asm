;---Automatically Generated from template 'bash' wrote by @aliben---
; @Copyright (C) 2021 All rights reserved.
; @file: loader.asm
; @author: aliben.develop@gmail.com
; @created_date: 2021-12-12 00:06:04
; @last_modified_date: 2021-12-12 23:06:14
; @brief: TODO
;---***********************************************---

org 10000h

;======= Init register ======
  mov ax, cs        ; CS: 0000
  mov ds, ax
  mov es, ax
  mov ax, 0x00
  mov ss, ax
  mov sp, 0x7c00    ;

;======= Display on screen : Start Loader......
  mov ax, 1301h
  mov bx, 000fh
  mov dx, 0200h
  mov cx, 12
  push ax,
  mov ax, ds
  mov es, ax
  pop ax
  mov bp, StringStartLoader
  int 10h

  jmp $

StringStartLoader: db "Start Loader"
