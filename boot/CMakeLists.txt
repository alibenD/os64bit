#---Automatically Generated from template 'CMakeList.txt' wrote by @aliben---
#
# @Copyright (C) 2021 All rights reserved.
# @filename: CMakeLists.txt
# @author: aliben.develop@gmail.com
# @create_date: 2021-12-04 22:56:49
# @last_modified_date: 2021-12-04 23:21:37
# @description: TODO
#---****************************************************************---


add_custom_command(OUTPUT GenerateBootBin
    COMMAND "nasm" ${CMAKE_SOURCE_DIR}/boot/boot.asm -o ${CMAKE_BINARY_DIR}/boot.bin
  )

add_custom_command(OUTPUT GenerateBootImage
    DEPENDS GenerateBootBin
    COMMAND "dd" if=${CMAKE_BINARY_DIR}/boot.bin of=${CMAKE_BINARY_DIR}/boot.img bs=512 count=1 conv=notrunc
    COMMAND cp ${CMAKE_SOURCE_DIR}/boot/bochsrc.floppy ${CMAKE_BINARY_DIR}/
  )

add_custom_target(BootSimple
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    DEPENDS GenerateBootImage
    COMMAND "bochs" -qf ${CMAKE_BINARY_DIR}/bochsrc.floppy
  )
