#---Automatically Generated from template 'CMakeList.txt' wrote by @aliben---
#
# @Copyright (C) 2021 All rights reserved.
# @filename: CMakeLists.txt
# @author: aliben.develop@gmail.com
# @create_date: 2021-12-04 22:56:49
# @last_modified_date: 2021-12-15 21:12:38
# @description: TODO
#---****************************************************************---


add_custom_command(OUTPUT GenerateBootBin
    COMMAND nasm -i${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/boot/boot.asm -o ${CMAKE_BINARY_DIR}/boot.bin
  )

add_custom_command(OUTPUT GenerateLoaderBin
    COMMAND nasm -i${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/boot/loader.asm -o ${CMAKE_BINARY_DIR}/loader.bin
  )

add_custom_command(OUTPUT GenerateBootImage
    DEPENDS GenerateBootBin GenerateLoaderBin
    COMMAND bximage -func=create -fd=1.44M -sectsize=512 ${CMAKE_BINARY_DIR}/boot.img -q
    COMMAND dd if=${CMAKE_BINARY_DIR}/boot.bin of=${CMAKE_BINARY_DIR}/boot.img bs=512 count=1 conv=notrunc
    COMMAND mcopy -i ${CMAKE_BINARY_DIR}/boot.img ${CMAKE_BINARY_DIR}/loader.bin ::/
    COMMAND dosfsck ${CMAKE_BINARY_DIR}/boot.img -l
  )

add_custom_command(OUTPUT PrepareBochSettings
    DEPENDS GenerateBootImage
    COMMAND cp ${CMAKE_SOURCE_DIR}/boot/bochsrc.floppy ${CMAKE_BINARY_DIR}/
  )

add_custom_target(BootWithFAT12
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    DEPENDS PrepareBochSettings
    COMMAND "bochs" -qf ${CMAKE_BINARY_DIR}/bochsrc.floppy
  )
