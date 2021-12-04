#---Automatically Generated from template 'option.cmake' wrote by @aliben---
#
# @Copyright (C) 2021 All rights reserved.
# @filename: find_package.cmake
# @author: aliben.develop@gmail.com
# @create_date: 2021-12-04 20:23:59
# @last_modified_date: NO_LAST_MODIFIED_DATE
# @description: TODO
#---****************************************************************---

# Add extra Find module
  list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_modules)
# Find *Config.cmake
  # Example: find_package(OpenCV REQUIRED)
  if(WITH_GLOG)
    add_definitions(-DENABLE_GLOG)
  endif()
  #find_package(OpenCV)
  #find_package(Eigen3)
  set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
  set(THREADS_PREFER_PTHREAD_FLAG TRUE)
  find_package(Threads REQUIRED)

  find_program(MEMORYCHECK_COMMAND NAMES valgrind)
  set(CTEST_MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full")
  include(CTest)

# Set&Add LIB PATH/LINKING_DIRECTORIES
  # Example: link_directories(${CMAKE_INSTALL_PREFIX}/lib)
  link_directories(${CMAKE_INSTALL_PREFIX}/lib)
  link_directories(${CMAKE_CURRENT_BINARY_DIR}/lib)

  set(APPLE_LIBS ${COCOA_LIBRARY} ${IOKit_LIBRARY} ${OpenGL_LIBRARY} ${CoreVideo_LIBRARY})
  set(APPLE_LIBS ${APPLE_LIBS} ${GLFW3_LIBRARY} ${ASSIMP_LIBRARY})
  set(LIBS ${LIBS} ${APPLE_LIBS})

  set( THIRD_PARTY_LIBS
    ${THIRD_PARTY_LIBS}
  )
