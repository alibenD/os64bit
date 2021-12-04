#---Automatically Generated from template 'option.cmake' wrote by @aliben---
#
# @Copyright (C) 2021 All rights reserved.
# @filename: option.cmake
# @author: aliben.develop@gmail.com
# @create_date: 2021-12-04 20:23:59
# @last_modified_date: NO_LAST_MODIFIED_DATE
# @description: TODO
#---****************************************************************---

#project name

# Option
  option(ENABLE_ALL_WARNING ON)
  option(ENABLE_WARNING_AS_ERROR OFF)
  option(ENABLE_WARNING_EXTRA ON)
  option(ENABLE_VERBOSE ON)
  option(ENABLE_PROFILER OFF)

  option(BUILD_GTESTS "Build test example cases with google test" OFF)
  option(WITH_GLOG "Enable google log in this case" OFF)

  option(BUILD_SHARED_LIBS "Build shared libraries" ON)

# CMake Build System Default Setup
if(CMAKE_BUILD_TYPE)
else()
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Default Build Type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Release Debug RelWithDebInfo MinSizeRel)
endif()

