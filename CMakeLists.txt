cmake_minimum_required(VERSION 3.13)
message(STATUS "Using CMake version ${CMAKE_VERSION}.")

# Policies
cmake_policy(SET CMP0048 NEW)  # The project() command manages VERSION variables.
cmake_policy(SET CMP0054 NEW)  # Only interpret if() arguments as variables or keywords when unquoted.
cmake_policy(SET CMP0074 NEW)  # find_package() uses <PackageName>_ROOT variables

# Project settings
project(ptools VERSION 1.0.0 LANGUAGES Fortran)
set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-${PROJECT_VERSION})

# Options
option(BUILD_DOC          "Building developer documentation."         OFF)
option(BUILD_DOC_INTERNAL "Building internal developer documentation" OFF)

# Fortan settings
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules CACHE PATH "Single directory for all Fortran modules.")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fall-intrinsics -funroll-loops -frecursive")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -std=f2008 -fall-intrinsics -funroll-loops -frecursive -Wall -Wextra -Warray-temporaries -Wpedantic -Wrealloc-lhs-all -fcheck=all -g")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -warn all -check all")
endif ()

# OpenMP
find_package(OpenMP 4.5 REQUIRED COMPONENTS Fortran)

# MPI
find_package(MPI 3.1 REQUIRED COMPONENTS Fortran)

# Doxygen
if (BUILD_DOC)
  find_package(Doxygen)
endif ()

# Installation directories
include(GNUInstallDirs)

# Add ptools library itself.
add_subdirectory(src)

# Tests
enable_testing()
add_subdirectory(tests)

# Install cmake files.
set(ptools_CONFIG_IN  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/ptools-config.cmake.in)
set(ptools_CONFIG_OUT ${CMAKE_CURRENT_BINARY_DIR}/cmake/ptools-config.cmake)
configure_file(${ptools_CONFIG_IN} ${ptools_CONFIG_OUT} @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/ptools-config.cmake
        DESTINATION cmake)
set(ptools_CONFIG_VERSION_IN  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/ptools-config-version.cmake.in)
set(ptools_CONFIG_VERSION_OUT ${CMAKE_CURRENT_BINARY_DIR}/cmake/ptools-config-version.cmake)
configure_file(${ptools_CONFIG_VERSION_IN} ${ptools_CONFIG_VERSION_OUT} @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/cmake/ptools-config-version.cmake
        DESTINATION cmake)

message(STATUS "Fortran compiler summary")
message("   Fortran compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
message("   Fortran compiler:    ${CMAKE_Fortran_COMPILER}")
if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  message("   Flags (debug):       ${CMAKE_Fortran_FLAGS_DEBUG}")
elseif ("${CMAKE_BUILD_TYPE}" STREQUAL "Release")
  message("   Flags (release):     ${CMAKE_Fortran_FLAGS_RELEASE}")
else ()
  message("   Flags:               ${CMAKE_Fortran_FLAGS}")
endif ()
