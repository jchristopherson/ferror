# CMake project file for MSAT.
# EXAMPLE: https://cmake.org/Wiki/CMakeForFortranExample
# For .MOD file locations: https://cmake.org/pipermail/cmake/2010-May/037106.html, https://cmake.org/cmake/help/v3.0/prop_tgt/Fortran_MODULE_DIRECTORY.html
# For linking static libraries: http://stackoverflow.com/questions/14077611/how-do-i-tell-cmake-to-link-in-a-static-library-in-the-source-directory
cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
project(ferror)

# Specify output directories
if (CMAKE_BUILD_TYPE MATCHES Debug)
  # Debug Build
  if (BUILD_SHARED_LIBS)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin/mod)
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib/mod)
  endif ()
elseif (CMAKE_BUILD_TYPE MATCHES Release)
  # Release Build
  if (BUILD_SHARED_LIBS)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/bin)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/bin)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/bin)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Release/bin/mod)
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Release/lib)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Release/lib/mod)
  endif ()
else ()
  # Default Condition
  if (BUILD_SHARED_LIBS)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/bin/mod)
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/Debug/lib/mod)
  endif ()
endif ()


# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  # Options: https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# Define the library
add_library(ferror src/ferror.f90)


