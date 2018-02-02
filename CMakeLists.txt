# CMake project file for FERROR
cmake_minimum_required(VERSION 3.0)
project(ferror C CXX Fortran)

# Define version information
set(linalg_VERSION_MAJOR 1)
set(linalg_VERSION_MINOR 3)
set(linalg_VERSION_PATCH 0)

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Define output directories, if undefined
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Put the example and test executables in the same directory as the shared 
# library.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    # gfortran
    # Options: https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html
    set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O1")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
    # ifort (untested)
    set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O1")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else()
    message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
    message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
    message ("No optimized Fortran compiler flags are known, we just try -O1...")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O1")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall")
endif()

# Build the library
add_subdirectory(src)

# ------------------------------------------------------------------------------
# INSTALLATION INSTRUCTINOS
# ------------------------------------------------------------------------------
# Define the installation instructions
install(DIRECTORY ${PROJECT_SOURCE_DIR}/include  
    DESTINATION ${CMAKE_INSTALL_PREFIX})
install(DIRECTORY ${CMAKE_BINARY_DIR}/mod DESTINATION fortran)
install(TARGETS ferror
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    CONFIGURATIONS Release)

# Include the documentation
install(DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html DESTINATION doc)

# TO DO: Investigate CPACK
# https://cmake.org/Wiki/CMake:Packaging_With_CPack
# https://cmake.org/pipermail/cmake/2010-June/037461.html

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
option(BUILD_FERROR_EXAMPLES "Build FERROR examples?" ON)
if (BUILD_FERROR_EXAMPLES)
    # Inform the user we're building the examples
    message(STATUS "Building FERROR examples.")

    # Build the examples
    add_subdirectory(examples)
endif()

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------
# C Based Testing
option(BUILD_FERROR_TESTS "Build FERROR tests?" ON)
if (BUILD_FERROR_TESTS)
    # Inform the user we're building the tests
    message(STATUS "Building FERROR tests.")

    # Build the tests
    add_subdirectory(test)
endif()

