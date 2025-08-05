# CMake project file for FERROR
cmake_minimum_required(VERSION 3.17)
project(
  ferror 
  LANGUAGES
    C
    Fortran
  VERSION "1.4.4"
)
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED TRUE)

# Build the C API?
option(BUILD_FERROR_C_API "Build FERROR C API?" OFF)

# Configure everything
add_subdirectory(configure)

# Determine if IFORT is being used
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  # IFORT uses a different IDATE and ITIME routine than GFORTRAN.  This flag
  # informs the code to be aware of the switch.
  set(CMAKE_Fortran_FLAGS "-fpp -DIFORT")
else()
  set(CMAKE_Fortran_FLAGS "-cpp")
endif()

# Build the library
add_subdirectory(src)

# Installation Instructions
install(
  EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
option(BUILD_FERROR_EXAMPLES "Build FERROR examples?" OFF)
if (BUILD_FERROR_EXAMPLES)
    # Inform the user we're building the examples
    message(STATUS "Building FERROR examples.")

    # Build the examples
    add_subdirectory(examples)
endif()

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------
option(BUILD_TESTING "Build FERROR tests")
if (BUILD_TESTING)
    # Inform the user we're building the tests
    message(STATUS "Building FERROR tests.")

    # Build the tests
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()
