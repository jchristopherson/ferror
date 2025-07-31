# CMake project file for FERROR
cmake_minimum_required(VERSION 3.17)
project(
  ferror 
  LANGUAGES
    C
    Fortran
  VERSION "1.4.3"
)
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED TRUE)

# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

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
add_fortran_library(
  ${PROJECT_NAME}
  ${PROJECT_INCLUDE_DIR}
  ${CMAKE_INSTALL_INCLUDEDIR}
  ${PROJECT_VERSION}
  ${PROJECT_VERSION_MAJOR}
  ${FERROR_SOURCES}
)
target_compile_options(
    ${PROJECT_NAME} 
    PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g>
    $<$<CONFIG:RELEASE>:-O3 -s>
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

    # If building a shared library, ensure the test directory gets access to the
    # library
    
    if (${BUILD_SHARED_LIBS})
      set(FERROR_SHARED_LIB_NAME ${CMAKE_SHARED_LIBRARY_PREFIX})
      string(APPEND FERROR_SHARED_LIB_NAME ${PROJECT_NAME})
      string(APPEND FERROR_SHARED_LIB_NAME ${CMAKE_SHARED_LIBRARY_SUFFIX})
      add_custom_command(
        TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        "${CMAKE_BINARY_DIR}/${FERROR_SHARED_LIB_NAME}"
        "${CMAKE_BINARY_DIR}/test"
      )
    endif()

    # Build the tests
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()
