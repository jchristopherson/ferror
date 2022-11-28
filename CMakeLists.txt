# CMake project file for FERROR
cmake_minimum_required(VERSION 3.0)
project(ferror C CXX Fortran)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

# Define version information
set(FERROR_MAJOR_VERSION 1)
set(FERROR_MINOR_VERSION 4)
set(FERROR_PATCH_VERSION 0)
set(FERROR_VERSION ${FERROR_MAJOR_VERSION}.${FERROR_MINOR_VERSION}.${FERROR_PATCH_VERSION})

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# By default, static library
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# Export all symbols on Windows when building libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Locate the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

# Define the include directories
include_directories(${PROJECT_SOURCE_DIR}/include)

# Build the library
add_subdirectory(src)

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
# C Based Testing
option(BUILD_TESTING "Build tests")
if (BUILD_TESTING)
    # Inform the user we're building the tests
    message(STATUS "Building FERROR tests.")

    # Build the tests
    include(CTest)
    enable_testing()
    add_subdirectory(test)
endif()
