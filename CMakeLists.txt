# CMake project file for FERROR
cmake_minimum_required(VERSION 3.0)
project(
  ferror 
  LANGUAGES
    C
    Fortran
  VERSION "1.4.0"
)

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
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/mod)

# Define the include directories
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/mod)
set(MODULE_DIR ${PROJECT_SOURCE_DIR}/mod)

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

# ------------------------------------------------------------------------------
# INSTALLATION INSTRUCTIONS
# ------------------------------------------------------------------------------
# Locate the "include" directories
set(ferror_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/include)
list(APPEND ferror_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/mod)

# Define the installation instructions
install(TARGETS ferror EXPORT ferrorTargets
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    CONFIGURATIONS Release
    INCLUDES DESTINATION include
)
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include
    DESTINATION ${CMAKE_INSTALL_PREFIX}
)
install(
  DIRECTORY ${MODULE_DIR}
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)

# Include the documentation
install(
  DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html 
  DESTINATION ${CMAKE_INSTALL_PREFIX}/doc/Fortran
)
install(
  DIRECTORY ${PROJECT_SOURCE_DIR}/doc/C/html
  DESTINATION ${CMAKE_INSTALL_PREFIX}/doc/C
)

# Configure
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    ${CMAKE_BINARY_DIR}/{$PROJECT_NAME}ConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion
)
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

configure_file(
    ${PROJECT_SOURCE_DIR}/ferrorConfig.cmake.in
    ${CMAKE_BINARY_DIR}/ferrorConfig.cmake
    COPYONLY
)
set(ConfigPackageLocation ferror/lib/cmake/ferror)
install(
    EXPORT ferrorTargets
    FILE ferrorTargets.cmake
    DESTINATION ${ConfigPackageLocation}
)
install(
    FILES
        ${CMAKE_BINARY_DIR}/ferrorConfig.cmake
        ${CMAKE_BINARY_DIR}/ferrorConfigVersion.cmake
    DESTINATION
        ${ConfigPackageLocation}
    COMPONENT
        Devel
)
