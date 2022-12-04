# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")
include(CMakePackageConfigHelpers)

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

# Utilize the GNU installation structure
include(GNUInstallDirs)

# Locate the local include directory
set(PROJECT_INCLUDE_DIR ${PROJECT_SOURCE_DIR}/include)
set(PROJECT_INCLUDE_DIR ${PROJECT_INCLUDE_DIR} PARENT_SCOPE)

# Define where the config files will be dumped
set(PROJECT_CONFIG_FILE_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})
set(PROJECT_CONFIG_FILE_DIR ${PROJECT_CONFIG_FILE_DIR} PARENT_SCOPE)

set(PROJECT_CONFIG_FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake")
set(PROJECT_CONFIG_FILE ${PROJECT_CONFIG_FILE} PARENT_SCOPE)

set(PROJECT_VERSION_FILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake")
set(PROJECT_VERSION_FILE ${PROJECT_VERSION_FILE} PARENT_SCOPE)

# Configure
configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/template.cmake
  ${PROJECT_CONFIG_FILE}
  INSTALL_DESTINATION ${PROJECT_CONFIG_FILE_DIR}
)
write_basic_package_version_file(
  ${PROJECT_VERSION_FILE}
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMinorVersion
)
install(
  FILES
    ${PROJECT_CONFIG_FILE}
    ${PROJECT_VERSION_FILE}
  DESTINATION ${PROJECT_CONFIG_FILE_DIR}
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/template.pc
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
  @ONLY
)
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)