#Define sources
set(ferror_sources ferror.f90 ferror_c_binding.f90)

# Build the library
add_library(ferror ${ferror_sources})

# ------------------------------------------------------------------------------
# INSTALLATION INSTRUCTIONS
# ------------------------------------------------------------------------------
# Define target information
set_property(TARGET ferror PROPERTY VERSION ${FERROR_VERSION})
set_property(TARGET ferror PROPERTY SOVERSION ${FERROR_MAJOR_VERSION})
set_property(TARGET ferror PROPERTY INTERFACE_ferror_MAJOR_VERSION ${FERROR_MAJOR_VERSION})
set_property(TARGET ferror APPEND PROPERTY COMPATIBLE_INTERFACE_STRING ferror_MAJOR_VERSION)

# Locate the "include" directories
set(ferror_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/include)

# Define the installation instructions
install(TARGETS ferror EXPORT ferrorTargets
    RUNTIME DESTINATION ferror/bin
    LIBRARY DESTINATION ferror/lib
    ARCHIVE DESTINATION ferror/lib
    CONFIGURATIONS Release
    INCLUDES DESTINATION ferror/include
)
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include
    DESTINATION ${CMAKE_INSTALL_PREFIX}/ferror
    COMPONENT Devel
)

# Include the documentation
install(DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html DESTINATION ferror/doc)
install(FILES ${PROJECT_SOURCE_DIR}/doc/refman.pdf DESTINATION ferror/doc)

# Define the version file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_BINARY_DIR}/ferrorConfigVersion.cmake"
    VERSION ${FERROR_VERSION}
    COMPATIBILITY AnyNewerVersion
)

export(EXPORT ferrorTargets
    FILE "${CMAKE_BINARY_DIR}/ferrorTargets.cmake"
)

# Define the configuration file
configure_file(
    "${PROJECT_SOURCE_DIR}/ferrorConfig.cmake.in"
    "${CMAKE_BINARY_DIR}/ferrorConfig.cmake"
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
        "${CMAKE_BINARY_DIR}/ferrorConfig.cmake"
        "${CMAKE_BINARY_DIR}/ferrorConfigVersion.cmake"
    DESTINATION
        ${ConfigPackageLocation}
    COMPONENT
        Devel
)
