# Get the macros and functions we'll need
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Install the library and necessary include files
install_library(
    ${PROJECT_NAME}
    ${CMAKE_INSTALL_LIBDIR}
    ${PROJECT_INCLUDE_DIR}
    ${CMAKE_INSTALL_INCLUDEDIR}
)

# Install the documentation files
install_documentation(
    ${PROJECT_SOURCE_DIR}/doc/html
    ${CMAKE_INSTALL_PREFIX}/doc/Fortran
)
install_documentation(
    ${PROJECT_SOURCE_DIR}/doc/C/html
    ${CMAKE_INSTALL_PREFIX}/doc/C
)

