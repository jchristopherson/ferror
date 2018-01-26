#Define sources
set(ferror_sources ferror.f90 ferror_c_binding.f90)

# Build the library
find_library(FERROR_LIB NAMES ferror)
add_library(ferror ${ferror_sources})
set_property(TARGET ferror PROPERTY IMPORTED_LOCATION "${FERROR_LIB}")

# Define the installation instructions
install(DIRECTORY ${PROJECT_SOURCE_DIR}/include DESTINATION c)
install(DIRECTORY ${CMAKE_BINARY_DIR}/mod DESTINATION fortran)
install(TARGETS ferror
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)