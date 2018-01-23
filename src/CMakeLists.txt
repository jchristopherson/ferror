#Define sources
set(ferror_sources ferror.f90 ferror_c_binding.f90)

# Build the library
add_library(ferror ${ferror_sources})