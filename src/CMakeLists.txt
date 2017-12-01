# This is the actual build script for the FERROR library.

set(ferror_sources ferror.f90 ferror_c_binding.f90)

add_library(ferror ${ferror_sources})