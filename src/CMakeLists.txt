#Define sources
set(ferror_sources 
    ferror.f90 
    ferror_implementation.f90 
    ferror_c_binding.f90
    ferror_c_implementation.f90
)

# Build the library
add_library(ferror ${ferror_sources})
