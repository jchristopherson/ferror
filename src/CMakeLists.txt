#Define sources
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

list(
    APPEND FERROR_SOURCES
    ${dir}/ferror.f90
    ${dir}/ferror_implementation.f90 
    ${dir}/ferror_c_binding.f90
)

set(FERROR_SOURCES ${FERROR_SOURCES} PARENT_SCOPE)
