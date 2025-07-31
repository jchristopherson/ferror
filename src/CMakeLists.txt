#Define sources
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

if (${BUILD_FERROR_C_API})
    list(
        APPEND FERROR_SOURCES
        ${dir}/ferror.f90
        ${dir}/ferror_implementation.f90 
        ${dir}/ferror_c_binding.f90
    )
else()
    list(
        APPEND FERROR_SOURCES
        ${dir}/ferror.f90
        ${dir}/ferror_implementation.f90
    )
endif()

set(FERROR_SOURCES ${FERROR_SOURCES} PARENT_SCOPE)
