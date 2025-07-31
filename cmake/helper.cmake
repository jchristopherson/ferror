# helper.cmake
#
# A collection of macros and functions making life with CMake and Fortran a
# bit simpler.

# Use to include and export headers
function(include_headers lib dir install_dir)
    target_include_directories(
        ${lib}
        INTERFACE
        $<BUILD_INTERFACE:${dir}>
        $<INSTALL_INTERFACE:${install_dir}>
    )
endfunction()

# Use instead of add_library.
function(add_fortran_library lib_name mod_dir include_install_dir version major)
    add_library(${lib_name} ${ARGN})
    set_target_properties(
        ${lib_name}
        PROPERTIES
            POSITION_INDEPENDENT_CODE TRUE
            OUTPUT_NAME ${lib_name}
            VERSION ${version}
            SOVERSION ${major}
            Fortran_MODULE_DIRECTORY ${include_install_dir}
    )
    target_include_directories(
        ${lib_name}
        PUBLIC
        $<BUILD_INTERFACE:${mod_dir}>
        $<INSTALL_INTERFACE:${include_install_dir}>
    )
endfunction()

# Links the supplied library
function(link_library targ lib include_dir)
    target_link_libraries(${targ} ${lib})
    target_include_directories(${targ} PUBLIC $<BUILD_INTERFACE:${include_dir}>)
endfunction()

# ------------------------------------------------------------------------------
# Helpful Macros
macro(print_all_variables)
    message(STATUS "---------- CURRENTLY DEFINED VARIABLES -----------")
    get_cmake_property(varNames VARIABLES)
    foreach(varName ${varNames})
        message(STATUS ${varName} = ${${varName}})
    endforeach()
    message(STATUS "---------- END ----------")
endmacro()
