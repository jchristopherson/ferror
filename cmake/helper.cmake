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
    add_library(${lib_name}lib ${ARGN})
    set_target_properties(
        ${lib_name}lib
        PROPERTIES
            POSITION_INDEPENDENT_CODE TRUE
            OUTPUT_NAME ${lib_name}
            VERSION ${version}
            SOVERSION ${major}
            Fortran_MODULE_DIRECTORY ${mod_dir}
    )
    target_include_directories(
        ${lib_name}lib
        PUBLIC
        $<BUILD_INTERFACE:${mod_dir}>
        $<INSTALL_INTERFACE:${include_install_dir}>
    )

    add_library(${lib_name} INTERFACE)
    target_link_libraries(${lib_name} INTERFACE ${lib_name}lib)
endfunction()

# Installs the library
function(install_library lib_name lib_install_dir mod_dir include_install_dir)
    install(
        TARGETS
            ${lib_name}
            ${lib_name}lib
        EXPORT ${lib_name}Targets
        LIBRARY DESTINATION ${lib_install_dir}
        ARCHIVE DESTINATION ${lib_install_dir}
    )
    install(
        EXPORT ${lib_name}Targets
        NAMESPACE "${lib_name}::"
        DESTINATION ${lib_install_dir}/cmake/${lib_name}
    )
    install(
        DIRECTORY ${mod_dir}
        DESTINATION ${include_install_dir}
    )
endfunction()

# Install the documentation files
function(install_documentation doc_dir install_dir)
    install(
        DIRECTORY ${doc_dir}
        DESTINATION ${install_dir}
    )
endfunction()

# ------------------------------------------------------------------------------
# Helpful Macros
macro(print_all_variables)
    message(STATUS "---------- CURRENTLY DEFIND VARIABLES -----------")
    get_cmake_property(varNames VARIABLES)
    foreach(varName ${varNames})
        message(STATUS ${varName} = ${${varName}})
    endforeach()
    message(STATUS "---------- END ----------")
endmacro()