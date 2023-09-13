include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

add_executable(ferror_test ferror_test.f90)
link_library(ferror_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
add_test(
    NAME ferror_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:ferror_test>
)

if (${BUILD_C_API})
    include_directories(
        ${CMAKE_CURRENT_SOURCE_DIR}
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    )
    add_executable(ferror_test_c C/ferror_test_c.c)
    link_library(ferror_test_c ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
    add_test(
        NAME ferror_test_c
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:ferror_test_c>
    )
endif()
