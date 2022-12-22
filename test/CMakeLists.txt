macro(create_test testname codename)
    add_executable(${testname} ${codename})
    target_link_libraries(${testname} ferror)
    target_include_directories(${testname} PUBLIC ${PROJECT_BINARY_DIR}/include)
    add_test(
        NAME ${testname}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${testname}>
    )
endmacro()

macro(create_c_test testname codename)
    add_executable(${testname} ${codename})
    target_link_libraries(${testname} ferror)
    add_test(
        NAME ${testname}
        WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        COMMAND $<TARGET_FILE:${testname}>
    )
endmacro()

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
)

create_test(ferror_test ferror_test.f90)
create_test(ferror_test_c ferror_test_c.c)