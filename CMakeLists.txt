cmake_minimum_required(VERSION 3.12)
project(riddle)

# Set the Fortran compiler
enable_language(Fortran)

# Add your source files
set(SOURCES
    main.f90
)

# Set the output executable name
set(EXECUTABLE_NAME riddle.x)

# Create the executable
add_executable(${EXECUTABLE_NAME} ${SOURCES})

# Add target for execution (only on unix-like systems)
add_custom_target(run
    COMMAND $<TARGET_FILE:riddle.x>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Running the riddle"
)
