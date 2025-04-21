cmake_minimum_required(VERSION 3.16)
set(PROJECT_NAME math_dsl)
project(
  ${PROJECT_NAME}
  VERSION 0.1
  LANGUAGES CXX)

# set c standar
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# export json for neovim
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# define source files
set(SOURCE_FILES src/main.cpp)
add_executable(${PROJECT_NAME} ${SOURCE_FILES})

# define include directory
set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)
target_include_directories(${PROJECT_NAME} PRIVATE ${INCLUDE_DIR})

# Use system wide installed Catch2
find_package(Catch2 3 REQUIRED)

#define test files
set(TEST_FILES test/math_expr_test.cpp)
add_executable(${PROJECT_NAME}_test ${TEST_FILES})

# define include directory
target_link_libraries(${PROJECT_NAME}_test PRIVATE Catch2::Catch2WithMain)
target_include_directories(${PROJECT_NAME}_test PRIVATE ${INCLUDE_DIR})
