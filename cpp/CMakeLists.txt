cmake_minimum_required(VERSION 3.20)
project(rocket-cpp)
include_directories(include)
add_executable(rocket-cpp src/rocket.cpp)
target_link_libraries(rocket-cpp fmt tiff)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O0")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -std=c++20")
set(CMAKE_CXX_COMPILER /usr/local/bin/clang++)
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
set(CMAKE_GENERATOR "Ninja")

# IT'S GREAT! If I run in release the NDEBUG flag is off :D 
SET(CMAKE_BUILD_TYPE Debug)
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE Release)
  set_directory_properties(PROPERTIES COMPILE_DEFINITIONS "NDEBUG")
ENDIF()


find_package(fmt REQUIRED)
