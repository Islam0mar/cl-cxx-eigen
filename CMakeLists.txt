cmake_minimum_required(VERSION 2.8.12)
project(clcxx-eigen)

set(PROJECT_PATH "${CMAKE_CURRENT_SOURCE_DIR}")

# Dependencies
# ============
find_library(CLCXX NAMES clcxx libclcxx.so PATHS "~/.local/lib" NO_DEFAULT_PATH)
find_package(Eigen3 REQUIRED)

# Options
set(CMAKE_VERBOSE_MAKEFILE true)
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

set(EXECUTABLE_OUTPUT_PATH "${PROJECT_PATH}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_PATH}/lib")
set(CMAKE_BINARY_DIR "${PROJECT_PATH}/bin")

# Source files
# ============

set(MY_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(MY_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

file(GLOB HEADER_GLOB
  ${MY_INCLUDE_DIR}/*.hpp
  )

file(GLOB SOURCES_GLOB
  ${MY_SOURCE_DIR}/*.cpp
  )

set(MY_HEADERS
  ${HEADER_GLOB}
  )

set(MY_SOURCES
  ${SOURCES_GLOB}
  )


# Output
# ======
add_library(${PROJECT_NAME} SHARED ${MY_SOURCES})

target_include_directories(${PROJECT_NAME} PUBLIC
  include
  "~/.local/include/"
  ${EIGEN3_INCLUDE_DIR}
)

target_compile_options(${PROJECT_NAME} PUBLIC "$<$<COMPILE_LANGUAGE:CXX>:-std=c++14>" "-Wall" "-ggdb3")


target_link_libraries(${PROJECT_NAME}
  ${CLCXX}
  "-Wl,--no-undefined" "-Wl,--no-allow-shlib-undefined"
)



