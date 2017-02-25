cmake_minimum_required (VERSION 3.6)

project(fub_span CXX)

add_definitions('-g')
add_definitions('-Wall')
add_definitions('-Wextra')
add_definitions('-Wfatal-errors')
add_definitions('-std=c++1z')
add_definitions('-fconcepts')
add_definitions('-pedantic')

include_directories("${PROJECT_SOURCE_DIR}/include")
include_directories("${PROJECT_SOURCE_DIR}/third-party/cmcstl2/include")
include_directories("${PROJECT_SOURCE_DIR}/third-party/Catch/include")

add_executable(dimensions "test/dimensions.t.cpp")
add_executable(accessor_native "test/native.t.cpp")
add_executable(span "test/span.t.cpp")
