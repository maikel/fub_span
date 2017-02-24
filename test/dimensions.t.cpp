//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "fub/span/dimensions.hpp"

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

using namespace fub;

TEST_CASE("Fullfills Concept")
{
  REQUIRE(( Dimensions< dimensions<100> >() ));
  REQUIRE(( Dimensions< dimensions<dyn> >() ));

  REQUIRE(( Dimensions< dimensions<100, dyn> >() ));
  REQUIRE(( Dimensions< dimensions<dyn, 100> >() ));
  REQUIRE(( Dimensions< dimensions<dyn, dyn> >() ));
  REQUIRE(( Dimensions< dimensions<100, 100> >() ));
}

TEST_CASE("Uses EBO")
{
  REQUIRE( sizeof(dimensions<100>) == 1                            );
  REQUIRE( sizeof(dimensions<dyn>) == sizeof(std::ptrdiff_t)       );
  REQUIRE( sizeof(dimensions<100,100>) == 1                        );
  REQUIRE( sizeof(dimensions<100,dyn>) == sizeof(std::ptrdiff_t)   );
  REQUIRE( sizeof(dimensions<dyn,100>) == sizeof(std::ptrdiff_t)   );
  REQUIRE( sizeof(dimensions<dyn,dyn>) == 2*sizeof(std::ptrdiff_t) );
}

TEST_CASE("extent is constexpr")
{
  //constexpr auto extent_0 = dimensions<100, dyn>::extent<0>();
  //constexpr auto extent_1 = dimensions<100, dyn>::extent<1>();
  //REQUIRE( extent_0 == 100 );
  //REQUIRE( extent_1 == dyn );
}

TEST_CASE("Access dynamic dimensions")
{
  dimensions<100, dyn> dims{42};
  REQUIRE( dims[0] == 100 );
  REQUIRE( dims[1] ==  42 );
}

TEST_CASE("Cosntructible")
{
  REQUIRE(( ranges::Constructible<dimensions<1>>() ));
  REQUIRE(( ranges::Constructible<dimensions<dyn>>() ));
  REQUIRE(( ranges::Constructible<dimensions<dyn>, int>() ));
  REQUIRE(( ! ranges::Constructible<dimensions<dyn>, int, int>() ));
  REQUIRE(( ! ranges::Constructible<dimensions<1, dyn>, int, int>() ));
  REQUIRE(( ! ranges::Constructible<dimensions<1>, int>() ));
}

TEST_CASE("EqualityComparable")
{
  dimensions<100, dyn> dims_1{42};
  dimensions<100, dyn> dims_2{24};
  REQUIRE( dims_1 == dims_1 );
  REQUIRE( dims_2 == dims_2 );
  REQUIRE( dims_1 != dims_2 );
}

TEST_CASE("access extents from static rank")
{
  // REQUIRE(( dimensions<dyn, 100, 200>::extent<0>::value == dyn ));
  // REQUIRE(( dimensions<dyn, 100, 200>::extent<1>::value == 100 ));
  // REQUIRE(( dimensions<dyn, 100, 200>::extent<2>::value == 200 ));
}
