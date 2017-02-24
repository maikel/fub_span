//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "fub/span/accessor/native.hpp"

#define CATCH_CONFIG_MAIN
#include <catch.hpp>

TEST_CASE("Concept fullfilled")
{
 	REQUIRE(( fub::concepts::span::Accessor<fub::accessor::native<int>, int>() ));
}

TEST_CASE("Types are native")
{
	REQUIRE(( fub::ranges::Same<fub::accessor::native<int>::pointer, int*>() ));
	REQUIRE(( fub::ranges::Same<fub::accessor::native<int>::reference, int&>() ));

	REQUIRE(( fub::ranges::Same<fub::accessor::native<int*>::pointer, int**>() ));
	REQUIRE(( fub::ranges::Same<fub::accessor::native<int*>::reference, int*&>() ));

	REQUIRE(( fub::ranges::Same<fub::accessor::native<int&>::pointer, int*>() ));
	REQUIRE(( fub::ranges::Same<fub::accessor::native<int&>::reference, int&>() ));
}