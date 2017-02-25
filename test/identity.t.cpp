//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "fub/span/accessor/native.hpp"
#include "fub/span/decorator/identity.hpp"
#include "fub/span/error_policy/terminate.hpp"

#define CATCH_CONFIG_MAIN
#include <catch.hpp>

TEST_CASE("Concepts")
{
	REQUIRE(( fub::concepts::span::Decorator<fub::decorator::identity, int,
		fub::accessor::native<int>>() ));

	REQUIRE(( fub::concepts::span::ErrorPolicy<fub::error_policy::terminate>() ));
}