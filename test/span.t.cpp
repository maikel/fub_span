//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "fub/span.hpp"
#include "fub/access_decorator/check_bounds.hpp"
#include "fub/error_policy/throw_exception.hpp"

#define CATCH_CONFIG_MAIN
#include <catch.hpp>

using namespace fub;
using storage = span<int>::storage;

TEST_CASE("empty")
{
  auto s = span<int>{};
  REQUIRE(s.size() == 0);
  REQUIRE(s.data() == nullptr);
}

TEST_CASE("uses ebo")
{
  REQUIRE( sizeof(span<int, 1>) == sizeof(int*) );
  REQUIRE( sizeof(span<int>)    == sizeof(int*) + sizeof(std::ptrdiff_t) );

  using check_bounds = access_decorator<access::check_bounds>;
  REQUIRE( sizeof(span<int, dyn, check_bounds>) == sizeof(span<int>) );
}

TEST_CASE("static case is not default constructible")
{
  REQUIRE(( !DefaultConstructible<span<int, 1>>() ));
}

TEST_CASE("construct from std::array")
{
  std::array<int, 2> xs = {1,2};
  span<const int> xv = xs;
  int count = 1;
  for (const int& x : xv) { REQUIRE(x == count++); }
}

template <typename T, auto Dim>
using span_ = fub::span<T, Dim, error_policy<ep::throw_exception<>> >;

TEST_CASE("construct from std::vector")
{
  std::vector<int> xs = {1,2};
  SECTION("dynamic lengths") {
    span<const int> xv = xs;
    REQUIRE(xv.size() == xs.size());
    int count = 1;
    for (const int& x : xv) { REQUIRE(x == count++); }
  }
  SECTION("static lengths") {
    span<const int, 2> xv = xs;
    REQUIRE(xv.size() == xs.size());
    int count = 1;
    for (const int& x : xv) { REQUIRE(x == count++); }
  }
  SECTION("wrong length") {
    REQUIRE_THROWS(( span_<const int, 3>(xs) ));
  }
}

TEST_CASE("subspan")
{
  auto vec = std::vector<int>{0,1,2,3,4,5,6,7,8,9};
  auto s = span<int>{vec};
  REQUIRE(s.size() == 10);
  REQUIRE(s[0] == 0);

  SECTION("static length")
  {
    auto sub = subspan<1, 4>(s);
    REQUIRE(( Same<decltype(sub), span<int, 4>>() ));
    REQUIRE( sub.data() == vec.data() + 1 );
    REQUIRE( sub.size() == 4 );
    REQUIRE( sub[0] == 1 );
  }

  SECTION("dynamic length")
  {
    auto sub = subspan(s, 1, 4);
    REQUIRE(( Same<decltype(sub), span<int>>() ));
    REQUIRE( sub.data() == vec.data() + 1 );
    REQUIRE( sub.size() == 4 );
    REQUIRE( sub[0] == 1 );
  }

  SECTION("drop types")
  {
    auto sub = take<10>(s);
    REQUIRE(( Same<decltype(sub), span<int, 10>>() ));
    REQUIRE(s == sub);

    auto d1 = drop<3>(s);
    auto d2 = drop<3>(sub);
    REQUIRE(( Same<decltype(d1), span<int>>() ));
    REQUIRE(( Same<decltype(d2), span<int, 7>>() ));
    REQUIRE(d1 == d2);
  }
}

// TEST_CASE("construct from native array")
// {
//   int xs[] = {1,2};
//   SECTION("dynamic lengths") {
//     span<const int> xv = xs;
//     REQUIRE(xv.size() == 2);
//   }
// }
