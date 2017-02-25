//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "fub/span.hpp"
#include <array>
#include <numeric>

namespace fub
{
	int sum(fub::span<int, 10> xs)
	{
		return std::accumulate(xs.begin(), xs.end(), 0);
	}
}

int main()
{
	std::array<int, 10> xs = {0,1,2,3,4,5,6,7,8,9};
	return fub::sum(xs);
}