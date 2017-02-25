//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_ERROR_POLICY_TERMINATE_HPP
#define FUB_ERROR_POLICY_TERMINATE_HPP

#include "fub/span/concepts.hpp"

namespace fub::error_policy
{
	struct ignore {
		template <typename Tag, ranges::Predicate Pred, typename... Args>
		constexpr void operator()(Tag, Pred pred [[maybe_unused]], Args&&...) const noexcept
		{}
	};
}

#endif
