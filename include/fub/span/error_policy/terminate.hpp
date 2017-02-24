//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_ERROR_POLICY_TERMINATE_HPP
#define FUB_ERROR_POLICY_TERMINATE_HPP

#include "fub/concepts.hpp"

namespace fub::error_policy
{
	struct terminate {
		ranges::Predicate{Pred}
		constexpr void run(Pred pred) const noexcept
		{
			if (!pred()) {
				std::terminate();
			}
		}

		template <typename Tag, ranges::Predicate Pred, typename... Args>
		constexpr void operator()(Tag, Pred pred, Args&&...) const noexcept
		{ run(std::move(pred)); }
	};
}

#endif
