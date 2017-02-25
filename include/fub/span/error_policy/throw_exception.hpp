//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef ERROR_POLICY_THROW_EXCEPTION_HPP
#define ERROR_POLICY_THROW_EXCEPTION_HPP

#include "fub/span/concepts.hpp"

namespace fub::error_policy
{
	template <class Exception = std::logic_error>
	struct throw_exception {

		template <ranges::Predicate Pred>
		constexpr void run(Pred pred) const
		{
			if (!pred()) {
				throw Exception{"Span Expection"};
			}
		}

		template <ranges::Predicate Pred, typename... Args>
		constexpr void run(Pred pred, Args&&... what) const
		{
			if (!pred()) {
				throw Exception{std::forward<Args>(what)...};
			}
		}

		template <typename Tag, ranges::Predicate Pred, typename... Args>
		constexpr void operator()(Tag, Pred pred, Args&&...) const
		{ run(std::move(pred)); }
	};
}

#endif