//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef ACCESS_DECORATOR_IDENTITY_HPP
#define ACCESS_DECORATOR_IDENTITY_HPP

#include "fub/span/concepts.hpp"

namespace fub::decorator
{
	struct identity {
		template <typename T, Accessor<T> A>
		struct accessor
			: ranges::detail::ebo_box<A, accessor>
		{
			using base_t = ranges::detail::ebo_box<A, accessor>;
			using pointer = typename A::pointer;
			using reference = typename A::reference;
			using iterator = pointer;
			using sentinel = pointer;

			constexpr const A& base() const noexcept
			{ return get(); }

			template <SpanStorage<pointer> S, Integral I, ErrorPolicy EP>
			constexpr reference access(const S& s, I i, const EP& error_policy [[maybe_unused]])
			const noexcept(noexcept(std::declval<base_t>().access(std::declval<pointer>(), 0)))
			{ return base().access(s.data(), i); }

			template <SpanStorage<pointer> S>
			constexpr iterator begin(const S& s) const noexcept
			{ return s.data(); }

			template <SpanStorage<pointer> S>
			constexpr iterator end(const S& s) const noexcept
			{ return s.data() + s.size(); }
		};
	};
}

#endif
