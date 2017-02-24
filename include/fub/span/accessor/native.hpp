//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_ACCESSOR_NATIVE_HPP
#define FUB_ACCESSOR_NATIVE_HPP

#include "fub/span/concepts.hpp"

namespace fub::accessor
{
	template <typename ValueType>
	struct native
	{
		// TYPES
		using value_type = ValueType;
		using pointer    = std::add_pointer_t<value_type>;
		using reference  = std::add_lvalue_reference_t<value_type>;

		/// \brief Returns `p[idx]`.
		ranges::Integral{I}
		constexpr reference access(pointer p, I idx) const noexcept
		{ return p[idx]; }
	};
}

#endif
