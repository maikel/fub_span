//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_CONCEPTS_HPP
#define FUB_CONCEPTS_HPP

#include <experimental/ranges/concepts>

namespace fub
{
	namespace ranges = std::experimental::ranges;

	///////////////////////////////////////////////////////////////////////////
	//                                                 CONCEPTS FOR DIMENSIONS
	// {{{

	enum class dynamic_extents_tag { };
	constexpr dynamic_extents_tag dyn [[maybe_unused]] { -1 };


	template <class T>
	concept bool DynamicExtent() {
		return ranges::Same<T, dynamic_extents_tag>();
	}

	template <auto T>
	concept bool DynamicExtent() {
		return DynamicExtent<decltype(T)>();
	}

	template <class E>
	concept bool Extent() {
		return ranges::Integral<E>() || DynamicExtent<E>();
	}

	// template <auto E>
	// concept bool Extent() {
	// 	return Extent<decltype(E)>();
	// }

	template <class D>
	concept bool Dimensions() {
		return requires (const D& dims, std::ptrdiff_t r) {
			{ D::rank() } noexcept                  -> ranges::Integral;
			{ D::rank_dynamic() } noexcept          -> ranges::Integral;
			{ D::is_dynamic_dimension(r) } noexcept -> ranges::Boolean;
			{ dims[r] } noexcept                    -> ranges::Integral;
		};
	}

	// }}}

	///////////////////////////////////////////////////////////////////////////
	//                                                       CONCEPTS FOR SPAN

	/// An Accessor type defines pointer, references and how to access data.
	template <typename A, typename ElementType>
	concept bool Accessor() {
		return requires {
			typename A::pointer;
			typename A::reference;
			ranges::Regular<typename A::pointer>();
			ranges::ConvertibleTo<ElementType*, typename A::pointer>();
			ranges::ConvertibleTo<
					typename A::reference,
					std::add_lvalue_reference_t<ElementType>
			>();
		} &&
		requires (const A& a, typename A::pointer ptr, std::ptrdiff_t i) {
			{ a.access(ptr, i) } -> typename A::reference;
		};
	}
}

#endif
