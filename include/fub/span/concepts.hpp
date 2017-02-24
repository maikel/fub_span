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

	enum class dynamic_extents_tag { };
	constexpr dynamic_extents_tag dyn [[maybe_unused]] { -1 };
}

namespace fub::concepts::span
{
	///////////////////////////////////////////////////////////////////////////
	//                                                 CONCEPTS FOR DIMENSIONS
	// {{{

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

	template <ranges::Semiregular S, ranges::Regular Pointer>
	concept bool Storage() {
		return requires (const S& storage) {
			{ storage.size() } noexcept -> ranges::Integral;
			{ storage.data() } noexcept -> Pointer;
		};
	}

	template <typename D, typename T, Accessor<T> A>
	concept bool Decorator() {
		return requires {
			typename D::pointer;
			typename D::reference;
			typename D::iterator;
			typename D::sentinel;
		} && requires (const D& d) {
			{ d.base() } noexcept -> ranges::ConvertibleTo<const A&>;
		};
	}

	struct construction_error_tag{};

	template <typename P>
	concept bool ErrorPolicy() {
		return ranges::Invocable<P, construction_error_tag{}, bool(&)()>();
	}
}

#endif
