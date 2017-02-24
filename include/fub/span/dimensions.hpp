//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

/// This file defines concepts around the span module

#ifndef FUB_DIMENSIONS_H
#define FUB_DIMENSIONS_H

#include "fub/span/concepts.hpp"

namespace fub
{
	namespace detail
	{
		/// Base case. This happens if we access a rank which is out of bounds
		ranges::Integral{I}
		constexpr I extent(I)
		noexcept
		{
			return 0;
		}

		/// Returns n-th Extent as an optional integral
		///
		/// If the n-th Extent is a dynamic one, it returns an empty optional.
		/// Recursion case.
		template <ranges::Integral I, concepts::span::Extent Head, concepts::span::Extent... Tail>
		constexpr std::optional<I>
		extent(I idx, Head head [[maybe_unused]], Tail... tail)
		noexcept
		{
			if (idx == 0) {
				if constexpr(ranges::Integral<Head>()) {
					return head;
				} else {
					return {};
				}
			}
			return extent(idx - 1, tail...);
		}

		/// value version of std::conditional, `true` case returns `T`.
		template <bool B, auto T, auto F>
		struct conditional { static constexpr auto value = T; };

		/// value version of std::conditional, `false` case returns `F`.
		template <auto T, auto F>
		struct conditional<false, T, F> { static constexpr auto value = F; };

		/// templated version of extent(i, Dims...)
		///
		/// This is better but sometimes causes ICE in certain circumstances.
		/// If these get fixed in GCC we can probably get rid of the other
		/// function.
		///
		/// Why is this version better? It doesnt return an optional but
		/// it returns a dynamic_dimension_tag if the n-th Extent is dynamic.
		template <std::size_t N, auto... Dims>
		struct static_extent;

		/// Out of Range or Base case
		template <std::size_t N>
		struct static_extent<N> {
			static constexpr std::size_t value = 0;
		};

		/// Recursion case
		template <std::size_t N, auto Dim, auto... Dims>
		struct static_extent<N, Dim, Dims...> {
			static constexpr auto value =
				conditional<
						(N == 0),
						Dim,
						static_extent<N-1, Dims...>::value
				>::value;
		};

		/// \brief helper function, base case (end of count)
		ranges::Integral{I}
		constexpr std::size_t
		count_dynamic_dimensions_(I count)
		noexcept
		{
			return count;
		}

		/// \brief helper function, recursion
		template <ranges::Integral I, concepts::span::Extent Head, concepts::span::Extent... Tail>
		constexpr std::size_t
		count_dynamic_dimensions_(I count, Head, Tail... tail)
		noexcept
		{
			if constexpr(concepts::span::DynamicExtent<Head>()) {
				return count_dynamic_dimensions_(count+1, tail...);
			}
			return count_dynamic_dimensions_(count, tail...);
		}

		/// \brief Returns number of dynamic dimensions in the template parameter pack
		template <auto... Dims>
		constexpr std::size_t
		count_dynamic_dimensions()
		noexcept
		{
			return count_dynamic_dimensions_(0, Dims...);
		}

		// }}}

		/// \brief Helper function, base case
		template <ranges::Integral I, ranges::Integral J>
		constexpr I
		count_dynamic_dimensions_until_(I count, J)
		noexcept
		{
			return count;
		}

		/// \brief Helper function, recursion
		template <
			ranges::Integral I,
			ranges::Integral J,
			concepts::span::Extent Head,
			concepts::span::Extent... Tail
		>
		constexpr I
		count_dynamic_dimensions_until_(I count, J idx, Head, Tail... tail)
		noexcept
		{
			if (idx == 0) {
				return count;
			}
			if constexpr (concepts::span::DynamicExtent<Head>()) {
				return count_dynamic_dimensions_until_(count+1, idx-1, tail...);
			}
			return count_dynamic_dimensions_until_(count, idx-1, tail...);
		}

		/// \brief Returns number of dynamic dimensions up to an given index idx.
		template <ranges::Integral I, concepts::span::Extent... Dims>
		constexpr std::size_t
		count_dynamic_dimensions_until(I idx, Dims... dims)
		noexcept
		{
			return count_dynamic_dimensions_until_(0, idx, dims...);
		}
	} // namespace detail }}}

	/// \brief Owns N dynamic dimensions and is an empty class for N = 0
	template <std::size_t N>
	struct dynamic_dimensions_storage;

	template <std::size_t N>
	constexpr bool operator==(const dynamic_dimensions_storage<N>& lhs,
			const dynamic_dimensions_storage<N>& rhs) noexcept;

	template <std::size_t N>
	constexpr bool operator!=(const dynamic_dimensions_storage<N>& lhs,
			const dynamic_dimensions_storage<N>& rhs) noexcept;

	/// Empty class case
	template <>
	struct dynamic_dimensions_storage<0>
	{
		using value_type = int;
		using index_type = int;
		using size_type = std::size_t;

		static constexpr size_type size() noexcept
		{ return 0; }

		constexpr value_type at(index_type) const noexcept
		{ return 0; }
	};

	template <>
	constexpr bool operator==(const dynamic_dimensions_storage<0>&,
			const dynamic_dimensions_storage<0>&) noexcept
	{ return true; }

	template <>
	constexpr bool operator!=(const dynamic_dimensions_storage<0>&,
			const dynamic_dimensions_storage<0>&) noexcept
	{ return false; }


	/// Contains N > 0 elements
	template <std::size_t N>
	struct dynamic_dimensions_storage
	{
		using value_type = std::ptrdiff_t;
		using index_type = std::ptrdiff_t;
		using size_type  = std::size_t;

		/// stores dynamic extents in a static array
		std::array<value_type, N> m_data;

		static constexpr std::size_t size() noexcept
		{ return N; }

		constexpr decltype(auto) at(index_type i) const noexcept
		{ return m_data[i]; }
	};

	template <std::size_t N>
	constexpr bool operator==(const dynamic_dimensions_storage<N>& lhs,
			const dynamic_dimensions_storage<N>& rhs) noexcept
	{ return lhs.m_data == rhs.m_data; }

	template <std::size_t N>
	constexpr bool operator!=(const dynamic_dimensions_storage<N>& lhs,
			const dynamic_dimensions_storage<N>& rhs) noexcept
	{ return !(lhs == rhs); }

	/// \brief This class stores dimensional extents.
	///
	/// An extent in each dimension can be either dynamically set at runtime
	/// or known statically at compile time. Each static dimension does not
	/// cost any memory. We call a set of dimensions static, if all extents
	/// are known to compile time and dynamic otherwise.
	///
	/// The class is mostly used to decode length information of one-dimensional
	/// and multi-dimensional arrays in a compact sense. This class can be used
	/// in combination with the empty base class optimization.
	template <auto... Dims>
	requires (
			sizeof...(Dims) > 0 &&
			(true && ... && concepts::span::Extent<decltype(Dims)>())
	)
	class dimensions
		: dynamic_dimensions_storage<detail::count_dynamic_dimensions<Dims...>()>
	{
		private:
			using storage = dynamic_dimensions_storage<detail::count_dynamic_dimensions<Dims...>()>;

			constexpr const storage& data() const noexcept
			{ return static_cast<const storage&>(*this); }

		public:
			using value_type = typename storage::value_type;
			using index_type = typename storage::index_type;
			using rank_type  = typename storage::size_type;

			template <typename... DynDims>
			requires (sizeof...(DynDims) <= detail::count_dynamic_dimensions<Dims...>())
			constexpr dimensions(DynDims... dims) noexcept
				: storage{static_cast<value_type>(dims)...}
			{}

			/// Returns the number of total dimensions in Dims...
			static constexpr rank_type rank()
			noexcept
			{ return sizeof...(Dims); }

			/// Returns the number of dynamic dimensions in Dims...
			static constexpr rank_type rank_dynamic()
			noexcept
			{ return detail::count_dynamic_dimensions<Dims...>(); }

			/// Checks wheter the extent at index is a dynamic one.
			static constexpr bool is_dynamic_dimension(rank_type r)
			noexcept
			{ return !extent__(r); }

			// TODO THIS CAUSES AN ICE IN G++
			// template <std::size_t Rank>
			// struct extent : detail::static_extent<Rank, Dims...> {};

			/// Returns the extents at dimension r as an integral,
			/// even for dynamic ones.
			constexpr index_type operator[](rank_type r) const
			noexcept
			{
				if (rank() <= r) {
					return 0;
				}
				auto ext = extent__(r);
				if (!ext) {
					auto idx = detail::count_dynamic_dimensions_until(r, Dims...);
					return data().at(idx);
				}
				return ext.value();
			}

			/// Attempts to access a static dimensions in Dims... at an index.
			/// In case of an dynamic extent at that index it returns an empty
			/// optional.
			static constexpr std::optional<index_type> extent__(rank_type r)
			noexcept
			{ return detail::extent<index_type>(r, Dims...); }

			/// Equality of dimensions mean equality in each extent (Type AND Value).
			friend
			constexpr bool operator==(const dimensions<Dims...>& lhs,  const dimensions<Dims...>& rhs)
			noexcept
			{ return lhs.data() == rhs.data(); }
	};

	/// `!(dim1 == dim2)`
	template <auto... Dims>
	constexpr bool operator!=(const dimensions<Dims...>& lhs, const dimensions<Dims...>& rhs)
	noexcept
	{ return !(lhs == rhs); }

	// }}}
}

#endif
