//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_SPAN_SPAN_HPP
#define FUB_SPAN_SPAN_HPP

#include "fub/span/dimensions.hpp"
#include "fub/span/accessor/native.hpp"
#include "fub/span/decorator/identity.hpp"
#include "fub/span/error_policy/terminate.hpp"

namespace fub
{
	/// \brief Store a pointer and length.
	///
	/// This class makes use of the empty base class optimization in the case
	/// of static lengths, if known to compile time.
	template <ranges::Regular Pointer, Dimensions Dims>
		requires (Dims::rank() == 1)
	class span_storage
		: ranges::detail::ebo_box<Dims, span_storage>
	{
		private:
			// TYPES
			using base = ranges::detail::ebo_box<Dims, span_storage>;
			using base::get;
		public:
			using pointer = Pointer;
			using dimensions_type = Dims;

			// CONSTRUCTORS

			/// \brief Constructs an empty storage
			///
			/// This is only for possible for dynamic lengths
			/// \TODO if static extents works, make this possible for static length 0
			constexpr span_storage()
			noexcept(
					std::is_nothrow_default_constructible_v<dimensions_type> &&
					std::is_nothrow_default_constructible_v<pointer>
			)
			requires (
					DefaultConstructible<base>() &&
					dimensions_type::is_dynamic_dimension(0)
			) = default;

			/// \brief Constructs storage from a pointer and length.
			///
			/// This constructor kicks in for dynamic dimensions
			ranges::Integral{I}
			constexpr span_storage(pointer data, I extent)
			noexcept (
				std::is_nothrow_constructible<dimensions_type, I>::value
				&& std::is_nothrow_copy_constructible<pointer>::value
			)
			requires (
					Constructible<base, I>() &&
					dimensions_type::is_dynamic_dimension(0)
			)
			: base{extent}
			, m_data{data}
			{ /* empty */ }

			/// \brief Constructs storage from a pointer
			///
			/// This constructor kicks in for static dimensions
			constexpr span_storage(pointer data)
			noexcept (
				std::is_nothrow_default_constructible<dimensions_type>::value
				&& std::is_nothrow_copy_constructible<pointer>::value
			)
			requires (
					DefaultConstructible<base>() &&
					!dimensions_type::is_dynamic_dimension(0)
			)
			: base{}
			, m_data{data}
			{ /* empty */ }

			// MEMBER ACCESS

			/// Returns the pointer which is wrapped
			constexpr pointer data() const noexcept
			{ return m_data; }

			/// Returns the number of elements which can be accessed through data()
			constexpr auto size() const noexcept
			{ return get()[0]; }

		private:
			/// A non-owning pointer to the first element of an array
			pointer m_data;
	};
	// }}}

	namespace detail::span
	{
	// {{{

		//////////////////////////////////////////////////////////////////////////
		//                                              [function.accessor<Ts...>]
		// {{{

		/// Find an accessor tagged type in Ps
		//
		template <typename T, typename... Ps>
		struct accessor;

		/// Default case returns the native accessor
		//
		template <typename T>
		struct accessor<T> {
			using type = access::native<T>;
		};

		/// Recursion
		//
		template <typename T, typename P, typename... Ps>
		struct accessor<T, P, Ps...> {
			using type = std::conditional_t<
				is_access_policy<P>::value,
				meta::_t<P>,
				meta::_t<accessor<T, Ps...>>
			>;
		};
		// }}}

		//////////////////////////////////////////////////////////////////////////
		//                                      [function.access_decorator<Ts...>]
		// {{{

		/// Find a access_decorator tagged type in Ps
		//
		template <typename... Ps>
		struct access_decorator;

		/// Default is to terminate on out of range accesses
		//
		template <>
		struct access_decorator<> {
			using type = access::check_bounds;
		};

		/// Recursion
		//
		template <typename P, typename... Ps>
		struct access_decorator<P, Ps...> {
			using type = std::conditional_t<
				is_access_decorator<P>::value,
				meta::_t<P>,
				meta::_t<access_decorator<Ps...>>
			>;
		};
		// }}}

		//////////////////////////////////////////////////////////////////////////
		//                                                [function.span_accessor]
		// {{{

		/// Constructs a span accessor based on a decorator and standard accessor
		//
		template <typename T, typename... Ps>
		struct span_accessor {
			using pointer_access = meta::_t<accessor<T, Ps...>>;
			using decorator = meta::_t<access_decorator<Ps...>>;
			using type = typename decorator::template accessor<pointer_access>;
		};

		/// Comforting typedef
		//
		template <typename T, typename... Ps>
		using span_accessor_t = meta::_t<span_accessor<T, Ps...>>;

		// }}}

		//////////////////////////////////////////////////////////////////////////
		//                                                 [function.error_policy]
		// {{{
		template <typename... Ps>
		struct error_policy;

		template <>
		struct error_policy<> { using type = ep::terminate; };

		template <typename P, typename... Ps>
		struct error_policy<P, Ps...> {
			using type = std::conditional_t<
				SpanErrorPolicy<P>(),
				meta::_t<P>,
				meta::_t<error_policy<Ps...>>
			>;
		};

		template <typename... Ps>
		using error_policy_t = meta::_t<error_policy<Ps...>>;
		// }}}

	// }}}
	}

	////////////////////////////////////////////////////////////////////////////
	//                                                           ERROR CHECKING
	// {{{

	////////////////////////////////////////////////////////////////////////////
	//                                                        [function.expects]
	// {{{
	template <typename ErrorPolicy, ranges::Invocable Check>
	void expects(const ErrorPolicy& policy, Check&& invariant)
	noexcept(noexcept(
			std::declval<const ErrorPolicy&>().run(
				expects_tag{},
				std::declval<Check>(),
				"Precondition Failure"
			)
	))
	{
		policy.run(
				expects_tag{},
				std::forward<Check>(invariant),
				"Precondition Failure"
		);
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                             [function.construction_check]
	// {{{
	template <typename ErrorPolicy, ranges::Invocable Check>
	void construction_check(const ErrorPolicy& ep, Check&& invariant)
	noexcept(noexcept(
			std::declval<const ErrorPolicy&>().run(
				construction_tag{},
				std::declval<Check>(),
				"Span Construction Error"
			)
	))
	{
		ep.run(
				construction_tag{},
				std::forward<Check>(invariant),
				"Span Construction Error"
		);
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                              [function.is_nothrow_access]
	// {{{
	template <typename T, typename... Args>
	struct is_nothrow_access {
		static constexpr bool value = noexcept(
				std::declval<const T&>().access(std::declval<Args>()...)
		);
	};
	// }}}

	// }}}


	template <
			typename ElementType,
			auto Extent = dyn,
			typename... Properties
	>
	requires (
			SpanPolicies<ElementType, Properties...>() &&
			NoDuplicateSpanPolicies<Properties...>()
	)
	class span
		: ranges::ext::compressed_pair<
				detail::decorated_accessor_t<ElementType, Properties...>,
				detail::error_policy_t<Properties...>
		  >
	{
	public:
		// TYPES
		using element_type = ElementType;
		using extent_type  = decltype(Extent);
		using accesssor = detail::accessor_t<ElementType, Properties...>;
		using decorator = detail::decorator_t<ElementType, Properties...>;
		using error_policy = detail::error_policy_t<Properties...>;

		using pointer = typename access_policy::pointer;
		using reference = typename access_policy::reference;
		using iterator = typename access_policy::iterator;
		using sentinel = typename access_policy::sentinel;

		using storage = span_storage<pointer, dimensions<Extent>>;
		// }}}

		// CONSTRUCTORS

		/// \brief Creates an empty span (spanning no elements).
		///
		/// This is only possible if we have a dynamicly sized span and all
		/// base classes and members are default constructible
		/// We it is statically sized to zero.
		constexpr span()
		noexcept (
				std::is_nothrow_default_constructible_v<base> &&
				std::is_nothrow_default_constructible_v<storage>
		)
		requires (
				DefaultConstructible<base>() &&
				DefaultConstructible<storage>()
		)
		= default;

		/// \brief Constructs span from an C-style array
		///
		/// \note This currently causes an ICE in gcc.
		template <std::size_t N>
		constexpr span(element_type (&array)[N])
		noexcept(
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		: access_policy{}
		, m_storage{make_storage(&array[0], N)}
		{ /* empty */ }

		/// \brief Constructs span from a std::array
		//
		template <typename S, std::size_t N>
		requires
				ConvertibleTo<
						 decltype(std::declval<std::array<S, N>&>().data()),
						 pointer
				>()
		constexpr span(std::array<S, N>& array)
		noexcept(
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		: access_policy{}
		, m_storage{make_storage(array.data(), N)}
		{ /* empty */ }

		/// \brief Constructs span from a const std::array
		//
		template <typename S, std::size_t N>
		requires
				ConvertibleTo<
						 decltype(std::declval<const std::array<S, N>&>().data()),
						 pointer
				>()
		constexpr span(const std::array<S, N>& array)
		noexcept
		: access_policy{}
		, m_storage{make_storage(array.data(), N)}
		{
		}

		/// \brief Constructs a span with dynamic lengths from a container
		//
		template <typename C>
		requires
				ConvertibleTo<
							decltype(std::declval<const C&>().data()),
							pointer
				>()
		constexpr span(const C& container)
		noexcept (
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		requires (
				DefaultConstructible<access_policy>()
		)
		: access_policy{}
		, m_storage{make_storage(container.data(), container.size())}
		{}

		/// \brief Constructs a span with dynamic lengths from a container
		//
		template <typename C>
		requires
				ConvertibleTo<
							decltype(std::declval<C&>().data()),
							pointer
				>()
		constexpr span(C& container)
		noexcept (
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		requires (
				DefaultConstructible<access_policy>()
		)
		: access_policy{}
		, m_storage{make_storage(container.data(), container.size())}
		{}

		/// \brief Creates a span from a given pointer and length.
		///
		/// This is the version for spans with dynamic length and thus does not
		/// throw a length_error.
		//
		ranges::Integral{I}
		constexpr span(pointer data, I num_elems)
		noexcept (
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		requires (
				DefaultConstructible<access_policy>()
		)
		: access_policy{}
		, m_storage{make_storage(data, num_elems)}
		{}

		/// \brief Constructs a span ranging over all elements inbetween
		///        two pointers
		//
		constexpr span(pointer first, pointer last)
		noexcept (
				std::is_nothrow_default_constructible_v<access_policy> &&
				noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
		)
		requires (
				DefaultConstructible<access_policy>()
		)
		: access_policy{}
		, m_storage{make_storage(data, std::distance(first, last))}
		{}
		// }}}

		////////////////////////////////////////////////////////////////////////
		//                                                        MEMBER ACCESS
		// {{{

		/// \brief Returns the underlying pointer.
		constexpr pointer data() const noexcept
		{ return m_storage.data(); }

		/// \brief Returns the access_policy of this span
		constexpr const access_policy& accessor() const& noexcept
		{ return static_cast<const access_policy&>(*this); }

		/// \brief Returns the access_policy of this span
		constexpr const error_policy_type& error_policy() const& noexcept
		{ return static_cast<const error_policy_type&>(*this); }

		/// \brief Contiguous and one-dimensional access
		ranges::Integral{I}
		constexpr reference operator[](I idx) const
		noexcept(
			is_nothrow_access<
				const access_policy&,
				const storage&,
				I,
				const error_policy_type&
			>::value
		)
		{ return accessor().access(m_storage, idx, error_policy()); }

		// }}}

		////////////////////////////////////////////////////////////////////////
		//                                                             CAPACITY
		// {{{

		/// \brief Returns the amount of accessible elements.
		///
		/// storage().access(size()-i) returns the element with largest address
		/// of all accessible elements in storage().
		constexpr auto size() const noexcept
		{ return m_storage.size(); }

		// }}}

		////////////////////////////////////////////////////////////////////////
		//                                                            ITERATORS
		// {{{

		constexpr iterator begin() const noexcept
		{ return accessor().begin(m_storage); }

		constexpr sentinel end() const noexcept
		{ return accessor().end(m_storage); }

		// }}}

	private:
		////////////////////////////////////////////////////////////////////////
		//                                               [function.make_storage]
		// {{{
		ranges::Integral{I}
		constexpr storage make_storage(pointer p, I num_elems) const
		noexcept
		requires DynamicExtent<extent_type>()
		{
			return {p, num_elems};
		}

		ranges::Integral{I}
		constexpr storage make_storage(pointer p, I num_elems) const
		noexcept(noexcept(
			construction_check(
				std::declval<const error_policy_type&>(),
				std::declval<bool(*)()>()
			)
		))
		requires ranges::Integral<extent_type>()
		{
			construction_check(error_policy(), [num_elems]{ return Extent <= num_elems; });
			return p;
		}
		// }}}

		storage m_storage;
	};
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                       EQUALITY OPERATORS
	// {{{
	template <class T, auto DimL, class... PropsL, class U, auto DimR, class... PropsR>
	requires EqualityComparable<T,U>()
	constexpr bool operator==(const span<T, DimL, PropsL...>& lhs,
														const span<U, DimR, PropsR...>& rhs)
	noexcept(noexcept(std::declval<T>() == std::declval<U>()))
	{
		return lhs.size() == rhs.size() &&
			std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
	}

	template <class T, auto DimL, class... PropsL,
						class U, auto DimR, class... PropsR>
	requires
			EqualityComparable<T,U>()
	constexpr bool operator!=(const span<T, DimL, PropsL...>& lhs,
														const span<U, DimR, PropsR...>& rhs)
	noexcept(noexcept(std::declval<T>() == std::declval<U>()))
	{
		return !(lhs == rhs);
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                                OBSERVERS
	// {{{

	////////////////////////////////////////////////////////////////////////////
	//                                                        [function.subspan]
	// {{{
	template <int First, int Length, typename T, auto Dim, typename... Props>
	requires (
			ranges::Integral<decltype(Dim)>() &&
			(First >= 0 && Length >= 0 && (First + Length <= Dim))
	)
	constexpr auto
	subspan(span<T, Dim, Props...> s)
	noexcept // requires-clause makes every check a success
	{
		return span<T, Length, Props...>{
				s.data() + First, Length
		};
	}

	template <int First, int Length, typename T, auto Dim, typename... Props>
	requires DynamicExtent<decltype(Dim)>()
	constexpr auto
	subspan(span<T, Dim, Props...> s)
	{
		construction_check(s.error_policy(), [=]{ return First + Length <= s.size(); });
		return span<T, Length, Props...>{
				s.data() + First, Length
		};
	}

	template <typename T, auto Dim, typename... Props>
	constexpr auto
	subspan(span<T, Dim, Props...> s, int first, int length)
	{
		expects(s.error_policy(), [=]{ return first >= 0 && length >= 0; });
		construction_check(s.error_policy(), [=]{ return first + length <= s.size(); });
		return span<T, dyn, Props...>{ s.data() + first, length };
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                           [function.drop]
	// {{{
	template <int N, typename T, auto Dim, typename... Props>
	requires ranges::Integral<decltype(Dim)>() && (N <= Dim)
	constexpr auto
	drop(span<T, Dim, Props...> s)
	noexcept
	{
		return span<T, Dim - N, Props...>{s.data() + N, Dim - N};
	}

	template <int N, typename T, auto Dim, typename... Props>
	requires DynamicExtent<decltype(Dim)>()
	constexpr auto
	drop(span<T, Dim, Props...> s)
	noexcept(
		std::is_nothrow_constructible_v<span<T, Dim, Props...>,
					 typename span<T, Dim, Props...>::pointer, std::ptrdiff_t>
	)
	{
		return span<T, Dim, Props...>{s.data() + N, s.size() - N};
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                           [function.take]
	// {{{
	template <int N, typename T, auto Dim, typename... Props>
	constexpr auto
	take(span<T, Dim, Props...> s)
	{ return subspan<0, N>(s); }
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                           [function.tail]
	// {{{
	template <typename T, auto Dim, typename... Props>
	constexpr auto
	tail(span<T, Dim, Props...> s)
	noexcept(noexcept(drop<1>(std::declval<span<T, Dim, Props...>>())))
	{
		return drop<1>(s);
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                          [function.empty]
	// {{{
	template <typename T, auto Dim, typename... Props>
	constexpr bool empty(span<T, Dim, Props...> s) noexcept
	{
		return s.size() == 0;
	}
	// }}}

	////////////////////////////////////////////////////////////////////////////
	//                                                          [function.front]
	// {{{
	template <typename T, auto Dim, typename... Props>
	constexpr decltype(auto) front(span<T, Dim, Props...> s)
	{
		return s[0];
	}
	// }}}

	// }}}
}

#endif
