//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_SPAN_SPAN_HPP
#define FUB_SPAN_SPAN_HPP

#include "fub/span/dimensions.hpp"
#include "fub/span/accessor/native.hpp"
#include "fub/span/decorator/identity.hpp"
#include "fub/span/error_policy/ignore.hpp"

namespace fub
{
	/// \brief Store a pointer and length.
	///
	/// This class makes use of the empty base class optimization in the case
	/// of static lengths, if known to compile time.
	template <ranges::Regular Pointer, concepts::span::Dimensions Dims>
		requires (Dims::rank() == 1)
	class span_storage
		: ranges::detail::ebo_box<Dims, span_storage<Pointer, Dims>>
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
					ranges::DefaultConstructible<base>() &&
					(dimensions_type::is_dynamic_dimension(0) || 
						Dims::extent__(0).value() == 0)
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
					ranges::Constructible<base, I>() &&
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
					ranges::DefaultConstructible<base>() &&
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

	/// \brief Two storages are equal if they refer to the same pointer and are equal
	///        in size.
	template <typename T, typename Dims>
	bool operator==(const span_storage<T, Dims>& lhs, const span_storage<T, Dims>& rhs)
	{ return lhs.data() == rhs.data() && lhs.size() == rhs.size(); }

	/// \brief Two storages ares different if they either differ in size or they refer
	///        to a different pointer
	template <typename T, typename Dims>
	bool operator!=(const span_storage<T, Dims>& lhs, const span_storage<T, Dims>& rhs)
	{ return !(lhs == rhs); }

	namespace detail
	{
		// FIND ACCESSOR

		/// Find a type `P` satisfying the `Accessor<P, T>()` concept in Ps...
		/// If no such type exist in `Ps...` it returns the accessor::native
		template <typename T, typename... Ps>
		struct accessor;

		/// Base case, returns the native accessor
		template <typename T>
		struct accessor<T> {
			using type = fub::accessor::native<T>;
		};

		/// Recursion
		template <typename T, typename P, typename... Ps>
		struct accessor<T, P, Ps...> {
			using type = std::conditional_t<
				concepts::span::Accessor<P, T>(), // if this is true
				P,                                // then go for `P`
				meta::_t<accessor<T, Ps...>>      // else look in `Ps...`
			>;
		};

		// FIND DECORATOR

		/// Find a Decorator type in `Ds...`
		template <
			typename T,
			concepts::span::Accessor<T> A,
			concepts::span::Storage<typename A::pointer> S,
			typename... Ds
		>
		struct decorator;

		/// This selects a default decorator
		/// The default is to do nothing but forward all requests to the
		/// underlying accessor.
		template <
			typename T,
			concepts::span::Accessor<T> A,
			concepts::span::Storage<typename A::pointer> S
		>
		struct decorator<T, A, S> {
			using type = fub::decorator::identity;
			// static_assert(
			// 	concepts::span::Decorator<type, T, A, S>(),
			// 	"Default decorator is not a decorator!"
			// );
		};

		/// Recursion
		template <
			typename T,
			concepts::span::Accessor<T> A,
			concepts::span::Storage<typename A::pointer> S,
			typename D, typename... Ds
		>
		struct decorator<T, A, S, D, Ds...> {
			using type = std::conditional_t<
				concepts::span::Decorator<D, T, A, S>(),
				D,
				meta::_t<decorator<T, A, S, Ds...>>
			>;
		};

		// FIND ERROR_POLICY

		/// Returns a type `E` in `Es` which satisfies the ErrorPolicy<E>() concept.
		template <typename... Es>
		struct error_policy;

		/// Base case. This returns the default error policy.
		/// Default is to ignore everything and to not perform any checking.
		template <>
		struct error_policy<> { using type = fub::error_policy::ignore; };

		/// Recursion goes through the type list and tries every type for the
		/// ErrorPolicy concept
		template <typename E, typename... Es>
		struct error_policy<E, Es...> {
			using type = std::conditional_t<
				concepts::span::ErrorPolicy<E>(),
				E,
				meta::_t<error_policy<Es...>>
			>;
		};

		template <typename... Es>
		using error_policy_t = typename error_policy<Es...>::type;

		// BUILD DECORATED ACCESSOR

		/// Constructs a span accessor based on a decorator and standard accessor
		template <typename T, auto Extent, typename... Ps>
		struct decorated_accessor {
			using accessor_t = meta::_t<accessor<T, Ps...>>;
			using storage_t = span_storage<typename accessor_t::pointer, dimensions<Extent>>;
			using decorator_t = meta::_t<decorator<T, accessor_t, storage_t, Ps...>>;
			using type = typename decorator_t::template accessor<T, accessor_t>;
		};

		/// typedef for comfort
		template <typename T, auto Extent, typename... Ps>
		using decorated_accessor_t = meta::_t<decorated_accessor<T, Extent, Ps...>>;

		/// \brief Calls the error policy with a given predicate and the `expects_tag`.
		template <concepts::span::ErrorPolicy E, ranges::Predicate P>
		void expects(E policy, P pred)
		noexcept(noexcept(std::invoke(std::declval<E>(), expects_tag{}, std::declval<P&&>(),"Precondition Failure")))
		{ std::invoke(policy, expects_tag{}, std::move(pred), "Precondition Failure"); }

		/// \brief Calls the error policy with a given predicate and the `construction_tag`.
		template <concepts::span::ErrorPolicy E, ranges::Predicate P>
		void construction_check(E policy, P pred)
		noexcept(noexcept(std::invoke(std::declval<E>(), construction_tag{}, std::declval<P&&>(), "Construction Error")))
		{ std::invoke(policy, construction_tag{}, std::move(pred), "Span Construction Error"); }

		template <typename T, typename... Args>
		struct is_nothrow_access {
			static constexpr bool value =
				noexcept(std::declval<const T&>().access(std::declval<Args>()...));
		};
	}

	template <
			typename ElementType,
			auto Extent = dyn,
			typename... Properties
	>
	class span
		: ranges::ext::compressed_pair<
				detail::decorated_accessor_t<ElementType, Extent, Properties...>,
				detail::error_policy_t<Properties...>
		  >
	{
		public:
			// TYPES
			using base = ranges::ext::compressed_pair<
							detail::decorated_accessor_t<ElementType, Extent, Properties...>,
							detail::error_policy_t<Properties...>
						>;
			using element_type = ElementType;
			using extent_type  = decltype(Extent);
			using accessor_type = detail::decorated_accessor_t<ElementType, Extent, Properties...>;
			using error_policy_type = detail::error_policy_t<Properties...>;

			using pointer = typename accessor_type::pointer;
			using reference = typename accessor_type::reference;
			using iterator = typename accessor_type::iterator;
			using sentinel = typename accessor_type::sentinel;

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
					ranges::DefaultConstructible<base>() &&
					ranges::DefaultConstructible<storage>()
			)
			= default;

			/// \brief Constructs span from an C-style array
			///
			/// \note This currently causes an ICE in gcc.
			template <std::size_t N>
			constexpr span(element_type (&array)[N])
			noexcept(
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			: base{}
			, m_storage{make_storage(&array[0], N)}
			{ /* empty */ }

			/// \brief Constructs span from a std::array
			template <typename S, std::size_t N>
			requires
					ranges::ConvertibleTo<
							 decltype(std::declval<std::array<S, N>&>().data()),
							 pointer
					>()
			constexpr span(std::array<S, N>& array)
			noexcept(
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			: base{}
			, m_storage{make_storage(array.data(), N)}
			{ /* empty */ }

			/// \brief Constructs span from a const std::array
			template <typename S, std::size_t N>
			requires
					ranges::ConvertibleTo<
							 decltype(std::declval<const std::array<S, N>&>().data()),
							 pointer
					>()
			constexpr span(const std::array<S, N>& array)
			noexcept
			: base{}
			, m_storage{make_storage(array.data(), N)}
			{
			}

			/// \brief Constructs a span with dynamic lengths from a container
			template <typename C>
			requires
					ranges::ConvertibleTo<
								decltype(std::declval<const C&>().data()),
								pointer
					>()
			constexpr span(const C& container)
			noexcept (
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			requires (
					ranges::DefaultConstructible<base>()
			)
			: base{}
			, m_storage{make_storage(container.data(), container.size())}
			{}

			/// \brief Constructs a span with dynamic lengths from a container
			template <typename C>
			requires
					ranges::ConvertibleTo<
								decltype(std::declval<C&>().data()),
								pointer
					>()
			constexpr span(C& container)
			noexcept (
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			requires (
					ranges::DefaultConstructible<base>()
			)
			: base{}
			, m_storage{make_storage(container.data(), container.size())}
			{}

			/// \brief Creates a span from a given pointer and length.
			///
			/// This is the version for spans with dynamic length and thus does not
			/// throw a length_error.
			ranges::Integral{I}
			constexpr span(pointer data, I num_elems)
			noexcept (
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			requires (
					ranges::DefaultConstructible<base>()
			)
			: base{}
			, m_storage{make_storage(data, num_elems)}
			{}

			/// \brief Constructs a span ranging over all elements inbetween
			///        two pointers
			//
			constexpr span(pointer first, pointer last)
			noexcept (
					std::is_nothrow_default_constructible_v<base> &&
					noexcept(std::declval<const span&>().make_storage(std::declval<pointer>(), 0))
			)
			requires (
					ranges::DefaultConstructible<base>()
			)
			: base{}
			, m_storage{make_storage(data, std::distance(first, last))}
			{}

			// ACCESSORS

			/// \brief Returns the underlying pointer.
			constexpr pointer data() const noexcept
			{ return m_storage.data(); }

			/// \brief Returns the accessor_type of this span
			constexpr const accessor_type& accessor() const& noexcept
			{ return static_cast<const accessor_type&>(base::first()); }

			/// \brief Returns the accessor of this span
			constexpr const error_policy_type& error_policy() const& noexcept
			{ return static_cast<const error_policy_type&>(base::second()); }

			/// \brief Contiguous and one-dimensional access
			ranges::Integral{I}
			constexpr reference operator[](I idx) const
			noexcept(
				detail::is_nothrow_access<
					const accessor_type&,
					const storage&,
					I,
					const error_policy_type&
				>::value
			)
			{ return accessor().access(m_storage, idx, error_policy()); }

			// }}}

			// CAPACITY

			/// \brief Returns the amount of accessible elements.
			///
			/// storage().access(size()-i) returns the element with largest address
			/// of all accessible elements in storage().
			constexpr auto size() const noexcept
			{ return m_storage.size(); }

			// }}}

			// ITERATORS

			constexpr iterator begin() const noexcept
			{ return accessor().begin(m_storage); }

			constexpr sentinel end() const noexcept
			{ return accessor().end(m_storage); }

			// }}}

		private:
			ranges::Integral{I}
			constexpr storage make_storage(pointer p, I num_elems) const
			noexcept
			requires concepts::span::DynamicExtent<extent_type>()
			{
				return {p, num_elems};
			}

			ranges::Integral{I}
			constexpr storage make_storage(pointer p, I num_elems) const
			noexcept(noexcept(detail::construction_check(std::declval<const error_policy_type&>(), std::declval<bool(*)()>())))
			requires ranges::Integral<extent_type>()
			{
				detail::construction_check(error_policy(), [num_elems]{ return Extent <= num_elems; });
				return p;
			}

			storage m_storage;
	};

	template <class T, auto DimL, class... PropsL, class U, auto DimR, class... PropsR>
	requires ranges::EqualityComparable<T,U>()
	constexpr bool operator==(const span<T, DimL, PropsL...>& lhs, const span<U, DimR, PropsR...>& rhs)
	noexcept(noexcept(std::declval<const T&>() == std::declval<const U&>()))
	{
		return lhs.size() == rhs.size() &&
			std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
	}

	template <class T, auto DimL, class... PropsL,
						class U, auto DimR, class... PropsR>
	requires ranges::EqualityComparable<T,U>()
	constexpr bool operator!=(const span<T, DimL, PropsL...>& lhs, const span<U, DimR, PropsR...>& rhs)
	noexcept(noexcept(std::declval<const T&>() == std::declval<const U&>()))
	{
		return !(lhs == rhs);
	}

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
	requires concepts::span::DynamicExtent<decltype(Dim)>()
	constexpr auto
	subspan(span<T, Dim, Props...> s)
	{
		detail::construction_check(s.error_policy(), [=]{ return First + Length <= s.size(); });
		return span<T, Length, Props...>{
				s.data() + First, Length
		};
	}

	template <typename T, auto Dim, typename... Props>
	constexpr auto
	subspan(span<T, Dim, Props...> s, int first, int length)
	{
		detail::expects(s.error_policy(), [=]{ return first >= 0 && length >= 0; });
		detail::construction_check(s.error_policy(), [=]{ return first + length <= s.size(); });
		return span<T, dyn, Props...>{ s.data() + first, length };
	}

	template <int N, typename T, auto Dim, typename... Props>
	requires ranges::Integral<decltype(Dim)>() && (N <= Dim)
	constexpr auto
	drop(span<T, Dim, Props...> s)
	noexcept
	{
		return span<T, Dim - N, Props...>{s.data() + N, Dim - N};
	}

	template <int N, typename T, auto Dim, typename... Props>
	requires concepts::span::DynamicExtent<decltype(Dim)>()
	constexpr auto
	drop(span<T, Dim, Props...> s)
	noexcept(
		std::is_nothrow_constructible_v<span<T, Dim, Props...>,
					 typename span<T, Dim, Props...>::pointer, std::ptrdiff_t>
	)
	{
		return span<T, Dim, Props...>{s.data() + N, s.size() - N};
	}

	template <int N, typename T, auto Dim, typename... Props>
	constexpr auto
	take(span<T, Dim, Props...> s)
	{ return subspan<0, N>(s); }

	template <typename T, auto Dim, typename... Props>
	constexpr auto
	tail(span<T, Dim, Props...> s)
	noexcept(noexcept(drop<1>(std::declval<span<T, Dim, Props...>>())))
	{
		return drop<1>(s);
	}

	template <typename T, auto Dim, typename... Props>
	constexpr bool empty(span<T, Dim, Props...> s) noexcept
	{
		return s.size() == 0;
	}

	template <typename T, auto Dim, typename... Props>
	constexpr decltype(auto) front(span<T, Dim, Props...> s)
	{
		return s[0];
	}
}

#endif
