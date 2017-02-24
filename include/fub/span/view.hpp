//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_VIEW_HPP
#define FUB_VIEW_HPP

#include "fub/span.hpp"

namespace fub
{
  ////////////////////////////////////////////////////////////////////////////
  //                                                              [class.view]
  template <
    typename ElementType,
    Dimensions Dims,
    typename... Properties
  >
  class view
  {
    public:
      ////////////////////////////////////////////////////////////////////////
      //                                                                TYPES
      using element_type = ElementType;
      using mapping_type = layout::standard_right::mapping<Dims>;
      using index_type = mapping_type::index_type;
      using accessor_type = accessor::native<element_type>;
      using span_type = span<
        element_type,
        linearize<Dims>,
        access_policy<accessor_type>
      >;

      ////////////////////////////////////////////////////////////////////////
      //                                                         CONSTRUCTORS
      // {{{
      constexpr view(span_type span, const Dims& dims);

      // }}}

      ////////////////////////////////////////////////////////////////////////
      //                                                        MEMBER ACCESS

      /// Returns a reference to the mapping
      constexpr const mapping_type& mapping() const& noexcept
      { return m_mapping; }

      /// Returns a copy of the object bound span
      constexpr span_type span() const noexcept
      { return m_first; }

      ///////////////////////////////////////////////////////////////////////
      //                                                      ELEMENT ACCESS

      template <Integral... Is>
      constexpr reference operator()(Is... indices) const
      noexcept(noexcept(std::declval<const view&>()(make_array(indices...))))
      {
        auto&& i = make_array(indices...);
        return (*this)(i);
      }

      constexpr reference operator()(const index_type& i) const
      noexcept(
          is_noexcept_accessor<span_type>::value &&
          is_noexcept_mapping<mapping_type>::value
      )
      { return m_span[mapping().map(i)]; }

      ///////////////////////////////////////////////////////////////////////
      //                                                           ITERATORS

      iterator begin() const& noexcept
      { return accessor().begin(*this); }

      sentinel end() const& noexcept
      { return accessor().end(*this); }

    private:
      span_type m_span;
      mapping_type m_mapping;
  };

  ///////////////////////////////////////////////////////////////////////////
  //                                                                CAPACITY

  template <View V, Integral I>
  Integral extent(const V& view, I rank)
  { return view.mapping().extent(rank); }

  template <View V>
  Integral rank(const V& view)
  { return view.mapping().rank(); }
}

#endif
