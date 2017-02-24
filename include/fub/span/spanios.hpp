//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef FUB_SPANIO_HPP
#define FUB_SPANIO_HPP

#include "fub/span.hpp"
#include <iosfwd>

namespace fub
{
  template <typename ElementType, auto Dim, typename... Properties>
  std::ostream& operator<<(
      std::ostream& out,
      const span<ElementType, Dim, Properties...>& s)
  {
    out << '[';
    if (!empty(s)) {
      out << front(s);
      for (const ElementType& x : tail(s)) {
        out << ", " << x;
      }
    }
    return out << ']';
  }
}

#endif
