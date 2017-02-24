//          Copyright Maikel Nadolski 2016.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef ACCESS_DECORATOR_CHECK_BOUNDS_HPP
#define ACCESS_DECORATOR_CHECK_BOUNDS_HPP

#include "fub/span/concepts.hpp"

namespace fub::decorator
{
  struct check_bounds {
    Accessor{A}
    struct accessor
      : private A
    {
      using pointer = typename A::pointer;
      using reference = typename A::reference;
      using iterator = pointer;
      using sentinel = pointer;

      constexpr const A& base() const noexcept
      { return static_cast<const A&>(*this); }

      template <SpanStorage<pointer> S, Integral I, typename EP>
      constexpr reference access(const S& s, I i, const EP& policy)
      const
      noexcept(noexcept(
          std::declval<const EP&>().run(
            out_of_bounds_tag{},
            std::declval<bool(*)()>(),
            "Out of Bounds")
      ))
      {
        policy.run(out_of_bounds_tag{},
                   [&s, i] { return 0 <= i && i < s.size(); },
                   "Out of Bounds");
        return base().access(s.data(), i);
      }

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
