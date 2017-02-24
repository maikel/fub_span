//          Copyright Maikel Nadolski 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef ERROR_POLICY_THROW_EXCEPTION_HPP
#define ERROR_POLICY_THROW_EXCEPTION_HPP

#include "fub/concepts.hpp"
#include <stdexcept>

namespace fub::ep
{
  template <class Exception = std::logic_error>
  struct throw_exception {
    template <ranges::Invocable Check>
    void run(Check check) const
    requires ranges::DefaultConstructible<Exception>()
    {
      if (!check()) {
        throw Exception{};
      }
    }

    template <ranges::Invocable Check, typename... Args>
    void run(Check check, Args&&... what) const
    {
      if (!check()) {
        throw Exception{std::forward<Args>(what)...};
      }
    }

    template <class Tag, ranges::Invocable Check, class... Args>
    void run(Tag, Check&& check, Args&&... args) const
    { run(std::forward<Check>(check), std::forward<Args>(args)...); }
  };
}

#endif
