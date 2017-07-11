// Copyright (C) 2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++14" }
// { dg-do compile }

// PR 78991
// This failed to compile with Clang because the result_of expression causes
// instantiation of _Iter_comp_iter::operator() outside the immediate context.

#include <algorithm>

struct function
{
  function() = default;

  template<typename F, typename = std::result_of_t<F&(int, int)>>
    function(F) { }

  bool operator()(int x, int y) const { return x < y; }
};

int main()
{
  int a[2]{ 2, 1 };
  std::sort(a, a+2, function{});
}
