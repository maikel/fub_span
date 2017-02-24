#include "fub/spanios.hpp"
#include <iostream>

int main()
{
  auto vec = std::vector<int>{0, 1, 2, 3, 4, 5};
  std::cout << fub::span<int>{vec} << '\n';
}
