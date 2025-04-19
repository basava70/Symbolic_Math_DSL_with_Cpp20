#include "expressions.hpp"
#include <iostream>
using namespace math_expr;
using namespace math_expr::utility;
int main() {
    auto variable = Variable<'x'>{};
    auto zero_scalar = Scalar<0>{};
    auto scalar1 = Scalar<1>{};
    auto scalar2 = Scalar<2>{};
    auto scalar3 = Scalar<3>{};
    auto scalar4 = Scalar<4>{};
    auto context = Input<InputPair<'x', 3>>{};

    std::cout << "Scalar";
    std::cout << "Success!!\n";
    return 0;
}
