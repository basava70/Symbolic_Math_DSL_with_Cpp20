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

    auto expr = (variable + variable) + variable;
    auto expr2 = (scalar3 * variable) + variable;
    std::cout << "expr : " << expr.expr()
              << " simplify : " << simplify(expr).expr() << "\n";
    auto test_expr = Variable<'x'>{} * Scalar<1>{};
    std::cout << " test_expr : " << test_expr.expr()
              << ", eval expected is 3 : " << test_expr.eval(context) << "\n";
    std::cout << "expr2 : " << expr2.expr() << "\n";
    // << " simplify : " << simplify(expr2).expr() << "\n";

    std::cout << "Success!!\n";
    return 0;
}
