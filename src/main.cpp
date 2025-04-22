#include "expressions.hpp"
#include <iostream>
using namespace math_dsl;
using namespace math_dsl::utility;
auto input = Input<InputPair<'x', 3>, InputPair<'y', 4>>{};
void print(auto const &var) {
    std::cout << "expr : " << var.expr() << " val : " << var.eval(input)
              << "\n";
}
int main() {
    auto x = Variable<'x'>{};
    auto y = Variable<'y'>{};
    auto zero_scalar = Scalar<0>{};
    auto one = Scalar<1>{};
    auto two = Scalar<2>{};
    auto three = Scalar<3>{};
    auto four = Scalar<4>{};

    std::cout << "one + x : " << (one + x).expr() << "\n";
    std::cout << "one + x + three : " << (one + x + three).expr() << "\n";
    std::cout << "one + two + x + three : " << (one + two + x + three).expr()
              << "\n";

    std::cout << "(one + four) + (two + three)"
              << ((one + four) + (two + three)).expr() << "\n";
    std::cout << "one - x : " << (one - x).expr() << "\n";
    std::cout << "one - x - three : " << (one - x - three).expr() << "\n";
    std::cout << "one - two - x - three : " << (one - two - x - three).expr()
              << "\n";
    auto expr = one * two * three * four;
    std::cout << "expr : " << expr.expr() << " val : " << expr.eval(input)
              << "\n";
    auto expr1 = (three * x);
    print(expr1);
    auto expr2 = (two * y);
    print(expr2);
    auto expr3 = expr1 * expr2; // 6xy
    print(expr3);
    auto expr4 = (one * x * y * four); // 4xy
    print(expr4);
    auto expr5 = expr3 * expr4;
    print(expr5);

    std::cout << "Success!!\n";
    return 0;
}
