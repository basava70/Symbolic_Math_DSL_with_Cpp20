#include "expressions.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace math_expr;
using Catch::Approx;

TEST_CASE("Scalar Evaluation and expr()") {
    SECTION("Positive Integer") {
        Scalar<42> s;
        REQUIRE(s.eval() == 42);
        REQUIRE(s.expr() == "42");
    }

    SECTION("Zero") {
        Scalar<0> s;
        REQUIRE(s.eval() == 0);
        REQUIRE(s.expr() == "0");
    }

    SECTION("Negative Integer") {
        Scalar<-7> s;
        REQUIRE(s.eval() == -7);
        REQUIRE(s.expr() == "-7");
    }

    SECTION("Positive Floating Point") {
        Scalar<3.14159> s;
        REQUIRE(s.eval() == Approx(3.14159));
        REQUIRE(s.expr() == "3.14159");
    }

    SECTION("Negative Floating Point") {
        Scalar<-2.5> s;
        REQUIRE(s.eval() == Approx(-2.5));
        REQUIRE(s.expr() == "-2.5");
    }
}

TEST_CASE("Variable Evaluation with Context") {
    Variable<'x'> x;
    SECTION("Positive Context") {
        using context = Input<InputPair<'x', 10>>;
        REQUIRE(x.eval(context{}) == 10);
        REQUIRE(x.expr() == "x");
    }
    SECTION("Zero Context") {
        using context = Input<InputPair<'x', 0>>;
        REQUIRE(x.eval(context{}) == 0);
        REQUIRE(x.expr() == "x");
    }
    SECTION("Negative Context") {
        using context = Input<InputPair<'x', -9>>;
        REQUIRE(x.eval(context{}) == -9);
        REQUIRE(x.expr() == "x");
    }
    SECTION("Positive floating Context") {
        using context = Input<InputPair<'x', 3.135>>;
        REQUIRE(x.eval(context{}) == 3.135);
        REQUIRE(x.expr() == "x");
    }
    SECTION("Negative Context") {
        using context = Input<InputPair<'x', -1.53743>>;
        REQUIRE(x.eval(context{}) == -1.53743);
        REQUIRE(x.expr() == "x");
    }
}

TEST_CASE("Addition") {
    SECTION("Associative Add") {
        auto expr = (Scalar<1>{} + Scalar<2>{}) + Scalar<3>{};
        REQUIRE(expr.eval(Input<>{}) == 6);
        REQUIRE(expr.expr() == "((1 + 2) + 3)");
    }
    SECTION("Addition by Zero") {
        auto expr = Variable<'x'>{} + Scalar<0>{};
        using context = Input<InputPair<'x', 5>>;
        REQUIRE(expr.eval(context{}) == 5);
        REQUIRE(expr.expr() == "(x + 0)");
    }
    SECTION("Variable + Scalar Evaluation") {
        auto expr = Variable<'x'>{} + Scalar<2>{};
        using context = Input<InputPair<'x', 3>>;
        REQUIRE(expr.eval(context{}) == 5);
        REQUIRE(expr.expr() == "(x + 2)");
    }

    SECTION("Scalar + Variable Evaluation") {
        auto expr = Scalar<2>{} + Variable<'x'>{};
        using context = Input<InputPair<'x', 3>>;
        REQUIRE(expr.eval(context{}) == 5);
        REQUIRE(expr.expr() == "(2 + x)");
    }

    SECTION("Nested Scalar + Variable + Scalar Evaluation") {
        auto expr = Scalar<1>{} + Variable<'x'>{} + Scalar<3>{};
        using context = Input<InputPair<'x', 4>>;
        REQUIRE(expr.eval(context{}) == 8);
        REQUIRE(expr.expr() == "((1 + x) + 3)");
    }

    SECTION("Nested Variable + Variable + Variable Evaluation") {
        auto expr = Variable<'x'>{} + Variable<'y'>{} + Variable<'z'>{};
        using context =
            Input<InputPair<'x', 5>, InputPair<'y', 7>, InputPair<'z', 2>>;
        REQUIRE(expr.eval(context{}) == 14);
        REQUIRE(expr.expr() == "((x + y) + z)");
    }
}

// NOTE: This is a compile-time error, so cannot be tested at runtime.
// TEST_CASE("Missing Variable triggers static_assert") {
//     Variable<'z'> z;
//     using context = Input<InputPair<'x', 5>>;
//     z.eval(context{}); // Triggers static_assert
// }

// TEST_CASE("Unary Negation") {
//     SECTION("Negation of compound expression") {
//         auto expr = -(Variable<'x'>{} + Scalar<3>{});
//         using context = Input<InputPair<'x', 1>>;
//         REQUIRE(expr.eval(context{}) == -4);
//         REQUIRE(expr.expr() == "-(x + 3)");
//     }
//     SECTION("Double negation") {
//         auto expr = -(-Variable<'x'>{});
//         using context = Input<InputPair<'x', 7>>;
//         REQUIRE(expr.eval(context{}) == 7);
//         REQUIRE(expr.expr() == "--x");
//     }
//     SECTION("Single value expression") {
//         auto expr = Variable<'x'>{};
//         auto negation = Negation(expr);
//         using context = Input<InputPair<'x', 3>>;
//         REQUIRE(negation.eval(context{}) == -3);
//         REQUIRE(negation.expr() == "-x");
//     }
//     SECTION("Using operator-") {
//         auto expr = -Variable<'x'>{};
//         using context = Input<InputPair<'x', -1>>;
//         REQUIRE(expr.eval(context{}) == 1);
//         REQUIRE(expr.expr() == "-x");
//     }
// }
//
// TEST_CASE("Subtraction and Binary operator-") {
//     SECTION("x - x = 0") {
//         auto expr = Variable<'x'>{} - Variable<'x'>{};
//         using context = Input<InputPair<'x', 10>>;
//         REQUIRE(expr.eval(context{}) == 0);
//         REQUIRE(expr.expr() == "(x - x)");
//     }
//     SECTION("Base Subtraction") {
//         auto expr = Variable<'x'>{} - Scalar<1>{};
//         using context =
//             Input<InputPair<'y', -2>, InputPair<'z', 0>, InputPair<'x', 5>>;
//         REQUIRE(expr.eval(context{}) == 4);
//         REQUIRE(expr.expr() == "(x - 1)");
//     }
//     SECTION("Chained Subtraction") {
//         auto expr = Scalar<10>{} - Variable<'x'>{} - Scalar<3>{};
//         using context = Input<InputPair<'x', 2>>;
//         REQUIRE(expr.eval(context{}) == 5);
//         REQUIRE(expr.expr() == "((10 - x) - 3)");
//     }
// }
//
// TEST_CASE("Multiplication and operator*") {
//     SECTION("Identity: x * 1 = x") {
//         auto expr = Variable<'x'>{} * Scalar<1>{};
//         using context = Input<InputPair<'x', 42>>;
//         REQUIRE(expr.eval(context{}) == 42);
//         REQUIRE(expr.expr() == "x");
//     }
//     SECTION("Annihilation: x * 0 = 0") {
//         auto expr = Variable<'x'>{} * Scalar<0>{};
//         using context = Input<InputPair<'x', 99>>;
//         REQUIRE(expr.eval(context{}) == 0);
//         REQUIRE(expr.expr() == "0");
//     }
//     SECTION("Chained Multiplication") {
//         auto expr = Scalar<2>{} * Variable<'x'>{} * Scalar<3>{};
//         using context = Input<InputPair<'x', 4>>;
//         REQUIRE(expr.eval(context{}) == 24);
//         REQUIRE(expr.expr() == "(3 * (2 * x))");
//     }
//     SECTION("Linearity") {
//         auto expr = Scalar<7>{} +
//                     Variable<'x'>{} * (Variable<'y'>{} - Variable<'x'>{}) +
//                     Scalar<1>{} * Variable<'z'>{};
//         using context =
//             Input<InputPair<'y', -2>, InputPair<'z', 0>, InputPair<'x', -2>>;
//         REQUIRE(expr.eval(context{}) == 7);
//         REQUIRE(expr.expr() == "((7 + (x * (y - x))) + z)");
//     }
// }
//
// TEST_CASE("Simplification of symbolic addition") {
//     SECTION("Scalar folding: Scalar<1> + Scalar<2> = Scalar<3>") {
//         auto expr = Scalar<1>{} + Scalar<2>{};
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.eval(Input<>{}) == 3);
//         REQUIRE(simplified.expr() == "3");
//     }
//
//     SECTION("Identity: x + 0 -> x") {
//         auto expr = Variable<'x'>{} + Scalar<0>{};
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "x");
//     }
//
//     SECTION("Identity: 0 + x -> x") {
//         auto expr = Scalar<0>{} + Variable<'x'>{};
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "x");
//     }
//
//     SECTION("Same operand: x + x -> 2 * x") {
//         auto expr = Variable<'x'>{} + Variable<'x'>{};
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(2 * x)");
//     }
//
//     SECTION("Nested: (x + 0) + (0 + x) -> 2 * x") {
//         auto x = Variable<'x'>{};
//         auto expr = (x + Scalar<0>{}) + (Scalar<0>{} + x);
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(2 * x)");
//     }
// }
//
// #include "expressions.hpp"
// #include <catch2/catch_test_macros.hpp>
// using namespace math_expr;
//
// TEST_CASE("Subtraction simplification") {
//     auto x = Variable<'x'>{};
//     auto zero = Scalar<0>{};
//     auto five = Scalar<5>{};
//     auto two = Scalar<2>{};
//
//     SECTION("x - 0 -> x") {
//         auto expr = x - zero;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "x");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 42>>{}) == 42);
//     }
//
//     SECTION("0 - x -> -x") {
//         auto expr = zero - x;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "-x");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 3>>{}) == -3);
//     }
//
//     SECTION("Scalar<5> - Scalar<2> -> Scalar<3>") {
//         auto expr = five - two;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "3");
//         REQUIRE(simplified.eval(Input<>{}) == 3);
//     }
//
//     SECTION("x - x -> Scalar<0>") {
//         auto expr = x - x;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 10>>{}) == 0);
//     }
//
//     SECTION("(x - x) + (5 - 5) -> 0") {
//         auto expr = (x - x) + (Scalar<5>{} - Scalar<5>{});
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 123>>{}) == 0);
//     }
//
//     SECTION("((x + 2) - (x + 2)) -> 0") {
//         auto expr = (x + Scalar<2>{}) - (x + Scalar<2>{});
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 99>>{}) == 0);
//     }
//
//     SECTION("(((x + x) - x) + x) -> (2 * x)") {
//         auto expr = ((x + x) - x) + x;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(2 * x)");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 10>>{}) == 20);
//     }
// }
// TEST_CASE("Multiplication simplification") {
//     auto x = Variable<'x'>{};
//     auto zero = Scalar<0>{};
//     auto one = Scalar<1>{};
//     auto three = Scalar<3>{};
//     auto five = Scalar<5>{};
//
//     SECTION("0 * x -> 0") {
//         auto expr = zero * x;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 123>>{}) == 0);
//     }
//
//     SECTION("x * 0 -> 0") {
//         auto expr = x * zero;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 999>>{}) == 0);
//     }
//
//     SECTION("1 * x -> x") {
//         auto expr = one * x;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "x");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 8>>{}) == 8);
//     }
//
//     SECTION("x * 1 -> x") {
//         auto expr = x * one;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "x");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 4>>{}) == 4);
//     }
//
//     SECTION("Scalar * Scalar -> Scalar") {
//         auto expr = three * five;
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "15");
//         REQUIRE(simplified.eval(Input<>{}) == 15);
//     }
//
//     SECTION("(x * 1) * (1 * x) -> x * x") {
//         auto expr = (x * one) * (one * x);
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(x * x)");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 3>>{}) == 9);
//     }
// }
//
// TEST_CASE("Deeply nested simplification tests") {
//     auto x = Variable<'x'>{};
//     auto y = Variable<'y'>{};
//     auto z = Variable<'z'>{};
//     auto one = Scalar<1>{};
//     auto zero = Scalar<0>{};
//     auto two = Scalar<2>{};
//     auto neg_one = Scalar<-1>{};
//
//     SECTION("((x + x) - (x + x)) + ((0 - z) + z) -> 0") {
//         auto expr = ((x + x) - (x + x)) + ((zero - z) + z);
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(
//                     Input<InputPair<'x', 42>, InputPair<'z', 10>>{}) == 0);
//     }
//
//     SECTION("((x + 0) + ((0 + x) + (x + 0))) -> 3 * x") {
//         auto expr = (x + zero) + ((zero + x) + (x + zero));
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(3 * x)");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 2>>{}) == 6);
//     }
//
//     SECTION("((x + 1) - (x + 1)) + (2 * y - y) -> y") {
//         auto expr = ((x + one) - (x + one)) + ((two * y) - y);
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "y");
//         REQUIRE(simplified.eval(
//                     Input<InputPair<'x', 0>, InputPair<'y', 9>>{}) == 9);
//     }
//
//     SECTION("((2 * x + x) + (x + 2 * x)) -> (6 * x)") {
//         auto expr = ((Scalar<2>{} * x + x) + (x + Scalar<2>{} * x));
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(6 * x)");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 1>>{}) == 6);
//     }
//
//     SECTION("((2 * x - x) - (x - 2 * x)) -> (2 * x)") {
//         auto expr = ((Scalar<2>{} * x - x) - (x - Scalar<2>{} * x));
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "(2 * x)");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 3>>{}) == 6);
//     }
//
//     SECTION("-((x + x) - (2 * x)) -> 0") {
//         auto expr = -((x + x) - (Scalar<2>{} * x));
//         auto simplified = simplify(expr);
//         REQUIRE(simplified.expr() == "0");
//         REQUIRE(simplified.eval(Input<InputPair<'x', 5>>{}) == 0);
//     }
// }
