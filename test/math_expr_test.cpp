#include "expressions.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace math_dsl;
using Catch::Approx;

TEST_CASE("Scalar: compile-time constants", "[Scalar]") {
    SECTION("Integral") {
        REQUIRE(Scalar<5>().eval() == 5);
        REQUIRE(Scalar<-42>().eval() == -42);
        REQUIRE(Scalar<0>().eval() == 0);
        REQUIRE(Scalar<5>().expr() == "5");
    }

    SECTION("Floating-point") {
        REQUIRE(Scalar<3.14>().eval() == Approx(3.14));
        REQUIRE(Scalar<-1.75>().eval() == Approx(-1.75));
        REQUIRE(Scalar<3.14>().expr() == "3.14");
    }
}

TEST_CASE("Variable: symbolic variable resolution", "[Variable]") {
    using X5 = Input<InputPair<'x', 5>>;
    using Y2 = Input<InputPair<'y', 2>>;
    Variable<'x'> x;
    Variable<'y'> y;

    SECTION("Evaluate against symbolic context") {
        REQUIRE(x.eval(X5{}) == 5);
        REQUIRE(y.eval(Y2{}) == 2);
    }

    SECTION("String representation") {
        REQUIRE(x.expr() == "x");
        REQUIRE(y.expr() == "y");
    }
}

TEST_CASE("Add: symbolic addition and flattening", "[Add]") {
    Scalar<1> one;
    Scalar<2> two;
    Variable<'x'> x;

    SECTION("Simple two-term") {
        auto expr = one + two;
        REQUIRE(expr.eval(Input<>{}) == 3);
        REQUIRE(expr.expr() == "(1 + 2)");
    }

    SECTION("Left-associative chaining") {
        auto expr = one + two + x;
        using ctx = Input<InputPair<'x', 3>>;
        REQUIRE(expr.eval(ctx{}) == 6);
        REQUIRE(expr.expr() == "(1 + 2 + x)");
    }

    SECTION("Right-additive flattening") {
        auto left = one + x;
        auto full = left + two;
        using ctx = Input<InputPair<'x', 4>>;
        REQUIRE(full.eval(ctx{}) == 7);
        REQUIRE(full.expr() == "(1 + x + 2)");
    }

    SECTION("Empty Add node prints ()") {
        Add<> empty;
        REQUIRE(empty.expr() == "()");
    }
}

TEST_CASE("Negation and Subtraction", "[Negation][Subtraction]") {
    Variable<'x'> x;
    Variable<'y'> y;
    Scalar<3> three;

    using Ctx = Input<InputPair<'x', 5>, InputPair<'y', 2>>;

    SECTION("Unary negation") {
        REQUIRE((-x).eval(Ctx{}) == -5);
        REQUIRE((-three).eval() == -3);
        REQUIRE((-x).expr() == "(-1 * x)");
        REQUIRE((-three).expr() == "-3");
    }

    SECTION("Double negation simplifies back") {
        auto expr = -(-x);
        REQUIRE(expr.eval(Ctx{}) == 5);
        REQUIRE(expr.expr() == "x");
    }

    SECTION("Scaled negation") {
        auto expr = -Scaled<2, decltype(x)>{x};
        REQUIRE(expr.eval(Ctx{}) == -10);
        REQUIRE(expr.expr() == "(-2 * x)");
    }

    SECTION("Subtraction as addition of negation") {
        auto expr = x - y;
        REQUIRE(expr.eval(Ctx{}) == 3);
        REQUIRE(expr.expr() == "(x + (-1 * y))");
    }

    SECTION("Composed: (x - y) + (-x)") {
        auto expr = (x - y) + (-x);
        REQUIRE(expr.eval(Ctx{}) == -2);
        REQUIRE(expr.expr() == "(x + (-1 * y) + (-1 * x))");
    }

    SECTION("Deeply nested composition with subtraction") {
        auto expr = x + (-x - y + y - three);
        REQUIRE(expr.eval(Ctx{}) == -3);
        REQUIRE(expr.expr() == "(x + (-1 * x) + (-1 * y) + y + (-1 * 3))");
    }
}
