#include "expressions.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace math_expr;
using Catch::Approx;

TEST_CASE("Scalar") {
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

TEST_CASE("Variable") {
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
    Scalar<1> one;
    Scalar<2> two;
    Scalar<3> three;
    Variable<'x'> x;

    SECTION("Simple scalar addition") {
        auto expr = one + two;
        REQUIRE(expr.eval(Input<>{}) == 3);
        REQUIRE(expr.expr() == "(1 + 2)");
    }

    SECTION("Scalar and variable addition") {
        using context = Input<InputPair<'x', 5>>;
        auto expr = one + x;
        REQUIRE(expr.eval(context{}) == 6);
        REQUIRE(expr.expr() == "(1 + x)");
    }

    SECTION("Nested addition: (1 + x) + 2") {
        using context = Input<InputPair<'x', 4>>;
        auto expr = (one + x) + two;
        REQUIRE(expr.eval(context{}) == 7);
        REQUIRE(expr.expr() == "(1 + x + 2)");
    }

    SECTION("Multi-term addition: 1 + 2 + x + 3") {
        using context = Input<InputPair<'x', 7>>;
        auto expr = one + two + x + three;
        REQUIRE(expr.eval(context{}) == 13);
        REQUIRE(expr.expr() == "(1 + 2 + x + 3)");
    }

    SECTION("Addition with only variable") {
        using context = Input<InputPair<'x', 9>>;
        auto expr = x + x + x + x;
        REQUIRE(expr.eval(context{}) == 36);
        REQUIRE(expr.expr() == "(x + x + x + x)");
    }

    SECTION("Empty Add expression") {
        Add<> empty;
        REQUIRE(empty.expr() == "()");
    }
}
