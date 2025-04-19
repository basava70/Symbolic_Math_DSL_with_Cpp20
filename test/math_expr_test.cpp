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
