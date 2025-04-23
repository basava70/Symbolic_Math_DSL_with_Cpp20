/**
 * \file expressions.hpp
 * \brief Compile-time symbolic math DSL using modern C++20.
 *
 * This header implements a domain-specific language (DSL) for symbolic
 * algebraic expressions with compile-time evaluation and formatting support.
 *
 * \note Inspired by expression template techniques used in libraries like
 * Eigen.
 *
 * All constructs are normalized to scaled form for uniformity, and composed
 * using CRTP for zero-overhead polymorphism. This DSL is extensible toward
 * simplification, symbolic differentiation, and backend-optimized evaluation.
 *
 * \author Seshadri Basava
 * \date 2025
 */

#ifndef MATH_DSL
#define MATH_DSL

#include "utility.hpp"

namespace math_dsl {

/**
 * \class InputPair
 * \brief Represents a symbolic key-value binding at compile time.
 * \tparam symbol Symbolic variable identifier (e.g., 'x')
 * \tparam v Compile-time constant value associated with symbol
 *
 * Used in symbolic environments (Input) to evaluate expressions.
 */
template <char symbol, auto v>
    requires IsArithmetic<decltype(v)>
struct InputPair {
        static constexpr char id = symbol;
        static constexpr auto value = v;
        using type = decltype(v);
        using is_input_pair = void;
};

/**
 * \brief Recursive compile-time lookup for a symbol in an input context.
 * \tparam symbol Symbol being searched
 * \tparam Tail Variadic list of InputPair types
 */
template <char symbol, IsInputPair... Tail> struct GetImpl;

template <char symbol, IsInputPair Head, IsInputPair... Tail>
struct GetImpl<symbol, Head, Tail...> {
        static constexpr auto value = []() {
            if constexpr (Head::id == symbol)
                return Head::value;
            else
                return GetImpl<symbol, Tail...>::value;
        }();
};

/**
 * \brief Triggers a compile-time error if the symbol is not found.
 */
template <char symbol> struct GetImpl<symbol> {
        static_assert(symbol != symbol, "Symbol not found in input context.");
};

/**
 * \class Input
 * \brief Compile-time context for evaluating symbolic expressions.
 * \tparam input_pairs A variadic list of InputPair bindings.
 *
 * Access values via `get<symbol>()`.
 */
template <IsInputPair... input_pairs> struct Input {
        template <char symbol> static constexpr auto get() {
            return GetImpl<symbol, input_pairs...>::value;
        }
        using is_input = void;
};

/**
 * \class ExpressionBase
 * \brief CRTP base class for all symbolic expressions.
 * \tparam Derived Expression type inheriting from this base.
 *
 * Provides `eval()` and `expr()` forwarding via static dispatch.
 */
template <typename Derived> struct ExpressionBase {
        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &input) const noexcept {
            return derived().eval(input);
        }

        [[nodiscard]] constexpr std::string expr() const {
            return derived().expr();
        }

    protected:
        constexpr Derived &derived() { return static_cast<Derived &>(*this); }
        constexpr const Derived &derived() const {
            return static_cast<const Derived &>(*this);
        }

        using is_expression = void;
};

/**
 * \class RawScalar
 * \brief Represents a compile-time constant expression.
 * \tparam v A literal arithmetic value.
 *
 * Used internally by Scalar via normalization: Scalar<3> → Scaled<1,
 * RawScalar<3>>.
 */
template <auto v>
    requires IsArithmetic<decltype(v)>
struct RawScalar : ExpressionBase<RawScalar<v>> {
        static constexpr decltype(v) value = v;
        using type = decltype(v);

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &) const noexcept {
            return value;
        }

        [[nodiscard]] constexpr auto eval() const noexcept { return value; }

        [[nodiscard]] constexpr std::string expr() const {
            return utility::format_floating_point(value);
        }

        using is_expression = void;
        using is_raw_scalar = void;
};

template <auto v>
constexpr inline decltype(v) RawScalar_v = RawScalar<v>::value;
template <auto v> using RawScalar_t = RawScalar<v>::type;

/**
 * \class RawVariable
 * \brief Represents a symbolic variable for compile-time evaluation.
 * \tparam symbol A character denoting the variable name (e.g., 'x').
 */
template <char symbol>
struct RawVariable : ExpressionBase<RawVariable<symbol>> {
        using type = char;

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &input) const noexcept {
            return input.template get<symbol>();
        }

        [[nodiscard]] constexpr std::string expr() const {
            return std::string(1, symbol);
        }

        using is_expression = void;
        using is_raw_variable = void;
};

/**
 * \class Scaled
 * \brief Represents scaled expressions: coeff * expr.
 * \tparam Coeff A compile-time constant coefficient.
 * \tparam E An expression type.
 */
template <auto Coeff, IsExpression E>
struct Scaled : ExpressionBase<Scaled<Coeff, E>> {
        using Expr = E;
        static constexpr auto coeff = Coeff;

        Scaled(E const &expr)
            : m_expr(expr) {}

        [[nodiscard]] constexpr auto eval() const noexcept {
            return coeff * m_expr.eval();
        }

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &input) const noexcept {
            if constexpr (coeff == 0)
                return 0;
            else
                return coeff * m_expr.eval(input);
        }

        [[nodiscard]] constexpr std::string expr() const {
            if constexpr (coeff == 1)
                return m_expr.expr();
            else if constexpr (coeff == -1)
                return "(-" + m_expr.expr() + ")";
            else if constexpr (coeff == 0)
                return "0";
            else
                return utility::format_floating_point(coeff) + m_expr.expr();
        }

        E m_expr;
        using is_expression = void;
        using is_scaled = void;
};

/**
 * \class Variable
 * \brief Public-facing symbolic variable.
 * \tparam symbol A character literal (e.g., 'x')
 */
template <char symbol> struct Variable : Scaled<1, RawVariable<symbol>> {
        constexpr Variable()
            : Scaled<1, RawVariable<symbol>>{RawVariable<symbol>{}} {}
        using is_variable = void;
};

/**
 * \class Scalar
 * \brief Public-facing compile-time constant expression.
 * \tparam v An arithmetic literal.
 */
template <auto v>
    requires IsArithmetic<decltype(v)>
struct Scalar : Scaled<1, RawScalar<v>> {
        constexpr Scalar()
            : Scaled<1, RawScalar<v>>{RawScalar<v>{}} {}
        static constexpr auto value = v;
        using is_scalar = void;
};

/**
 * \class Add
 * \brief Variadic addition expression.
 * \tparam Exprs A list of symbolic expressions.
 *
 * Recursively flattens nested Add types through operator+.
 */
template <IsExpression... Exprs> struct Add : ExpressionBase<Add<Exprs...>> {
        std::tuple<Exprs...> m_exprs;

        constexpr explicit Add(Exprs const &...exprs)
            : m_exprs(exprs...) {}

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &input) const noexcept {
            return std::apply(
                [&](auto const &...exprs) { return (exprs.eval(input) + ...); },
                m_exprs);
        }

        [[nodiscard]] std::string expr() const {
            if constexpr (sizeof...(Exprs) == 0) {
                return "()";
            } else {
                return std::apply(
                    [&](auto const &first, auto const &...rest) {
                        std::string result = "(" + first.expr();
                        ((result += " + " + rest.expr()), ...);
                        result += ")";
                        return result;
                    },
                    m_exprs);
            }
        }

        using is_expression = void;
        using is_add = void;
};

template <IsExpression... Exprs> Add(Exprs...) -> Add<Exprs...>;

/// \name Operator Overloads
/// \{

/** \brief Addition of two expressions. */
template <IsExpression LHS, IsExpression RHS>
constexpr auto operator+(LHS const &lhs, RHS const &rhs) {
    return Add<LHS, RHS>(lhs, rhs);
}

/** \brief Left-associative flattening of Add with new term. */
template <IsExpression... LHSExprs, IsExpression RHS>
constexpr auto operator+(Add<LHSExprs...> const &lhs, RHS const &rhs) {
    return std::apply(
        [&](auto const &...exprs) {
            return Add<LHSExprs..., RHS>{exprs..., rhs};
        },
        lhs.m_exprs);
}

template <IsExpression LHS, IsExpression... RHSExprs>
constexpr auto operator+(LHS const &lhs, Add<RHSExprs...> const &rhs) {
    return std::apply(
        [&](auto const &...terms) {
            return Add<LHS, RHSExprs...>{lhs, terms...};
        },
        rhs.m_exprs);
}

template <IsExpression... LHSExprs, IsExpression... RHSExprs>
constexpr auto operator+(Add<LHSExprs...> const &lhs,
                         Add<RHSExprs...> const &rhs) {
    return std::apply(
        [&](auto const &...rhs_terms) {
            return (lhs + ... + rhs_terms); // recursive reuse of Add + Expr
        },
        rhs.m_exprs);
}

/** \brief Unary negation of an expression. */
template <IsExpression Expr> constexpr auto operator-(Expr const &expr) {
    return Scaled<-1, Expr>{expr};
}

/** \brief Specialization for negating an existing Scaled term. */
template <IsScaled S> constexpr auto operator-(S const &s) {
    constexpr auto new_coeff = -1 * S::coeff;
    using Expr = typename S::Expr;
    return Scaled<new_coeff, Expr>{s.m_expr};
}

/** \brief Subtraction is defined as lhs + (-rhs). */
template <IsExpression LHS, IsExpression RHS>
constexpr auto operator-(LHS const &lhs, RHS const &rhs) {
    return lhs + (-rhs);
}

template <IsExpression... Exprs>
struct Multiplication : ExpressionBase<Multiplication<Exprs...>> {
        constexpr Multiplication(Exprs const &...exprs)
            : m_exprs(exprs...) {}

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &input) const noexcept {

            return std::apply(
                [&](auto const &...terms) { return (terms.eval(input) * ...); },
                m_exprs);
        }

        [[nodiscard]] std::string expr() const {
            return std::apply(
                [&](auto const &first, auto const &...last) {
                    std::string result = first.expr();
                    ((result += last.expr()), ...);
                    return result;
                },
                m_exprs);
        }

        std::tuple<Exprs...> m_exprs;
        using is_expression = void;
        using is_muliplication = void;
};

// Base operator*
template <IsScalar LHS, IsScalar RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    constexpr auto new_coeff = LHS::value * RHS::value;
    return Scalar<new_coeff>{};
}

template <IsScalar LHS, IsPureScaled RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    constexpr auto new_coeff = LHS::value * RHS::coeff;
    return Scaled<new_coeff, typename RHS::Expr>{rhs.m_expr};
}

template <IsPureScaled LHS, IsScalar RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    constexpr auto new_coeff = RHS::value * LHS::coeff;
    return Scaled<new_coeff, typename LHS::Expr>{lhs.m_expr};
}

template <IsPureScaled LHS, IsPureScaled RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    constexpr auto new_coeff = LHS::coeff * RHS::coeff;
    using LExpr = LHS::Expr;
    using RExpr = RHS::Expr;
    using NewExpr = Multiplication<Scaled<1, LExpr>, Scaled<1, RExpr>>;
    return Scaled<new_coeff, NewExpr>{
        NewExpr{Scaled<1, LExpr>{lhs.m_expr}, Scaled<1, RExpr>{rhs.m_expr}}};
}

template <IsPureScaled LHS, auto Coeff, IsPureScaled... RHSExprs>
    requires IsArithmetic<decltype(Coeff)>
constexpr auto
operator*(LHS const &lhs,
          Scaled<Coeff, Multiplication<RHSExprs...>> const &rhs) {
    constexpr auto new_coeff = LHS::coeff * Coeff;
    using LExpr = typename LHS::Expr;
    using NewExpr = Multiplication<Scaled<1, LExpr>, RHSExprs...>;
    return std::apply(
        [&](auto const &...rest) {
            return Scaled<new_coeff, NewExpr>{
                NewExpr{Scaled<1, LExpr>{lhs.m_expr}, rest...}};
        },
        rhs.m_expr.m_exprs);
}

template <auto Coeff, IsPureScaled... LHSExprs, IsPureScaled RHS>
    requires IsArithmetic<decltype(Coeff)>
constexpr auto operator*(Scaled<Coeff, Multiplication<LHSExprs...>> const &lhs,
                         RHS const &rhs) {
    constexpr auto new_coeff = Coeff * RHS::coeff;
    using RExpr = RHS::Expr;
    using NewExpr = Multiplication<LHSExprs..., Scaled<1, RExpr>>;
    return std::apply(
        [&](auto const &...rest) {
            return Scaled<new_coeff, NewExpr>{
                NewExpr{rest..., Scaled<1, RExpr>{rhs.m_expr}}};
        },
        lhs.m_expr.m_exprs);
}

template <auto C1, IsScaled... LHSExprs, auto C2, IsScaled... RHSExprs>
constexpr auto operator*(Scaled<C1, Multiplication<LHSExprs...>> const &lhs,
                         Scaled<C2, Multiplication<RHSExprs...>> const &rhs) {
    constexpr auto new_coeff = C1 * C2;
    using NewExpr = Multiplication<LHSExprs..., RHSExprs...>;

    return std::apply(
        [&](auto const &...lhsterms) {
            return std::apply(
                [&](auto const &...rhsterms) {
                    return Scaled<new_coeff, NewExpr>{
                        NewExpr{lhsterms..., rhsterms...}};
                },
                rhs.m_expr.m_exprs);
        },
        lhs.m_expr.m_exprs);
}

template <typename... Ts> constexpr auto make_add(Ts const &...args) {
    return Add<Ts...>{args...};
}

template <IsPureScaled LHS, IsExpression... RHSExprs>
constexpr auto operator*(LHS const &lhs, Add<RHSExprs...> const &rhs) {
    return std::apply(
        [&](auto const &...terms) { return make_add((lhs * terms)...); },
        rhs.m_exprs);
}

template <IsExpression... LHSExprs, IsPureScaled RHS>
constexpr auto operator*(Add<LHSExprs...> const &lhs, RHS const &rhs) {
    return std::apply(
        [&](auto const &...terms) { return make_add((terms * rhs)...); },
        lhs.m_exprs);
}

//[FIXME]
// Have to go to Add + Add I think
// template <IsExpression... LHSExprs, IsExpression... RHSExprs>
// constexpr auto operator*(Add<LHSExprs...> const &lhs,
//                          Add<RHSExprs...> const &rhs) {
//     return std::apply(
//         [&](auto const &...lhs_terms) {
//             return make_add(([](auto const &lhs_term) {
//                 return std::apply(
//                     [&](auto const &...rhs_terms) {
//                         return make_add((lhs_term * rhs_terms)...);
//                     },
//                     rhs.m_exprs);
//             }(lhs_terms)...));
//         },
//         lhs.m_exprs);
// }

} // namespace math_dsl

#endif // MATH_DSL
