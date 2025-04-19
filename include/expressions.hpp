#ifndef MATH_EXPRESSION
#define MATH_EXPRESSION

#include "utility.hpp"
#include <numeric>

namespace math_expr {

/**
 * \class InputPair
 * \brief Represents a symbolic key-value binding used in symbolic context.
 *
 * \tparam symbol A character literal representing the symbolic name (e.g.,
 * 'x').
 * \tparam v    The compile-time constant value associated with the symbol.
 *
 * This structure is used as the building block for input in the symbolic
 * environments like \c Input<InputPair<'x', 5>>.
 *
 * \see Input, IsInputPair
 */
template <char symbol, auto v>
    requires IsArithmetic<decltype(v)>
struct InputPair {
        static constexpr char id = symbol; ///< Symbolic identifier (e.g., 'x')
        static constexpr auto value =
            v;                      ///< Constant value bound to the identifier
        using type = decltype(v);   ///< Deduced type of the value
        using is_input_pair = void; ///< Tag used for concept detection
};

/**
 * \class GetImpl
 * \brief Recursive compile-time metafunction to retrieve the value for a given
 * symbol.
 *
 * This class performs a compile-time search through a variadic list of
 * \c InputPair types to resolve the value associated with a given symbol.
 * It powers the symbolic lookup functionality in \c Input::get().
 *
 * \tparam symbol The symbol being looked up (e.g., 'x').
 * \tparam Tail A variadic pack of \c InputPair types representing the
 * environment.
 *
 * \note This lookup is evaluated entirely at compile-time and is
 * constexpr-safe.
 *
 * \see Input, InputPair
 */

/**
 * \brief Primary template declaration for \c GetImpl.
 */
template <char symbol, IsInputPair... Tail> struct GetImpl;

/**
 * \brief Recursive case of \c GetImpl.
 *
 * Compares \c Head::id with the target \c symbol.
 * If matched, returns \c Head::value; otherwise recurses.
 *
 * \tparam symbol The symbol being searched for.
 * \tparam Head The current \c InputPair being checked.
 * \tparam Tail The remaining input pairs.
 */
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
 * \brief Base case of \c GetImpl that triggers a compile-time failure.
 *
 * This specialization is instantiated only when no matching symbol
 * is found in the input list.
 */
template <char symbol> struct GetImpl<symbol> {
        static_assert(symbol != symbol,
                      "Symbol not found in the input context");
};

/**
 * \class Input
 * \brief Represents a symbolic context made up of InputPairs.
 *
 * Used for compile-time evaluation of symbolic expressions such as
 * \c Variable<'x'>, by resolving them through \c get<symbol>().
 *
 * \tparam input_pairs A variadic list of \c InputPair bindings.
 *
 * \see InputPair, GetImpl, Variable
 */

// [TODO] Runtime input
template <IsInputPair... input_pairs> struct Input {
        template <char symbol> static constexpr auto get() {
            return GetImpl<symbol, input_pairs...>::value;
        }
        using is_input = void; ///< Tag used for concept detection (\c IsInput)
};

/**
 * \class ExpressionBase
 * \brief CRTP base class for all symbolic expression types.
 *
 * Provides uniform \c eval() and \c expr() forwarding interfaces for derived
 * types. Enables static polymorphism for symbolic operations like \c Add,
 * \c Scalar, \c Variable.
 *
 * \tparam Derived The expression type inheriting from this base.
 *
 * \see IsExpression
 */
template <typename Derived> struct ExpressionBase {
        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return derived().eval(context);
        }

        [[nodiscard]] constexpr std::string expr() const {
            return derived().expr();
        }

    protected:
        constexpr Derived &derived() { return static_cast<Derived &>(*this); }
        constexpr const Derived &derived() const {
            return static_cast<const Derived &>(*this);
        }

        using is_expression = void; ///< Tag used for concept detection
};

/**
 * \class Scalar
 * \brief Represents a compile-time constant value in a symbolic expression.
 *
 * \tparam v A constant value of an arithmetic type.
 *
 * \c Scalar can be evaluated with or without context, and prints as its raw
 * value.
 *
 * \see ExpressionBase
 */
template <auto v>
    requires IsArithmetic<decltype(v)>
struct Scalar : ExpressionBase<Scalar<v>> {
        static constexpr decltype(v) value = v; ///< The constant value
        using type = decltype(v);               ///< Deduced type of the value

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &) const noexcept {
            return value;
        }

        [[nodiscard]] constexpr auto eval() const noexcept { return value; }

        [[nodiscard]] constexpr std::string expr() const {
            return utility::format_floating_point(value);
        }

        using is_expression = void;
        using is_scalar = void;
};

template <auto v> constexpr inline decltype(v) Scalar_v = Scalar<v>::value;
template <auto v> using Scalar_t = Scalar<v>::type;

/**
 * \class Variable
 * \brief Represents a symbolic variable that evaluates using a symbolic input
 * context.
 *
 * \tparam symbol A character literal denoting the variable symbol (e.g., 'x').
 *
 * \see Input, ExpressionBase
 */
template <char symbol> struct Variable : ExpressionBase<Variable<symbol>> {
        using type = char;

        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return context.template get<symbol>();
        }

        [[nodiscard]] constexpr std::string expr() const {
            return std::string(1, symbol);
        }

        using is_expression = void;
        using is_variable = void;
};

template <auto Coeff, IsExpression E>
struct Scaled : ExpressionBase<Scaled<Coeff, E>> {
        using Expr = E;
        static constexpr auto coeff = Coeff;

        Scaled(E const &expr)
            : m_expr(expr) {}
        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &context) const noexcept {
            if constexpr (coeff == 0)
                return 0;
            else
                return coeff * m_expr.eval(context);
        }

        [[nodiscard]] constexpr std::string expr() const {
            if constexpr (coeff == 1)
                return m_expr.expr();
            else if constexpr (coeff == 0)
                return "0";
            else {
                return "(" + utility::format_floating_point(coeff) + " * " +
                       m_expr.expr() + ")";
            }
        }

        E m_expr;
        using is_expression = void;
        using is_scaled = void;
};

// [TODO]
// Look into std::move and std::forward aspects to
// avoid copies in both lvalue and rvalue expressions.
//
// Add variadic templates.
template <IsExpression... Exprs> struct Add : ExpressionBase<Add<Exprs...>> {
        std::tuple<Exprs...> m_exprs;

        constexpr explicit Add(Exprs const &...exprs)
            : m_exprs(exprs...) {}

        template <IsInput Input>
        [[nodiscard]] constexpr auto eval(Input const &context) const noexcept {
            return std::apply(
                [&](auto const &...exprs) {
                    return (exprs.eval(context) + ...);
                },
                m_exprs);
        }

        [[nodiscard]] std::string expr() const {
            if constexpr (sizeof...(Exprs) == 0) {
                return "()";
            } else {
                std::array<std::string, sizeof...(Exprs)> expressions{
                    std::apply(
                        [](auto const &...terms) {
                            return std::array<std::string, sizeof...(terms)>{
                                terms.expr()...};
                        },
                        m_exprs)};
                return "(" +
                       std::accumulate(std::next(expressions.begin()),
                                       expressions.end(), expressions[0],
                                       [](std::string a, std::string const &b) {
                                           return std::move(a) + " + " + b;
                                       }) +
                       ")";
            }
        }

        using is_expression = void;
        using is_add = void;
};

} // namespace math_expr

#endif // MATH_EXPRESSION
