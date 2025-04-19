#ifndef MATH_EXPRESSION
#define MATH_EXPRESSION

#include "utility.hpp"

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
/**
 * \class Add
 * \brief Symbolic addition of two expressions.
 *
 * \tparam LHS Left-hand side expression (must satisfy \c IsExpression).
 * \tparam RHS Right-hand side expression (must satisfy \c IsExpression).
 *
 * Provides recursive \c eval() and symbolic \c expr() printing.
 *
 * \see operator+
 */
template <IsExpression LHS, IsExpression RHS>
struct Add : ExpressionBase<Add<LHS, RHS>> {
        constexpr Add(LHS const &lhs, RHS const &rhs)
            : m_lhs(lhs),
              m_rhs(rhs) {} ///< constructor.

        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return m_lhs.eval(context) + m_rhs.eval(context);
        }

        [[nodiscard]] std::string expr() const {
            return "(" + m_lhs.expr() + " + " + m_rhs.expr() + ")";
        }

        using is_expression = void;
        using is_add = void;

        LHS m_lhs; ///< Left-hand side operand
        RHS m_rhs; ///< Right-hand side operand
};

/**
 * \brief Deduction guide for \c Add, allowing CTAD with two expressions.
 */
template <IsExpression LHS, IsExpression RHS> Add(LHS, RHS) -> Add<LHS, RHS>;

/**
 * \brief Overload for symbolic addition of two expressions.
 *
 * \returns An \c Add<LHS, RHS> expression tree node.
 *
 * \note Requires both operands to satisfy \c IsExpression.
 */
template <IsExpression LHS, IsExpression RHS>
constexpr auto operator+(LHS const &lhs, RHS const &rhs) {
    return Add(lhs, rhs);
}

/**
 * \class Negation
 * \brief Symbolic unary negation of a single expression.
 *
 * \tparam Expr A symbolic expression (must satisfy \c IsExpression).
 *
 * This class represents the unary minus operation (e.g., \c -x or \c -(x + 3)).
 * It evaluates by recursively evaluating the inner expression and negating the
 * result. The symbolic string form is prefixed with a minus sign and
 * parenthesized if nested.
 *
 * \see ExpressionBase, operator-
 */
template <IsExpression Expr> struct Negation : ExpressionBase<Negation<Expr>> {
        /// Construct from a single sub-expression
        constexpr explicit Negation(Expr const &expr)
            : m_expr(expr) {}

        /// Evaluate as the negative of the inner expression
        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return static_cast<decltype(m_expr.eval(context))>(-1) *
                   m_expr.eval(context);
        }

        /// Symbolic representation: prefix with '-'
        [[nodiscard]] std::string expr() const { return "-" + m_expr.expr(); }

        using is_expression = void;
        using is_negation = void;

        Expr m_expr; ///< Stored expression to negate
};

/**
 * \brief Deduction guide for \c Negation, allowing CTAD from a single
 * expression.
 */
template <IsExpression Expr> Negation(Expr) -> Negation<Expr>;
/**
 * \brief Overload for symbolic negation of a given expression.
 *
 * \returns A \c Negation<Expr> expression node.
 *
 * \note Requires the expression to satisfy \c IsExpression.
 */
template <IsExpression Expr> constexpr auto operator-(Expr const &expr) {
    return Scaled<-1, Expr>{expr};
    // return Negation<Expr>(expr);
}

/**
 * \class Subtraction
 * \brief Symbolic binary subtraction of two expressions.
 *
 * \tparam LHS The left-hand side expression (must satisfy \c IsExpression).
 * \tparam RHS The right-hand side expression (must satisfy \c IsExpression).
 *
 * This class represents a symbolic subtraction expression \c (lhs - rhs).
 * Evaluation computes the difference between the recursively evaluated
 * operands. The string form is rendered as \c lhs - rhs without additional
 * parentheses, assuming operands are already properly parenthesized by their
 * respective types.
 *
 * \see operator-, ExpressionBase, Negation
 */
template <IsExpression LHS, IsExpression RHS>
struct Subtraction : ExpressionBase<Subtraction<LHS, RHS>> {
        constexpr Subtraction(LHS const &lhs, RHS const &rhs)
            : m_lhs(lhs),
              m_rhs(rhs) {}; ///< constructor
        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return m_lhs.eval(context) - m_rhs.eval(context);
        }

        [[nodiscard]] std::string expr() const {
            return "(" + m_lhs.expr() + " - " + m_rhs.expr() + ")";
        }
        using is_expression = void;

        LHS m_lhs; ///< Left-hand side operand
        RHS m_rhs; ///< Right-hand side operand
};
/**
 * \brief Deduction guide for \c Subtraction, enabling CTAD with two
 * expressions.
 */
template <IsExpression LHS, IsExpression RHS>
Subtraction(LHS, RHS) -> Subtraction<LHS, RHS>;
/**
 * \brief Overload for symbolic binary subtraction of two expressions.
 *
 * Constructs a \c Subtraction<LHS, RHS> expression node representing \c lhs -
 * rhs.
 *
 * \returns A symbolic expression object.
 *
 * \note Requires both operands to satisfy \c IsExpression.
 */
template <IsExpression LHS, IsExpression RHS>
constexpr auto operator-(LHS const &lhs, RHS const &rhs) {
    return Subtraction(lhs, rhs);
};

/**
 * \class Multiplication
 * \brief Symbolic multiplication of two expressions.
 *
 * \tparam LHS Left-hand side expression (must satisfy \c IsExpression).
 * \tparam RHS Right-hand side expression (must satisfy \c IsExpression).
 *
 * Produces a new symbolic expression representing the product \c (lhs * rhs).
 * Supports recursive evaluation and stringification.
 *
 * \see operator*, ExpressionBase
 */
template <IsExpression LHS, IsExpression RHS>
struct Multiplication : ExpressionBase<Multiplication<LHS, RHS>> {

        constexpr Multiplication(LHS const &lhs, RHS const &rhs)
            : m_lhs(lhs),
              m_rhs(rhs) {}; ///< constructor

        template <IsInput input>
        [[nodiscard]] constexpr auto eval(input const &context) const noexcept {
            return m_lhs.eval(context) * m_rhs.eval(context);
        }

        [[nodiscard]] std::string expr() const {
            return "(" + m_lhs.expr() + " * " + m_rhs.expr() + ")";
        }
        using is_expression = void;
        using is_multiplication = void;

        LHS m_lhs; ///< Left-hand side operand
        RHS m_rhs; ///< Right-hand side operand
};

/**
 * \brief Deduction guide for \c Multiplication, enabling CTAD with two
 * expressions.
 */
template <IsExpression LHS, IsExpression RHS>
Multiplication(LHS, RHS) -> Multiplication<LHS, RHS>;

/**
 * \brief Overload for symbolic multiplication of two expressions.
 *
 * \returns A \c Multiplication<LHS, RHS> expression node.
 *
 * \note Requires both operands to satisfy \c IsExpression.
 */
template <IsExpression LHS, IsExpression RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    return Multiplication<LHS, RHS>(lhs, rhs);
}
template <IsScalar LHS, IsExpression RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    return Scaled<LHS::value, RHS>{rhs};
}
template <IsExpression LHS, IsScalar RHS>
constexpr auto operator*(LHS const &lhs, RHS const &rhs) {
    return Scaled<RHS::value, LHS>{lhs};
}

// === Simplification Concepts and Rules ===

//[TODO]
// Mix both (ax + bx) -> (a+b)x and (ax + x) -> (a+1)x
// If possible, mix the same with subtraction too.

// [TODO] Simplification rules to implement
//
// - Add:
//   - Scalar<v1> + Scalar<v2>           -> Scalar<v1 + v2>  => done
//   - x + 0                             -> x                => done
//   - 0 + x                             -> x                => done
//   - x + x                             -> Scalar<2> * x    => done
//
// - Subtraction:
//   - x - 0                             -> x
//   - 0 - x                             -> -x
//   - Scalar<v1> - Scalar<v2>           -> Scalar<v1 - v2>
//   - x - x                             -> Scalar<0>
//
// - Negation:
//   - -(-x)                             -> x
//   - -Scalar<v>                        -> Scalar<-v>
//   - -Scalar<0>                        -> Scalar<0>
//
// - Multiplication:
//   - x * 0                             -> Scalar<0>
//   - 0 * x                             -> Scalar<0>
//   - x * 1                             -> x
//   - 1 * x                             -> x
//   - Scalar<v1> * Scalar<v2>           -> Scalar<v1 * v2>
//
// - Recursive simplification of subexpressions:
//   - simplify(lhs), simplify(rhs) before forming new expressions
//
// - Optional (advanced/future):
//   - x + y + x                         -> Scalar<2> * x + y
//   - Canonical ordering                -> (x + y) == (y + x)
//   - Flatten nested additions          -> (x + (y + z)) -> ((x + y) + z)
//   - Structural equality/comparison    -> equal(x + 0, x) == true

// === Simplification Overloads ===

//[TODO]
// change simplify to a member function and use CRTP
template <char Symbol> constexpr auto simplify_impl(Variable<Symbol> const &v) {
    return v;
}

template <auto V> constexpr auto simplify_impl(Scalar<V> const &s) { return s; }

// x + 0 -> x
template <IsExpression LHS>
constexpr auto simplify_impl(Add<LHS, Scalar<0>> const &) {
    return LHS{};
}
// 0 + x -> x
template <IsExpression RHS>
constexpr auto simplify_impl(Add<Scalar<0>, RHS> const &) {
    return RHS{};
}
// x + x -> 2x
template <IsExpression Expr>
constexpr auto simplify_impl(Add<Expr, Expr> const &) {
    return Scaled<2, Expr>{Expr{}};
}
// Scalar<u> + Scalar<v> -> Scalar<u+v>
template <IsScalar LHS, IsScalar RHS>
constexpr auto simplify_impl(Add<LHS, RHS> const &) {
    return Scalar<LHS::value + RHS::value>{};
}
// ax + x -> (a+1)x
template <IsScaled LHS, IsExpression RHS>
constexpr auto simplify_impl(Add<LHS, RHS> const &) {
    if constexpr (std::is_same_v<typename LHS::Expr, RHS>) {
        return Scaled<LHS::coeff + 1, RHS>{RHS{}};
    }
}
// x + ax -> (1+a)x
template <IsExpression LHS, IsScaled RHS>
constexpr auto simplify_impl(Add<LHS, RHS> const &) {
    if constexpr (std::is_same_v<typename RHS::Expr, LHS>) {
        return Scaled<RHS::coeff + 1, LHS>{LHS{}};
    }
}
// ax + bx -> (a+b)x
template <IsScaled LHS, IsScaled RHS>
constexpr auto simplify_impl(Add<LHS, RHS> const &) {
    if constexpr (std::is_same_v<typename LHS::Expr, typename RHS::Expr>) {
        using Expr = RHS::Expr;
        return Scaled<LHS::coeff + RHS::coeff, Expr>{Expr{}};
    }
}
// fallback for Scaled<Coeff, Epxr>{Expr{}}
template <IsScaled S> constexpr auto simplify_impl(S const &) {
    return Scaled<S::coeff, typename S::Expr>{typename S::Expr{}};
}

/// \brief Simplify symbolic addition
template <IsExpression LHS, IsExpression RHS>
constexpr auto simplify_impl(Add<LHS, RHS> const &expr) {
    auto simplified_lhs = simplify_impl(expr.m_lhs);
    auto simplified_rhs = simplify_impl(expr.m_rhs);
    using SimplifiedLHS = decltype(simplified_lhs);
    using SimplifiedRHS = decltype(simplified_rhs);

    return Add<SimplifiedLHS, SimplifiedRHS>{simplified_lhs, simplified_rhs};
}

/// \brief Top-level simplify function
template <IsExpression Expr> constexpr auto simplify(Expr const &expr) {
    auto simplified_expr = simplify_impl(expr);
    using SimplifiedExpr = decltype(simplified_expr);
    if constexpr (std::is_same_v<SimplifiedExpr, Expr>)
        return simplified_expr;
    else
        return simplify_impl(simplified_expr);
}

} // namespace math_expr

#endif // MATH_EXPRESSION
