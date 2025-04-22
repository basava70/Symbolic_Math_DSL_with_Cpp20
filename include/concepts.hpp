/**
 * \concept IsArithmetic
 * \brief Matches any arithmetic type except \c char, for compile-time
 * constants.
 */
#include <type_traits>
template <typename T>
concept IsArithmetic = std::is_arithmetic_v<T>;
/**
 * \concept IsInputPair
 * \brief Matches types tagged with \c is_input_pair.
 *
 * This concept is satisfied when a type defines the marker alias
 * \c using is_input_pair = void; which identifies it as a valid
 * compile-time input pair.
 *
 * Used to constrain \c Input, \c GetImpl, and other components of the
 * symbolic evaluation context to accept only suitable bindings.
 *
 * \see InputPair
 */
template <typename T>
concept IsInputPair = requires { typename T::is_input_pair; };
/**
 * \concept IsInput
 * \brief Matches types tagged with \c using is_input = void;, such as
 * \c Input<...>.
 */
template <typename T>
concept IsInput = requires { typename T::is_input; };
/**
 * \concept IsExpression
 * \brief Matches types tagged with \c using is_expression = void;,
 *        i.e., valid symbolic expressions.
 */
template <typename T>
concept IsExpression = requires { typename T::is_expression; };
template <typename T>
concept IsRawVariable =
    requires { typename T::is_rawvariable; } && IsExpression<T>;
template <typename T>
concept IsRawScalar = requires { typename T::is_rawscalar; } && IsExpression<T>;
template <typename T>
concept IsScalar = requires { typename T::is_scalar; } && IsExpression<T>;
template <typename T>
concept IsScaled = requires { typename T::is_scaled; } && IsExpression<T>;
template <typename T>
concept IsPureScaled =
    requires { typename T::is_scaled; } && !IsScalar<T> && IsExpression<T>;
template <typename T>
concept IsAddition = requires { typename T::is_addition; } && IsExpression<T>;
template <typename T>
concept IsMultiplication =
    requires { typename T::is_multiplication; } && IsExpression<T>;
/// \concept IsScalarZero
/// \brief Matches only Scalar<0> expressions (used for identity simplification)
template <typename T>
concept IsScalarZero = IsScalar<T> && (T::value == 0);

/// \concept IsScalarOne
/// \brief Matches only Scalar<1> expressions (used for identity simplification)
template <typename T>
concept IsScalarOne = IsScalar<T> && (T::value == 1);

template <typename T>
concept IsNotScalar = IsExpression<T> && !IsScalar<T>;

template <IsMultiplication T> struct is_scalar_multiple {
        using LHS = decltype(std::declval<T>().m_lhs);
        using RHS = decltype(std::declval<T>().m_rhs);

        static constexpr bool value = (IsScalar<LHS> && IsNotScalar<RHS>) ||
                                      (IsNotScalar<LHS> && IsScalar<RHS>);
};

template <typename T>
concept IsScalarMultiple = IsExpression<T> && is_scalar_multiple<T>::value;
