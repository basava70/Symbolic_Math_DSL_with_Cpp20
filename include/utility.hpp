#ifndef MATH_EXPRESSION_UTILITY
#define MATH_EXPRESSION_UTILITY
#include "concepts.hpp"
#include <iostream>
#include <sstream>
#include <string>

namespace math_expr::utility {

/**
 * \brief Formats a floating-point number to a fixed precision without trailing
 * zeros.
 *
 * Converts the given double \c value into a string with fixed-point notation.
 * Trailing zeros and a dangling decimal point are trimmed to produce clean
 * output.
 *
 * \param value The floating-point number to format.
 * \param precision Number of digits after the decimal (default is 6).
 * \return A trimmed string representation of the number.
 *
 * \example
 * \code
 * format_floating_point(3.140000); // returns "3.14"
 * format_floating_point(42.0);     // returns "42"
 * \endcode
 */
inline std::string format_floating_point(double value, int precision = 6) {
    std::ostringstream oss;
    oss.precision(precision);
    oss << std::fixed << value;
    std::string str = oss.str();

    // Trim trailing zeros
    auto end = str.find_last_not_of('0');
    if (end != std::string::npos) {
        if (str[end] == '.') {
            // Remove the decimal point if it's dangling
            str.erase(end);
        } else {
            str.erase(end + 1);
        }
    }

    return str;
}

/**
 * \brief Checks if a symbolic expression string is composite (i.e., contains
 * operators).
 *
 * An expression is considered composite if it contains any arithmetic operator
 * like \c +, \c -, \c *, or \c /.
 *
 * \param expr The expression string to inspect.
 * \return \c true if the expression contains any binary operators; otherwise \c
 * false.
 */
inline bool is_composite(std::string const &expr) {
    return expr.find_first_of("+*/-") != std::string::npos;
}

// #include <cxxabi.h>
// #include <typeinfo>
//
// template <typename T> std::string demangled_type_name() {
//     int status;
//     char *name = abi::__cxa_demangle(typeid(T).name(), 0, 0, &status);
//     std::string result = (status == 0) ? name : typeid(T).name();
//     std::free(name);
//     return result;
// }
//
// template <IsExpression Expr> void print_type_name(Expr const &expr) {
//     std::cout << "expr : " << demangled_type_name<Expr>() << "\n";
// }

} // namespace math_expr::utility
#endif
