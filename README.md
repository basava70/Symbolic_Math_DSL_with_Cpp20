# Symbolic Math DSL with C++20

A compile-time symbolic algebra library implemented in modern C++20 using template metaprogramming. This project provides a zero-overhead symbolic math DSL that models algebraic expressions with full compile-time evaluation, formatting, and simplification support. Designed with extensibility and clarity in mind, it is inspired by expression template techniques in libraries like Eigen.

---

## 🚀 Features

- **Compile-Time Evaluation**: Symbolic expressions are evaluated entirely at compile time using `constexpr` techniques.
- **Expression Simplification**: Includes simplification rules such as: (Have to be implemented)
  - `x + x → 2 * x`
  - `a * x + b * x → (a + b) * x` (already implemented)
- **CRTP-Based Expression Hierarchy**: All expressions inherit from `ExpressionBase<Derived>` for zero-cost polymorphism.
- **Uniform Representation**: All expressions normalized as `Scaled<Coeff, Expr>` to simplify folding, evaluation, and transformation.
- **Algebraic Operators**: Natural support for `+`, `-`, `*`, variadic `Add`, and `Multiplication` with correct operator precedence.
- **Tuple-Based Internal Storage**: Variadic expressions internally stored in `std::tuple` and manipulated via `std::apply`.
- **Fully Header-Only**: Just include the headers and use the DSL in your project.

---

## 📂 Directory Structure

```
Symbolic_Math_DSL_with_Cpp20/
├── include/
│   ├── expressions.hpp   # Core DSL types and operators
│   ├── concepts.hpp      # Concepts for constraining symbolic types
│   └── utility.hpp       # Formatting and type utilities
├── tests/
│   └── test.cpp          # Catch2 unit tests
├── CMakeLists.txt        # Build file for tests (Catch2)
└── README.md             # This file
```

---

## 🔧 Example Usage

```cpp
#include "expressions.hpp"
using namespace math_dsl;

constexpr Variable<'x'> x;
constexpr Variable<'y'> y;
constexpr Scalar<2> two;

constexpr auto expr = x * (y + two);
static_assert(expr.eval(Input<InputPair<'x', 3>, InputPair<'y', 5>>{}) == 21);

std::cout << expr.expr(); // prints: (x*y + 2x)
```

---

## 🧪 Building and Testing

This project uses [Catch2](https://github.com/catchorg/Catch2) for unit testing.

```bash
git clone https://github.com/basava70/Symbolic_Math_DSL_with_Cpp20.git
cd Symbolic_Math_DSL_with_Cpp20
mkdir build && cd build
cmake ..
make
./check.sh
./run.sh
```

---

## ✅ Planned Features

- [ ] Division operator `/` support
- [ ] Exponentiation (e.g., `x^n`)
- [ ] Symbolic logarithms (e.g., `log(x)`)
- [ ] Symbolic differentiation (`d/dx` support)

---

## 📜 License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---
