#!/bin/bash

if cmake --build build; then
    echo "Build succeeded. Running tests..."
    ./build/math_dsl_test
else
    echo "Build failed. Skipping tests."
    exit 1
fi
