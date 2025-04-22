#!/bin/bash

if cmake --build build; then
    echo "Build succeeded. Running program..."
    ./build/math_dsl
else
    echo "Build failed. Skipping program."
    exit 1
fi
