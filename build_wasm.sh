#!/usr/bin/env bash

set -xe

cd jopwas && \
    wasm-pack build -t no-modules && \
    cd .. && \
    cp jopwas/pkg/* www/
