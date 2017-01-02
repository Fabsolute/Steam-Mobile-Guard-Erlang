#!/bin/sh
gmake
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname steam_guard_code_generator_dev \
    -s steam_guard_code_generator \
    -s reloader
