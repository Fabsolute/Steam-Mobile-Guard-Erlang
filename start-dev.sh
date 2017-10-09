#!/bin/sh
rebar g-d
rebar compile
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname steam_guard_code_generator_dev \
    -s steam_mobile_guard
