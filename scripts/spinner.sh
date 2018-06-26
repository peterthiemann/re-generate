#!/bin/bash

"$@" &

while kill -0 $!; do
    printf '.'
    sleep 2
done

printf '\n'
