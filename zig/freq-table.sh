#!/bin/bash
zig run tournament.zig 2>&1 | tr ' ' '\n' | sort | uniq -c | awk '{t=$1; $1=$2; $2=t; print}'
