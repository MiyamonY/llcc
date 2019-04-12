#! /bin/sh
try() {
    expected="$1"
    input="$2"

    ./main.rkt "$input" > tmp.s
    gcc -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$expected expected, but got $actual"
        exit 1
    fi
}

try 0 0
try 42 42
try 6 "1 + 2 + 3"
try 48 "23 - 1 + 24 + 2"

echo "OK"
