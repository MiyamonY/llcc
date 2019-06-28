#!/bin/bash
try() {
  expected="$1"
  input="$2"

  racket llcc.rkt "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 42 42
try 21 '5+20-4'
try 41 ' 12   + 34 - 5'
try 17 '4 * 3 + 5'
try 19 '4 + 3 * 5'
try 2 '4/4 + 1'
try 31 '3*3/2 + 10*30/10 -3'

echo OK
