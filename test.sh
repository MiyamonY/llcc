#!/bin/bash
try() {
  expected="$1"
  input="$2"

  racket llcc.rkt "$input" > tmp.s
  gcc -c test.c
  gcc -o tmp tmp.s test.o
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

try 42 'main(){42;}'
try 21 'main(){5+20-4;}'
try 41 'main(){ 12   + 34 - 5;}'
try 17 'main(){4 * 3 + 5;}'
try 19 'main(){4 + 3 * 5;}'
try 2 'main(){4/4 + 1;}'
try 31 'main(){3*3/2 + 10*30/10 -3;}'
try 11 'main(){(3+2)*3/3*2+1;}'
try 15 'main(){(((2* 17)+((3  *2)+3)  ) - ((3+4)*(2/1)+(3*4+4)) + 2);}'
try 10 'main(){ 30-20;}'
try 10 'main(){(-10+20);}'
try 15 'main(){(-10)+20 + + 30--10*-1 + -15;}'
try 0 'main(){2 == 2 * 3;}'
try 4 'main(){(3*2 == 4 + 2) * 4;}'
try 2 'main(){(2 != 2) * 4+2/(2 != 1);}'
try 0 'main(){15<3 * 4;}'
try 0 'main(){15<3*5;}'
try 1 'main(){15<12+4;}'
try 0 'main(){15<=3 * 4;}'
try 1 'main(){15<=3*5;}'
try 1 'main(){15<=14+2;}'
try 0 'main(){0<=12<0;}'
try 1 'main(){15>3 * 4;}'
try 0 'main(){15>3*5;}'
try 0 'main(){15>12+4;}'
try 1 'main(){30/2>=3*5-1;}'
try 1 'main(){3*5>=3*5;}'
try 0 'main(){10+5>=28/2+2;}'
try 12 'main(){abc=10; abc+2;}'
try 28 'main(){awf=awef=3+2*2; awef+awf*3;}'
try 170 'main(){abc=3*4+5; return 10*abc; abc=0; return abc;}'
try 10 'main(){if (3*4*0) return 1; else return 10;}'
try 3 'main(){if ( 2*4+1 ) x=3; else x=5; return x;}'
try 5 'main(){x=0; while(x<5) x = x + 1; return x;}'
try 4 'main(){x=0; y=0; while (x < 4) x = y = y +1; return y;}'
try 20 'main(){y = 0; for(x=0; x < 10; x = x + 1) y = y+2; return y;}'
try 6 'main(){{a=2; b=3; c = a*b; return c;}}'
try 3 'main(){a=0; if(a){return a*1;} else {return 3;}}'
try 10 'main(){x=10; y = 0; while(x>0){y = y+1; x = x -1;} return y;}'
try 128 'main(){y=1; for(x=0; x < 7; x = x+1) {y = 2*y;} return y;}'
try 15 'test0(){return 10;} test() { 3+4; } main(){return 5+test0();}'
try 64 'main(){x = test1(5); return x*x;}'
try 21 'main(){return test6(1,2,3,4,5,6);}'

echo OK
