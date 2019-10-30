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

try 42  'int main(){42;}'
try 21  'int main(){5+20-4;}'
try 41  'int main(){ 12   + 34 - 5;}'
try 17  'int main(){4 * 3 + 5;}'
try 19  'int main(){4 + 3 * 5;}'
try 2   'int main(){4/4 + 1;}'
try 31  'int main(){3*3/2 + 10*30/10 -3;}'
try 11  'int main(){(3+2)*3/3*2+1;}'
try 15  'int main(){(((2* 17)+((3  *2)+3)  ) - ((3+4)*(2/1)+(3*4+4)) + 2);}'
try 10  'int main(){ 30-20;}'
try 10  'int main(){(-10+20);}'
try 15  'int main(){(-10)+20 + + 30--10*-1 + -15;}'
try 0   'int main(){2 == 2 * 3;}'
try 4   'int main(){(3*2 == 4 + 2) * 4;}'
try 2   'int main(){(2 != 2) * 4+2/(2 != 1);}'
try 0   'int main(){15<3 * 4;}'
try 0   'int main(){15<3*5;}'
try 1   'int main(){15<12+4;}'
try 0   'int main(){15<=3 * 4;}'
try 1   'int main(){15<=3*5;}'
try 1   'int main(){15<=14+2;}'
try 0   'int main(){0<=12<0;}'
try 1   'int main(){15>3 * 4;}'
try 0   'int main(){15>3*5;}'
try 0   'int main(){15>12+4;}'
try 1   'int main(){30/2>=3*5-1;}'
try 1   'int main(){3*5>=3*5;}'
try 16  'int main(){x = 3;y = 8; return *(&x-8) + *&y;}'
try 3   'int main(){x = 3; y = &x; return *y;}'
try 3   'int main(){x=3;y=5;z=&y+8;return *z;}'
try 0   'int main(){10+5>=28/2+2;}'
try 12  'int main(){abc=10; abc+2;}'
try 28  'int main(){awf=awef=3+2*2; awef+awf*3;}'
try 170 'int main(){abc=3*4+5; return 10*abc; abc=0; return abc;}'
try 10  'int main(){if (3*4*0) return 1; else return 10;}'
try 3   'int main(){if ( 2*4+1 ) x=3; else x=5; return x;}'
try 5   'int main(){x=0; while(x<5) x = x + 1; return x;}'
try 4   'int main(){x=0; y=0; while (x < 4) x = y = y +1; return y;}'
try 20  'int main(){y = 0; for(x=0; x < 10; x = x + 1) y = y+2; return y;}'
try 6   'int main(){{a=2; b=3; c = a*b; return c;}}'
try 3   'int main(){a=0; if(a){return a*1;} else {return 3;}}'
try 10  'int main(){x=10; y = 0; while(x>0){y = y+1; x = x -1;} return y;}'
try 128 'int main(){y=1; for(x=0; x < 7; x = x+1) {y = 2*y;} return y;}'
try 11  'int test(){x=3; return x*x;}
         int test0(){y = 3; 2*y;}
         int main(){return 5+test0();}'
try 64  'int test1(int x) { return x+3; }
         int main(){x = test1(5); return x*x;}'
try 20  'int test2(int x, int y) {a=x+y; return a*a;}
         int test3(int a, int b, int c){ x=a+b*c; return x - 3;}
         int main(){return test2(1,2) + test3(2, 3,4);}'
try 24  'int test4(int a, int b, int c, int d){x = a*b + c *d; return x;}
         int test5(int x, int y, int z, int w, int v){v = x*z*v-y*w; return v;}
         int main(){return test4(2,3,4,5) + test5(3,2,1,4,2);}'
try 21  'int test6(int a, int b, int c, int d, int e, int f){return a+b+c+d+e+f;}
         int main(){return test6(1,2,3,4,5,6);}'
try 89  'int fib(int n){if(n==0){return 1;} else if(n==1){return 1;} else {return fib(n-1) + fib(n-2);}}
         int main(){return fib(10);}'
echo OK
