# Automatic Differentiation Using Dual Numbers

## Background
The complex numbers can be obtained by adjoining an element <img src="/svgs/6c9dcbb7c305a5530e5f3c4d3296dc08.svg" align=middle width=55.95995954999998pt height=26.76175259999998pt/>.

Similarly, we can obtain a new number system called the dual numbers by adjoining an element <img src="/svgs/b89a5b127257726d9f05175eb985f52d.svg" align=middle width=45.17680365pt height=26.76175259999998pt/>.

Using these basic properties, we can apply the dual numbers to Taylor series to extend functions such as <img src="/svgs/190083ef7a1625fbc75f243cffb9c96d.svg" align=middle width=9.81741584999999pt height=22.831056599999986pt/> that has a power series, we have $f(x+\varepsilon) = f(x) + f'(x)\varepsilon. This allows us to effortlessly compute derivatives of complicated functions using dual numbers.

## Purpose
This program uses the properties outlined above to numerically compute derivatives of mathematical functions using dual numbers. The user can enter a function, the number of derivatives to take, and the point at which to evaluate the derivative.

## Usage
* Install [stack](https://haskellstack.org/)
* Clone this repository
* CD to the root project directory
* Run `stack run`

## Examples
```
Welcome to the derivative calculator! Enter your function [f(x)], the order [n] of the derivative you'd like to compute, and the point [a] at which you'd like to compute the derivative.

f(x) = sin(x)

n = 1

a = 3.14159265

The result is: -1.0

f(x) = sin(x^x)^tan(x) + 2*x

n = 4

a = 5

The result is: 5.789939022697798e18
```
