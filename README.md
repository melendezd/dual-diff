# Automatic Differentiation Using Dual Numbers

## Background
The complex numbers can be obtained by adjoining an element <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/77a3b857d53fb44e33b53e4c8b68351a.svg?invert_in_darkmode" align=middle width=5.663225699999989pt height=21.68300969999999pt/> to the real numbers such that <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/c1f684c461e1613568d153effd6ca489.svg?invert_in_darkmode" align=middle width=43.17452699999998pt height=26.76175259999998pt/>. Elements of the resulting set are of the form <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/15f0b47cb1607d74c1f8c2fb08bd5976.svg?invert_in_darkmode" align=middle width=41.49836789999999pt height=22.831056599999986pt/>, where <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg?invert_in_darkmode" align=middle width=8.68915409999999pt height=14.15524440000002pt/> and <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/4bdc8d9bcfb35e1c9bfb51fc69687dfc.svg?invert_in_darkmode" align=middle width=7.054796099999991pt height=22.831056599999986pt/> are real numbers. Arithmetic operations are done using the distributive property and the property <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/6c9dcbb7c305a5530e5f3c4d3296dc08.svg?invert_in_darkmode" align=middle width=55.95995954999998pt height=26.76175259999998pt/>.

Similarly, we can obtain a new number system called the dual numbers by adjoining an element <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/9ae7733dac2b7b4470696ed36239b676.svg?invert_in_darkmode" align=middle width=7.66550399999999pt height=14.15524440000002pt/> to the real numbers such that <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/ad6370c8c8de22b67ebb85cbc747ef57.svg?invert_in_darkmode" align=middle width=45.17680365pt height=26.76175259999998pt/>. Elements of the resulting set are of the form <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/a082bb541ca53a4b2855b509c483712c.svg?invert_in_darkmode" align=middle width=42.507532649999995pt height=22.831056599999986pt/>, where <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg?invert_in_darkmode" align=middle width=8.68915409999999pt height=14.15524440000002pt/> and <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/4bdc8d9bcfb35e1c9bfb51fc69687dfc.svg?invert_in_darkmode" align=middle width=7.054796099999991pt height=22.831056599999986pt/> are real numbers. Arithmetic operations are done using the distributive property and the property <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/ad6370c8c8de22b67ebb85cbc747ef57.svg?invert_in_darkmode" align=middle width=45.17680365pt height=26.76175259999998pt/>. 

Using these basic properties, we can apply the dual numbers to Taylor series to extend functions such as <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/c82d435c5be3997122264dd5b8a9268a.svg?invert_in_darkmode" align=middle width=98.42478854999999pt height=24.65753399999998pt/> etc from the real numbers to the dual numbers. The most useful result of this is the fact that for any analytic function <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/190083ef7a1625fbc75f243cffb9c96d.svg?invert_in_darkmode" align=middle width=9.81741584999999pt height=22.831056599999986pt/>, we have <img src="https://rawgit.com/melendezd/dual-diff/master/svgs/08d1a50de86a69ca45ad7cf5e9c66ae7.svg?invert_in_darkmode" align=middle width=178.03639754999998pt height=24.7161288pt/>. This allows us to effortlessly compute derivatives of complicated functions using dual numbers.

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
