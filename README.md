# Automatic Differentiation Using Dual Numbers

## Background
The complex numbers can be obtained by adjoining an element <img src="/svgs/77a3b857d53fb44e33b53e4c8b68351a.svg" align=middle width=5.663225699999989pt height=21.68300969999999pt/> to the real numbers such that <img src="/svgs/fe91c41efa033b5ed00a66135f819b7d.svg" align=middle width=55.95995954999998pt height=26.76175259999998pt/>. Elements of the resulting set are of the form <img src="/svgs/15f0b47cb1607d74c1f8c2fb08bd5976.svg" align=middle width=41.49836789999999pt height=22.831056599999986pt/>, where <img src="/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg" align=middle width=8.68915409999999pt height=14.15524440000002pt/> and <img src="/svgs/4bdc8d9bcfb35e1c9bfb51fc69687dfc.svg" align=middle width=7.054796099999991pt height=22.831056599999986pt/> are real numbers. Arithmetic operations are done using the distributive property and the property <img src="/svgs/6c9dcbb7c305a5530e5f3c4d3296dc08.svg" align=middle width=55.95995954999998pt height=26.76175259999998pt/>.

Similarly, we can obtain a new number system called the dual numbers by adjoining an element <img src="/svgs/9ae7733dac2b7b4470696ed36239b676.svg" align=middle width=7.66550399999999pt height=14.15524440000002pt/> to the real numbers such that <img src="/svgs/ad6370c8c8de22b67ebb85cbc747ef57.svg" align=middle width=45.17680365pt height=26.76175259999998pt/>, but <img src="/svgs/9affadd77b3afdde730f1e8c2a1264c4.svg" align=middle width=37.80234314999999pt height=22.831056599999986pt/>. Elements of the resulting set are of the form <img src="/svgs/0032d7518d4e9b05704609087c7df978.svg" align=middle width=43.50064454999998pt height=22.831056599999986pt/>, where <img src="/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg" align=middle width=8.68915409999999pt height=14.15524440000002pt/> and <img src="/svgs/4bdc8d9bcfb35e1c9bfb51fc69687dfc.svg" align=middle width=7.054796099999991pt height=22.831056599999986pt/> are real numbers. Arithmetic operations are done using the distributive property and the property <img src="/svgs/b89a5b127257726d9f05175eb985f52d.svg" align=middle width=45.17680365pt height=26.76175259999998pt/>.

Using these basic properties, we can apply the dual numbers to Taylor series to extend functions such as <img src="/svgs/c2826e01d21cc9e30e6df139ebdd31e6.svg" align=middle width=81.91785359999999pt height=21.95701200000001pt/>, etc. from the real numbers to the dual numbers. The most useful result of this is the fact that for any function <img src="/svgs/190083ef7a1625fbc75f243cffb9c96d.svg" align=middle width=9.81741584999999pt height=22.831056599999986pt/> that has a power series, we have <img src="/svgs/08d1a50de86a69ca45ad7cf5e9c66ae7.svg" align=middle width=178.03639754999998pt height=24.7161288pt/>. This allows us to effortlessly compute derivatives of complicated functions using dual numbers.

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
