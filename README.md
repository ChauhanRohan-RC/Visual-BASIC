# Numerical Computation Programs in Visual BASIC
#### This repository is a collection of computational programs written in visual BASIC.Up until now, it contains 4 programs

## 1. Number System Convertor
* Converts a given number from original numerical base (any) to a desired numeric base
* For features and usage, see [Number System Example](number_system_convertor/print/number_system_output.pdf)

## 2. Matrix Calculator
* A calculator for matrices of any dimension in BASIC
* For features and usage, see [Matrix calculator Example](matrix_calculator/print/output.pdf)
* Input Modes
  * `1` : Read from hardcoded data
  * `Any` : Input Manually
####
* Basic Commands
  * `info` : Information Guide
  * `input` : Input Matrix
  * `result` : Last Result Matrix
  * `continue` : Continue Calculation (Set [Input] = [Result])
  * `restore data` : Restore Hardcoded Sample Data
####
* Unitary Transforms
  * `add scaler` : Add a Scaler to all matrix elements
  * `mult scaler` : Multiply Scaler to all matrix elements
  * `det` : compute matrix determinant
  * `transpose` : compute matrix transpose
  * `minor` : compute Minor Matrix
  * `cofactor` : compute Cofactor Matrix
  * `adjoint` : compute Adjoint Matrix
  * `inverse` : compute Inverse Matrix
  * `pow` : compute Power Matrix
####
* Binary Transforms
  * `add` : Add Matrices (with scale)
  * `mult` : Multiply Matrices (with scale)

## 3. Numerical Differentiator and Integrator
* A visual BASIC program for compute derivatives and integrals
* Integration Algorithms
  * Trapezoid
  * Simpson 1/3
  * Simpson 3/8
* For features and usage, see [Numerical Calculus Example](numerical_calculus/print/numerical_calculus.pdf)

## 4. Numerical Root Finder
* A visual BASIC program to find the roots of ANY FUNCTION using numerical methods
* Algorithms
  * Binary Bisection
  * Regular Falsi (or Chord)
  * Secant
  * Newton Raphson (requires the function to be differentiable)
* For features and usage, see [Numerical Roots Finder Example](numerical_roots/print/numerical_roots_output.pdf)