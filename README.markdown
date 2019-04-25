# CL-CXX-Eigen - Common Lisp and CXX-Eigen interoperation 

This is a linear algebra lib. to be used in CL

## Prerequisites

- [CL-CXX](https://github.com/Islam0mar/cl-cxx) is installed with C++ deps.

## Installation

Clone into `home/common-lisp/programs/` directory. 

``` bash 
cd cl-cxx-eigen
mkdir build
cd build
cmake ..
make 
```
Then open CL and `asdf:test-system "cxx-eigen"`
 

### Suppored Eigen types

currently `Eigen::MatrixXd`, but can be extended easily. 

for more functions, see `src/eigen.cpp` 

### Example

```common lisp
(asdf:load-system :cxx-eigen) ;; or (ql:quickload :cxx-eigen)
(cxx-eigen:init)

(in-package :cxx-eigen)

CXX-EIGEN> (m.print (create-mat2 3 4))
"0 0 0 0
0 0 0 0
0 0 0 0"

CXX-EIGEN> (defvar x (create-mat2 3 3))
X
CXX-EIGEN> (m.print x)
"0 0 0
0 0 0
0 0 0"
CXX-EIGEN> (m.set-ones x)
; No value
CXX-EIGEN> (m.print x)
"1 1 1
1 1 1
1 1 1"
CXX-EIGEN> (m.set-identity x)
; No value
CXX-EIGEN> (m.print x)
"1 0 0
0 1 0
0 0 1"
CXX-EIGEN> (m.set-at-index x 0 1 2.1d0)
; No value
CXX-EIGEN> (m.set-at-index x 0 2 3.2d0)
; No value
CXX-EIGEN> (m.set-at-index x 1 2 4.3d0)
; No value
CXX-EIGEN> (m.print x)
"  1 2.1 3.2
  0   1 4.3
  0   0   1"
CXX-EIGEN> (m.determinant x)
1.0d0
CXX-EIGEN> (m.print (m* x (m.inverse x)))
"1 0 0
0 1 0
0 0 1"
CXX-EIGEN> (m.print (m.inverse x))
"   1 -2.1 5.83
   0    1 -4.3
   0    0    1"
CXX-EIGEN> (defvar A (create-matrix2 3 3))
A
CXX-EIGEN> (m.set-from-list A '(2d0 4d0 -2d0 4d0 9d0 -3d0 -2d0 -3d0 7d0) 3 3)
; No value
CXX-EIGEN> (defvar L (m.l A))
L
CXX-EIGEN> (defvar U (m.u A))
U
CXX-EIGEN> (defvar P (m.p A))
P
CXX-EIGEN> (m.print L)
"        1         0         0
     -0.5         1         0
      0.5 -0.333333         1"
CXX-EIGEN> (m.print U)
"      4       9      -3
      0     1.5     5.5
      0       0 1.33333"
CXX-EIGEN> (m.print P)
"0 1 0
0 0 1
1 0 0"
CXX-EIGEN> (m.print A)
" 2  4 -2
 4  9 -3
-2 -3  7"
CXX-EIGEN> (m.print (m* (m.inverse p) (m* l u)))
" 2  4 -2
 4  9 -3
-2 -3  7"
CXX-EIGEN> (defvar B (create-matrix))
B
CXX-EIGEN> (m.set-from-list B '(2d0 8d0 10d0) 3 1)
; No value
CXX-EIGEN> (m.print (m.solve A B))
"-1
 2
 2"
```

### NOTE

Tested on SBCL 1.4.5.debian

## Copyright

Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)

## License

Licensed under the MIT License.
