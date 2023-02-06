#include <clcxx/clcxx.hpp>
#include <string>

#include "clcxx_eigen.hpp"

CLCXX_PACKAGE EIGEN(clcxx::Package& pack) {
  using EigenMat = EigenMatWrapper<Eigen::MatrixXd, double>;
  pack.defclass<EigenMat, true>("Matrix")
      .constructor<Eigen::Index, Eigen::Index>()
      .defmethod("m.resize",
                 F_PTR([](EigenMat& m, Eigen::Index i, Eigen::Index j) {
                   return m.resize(i, j);
                 }))
      .defmethod("m.block", F_PTR(&EigenMat::getBlock))
      .defmethod("m.row", F_PTR(&EigenMat::getRow))
      .defmethod("m.col", F_PTR(&EigenMat::getCol))
      .defmethod("m.size", F_PTR(&EigenMat::size))
      .defmethod("m.rows", F_PTR(&EigenMat::rows))
      .defmethod("m.cols", F_PTR(&EigenMat::cols))
      .defmethod("m.get-at-index", F_PTR(&EigenMat::get))
      .defmethod("m.set-at-index",
                 F_PTR([](EigenMat& this_, Eigen::Index i, Eigen::Index j,
                          double val) { this_.set(i, j, val); }))
      .defmethod("m.set-matrix", F_PTR([](EigenMat& this_, const EigenMat& m) {
                   this_.set(m);
                 }))
      .defmethod("m.set-zero", F_PTR(&EigenMat::set0))
      .defmethod("m.set-ones", F_PTR(&EigenMat::set1))
      .defmethod("m.set-identity", F_PTR(&EigenMat::setId))
      .defmethod("%m.set-from-array", F_PTR(&EigenMat::setFromArray))
      .defmethod("m.trace", F_PTR(&EigenMat::trace))
      .defmethod("m.sum", F_PTR(&EigenMat::sum))
      .defmethod("m.prod", F_PTR(&EigenMat::prod))
      .defmethod("m.mean", F_PTR(&EigenMat::mean))
      .defmethod("m.norm", F_PTR(&EigenMat::norm))
      .defmethod("m.squared-norm", F_PTR(&EigenMat::squaredNorm))
      .defmethod("m.print", F_PTR(&EigenMat::print))
      .defmethod("m.scale", F_PTR(&EigenMat::scale))
      .defmethod("m.add-scalar",
                 F_PTR([](EigenMat& this_, double val) { this_.add(val); }))
      .defmethod("m.add-mat", F_PTR([](EigenMat& this_, const EigenMat& a) {
                   this_.add(a);
                 }))
      .defmethod("m.multiply", F_PTR(&EigenMat::multiply))
      .defmethod("m.inverse", F_PTR(&EigenMat::inv))
      .defmethod("m.full-inverse", F_PTR(&EigenMat::mInv))
      .defmethod("m.transpose", F_PTR(&EigenMat::trans))
      .defmethod("m.determinant", F_PTR(&EigenMat::det))
      .defmethod("m.full-determinant", F_PTR(&EigenMat::mDet))
      .defmethod("m.rank", F_PTR(&EigenMat::rankLU))
      .defmethod("m.full-q", F_PTR(&EigenMat::mQ))
      .defmethod("m.full-p", F_PTR(&EigenMat::mP))
      .defmethod("m.p", F_PTR(&EigenMat::squareMP))
      .defmethod("m.l", F_PTR(&EigenMat::squareML))
      .defmethod("m.u", F_PTR(&EigenMat::squareMU))
      .defmethod("m.lower-Cholesky", F_PTR(&EigenMat::mLCholesky))
      .defmethod("m.upper-Cholesky", F_PTR(&EigenMat::mUCholesky))
      .defmethod("m.eigen-values", F_PTR(&EigenMat::eigenVals))
      .defmethod("m.full-solve", F_PTR(&EigenMat::solveLU))
      .defmethod("m.solve", F_PTR(&EigenMat::solveSquareLU))
      .defmethod("m.solve-Cholesky", F_PTR(&EigenMat::solveCholesky))
      .defmethod("m.full-lower", F_PTR(&EigenMat::mU))
      .defmethod("m.full-upper", F_PTR(&EigenMat::mL));

  pack.defun("m*", F_PTR([](const EigenMat& x, const EigenMat& y) -> EigenMat {
               return static_cast<EigenMat>(x * y);
             }));
  pack.defun("m+", F_PTR([](const EigenMat& x, const EigenMat& y) -> EigenMat {
               return static_cast<EigenMat>(x + y);
             }));
}
