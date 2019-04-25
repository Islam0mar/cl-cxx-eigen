#include <clcxx/clcxx.hpp>
#include <string>
#include "clcxx_eigen.hpp"

CLCXX_PACKAGE EIGEN(clcxx::Package& pack) {
  using EigenMat = EigenMatWrapper<Eigen::MatrixXd, double>;

  pack.defclass<EigenMat, true>("Matrix")
      .constructor<Eigen::Index, Eigen::Index>()
      .defmethod("m.resize",
                 static_cast<void (EigenMat::*)(Eigen::Index, Eigen::Index)>(
                     &EigenMat::resize))
      .defmethod("m.block", &EigenMat::getBlock)
      .defmethod("m.row", &EigenMat::getRow)
      .defmethod("m.col", &EigenMat::getCol)
      .defmethod("m.size", &EigenMat::size)
      .defmethod("m.rows", &EigenMat::rows)
      .defmethod("m.cols", &EigenMat::cols)
      .defmethod("m.get-at-index", &EigenMat::get)
      .defmethod(
          "m.set-at-index",
          static_cast<void (EigenMat::*)(Eigen::Index, Eigen::Index, double)>(
              &EigenMat::set))
      .defmethod(
          "m.set-matrix",
          static_cast<void (EigenMat::*)(const EigenMat&)>(&EigenMat::set))
      .defmethod("m.set-zero", &EigenMat::set0)
      .defmethod("m.set-ones", &EigenMat::set1)
      .defmethod("m.set-identity", &EigenMat::setId)
      .defmethod("%m.set-from-array", &EigenMat::setFromArray)
      .defmethod("m.trace", &EigenMat::trace)
      .defmethod("m.sum", &EigenMat::sum)
      .defmethod("m.prod", &EigenMat::prod)
      .defmethod("m.mean", &EigenMat::mean)
      .defmethod("m.norm", &EigenMat::norm)
      .defmethod("m.squared-norm", &EigenMat::squaredNorm)
      .defmethod("m.print", &EigenMat::print)
      .defmethod("m.scale", &EigenMat::scale)
      .defmethod("m.add-scalar",
                 static_cast<void (EigenMat::*)(const double&)>(&EigenMat::add))
      .defmethod("m.add-mat", static_cast<void (EigenMat::*)(const EigenMat&)>(
                                  &EigenMat::add))
      .defmethod("m.multiply", &EigenMat::multiply)
      .defmethod("m.inverse", &EigenMat::inv)
      .defmethod("m.full-inverse", &EigenMat::mInv)
      .defmethod("m.transpose", &EigenMat::trans)
      .defmethod("m.determinant", &EigenMat::det)
      .defmethod("m.full-determinant", &EigenMat::mDet)
      .defmethod("m.rank", &EigenMat::rankLU)
      .defmethod("m.full-q", &EigenMat::mQ)
      .defmethod("m.full-p", &EigenMat::mP)
      .defmethod("m.p", &EigenMat::squareMP)
      .defmethod("m.l", &EigenMat::squareML)
      .defmethod("m.u", &EigenMat::squareMU)
      .defmethod("m.lower-Cholesky", &EigenMat::mLCholesky)
      .defmethod("m.upper-Cholesky", &EigenMat::mUCholesky)
      .defmethod("m.eigen-values", &EigenMat::eigenVals)
      .defmethod("m.full-solve", &EigenMat::solveLU)
      .defmethod("m.solve", &EigenMat::solveSquareLU)
      .defmethod("m.solve-Cholesky", &EigenMat::solveCholesky)
      .defmethod("m.full-lower", &EigenMat::mU)
      .defmethod("m.full-upper", &EigenMat::mL);

  pack.defun("m*", [](const EigenMat& x, const EigenMat& y) -> EigenMat {
    return static_cast<EigenMat>(x * y);
  });
  pack.defun("m+", [](const EigenMat& x, const EigenMat& y) -> EigenMat {
    return static_cast<EigenMat>(x + y);
  });
}
