#include <clcxx/clcxx.hpp>
#include <string>

#include "clcxx_eigen.hpp"

CLCXX_PACKAGE EIGEN(clcxx::Package& pack) {
  using EigenMat = EigenMatWrapper<
      Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>;

  pack.defclass<EigenMat, true>("Matrix")
      .constructor<Eigen::Index, Eigen::Index>()
      .defmethod("m.resize",
                 F_PTR([](EigenMat& m, Eigen::Index i, Eigen::Index j) {
                   return m.resize(i, j);
                 }))
      .defmethod("m.block", F_PTR(&EigenMat::GetBlock))
      .defmethod("m.row", F_PTR(&EigenMat::GetRow))
      .defmethod("m.col", F_PTR(&EigenMat::GetCol))
      .defmethod("m.size", F_PTR(&EigenMat::size))
      .defmethod("m.rows", F_PTR(&EigenMat::rows))
      .defmethod("m.cols", F_PTR(&EigenMat::cols))
      .defmethod("m.get-at-index", F_PTR(&EigenMat::Get))
      .defmethod("m.set-at-index",
                 F_PTR([](EigenMat& this_, Eigen::Index i, Eigen::Index j,
                          double val) { this_.Set(i, j, val); }))
      .defmethod("m.set-matrix", F_PTR([](EigenMat& this_, const EigenMat& m) {
                   this_.Set(m);
                 }))
      .defmethod("m.set-zero", F_PTR(&EigenMat::Set0))
      .defmethod("m.set-ones", F_PTR(&EigenMat::SetOnes))
      .defmethod("m.set-identity", F_PTR(&EigenMat::SetIdentity))
      .defmethod("%m.set-from-array", F_PTR(&EigenMat::SetFromArray))
      .defmethod("%m.get-as-array", F_PTR(&EigenMat::GetAsArray))
      .defmethod("m.trace", F_PTR(&EigenMat::trace))
      .defmethod("m.sum", F_PTR(&EigenMat::sum))
      .defmethod("m.prod", F_PTR(&EigenMat::prod))
      .defmethod("m.mean", F_PTR(&EigenMat::mean))
      .defmethod("m.norm", F_PTR(&EigenMat::norm))
      .defmethod("m.squared-norm", F_PTR(&EigenMat::squaredNorm))
      .defmethod("m.print", F_PTR(&EigenMat::Print))
      .defmethod("m.scale", F_PTR(&EigenMat::Scale))
      .defmethod("m.add-scalar",
                 F_PTR([](EigenMat& this_, double val) { this_.Add(val); }))
      .defmethod("m.add-mat", F_PTR([](EigenMat& this_, const EigenMat& a) {
                   this_.Add(a);
                 }))
      .defmethod("m.multiply", F_PTR(&EigenMat::Multiply))
      .defmethod("m.inverse", F_PTR(&EigenMat::Inv))
      .defmethod("m.inverse-full-pivot-lu", F_PTR(&EigenMat::FullPivInv))
      .defmethod("m.transpose", F_PTR(&EigenMat::Trans))
      .defmethod("m.determinant", F_PTR(&EigenMat::Det))
      .defmethod("m.determinant-full-pivot-lu", F_PTR(&EigenMat::FullPivDet))
      .defmethod("m.rank-full-pivot-lu", F_PTR(&EigenMat::FullPivRank))
      .defmethod("m.q-full-pivot-lu", F_PTR(&EigenMat::FullPivQ))
      .defmethod("m.p-full-pivot-lu", F_PTR(&EigenMat::FullPivP))
      .defmethod("m.l-full-pivot-lu", F_PTR(&EigenMat::FullPivU))
      .defmethod("m.u-full-pivot-lu", F_PTR(&EigenMat::FullPivL))

      .defmethod("m.p", F_PTR(&EigenMat::SquareMP))
      .defmethod("m.l", F_PTR(&EigenMat::SquareML))
      .defmethod("m.u", F_PTR(&EigenMat::SquareMU))
      .defmethod("m.l-cholesky", F_PTR(&EigenMat::CholeskyL))
      .defmethod("m.u-Cholesky", F_PTR(&EigenMat::CholeskyU))

      // .defmethod("m.eigen-values", F_PTR(&EigenMat::EigenVals))
      // .defmethod("m.eigen-vectors", F_PTR(&EigenMat::EigenVectors))
      .defmethod("m.solve-full-pivot-lu", F_PTR(&EigenMat::SolveFullPivLU))
      .defmethod("m.solve", F_PTR(&EigenMat::SolveSquareLU))
      .defmethod("m.solve-cholesky", F_PTR(&EigenMat::SolveCholesky))
      .defmethod("m.solve-svd", F_PTR(&EigenMat::SolveSVD));

  pack.defun("m*", F_PTR([](const EigenMat& x, const EigenMat& y) -> EigenMat {
               return static_cast<EigenMat>(x * y);
             }));
  pack.defun("m+", F_PTR([](const EigenMat& x, const EigenMat& y) -> EigenMat {
               return static_cast<EigenMat>(x + y);
             }));
}
