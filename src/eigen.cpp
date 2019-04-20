#include <clcxx/clcxx.hpp>
#include <string>
#include "clcxx_eigen.hpp"

CLCXX_PACKAGE EIGEN(clcxx::Package& pack) {
  using MyEigen = EigenClassWrapper<Eigen::MatrixXd, double>;

  pack.defclass<MyEigen, true>("Mat")
      .constructor<Eigen::Index, Eigen::Index>()
      .defmethod("m.resize",
                 static_cast<void (MyEigen::*)(Eigen::Index, Eigen::Index)>(
                     &MyEigen::resize))
      .defmethod("m.get-at-index", &MyEigen::get)
      .defmethod(
          "m.set-at-index",
          static_cast<void (MyEigen::*)(Eigen::Index, Eigen::Index, double)>(
              &MyEigen::set))
      .defmethod("m.set-matrix",
                 static_cast<void (MyEigen::*)(const MyEigen&)>(&MyEigen::set))
      .defmethod("m.set-zero", &MyEigen::set0)
      .defmethod("m.set-ones", &MyEigen::set1)
      .defmethod("m.set-identity", &MyEigen::setId)
      .defmethod("m.trace", &MyEigen::trace)
      .defmethod("m.sum", &MyEigen::sum)
      .defmethod("m.prod", &MyEigen::prod)
      .defmethod("m.mean", &MyEigen::mean)
      .defmethod("m.norm", &MyEigen::norm)
      .defmethod("m.squared-norm", &MyEigen::squaredNorm)
      .defmethod("m.print", &MyEigen::print)
      .defmethod("m.scale", &MyEigen::scale)
      .defmethod("m.add-scalar",
                 static_cast<void (MyEigen::*)(const double&)>(&MyEigen::add))
      .defmethod("m.add-mat",
                 static_cast<void (MyEigen::*)(const MyEigen&)>(&MyEigen::add))
      .defmethod("m.multiply", &MyEigen::multiply)
      .defmethod("m.inverse", &MyEigen::inv)
      .defmethod("m.transpose", &MyEigen::trans)
      .defmethod("m.determinant", &MyEigen::det);

  pack.defun("m*", [](const MyEigen& x, const MyEigen& y) -> MyEigen {
    return static_cast<MyEigen>(x * y);
  });
  pack.defun("m+", [](const MyEigen& x, const MyEigen& y) -> MyEigen {
    return static_cast<MyEigen>(x + y);
  });
}
