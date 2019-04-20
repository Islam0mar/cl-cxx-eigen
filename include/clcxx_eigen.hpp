#pragma once

#include <Eigen/Core>
#include <Eigen/Eigen>
#include <Eigen/Eigenvalues>
#include <Eigen/Geometry>
#include <cstring>
#include <sstream>
#include <string>

template <typename EigenT, typename ElementT>
class EigenClassWrapper : public EigenT {
  using type = EigenClassWrapper<EigenT, ElementT>;

 public:
  EigenClassWrapper() : EigenT() {}
  EigenClassWrapper(Eigen::Index rows, Eigen::Index cols) {
    *this = this->Zero(rows, cols);
  }
  // This constructor allows you to construct MyVectorType from Eigen
  // expressions
  explicit EigenClassWrapper(const EigenT& obj) : EigenT(obj) {}

  // This method allows you to assign Eigen expressions to MyVectorType
  type& operator=(const EigenT& other) {
    this->EigenT::operator=(other);
    return *this;
  }

  void set(Eigen::Index i, Eigen::Index j, ElementT value) {
    if ((this->rows() > i) && (this->cols() > j) && (i > -1) && (j > -1)) {
      this->operator()(i, j) = value;
    } else {
      throw std::runtime_error("Wrong index");
    }
  }
  void set(const type& obj) { this->EigenT::operator=(obj); }
  void set0() { this->setZero(); }
  void setId() { this->setIdentity(); }
  void set1() { this->setOnes(); }
  ElementT get(Eigen::Index i, Eigen::Index j) {
    if ((this->rows() > i) && (this->cols() > j) && (i > -1) && (j > -1)) {
      return this->operator()(i, j);
    } else {
      throw std::runtime_error("Wrong index");
    }
  }
  std::string print() {
    std::stringstream s;
    s << *this;
    return s.str();
  }
  void scale(const ElementT& value) { *this *= value; }
  void add(const ElementT& value) {
    this->unaryExpr([value](double x) { return x + value; });
  }
  void add(const type& obj) { *this += (type)obj; }
  void multiply(const type& obj) { *this *= obj; }
  type inv() { return static_cast<type>(this->inverse()); }
  type trans() { return static_cast<type>(this->transpose()); }
  ElementT det() { return this->determinant(); }
};
