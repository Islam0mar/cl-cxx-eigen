#pragma once

#include <Eigen/Core>
#include <Eigen/Eigen>
#include <Eigen/Eigenvalues>
#include <Eigen/Geometry>
#include <Eigen/Sparse>
#include <cstdio>
#include <filesystem>
#include <iostream>
#include <sstream>
#include <string>

template <typename EigenT>
class EigenMatWrapper : public EigenT {
  using ElementT = typename EigenT::Scalar;
  using ThisType = EigenMatWrapper<EigenT>;
  using RowMatrix =
      Eigen::Matrix<ElementT, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

 public:
  EigenMatWrapper() : EigenT() {}
  EigenMatWrapper(Eigen::Index rows, Eigen::Index cols) {
    *this = this->Zero(rows, cols);
  }
  // This constructor allows you to construct MyVectorType from Eigen
  // expressions
  explicit EigenMatWrapper(const EigenT& obj) : EigenT(obj) {}

  // This method allows you to assign Eigen expressions to MyVectorType
  ThisType& operator=(const EigenT& other) {
    this->EigenT::operator=(other);
    return *this;
  }

  void Set(Eigen::Index i, Eigen::Index j, ElementT value) {
    if ((this->rows() > i) && (this->cols() > j) && (i > -1) && (j > -1)) {
      this->operator()(i, j) = value;
    } else {
      throw std::runtime_error("Wrong index");
    }
  }
  void Set(const ThisType& obj) { this->EigenT::operator=(obj); }
  void Set0() { this->setZero(); }
  void SetIdentity() { this->setIdentity(); }
  void SetOnes() { this->setOnes(); }
  ThisType GetCol(Eigen::Index i) {
    return static_cast<ThisType>(this->col(i));
  }
  ThisType GetRow(Eigen::Index i) {
    return static_cast<ThisType>(this->row(i));
  }
  ThisType GetBlock(Eigen::Index i, Eigen::Index j, Eigen::Index k,
                    Eigen::Index l) {
    return static_cast<ThisType>(this->block(i, j, k, l));
  }
  ElementT Get(Eigen::Index i, Eigen::Index j) {
    if ((this->rows() > i) && (this->cols() > j) && (i > -1) && (j > -1)) {
      return this->operator()(i, j);
    } else {
      throw std::runtime_error("Wrong index");
    }
  }

  std::string Print() {
    std::stringstream s;
    s << std::endl;
    s << *this;
    return s.str();
  }

  void PrintToFile(std::string path) {
    if (path == "") {
      const auto kTmpDir = std::filesystem::temp_directory_path();
      const auto kStdOutputFile = kTmpDir.string() + "/clcxx_stdout.txt";
      path = kStdOutputFile;
    }
    if (std::freopen(path.c_str(), "w", stdout) != nullptr) {
      auto a = *this;
      std::cout << a << std::endl;
      std::fclose(stdout);
    }
  }

  void Scale(const ElementT value) { *this *= value; }
  void Add(const ElementT value) {
    *this = this->unaryExpr([value](double x) { return x + value; });
  }
  void Add(const ThisType& obj) { *this += (ThisType)obj; }
  void Multiply(const ThisType& obj) { *this *= obj; }
  ThisType Inv() { return static_cast<ThisType>(this->inverse()); }
  ThisType Trans() { return static_cast<ThisType>(this->transpose()); }
  ElementT Det() { return this->determinant(); }
  void SetFromArray(ElementT arr[], Eigen::Index i, Eigen::Index j) {
    *this = Eigen::Map<RowMatrix>(arr, i, j);
  }

  ElementT* GetAsArray() { return this->data(); }

  Eigen::Index FullPivRank() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return lu.rank();
  }
  ThisType FullPivL() {
    Eigen::FullPivLU<EigenT> lu(*this);
    ThisType l =
        static_cast<ThisType>(this->Identity(this->rows(), this->cols()));
    l.block(0, 0, this->rows(), this->cols())
        .template triangularView<Eigen::StrictlyLower>() = lu.matrixLU();
    return l;
  }
  ThisType FullPivU() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return static_cast<ThisType>(
        lu.matrixLU().template triangularView<Eigen::Upper>());
  }
  ThisType FullPivQ() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return static_cast<ThisType>(lu.permutationQ());
  }
  ThisType FullPivInv() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return static_cast<ThisType>(lu.inverse());
  }
  ElementT FullPivDet() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return lu.determinant();
  }
  ThisType FullPivP() {
    Eigen::FullPivLU<EigenT> lu(*this);
    return static_cast<ThisType>(lu.permutationP());
  }
  ThisType CholeskyL() { return static_cast<ThisType>(this->llt().matrixL()); }
  ThisType CholeskyU() { return static_cast<ThisType>(this->llt().matrixU()); }
  // type EigenVals() {
  //   Eigen::VectorXcd eivals = this->eigenvalues();
  //   return static_cast<type>(eivals);
  // }
  // type EigenVectors() {
  //   Eigen::VectorXcd eivals = this->eigenvectors();
  //   return static_cast<type>(eivals);
  // }
  ThisType SquareML() {
    ThisType l =
        static_cast<ThisType>(this->Identity(this->rows(), this->cols()));
    l.block(0, 0, this->rows(), this->cols())
        .template triangularView<Eigen::StrictlyLower>() =
        this->lu().matrixLU();
    return l;
  }
  ThisType SquareMU() {
    return static_cast<ThisType>(
        this->lu().matrixLU().template triangularView<Eigen::Upper>());
  }
  ThisType SquareMP() {
    return static_cast<ThisType>(this->lu().permutationP());
  }
  // Ax = b ,returns x
  ThisType SolveCholesky(const ThisType& b) {
    return static_cast<ThisType>(this->llt().solve(b));
  }
  ThisType SolveFullPivLU(const ThisType& b) {
    Eigen::FullPivLU<EigenT> lu(*this);
    return static_cast<ThisType>(lu.solve(b));
  }
  ThisType SolveSquareLU(const ThisType& b) {
    return static_cast<ThisType>(this->lu().solve(b));
  }
  ThisType SolveSVD(const ThisType& b) {
    Eigen::BDCSVD<EigenT> SVD(*this, Eigen::ComputeThinU | Eigen::ComputeThinV);
    return static_cast<ThisType>(SVD.solve(b));
  }
};
