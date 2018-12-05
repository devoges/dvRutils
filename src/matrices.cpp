// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

#include <RcppArmadillo.h>
#include <RcppEigen.h>

// [[Rcpp::export]]
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
  Eigen::MatrixXd C = A * B;

  return Rcpp::wrap(C);
}

//[[Rcpp::export]]
Eigen::MatrixXd eigenMapMatInv(const Eigen::Map<Eigen::MatrixXd> A){
  Eigen::MatrixXd Ainv = A.inverse();
  return Ainv;
}

//[[Rcpp::export]]
Eigen::VectorXd eigenMapMatEig(const Eigen::Map<Eigen::MatrixXd> A){
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(A);
  return es.eigenvalues();
}
