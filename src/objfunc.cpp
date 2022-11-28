#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double obj(arma::mat dummy_mat,arma::mat dummy_diag,
                   arma::mat U,arma::mat G, arma::mat B, arma::mat Jn){

    int N=U.n_rows;
    int m=dummy_diag.n_rows/N;

    arma::mat PP=B.t()*dummy_mat.t()*Jn*U*arma::inv(U.t()*U)*U.t()*Jn*dummy_mat*B;
    double obval=sum(arma::diagvec(PP))/(N*(pow(m,2)));;

    obval=obval*(-1);
    return obval;

}
