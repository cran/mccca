# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

calc_Ddiag_sq <- function(dummy_diag) {
    .Call(`_mccca_calc_Ddiag_sq`, dummy_diag)
}

calc_JnDumB <- function(Jn, dummy_mat, B) {
    .Call(`_mccca_calc_JnDumB`, Jn, dummy_mat, B)
}

create_Jn <- function(n_all) {
    .Call(`_mccca_create_Jn`, n_all)
}

obj <- function(dummy_mat, dummy_diag, U, G, B, Jn) {
    .Call(`_mccca_obj`, dummy_mat, dummy_diag, U, G, B, Jn)
}

updateB <- function(dummy_mat, Ddiag_sq, U, Jn, m, lowdim) {
    .Call(`_mccca_updateB`, dummy_mat, Ddiag_sq, U, Jn, m, lowdim)
}

calc_updateG <- function(U, X, J) {
    .Call(`_mccca_calc_updateG`, U, X, J)
}

