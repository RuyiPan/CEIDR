#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// // [[Rcpp::export]]
// arma::mat adjust_local_variance(const arma::mat& m1,
//                                 const arma::sp_mat& NNmatrix) {
//   int N = m1.n_rows;  // number of individuals
//   int V = m1.n_cols;  // number of vertices
//
//   arma::mat norm_mat(N, V, arma::fill::zeros);
//
//   for (int v = 0; v < V; ++v) {
//     arma::uvec neighbors = arma::find(NNmatrix.row(v));
//     int K = neighbors.n_elem;
//
//     if (K == 0) {
//       norm_mat.col(v).fill(arma::datum::nan);
//       continue;
//     }
//
//     if (K == 1) {
//       norm_mat.col(v) = m1.col(neighbors[0]);
//       continue;
//     }
//
//     arma::mat X = m1.cols(neighbors);  // N x K
//
//     // Row-wise sum of squares
//     arma::vec x_ss = arma::sum(X % X, 1);  // N x 1
//
//     // Sample variance (uncentered)
//     arma::vec x_var = x_ss / static_cast<double>(K - 1);
//
//     arma::vec denom = arma::sqrt(x_var);  // N x 1
//
//     for (int i = 0; i < N; ++i) {
//       norm_mat(i, v) = (denom(i) > 0) ? m1(i, v) / denom(i) : m1(i, v);
//     }
//   }
//
//   return norm_mat;
// }


// [[Rcpp::export]]
arma::mat MeanVarWithin(const arma::mat& m1,
                                   const arma::sp_mat& NNmatrix) {
  int N = m1.n_rows;
  int V = m1.n_cols;
  arma::mat m1_norm(N, V, arma::fill::zeros);

  for (int v = 0; v < V; ++v) {
    arma::uvec neighbors = arma::find(NNmatrix.row(v));
    int K = neighbors.n_elem;

    if (K == 0) {
      m1_norm.col(v).fill(arma::datum::nan);
      continue;
    }

    // Submatrix: N x K (each row is one subject’s neighbor values)
    arma::mat neighbor_vals = m1.cols(neighbors);

    // Compute row-wise mean and std dev
    arma::vec row_mean = arma::mean(neighbor_vals, 1);            // N x 1
    arma::vec row_sd   = arma::stddev(neighbor_vals, 0, 1);       // N x 1, normalize by N-1

    for (int i = 0; i < N; ++i) {
      double sd = row_sd(i);
      double mu = row_mean(i);
      // Apply z-score normalization
      m1_norm(i, v) = (sd > 0) ? (m1(i, v) - mu) / sd : m1(i, v);
    }
  }

  return m1_norm;
}


