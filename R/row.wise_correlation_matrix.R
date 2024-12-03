row.wise_correlation_matrix <- function(x){
  # Normalize rows (subtract mean, divide by standard deviation)
  x_matrix <- t(scale(t(x), center = TRUE, scale = TRUE))

  # Compute pairwise correlations
  return(x_matrix %*% t(x_matrix) / (ncol(x) - 1))
}

# ## Testing whether this ChatGPT generated function works as intended
# # Library to benchmark
# library(microbenchmark)
#
# # Set seed and create matrix
# set.seed(20241203)
# a <- matrix(rnorm(100000), nrow = 1000)
#
# # Compare performance
# microbenchmark(cor(t(a)), row.wise_correlation_matrix(a), check='equal')
# # Unit: milliseconds
# # expr                               min        lq      mean   median       uq       max    neval
# # cor(t(a))                      53.509665 55.590636 60.435955 60.71647 62.62116 125.25003   100
# # row.wise_correlation_matrix(a)  5.488076  6.749403  9.362821  7.39834 11.87622  22.94047   100
#
# # Make sure the results are the same
# cor_mat1 <- cor(t(a))
# cor_mat2 <- row.wise_correlation_matrix(a)
# mean(round(cor_mat1, 4) == round(cor_mat2, 4))
# # When rounded, all values are the same but not without which is due to numerical
# # differences.
# cor_mat1[1, 2] #0.05695812
# cor_mat2[1, 2] #0.05695812
