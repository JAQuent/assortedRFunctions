count_pairs <- function(n, k){
  factorial(n)/(factorial(k)*factorial(n-k))
}