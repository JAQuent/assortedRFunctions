dispDesignMatrix <- function(x){
  image(t(apply(x, 2, rev)), axes = FALSE)
}
