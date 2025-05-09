# Function to calculate correlation between two 1-column XIFTIs
xifti_correlation <- function(xii1, xii2){
  return(cor(as.matrix(xii1),
             as.matrix(xii2)))
}
