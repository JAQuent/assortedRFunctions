bayesFactor_labeling <- function(BF10){
  
  # Based on https://www.r-bloggers.com/what-does-a-bayes-factor-feel-like/
  bfs_labels     <- c('> 100',
                      '100 - 30',
                      '30 – 10',
                      '10 – 3',
                      '3 – 1',
                      '1',
                      '1 - 1/3',
                      '1/3 – 1/10',
                      '1/10 – 1/30',
                      '1/30 – 1/100',
                      '< 1/100')
  
  # Counting BFS according to labels
  BF10_labeled <- rep(NA, length(BF10))
  BF10_labeled[which(BF10 > 100)]                  <- 1
  BF10_labeled[which(BF10 <= 100 & BF10 > 30)]     <- 2
  BF10_labeled[which(BF10 <= 30 & BF10 > 10)]      <- 3
  BF10_labeled[which(BF10 <= 10 & BF10 > 3)]       <- 4
  BF10_labeled[which(BF10 <= 3 & BF10 > 1)]        <- 5
  BF10_labeled[which(BF10 == 1)]                   <- 6 
  BF10_labeled[which(BF10 < 1 & BF10 >= 1/3)]      <- 7
  BF10_labeled[which(BF10 < 1/3 & BF10 >= 1/10)]   <- 8
  BF10_labeled[which(BF10 < 1/10 & BF10 >= 1/30)]  <- 9
  BF10_labeled[which(BF10 < 1/30 & BF10 >= 1/100)] <- 10
  BF10_labeled[which(BF10 < 1/100)]                <- 11
  
  return(factor(BF10_labeled, levels = 1:11, labels = bfs_labels))
}