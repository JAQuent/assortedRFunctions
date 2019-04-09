createResultTable <- function(x){
  # Creating a nice looking table
  if(inherits(x, "glmerMod")){
    # For glmer table
    xTable        <- summary(x)$coefficients
    xTable        <- data.frame(xTable)
    xTable[, 1]   <- round(xTable[, 1], 2)
    xTable[, 2]   <- round(xTable[, 2], 2)
    xTable[, 3]   <- round(xTable[, 3], 2)
    xTable[, 4]   <- round(xTable[, 4], 4)
    xTable        <- cbind(xTable, sigStars(xTable[, 4]))
    names(xTable) <- c('Estimate', 'SE', 'Z', 'P', 'Sig')
  } else if(inherits(x, 'lmerModLmerTest')){
    xTable        <- summary(x)$coefficients
    xTable        <- data.frame(xTable)
    xTable[, 1]   <- round(xTable[, 1], 2)
    xTable[, 2]   <- round(xTable[, 2], 2)
    xTable[, 3]   <- round(xTable[, 3], 2)
    xTable[, 4]   <- round(xTable[, 4], 2)
    xTable[, 5]   <- round(xTable[, 5], 4)
    xTable        <- cbind(xTable, sigStars(xTable[, 5]))
    names(xTable) <- c('Estimate', 'SE', 'DF', 'T', 'P', 'Sig')
  } else if(inherits(x, 'anova')){
    if(attributes(x)$heading == "Analysis of Variance Table of type III  with  Satterthwaite \napproximation for degrees of freedom"){
      # Only ANOVA on lmerTest models with Satterthwaite approximation
      xTable        <- data.frame(x)
      xTable[, 1]   <- round(xTable[, 1], 2)
      xTable[, 2]   <- round(xTable[, 2], 2)
      xTable[, 3]   <- round(xTable[, 3], 2)
      xTable[, 4]   <- round(xTable[, 4], 2)
      xTable[, 5]   <- round(xTable[, 5], 2)
      xTable[, 6]   <- round(xTable[, 6], 4)
      xTable        <- cbind(xTable, sigStars(xTable[, 6]))
      names(xTable) <- c('SS', 'MSS', 'nDF', 'dDF', 'F', 'P', 'Sig')
    } else {
      xTable <- data.frame('######', 'No known model', '######')
      names(xTable) <- c('%%%', '***', '&&&')
    }
  } else {
    xTable <- data.frame('######', 'No known model', '######')
    names(xTable) <- c('%%%', '***', '&&&')
  }
  return(xTable)
}
