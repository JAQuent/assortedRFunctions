savage_dickey_ratio <- function(postDist, priorDensity, areaPrior = 0.5, alternative = 'two.sided'){
  # Get polspline if not loaded already
  if(!any(search() == "package:polspline")){
    require(polspline)
  }

  # Calculate BF according to alternative
  if(alternative  == 'greater'){
    # Fit logspline to postDist
    fit.posterior  <- logspline(postDist)
    posterior      <- dlogspline(0, fit.posterior)

    # Normalise to 1 with area over zero
    areaPosterior <- sum(postDist > 0)/length(postDist)
    posterior.OR  <- posterior/areaPosterior
    prior.OR      <- priorDensity/areaPrior

    # Calculate BF10
    BF10          <- prior.OR/posterior.OR

  } else if (alternative  == 'less'){
    # Fit logspline to postDist
    fit.posterior  <- logspline(postDist)
    posterior      <- dlogspline(0, fit.posterior)

    # Normalise to 1 with area under zero
    areaPosterior <- sum(postDist < 0)/length(postDist)
    posterior.OR  <- posterior/areaPosterior
    prior.OR      <- priorDensity/areaPrior

    # Calculate BF10
    BF10          <- prior.OR/posterior.OR

  } else if (alternative  == 'two.sided'){
    # Fit logspline to postDist
    fit.posterior  <- logspline(postDist)
    posterior      <- dlogspline(0, fit.posterior)

    # Calculate BF10
    BF10           <- priorDensity/posterior

  } else {
   stop('The argument >> alternative << was not specified correctly. Use "less", "greater" or "two.sided".')
  }

  # Returns BF10
  return(BF10)
}


# # Example usage
# # Get a posterior distribution N(2, 0.5)
# postDist     <- rnorm(100000, 2, 0.5)
#
# # Get the density of the prior N(0, 1) at 0
# priorDensity <- dnorm(0, 0, 1)
#
# # Specify areaPrior (only need if not two.sided). In this examples the prior is symmetric around zero so
# areaPrior <- 0.5
#
# # Calculate BF10
# savage_dickey_ratio(postDist, priorDensity, areaPrior, 'greater')
