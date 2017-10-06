
#############################################################################################################
#' Simple(st) observation model
#'
#' A simplistic observation model, based on the lognormal or normal distribution.
#'
#' @param scale Indicates if you are modelling density ("Dens") or Abundance ("Abund").
#' @param value The true value of abundance/density
#' @param bias  Relative bias, where 1=no bias, 0-0.99 = bias low, >1 = bias high
#' @param cv Coef of variation of the monitoring program/survey - in relation to true value.
#' @param LogNorm Indicates if a lognormal ("LND") or normal ("ND") density model is used
#'
#' @return obs1 Mean value of the observation (i.e. observation)
#'
#' @export

obs_mod1 <- function(scale="Abund", value=1000, bias=1, cv=0.2, LogNorm="ND"){

  obs1 <-  switch(LogNorm,
                  LND={rlnorm(n=1, meanlog=log(value*bias), sdlog=cv)},
                  ND={rnorm(n=1,mean=value*bias, sd=cv*value)})

  obs1 <- switch(scale,
                 Abund={round(obs1)},
                 Dens={obs1})
  obs1
}
