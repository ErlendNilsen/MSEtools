

#############################################################################################################
#' Implementation of harvest quota
#'
#' The simplest form of an implementation model - basically just a random draw from a
#' binomial (ModType="A") or a Poisson (ModType="B") distribution.
#'
#'@param TAC Total allowable catch. Usually the output from a harvest decision model. Note that TAC must be an integer number
#' when ModType="A". 
#'@param ModType Type of probability model, being either A) Binomial or B) Poisson
#'@param p Probability of harvest in the binomial model, or "bias" in the Poisson model
#'@return H_I Number of harvested animals
#'@export

Impl1 <- function(TAC=10, ModType="A", p=0.7){
  H_I <- switch(ModType,
                A={rbinom(1, size=TAC, p=p)},
                B=(rpois(n=1, lambda=TAC*p)))

  H_I

}



##############################################################################################################
#' Schaefer's CPUE model
#'
#' Schaefer`s catch pr unit effort (CPUE) model. Assumes that catchability (q) is constant
#' and not dependent on population density.
#'
#' @param Effort defines the effort, that will be multiplied by q to estimate total catch
#' @param Pop_state Population abundance (or biomass)
#' @param q Catchability coeficient
#'
#' @return H_I Number of harvested individuals (or harvested biomass)
#' @export

Impl2 <- function(Effort=3, Pop_state=100, q=0.1){
  H_I <- Effort*Pop_state*q
  H_I

}



##############################################################################################################
#' Csirke_MacCall`s CPUE model
#'
#' Csirke_MacCall`s catch pr unit effort (CPUE) model. Assumes that catchability (q) increase as
#' population density (or biomass) decreases.
#'
#' @param Effort defines the effort, that will be multiplied by q to estimate total catch
#' @param Pop_state Population abundance (or biomass)
#' @param alpha Parameter is catchability model
#' @param beta Parameter in catchability model
#'
#' @return H_I Number of harvested individuals (or harvested biomass)
#' @export

Impl3 <- function(Effort=3, alpha=0.13, beta=0.1, Pop_state=10){

  q_star <- alpha*Pop_state^(-beta)
  H_I <- Effort * q_star * Pop_state
  H_I
}



##############################################################################################################
#' Csirke_MacCall`s CPUE model
#'
#' Csirke_MacCall`s catch pr unit effort (CPUE) model. Assumes that catchability (q) increase as
#' population density (or biomass) decreases. Modified to include lower threshold where harvest
#' is abondened, determined from an observation of population state erging from an observation
#' model. This is a shortcut, as this threshold is usually determined in a harvest decision model.
#'
#' @param Effort defines the effort, that will be multiplied by q to estimate total catch
#' @param Pop_state Population abundance (or biomass)
#' @param Est_Pop_state Estimated population size emerging from observation model (i.e. monitoring)
#' @param alpha Parameter is catchability model
#' @param beta Parameter in catchability model
#' @param c Threshold where harvesting is abondend
#'
#' @return H_I Number of harvested individuals (or harvested biomass)
#' @export


Impl3b <- function(Effort=3, alpha=0.13, beta=0.1, Pop_state=10, Est_Pop_state=10, c=10){

  q_star <- alpha*Pop_state^(-beta)
  H_I <- Effort * q_star * Pop_state

  H_I <- ifelse(Est_Pop_state < c, 0, H_I)
}



