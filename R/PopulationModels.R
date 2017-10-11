
#' Theta-logistic population model - harvest before density dependent growth 
#'
#' A theta-logistic (theta-Ricker) population model, including the possibility for harvest. Harvest is applied
#' as a pulse, removing a pre-determined number of individuals. After harvest the population
#' grows according to the parameters in the model. Environmental stochasticity (process error) is modelled as an 
#'  additive effect on r(t). When theta = 1, the model it the conventional Ricker model .
#'
#'@param X_t0 Population size (X) at time t=0 (i.e. at the time step before the function applies)
#'@param sigma2_e environmental stoachasticity
#'@param N_Harv Number of individuals harvested (in year t0)
#'@param K Carrying capacity of the theta-logistic model
#'@param r_max Maximum growth rate (i.e. intrinsic growth rate)
#'@return Data frame containing the following columns:
#'@return eps: Value of the stochastic component in year t
#'@return X_star Post-harvest population size
#'@return r Population growth rate
#'@return X_t1 Population size (X) in year t=1
#'@export


PopMod_TLpre <- function(X_t0=100, sigma2_e=0.2, N_Harv=20, K=200, theta=1, r_max=1.0){

  eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
  X_star <- X_t0-N_Harv

  r <- (r_max*(1-(X_star/K)^theta))+eps
  X_t1 <- X_star*exp(r)

  PopRes <- as.data.frame(matrix(ncol=4, nrow=1))
  PopRes[1,1] <- eps
  PopRes[1,2] <- X_star
  PopRes[1,3] <- r
  PopRes[1,4] <- X_t1
  colnames(PopRes) <- c("eps", "X_star", "r", "X_t1")

  PopRes

}



##############################################################################################################
#' Gompertz population model - harvest before density dependent growth 
#'
#' A gompertz-type population model, including the possibility for harvest. Harvest is applied
#' as a pulse removing a pre-determined number of individuals. After harvest the (model) population
#' grows according to the parameters in the model.
#'
#'@param X_t0 Population size (X) at time t=0 (i.e. at the time step before the function applies)
#'@param sigma2_e environmental stochasticity
#'@param N_Harv Number of individuals harvested (in year t0)
#'@param K Carrying capacity of the theta-logistic model
#'@param r_max Maximum growth rate (i.e. intrinsic growth rate)
#'@return Data frame containing the following columns:
#'@return eps: Value of the stochastic component in year t
#'@return X_star Post-harvest population size
#'@return X_t1 Population size (X) in year t=1
#'@return lam Population growth rate (lambda) in year t
#'@export

PopMod_Gomp <- function(X_t0=100, sigma2_e=0.2, N_Harv=20, K=200, r_max=1.0){

  eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
  X_star <- X_t0-N_Harv

  r_1 <- r_max+eps
  beta <- r_max/log(K)

  X_t1 <- (exp(r_1))*(X_star^(1-beta))

  PopRes <- as.data.frame(matrix(ncol=4, nrow=1))
  PopRes[1,1] <- eps
  PopRes[1,2] <- X_star
  PopRes[1,3] <- X_t1
  PopRes[1,4] <- X_t1/X_star
  colnames(PopRes) <- c("eps", "X_star", "X_t1", "lam")

  PopRes

}



##############################################################################################################
#' Theta-logistic population model - variant
#'
#' A theta-logistic population model Variant (See Aanes et al. 2002 for model formulation), including the possibility for harvest. Harvest is 
#' applied as a pulse removing a pre-determined number of individuals. After harvest the population
#' grows according to the parameters governing the dynamics of the (model) population. Note that this type of model become the Gompertz model
#' as theta approaches 0. 
#'
#'@param X_t0 Population size (X) at time t=0 (i.e. at the time step before the function applies)
#'@param sigma2_e environmental stoachasticity
#'@param N_Harv Number of individuals harvested (in year t0)
#'@param K Carrying capacity of the theta-logistic model
#'@param r_max Maximum growth rate (i.e. intrinsic growth rate)
#'@return Data frame containing the following columns:
#'@return eps: Value of the stochastic component in year t
#'@return X_star Post-harvest population size
#'@return r Population growth rate
#'@return X_t1 Population size (X) in year t=1
#'@export

PopMod_TLvar <- function(X_t0=100, sigma2_e=0.2, N_Harv=20, K=200, theta=1, r_max=1.0){

  eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
  X_star <- X_t0-N_Harv

  r <- ((r_max/(1-K^-theta))*(1-(X_star/K)^theta))+eps
  X_t1 <- X_star*exp(r)

  PopRes <- as.data.frame(matrix(ncol=4, nrow=1))
  PopRes[1,1] <- eps
  PopRes[1,2] <- X_star
  PopRes[1,3] <- r
  PopRes[1,4] <- X_t1
  colnames(PopRes) <- c("eps", "X_star", "r", "X_t1")

  PopRes

}


####################################################################################
#' Theta-logistic population model - harvest after density dependent growth 
#'
#' A theta-logistic population model, including the possibility for harvest. Harvest is applied
#' as a pulse, removing a pre-determined number of individuals. Before harvest is applied, the population
#' grows according to the parameters governing the dynamics of the (model) population. When theta = 1, this become the 
#' conventional Ricker model. 
#'
#'@param X_t0 Population size (X) at time t=0 (i.e. at the time step before the function applies)
#'@param sigma2_e environmental stoachasticity
#'@param N_Harv Number of individuals harvested (in year t0)
#'@param K Carrying capacity of the theta-logistic model
#'@param r_max Maximum growth rate (i.e. intrinsic growth rate)
#'@return Data frame containing the following columns:
#'@return eps: Value of the stochastic component in year t
#'@return X_star Post-harvest population size
#'@return r Population growth rate
#'@return X_t1 Population size (X) in year t=1
#'@export

PopMod_TLpost <- function(X_t0=100, sigma2_e=0.2, N_Harv=20, theta=1, K=200, r_max=1.3){

  eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
  X_star <- X_t0-N_Harv

  r <- (r_max*(1-(X_t0/K)^theta))+eps
  X_t1 <- (X_t0*exp(r))-N_harv

  PopRes <- as.data.frame(matrix(ncol=4, nrow=1))
  PopRes[1,1] <- eps
  PopRes[1,2] <- X_star
  PopRes[1,3] <- r
  PopRes[1,4] <- X_t1
  colnames(PopRes) <- c("eps", "X_star", "r", "X_t1")

  PopRes

}





