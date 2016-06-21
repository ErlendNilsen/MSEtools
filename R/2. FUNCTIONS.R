


##############################################################################################################
#### 4. Population model: THETA-LOGISTIC MODEL
### Input values:
###   X_t0: Population size (X) at time t=0 (i.e. at the time step before the function applies)
###   sigma2_e: environmental stoachasticity
###   N_Harv: Number of individuals harvested (in year t0)
###   K: Carrying capacity of the theta-logistic model
###   r_max: Maximum growth rate (i.e. intrinsic growht rate)
### Output values:
###   PopRes: Data frame containing the follwoing columns:
###      eps: Value of the stocathic component in year t
###     X_star: Post-harvest population size
###     r: Population growth rate
###     X_t1: Population size (X) in year t=1


PopMod1 <- function(X_t0=100, sigma2_e=0.2, N_Harv="H", K=200, theta=1, r_max=1.3){

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
#### 5. Population model 2: GOMPERTZ-MODEL
### Input values:
###   X_t0: Population size (X) at time t=0 (i.e. at the time step before the function applies)
###   sigma2_e: environmental stoachasticity
###   N_Harv: Number of individuals harvested (in year t0)
###   K: Carrying capacity of the theta-logistic model
###   r_max: Maximum growth rate (i.e. intrinsic growht rate)
### Output values:
###   PopRes: Data frame containing the follwoing columns:
###      eps: Value of the stocathic component in year t
###     X_star: Post-harvest population size
###     r: Population growth rate
###     X_t1: Population size (X) in year t=1


PopMod2 <- function(X_t0=100, sigma2_e=0.2, N_Harv="H", K=200, r_max=1.3){

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
#### 4B. Population model: THETA-LOGISTIC MODEL Variant (See Aanes et al. 2002 for model formulation)
### Input values:
###   X_t0: Population size (X) at time t=0 (i.e. at the time step before the function applies)
###   sigma2_e: environmental stoachasticity
###   N_Harv: Number of individuals harvested (in year t0)
###   K: Carrying capacity of the theta-logistic model
###   r_max: Maximum growth rate (i.e. intrinsic growht rate)
### Output values:
###   PopRes: Data frame containing the follwoing columns:
###      eps: Value of the stocathic component in year t
###     X_star: Post-harvest population size
###     r: Population growth rate
###     X_t1: Population size (X) in year t=1


PopMod1b <- function(X_t0=100, sigma2_e=0.2, N_Harv="H", K=200, theta=1, r_max=1.3){

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

##############################################################################################################
#### 4c. Population model: THETA-LOGISTIC MODEL - with density dependence of X (not X_star)
### Input values:
###   X_t0: Population size (X) at time t=0 (i.e. at the time step before the function applies)
###   sigma2_e: environmental stoachasticity
###   N_Harv: Number of individuals harvested (in year t0)
###   K: Carrying capacity of the theta-logistic model
###   r_max: Maximum growth rate (i.e. intrinsic growht rate)
### Output values:
###   PopRes: Data frame containing the follwoing columns:
###      eps: Value of the stocathic component in year t
###     X_star: Post-harvest population size
###     r: Population growth rate
###     X_t1: Population size (X) in year t=1


PopMod1c <- function(X_t0=100, sigma2_e=0.2, N_Harv="H", K=200, theta=1, r_max=1.3){

  eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
  X_star <- X_t0-N_Harv

  r <- (r_max*(1-(X_t0/K)^theta))+eps
  X_t1 <- X_star*exp(r)

  PopRes <- as.data.frame(matrix(ncol=4, nrow=1))
  PopRes[1,1] <- eps
  PopRes[1,2] <- X_star
  PopRes[1,3] <- r
  PopRes[1,4] <- X_t1
  colnames(PopRes) <- c("eps", "X_star", "r", "X_t1")

  PopRes

}





