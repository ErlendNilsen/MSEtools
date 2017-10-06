
##########################################################################################################
#' Harvest decision model - when population estimates are available.
#'
#' Select harvest decision model (proportional harvest, constant quota, proportional threshold or threshold harvesting)
#' to be applied. Each model are supplied with a set of control parameters.
#'
#' @param HD_type Type of harvest decision model, being one of three:
#' A: Proportional harvest (i.e. constant harvest mortality)
#' B: Threshold harvest (no harvest when bellow threshold c)
#' C: constant quota harvest (i.e. remove same number of individuals regardless of pop. size)
#' D: Proportional threshold harvest (i.e. removal of a proportion of the popualation above the threshold).
#' @param qu Depending on type of harvest model: for proportional harvest, this is the harvest rate,
#' for constant quota this is the number of individuals removed. For proportional threshold harvest,
#' this is the proportion above the threshold that is removed.  Not relevant for (absolute) threshold
#' harvesting
#' @param c Threshold in the threshold harvest model and proportional threshold model
#' @param PopState_est The (estimated) population size available to managers when making decision
#'
#' @return TAC Total allowable catch emerging from model and parameters
#' @export

HarvDec1 <- function(HD_type="A", c=1000, qu=0.2, PopState_est=100){
  TAC <- switch(HD_type,
                A={PopState_est*qu},
                B={ifelse(PopState_est>c, PopState_est-c, 0)},
                C={qu},
                D={ifelse(PopState_est>c, qu*(PopState_est-c), 0)})

  TAC
}


##########################################################################################################
#' Harvest decision model - when measures of hunters frustration is available
#'
#' Harvest size (qouta or effort) is decided based on information about
#' the level of frustration among hunters.
#'
#' @param qu Scaling parameter deciding how much the managers will adjust qouta 
#' as a response to hunters frustration,
#' @param TAC_t0 The total allowable catch (TAC) the previous year
#' @param UR_targ The target level of frustration that the managers are aiming for
#' @param UR_est The measured or estimated level of frustraion among hunters
#'
#' @return TAC Total allowable catch emerging from model and parameters
#' @export

HarvDec2 <- function(TAC_t0= 2,  qu=0.2, UR_targ=0.8, UR_est=0.7){
  
  
  TAC <- ifelse(UR_targ>round(UR_est, 1), TAC_t1+qu, 
                ifelse(UR_targ<round(UR_est,1), TAC_t1-qu, TAC_t1))
  TAC
  
}


############################################################################################################
#' Harvest decision model - when measures of hunters frustration is available
#'
#' Harvest size (qouta or effort) is decided based on information about
#' the level of frustration among hunters.
#'
#' @param qu Scaling parameter deciding how much the managers will adjust qouta 
#' as a response to hunters frustration,
#' @param TAC_t0 The total allowable catch (TAC) the previous year
#' @param UR_targ The target level of frustration that the managers are aiming for
#' @param UR_est The measured or estimated level of frustraion among hunters
#'
#' @return TAC Total allowable catch emerging from model and parameters
#' @export

HarvDec_E <- function(TAC_t0= 2,  qu=0.2, UR_targ=0.8, UR_est=0.7){
  
  
  TAC <- ifelse(UR_targ>round(UR_est, 1), TAC_t1+qu, 
                ifelse(UR_targ<round(UR_est,1), TAC_t1-qu, TAC_t1))
  TAC
  
}





