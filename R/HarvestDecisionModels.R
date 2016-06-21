
##########################################################################################################
#' General harvest decision model
#'
#' Select harvest decision model (proportional harvest, constant quota or threshold harvesting)
#' to be applied. Each model are supplied with a set of control parameters.
#'
#' @param HD_type Type of harvest decision model, being one of three:
#' A: Proportional harvest (i.e. constant harvest mortality)
#' B: Threshold harvest (no harvest when bellow threshold c)
#' C: constant quota harvest (i.e. remove same number of individuals regardless of pop. size)
#' @param qu Depending on type of harvest model: for proportional harvest, this is the harvest rate,
#' for constant quota this is the number of individuals removed. Not relevant for (absolute) threshold
#' harvesting
#' @param c Threshold in the threshold harvest model
#' @param PopState_est The (estimated) population size available to managers when making decision
#'
#' @value TAC Total allowable cathc emerging from model and parameters
#' @export

HarvDec1 <- function(HD_type="A", c=1000, qu=0.2, PopState_est=100){
  TAC <- switch(HD_type,
                A={PopState_est*qu},
                B={ifelse(PopState_est>c, PopState_est-c, 0)},
                C={qu},
                D={ifelse(PopState_est>c, qu(PopState_est-c), 0)})

  TAC
}
