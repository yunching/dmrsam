#' Market_capping
#'
#' @param mv Vector of market values
#' @param mv_cap_wts Vector of wts to cap each mv
#' @param step_amt Dollar amount to distribute at each step
#' @param debug Toggle if debugging messages should be displayed
#'
#' @return
#' @export
#'
#' @examples
market_capping <- function(mv, mv_cap_wts, step_amt=1e6, debug=FALSE){
  #check mv_cap_wts are valid
  if (sum(mv_cap_wts < 0 )){
    stop("There should not be any negative weights in mv_cap_wts.")
  }

  if (sum(mv_cap_wts > 100 )){
    stop("There should not be any weights > 100% in mv_cap_wts.")
  }

  #mv_cap_wts should be same length as mv
  if (length(mv) != length(mv_cap_wts)){
    stop("mv and mv_cap_wts should be of same length.")
  }

  #step_amount cannot be larger than total_mv
  if (step_amt > sum(mv)){
    stop("step_amount cannot be larger than sum(mv)")
  }

  #calculate uncapped weights
  total_mv <- sum(mv)
  uncapped_wts <- mv / total_mv

  #begin capping algorithm
  #in essence, distribute 1 dollar and check capping
  #if cap hits, re-distribute to all others
  #rinse and repeat
  allocated_mv <- rep(0, length(mv))
  step_mv <- rep(0, length(mv))
  step_wts <- uncapped_wts

  while (sum(allocated_mv) < total_mv) {
    if (debug==TRUE){
      message(c("Allocated MV: ", sum(allocated_mv)))
    }

    #rescale step_wts if they don't add to 1
    if (sum(step_wts)!=1){
      step_wts <- step_wts / sum(step_wts)
    }

    step_mv = step_amt * step_wts
    post_step_mv = allocated_mv + step_mv

    #if increment in mv violates capping weights
    post_step_wts <- post_step_mv / total_mv * 100
    if (sum(post_step_wts > mv_cap_wts) > 0) {
      unviolated_rows <- which(post_step_wts <= mv_cap_wts)
      violated_rows <- which(post_step_wts > mv_cap_wts)

      #figure out what the new step wts should be
      step_wts[unviolated_rows] <- mv[unviolated_rows] / total_mv
      step_wts[violated_rows] <- rep(0, length(violated_rows))
    }
    else{
      #if not violated, just increment allocation to each country
      allocated_mv <- allocated_mv + step_mv
    }
  }

  capped_mv <- allocated_mv
  capped_mv_wts <- capped_mv / sum(capped_mv) * 100
  output <- cbind(mv, mv_cap_wts, uncapped_wts * 100, capped_mv, capped_mv_wts)
  colnames(output) <- c("mv", "mv_cap_wts", "uncapped_wts", "capped_mv", "capped_mv_wts")
  return(output)
}

market_capping2 <- function(mv, mv_cap_wts, step_amt=1e6, debug=FALSE){
  #check mv_cap_wts are valid
  if (sum(mv_cap_wts < 0 )){
    stop("There should not be any negative weights in mv_cap_wts.")
  }

  if (sum(mv_cap_wts > 1 )){
    stop("There should not be any weights > 100% in mv_cap_wts.")
  }

  #mv_cap_wts should be same length as mv
  if (length(mv) != length(mv_cap_wts)){
    stop("mv and mv_cap_wts should be of same length.")
  }

  #step_amount cannot be larger than total_mv
  if (step_amt > sum(mv)){
    stop("step_amount cannot be larger than sum(mv)")
  }

  #calculate uncapped weights
  total_mv <- sum(mv)
  uncapped_wts <- mv / total_mv

  #if no capping constraints apply at all
  #then capped constraints = uncapped constraints
  if (sum(uncapped_wts < mv_cap_wts) == length(mv)){
    capped_mv_wts <- uncapped_wts
    capped_mv <- capped_mv_wts * total_mv
  }

  output <- cbind(mv, mv_cap_wts, uncapped_wts, capped_mv, capped_mv_wts)
  colnames(output) <- c("mv", "mv_cap_wts", "uncapped_wts", "capped_mv", "capped_mv_wts")
  return(output)
}


