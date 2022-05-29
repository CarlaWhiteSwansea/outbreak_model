#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n.sim number of simulations to run
#' @param num.initial.cases Initial number of cases in each initial cluster
#' @param num.initial.clusters Number of initial clusters
#' @param cap_max_days Maximum number of days to run process for
#' @param cap_cases Maximum number of cases to run process for
#' @param r0isolated basic reproduction number for isolated cases
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.iso dispersion parameter for negative binomial distribution for isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param dist_shape shape of the serial distribution
#' @param dist_scale scale of the serial distribution
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#'
#' @importFrom purrr safely
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' res <- scenario_sim(n.sim = 5,
#' num.initial.cases = 5,
#' cap_max_days = 365,
#' cap_cases = 2000,
#' r0isolated = 0,
#' r0community = 2.5,
#' disp.iso = 1,
#' disp.com = 0.16,
#' k = 0.7,
#' delay_shape = 2.5,
#' delay_scale = 5,
#' prop.asym = 0)
#' #' }
#'
scenario_sim <- function(n.sim = NULL, cap_max_days = NULL, cap_cases = NULL,
                         r0isolated = NULL, r0community = NULL, disp.iso = NULL, disp.com = NULL, k = NULL,
                         dist_shape = NULL, dist_scale = NULL, delay_shape = NULL, delay_scale = NULL, num.initial.cases = NULL, prop.asym = NULL,
                         r0subclin = NULL, disp.subclin = NULL) {

  # Set infectiousness of subclinical cases to be equal to clinical cases unless specified otherwise
  if(is.null(r0subclin)) {
    r0subclin <- r0community
  }

  if(is.null(disp.subclin)) {
    disp.subclin <- disp.com
  }
  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                             cap_max_days = cap_max_days,
                                             cap_cases = cap_cases,
                                             r0isolated = r0isolated,
                                             r0community = r0community,
                                             r0subclin = r0subclin,
                                             disp.subclin = disp.subclin,
                                             disp.iso = disp.iso,
                                             disp.com = disp.com,
                                             dist_shape = dist_shape,
                                             dist_scale = dist_scale,
                                             delay_shape = delay_shape,
                                             delay_scale = delay_scale,
                                             k = k,
                                             prop.asym = prop.asym))


  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[, sim := rep(1:n.sim, rep(floor(cap_max_days) + 1, n.sim)), ]
  return(res)
}
