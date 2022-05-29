#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#' @examples
#'
dist_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp vector of samples from the incubation period distribution
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#'
#' @return
#' @export
#' @importFrom sn rsn
#' @examples
#'
inf_fn <- function(inc_samp = NULL, k = NULL) {

  out <- sn::rsn(n = length(inc_samp),
                 xi = inc_samp,
                 omega = 2,
                 alpha = k)

  out <- ifelse(out < 1, 1, out)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @return
#' @export
#' @inheritParams detect_extinct
#' @examples
#'
extinct_prob <- function(outbreak_df_day  = NULL, cap_cases  = NULL) {

  n_sim <- max(outbreak_df_day$sim)

  out <- outbreak_df_day %>%
    # new variable extinct = 1 if cases in days 84:112 (weeks 10-12) all 0, 0 if not
    detect_extinct(cap_cases) %>%
    # number of runs where extinct = TRUE / number of runs
    .$extinct %>%
    sum(.) / n_sim

  return(out)
}


#' Calculate proportion of outbreaks that went extinct
#' @author Joel Hellewell
#' @param outbreak_df_day data.table  daily cases producted by the outbreak model
#' @param cap_cases integer number of cumulative cases at which the branching process was terminated
#'
#' @return
#' @export
#' @importFrom dplyr group_by filter summarise ungroup
#' @examples
#'
detect_extinct <- function(outbreak_df_day  = NULL, cap_cases  = NULL) {

  outbreak_df_day %>%
    dplyr::group_by(sim) %>% # group by simulation run
    dplyr::filter(day %in% 84:112) %>%
    dplyr::summarise(extinct =
                       ifelse(all(daily_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0)) %>%
    dplyr::ungroup()

}

