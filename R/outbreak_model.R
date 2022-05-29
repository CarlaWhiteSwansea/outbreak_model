
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @inheritParams outbreak_step
#' @param dist_shape shape of the serial distribution
#' @param dist_scale scale of the serial distribution
#' @param delay_shape numeric shape parameter of delay distribution
#' @param delay_scale numeric scale parameter of delay distribution
#'
#' @return data.table of cases by week, cumulative cases, and the effective reproduction number of the outreak
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#'
#'\dontrun{
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(2, 4)
#' # generate initial cases
#' case_data <- outbreak_setup(num.initial.cases = 5,
#'                             incfn=incfn,
#'                             delayfn = delayfn,
#'                             k=1.95,
#'                             prop.asym=0)
#' # generate next generation of cases
#' case_data <- outbreak_step(case_data = case_data,
#'                            disp.iso = 1,
#'                            disp.com = 0.16,
#'                            disp.subclin = 0.16,
#'                            r0isolated = 0,
#'                            r0community = 2.5,
#'                            r0subclin = 1.25,
#'                            prop.asym = 0,
#'                            incfn = incfn,
#'                            delayfn = delayfn,
#'                            prop.ascertain = 0,
#'                            k = 1.95)
#'}
outbreak_model <- function(num.initial.cases = NULL, cap_max_days = NULL, cap_cases = NULL,
                           r0isolated = NULL, r0community = NULL, r0subclin = NULL,
                           disp.iso = NULL, disp.com = NULL, disp.subclin = NULL,
                           k = NULL, dist_shape = NULL, delay_shape = NULL,
                           dist_scale = NULL, delay_scale = NULL, prop.asym = NULL) {

  # Set up functions to sample from distributions
  # incubation period sampling function
  incfn <- dist_setup(dist_shape = dist_shape,
                      dist_scale = dist_scale)
  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run
  # onset to isolation delay sampling function
  delayfn <- dist_setup(delay_shape,
                        delay_scale)

  # Set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(num.initial.cases = num.initial.cases,
                            incfn = incfn,
                            prop.asym = prop.asym,
                            delayfn = delayfn,
                            k = k)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data = case_data,
                             disp.iso = disp.iso,
                             disp.com = disp.com,
                             disp.subclin = disp.subclin,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             r0subclin = r0subclin,
                             incfn = incfn,
                             delayfn = delayfn,
                             k = k,
                             prop.asym = prop.asym)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total.cases <- nrow(case_data)
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # # Prepare output
  daily_cases <- case_data[, day := floor(onset)
                            ][, .(daily_cases = .N), by = day]
  # maximum outbreak days
  max_day <- floor(cap_max_days)
  # weeks with 0 cases in 0:max_day
  missing_days <- (0:max_day)[!(0:max_day %in% daily_cases$day)]

  # add in missing days if any are missing
  if (length(missing_days > 0)) {
    daily_cases <- data.table::rbindlist(list(daily_cases,
                                               data.table(day = missing_days,
                                                          daily_cases = 0)))
  }
  # order and sum up
  daily_cases <- daily_cases[order(day)
                               ][, cumulative := cumsum(daily_cases)]
  # cut at max_day
  daily_cases <- daily_cases[day <= max_day]

  # Add effective R0
  daily_cases <- daily_cases[, `:=`(effective_r0 = mean(effective_r0_vect,
                                                          na.rm = TRUE),
                                        cases_per_gen = list(cases_in_gen_vect))]
  # return
  return(daily_cases)
  # return(case_data)
}
