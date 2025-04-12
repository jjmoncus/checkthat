#' Generic function for checking that batteries are all NA or not NA
#'
#' @keywords internal
#'
#' @importFrom rlang enquos
constructor <- function(data, fun, ...) {
  dots <- rlang::enquos(...)

  data %>%
    dplyr::select(!!!dots) %>%
    purrr::every(function(x) do.call(fun, args = list(.x = x, .p = is.na)))
}


#' Check that a group of variables contains no missing values
#'
#' @export
#'
#' @importFrom purrr none
all_not_NA <- function(data, ...) constructor(data, fun = none, ...)

#' Check that a group of variables contains only missing values
#'
#' @export
#'
#' @importFrom purrr every
all_NA <- function(data, ...) constructor(data, fun = every, ...)
