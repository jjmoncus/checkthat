
#' Check that the levels of a group of variables match a reference set
#'
#' It's important that Tech Bullseye battery items all contain the same
#' response options.
#'
#' @export
#'
#' @importFrom rlang enquo
#' @importFrom purrr every
check_levels <- function(data,
                         vars,
                         ref_levels,
                         strictness = "identical") {

  stopifnot(strictness %in% c("identical", "setequal"))

  quo_vars <- enquo(vars)

  data %>%
    select(!!quo_vars) %>%
    every(~do.call(strictness,
                   args = list(levels(.x), ref_levels)))
}
