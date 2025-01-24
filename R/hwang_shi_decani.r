#' Hwang–Shih–DeCani Alpha-Spending Function
#'
#' @param information_rate Numeric in [0,1], the fraction of total information at the current interim.
#' @param alpha_total Numeric, the overall (final) alpha level (default 0.05).
#' @param lambda Numeric, the HSD shape parameter (default 2).
#'
#' @return The alpha spent (i.e., the p-value boundary) at this interim.
#'
hwang_shih_decani <- function(information_rate,
                              alpha_total = 0.05,
                              lambda = 2) {
  # Handle the limiting case: lambda very close to 0 => Pocock-like design
  if (abs(lambda) < 1e-12) {
    return(alpha_total * information_rate)
  }
  # General case
  alpha_spent <- alpha_total * (1 - exp(-lambda * information_rate)) / (1 - exp(-lambda))
  return(alpha_spent)
}
