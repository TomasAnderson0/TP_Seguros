dist_cuantias <- function(N, cuantias) {
  uniforme <- trunc(runif(N, min = 1, max = length(cuantias)+1))
  resultado <- cuantias[uniforme]
}
