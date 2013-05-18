`NSrates` <- function ( Coeff, maturity )
  {
    Curve <- try.xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff), error=as.matrix)
    colnames(Curve) <- make.names(maturity)
    Coeff <- as.matrix(Coeff)
    for (i in 1:nrow(Curve)) {
        Curve[i, ] <- as.numeric(Coeff[i, 1]) * rep(1, length(maturity)) +
            as.numeric(Coeff[i, 2]) * as.numeric(.factorBeta1(Coeff[i,
                4], maturity)) + as.numeric(Coeff[i, 3]) * as.numeric(.factorBeta2(Coeff[i,
            4], maturity))
    }
    return(Curve)
}
