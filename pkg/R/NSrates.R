`NSrates` <- function ( Coeff, maturity )
  {
    Coeff <- try.xts( Coeff, error=as.matrix )
    Curve <- matrix( 0, nrow(Coeff), length(maturity) )
    colnames(Curve) <- make.names(maturity)

    for(i in 1:nrow(Coeff)) 
      {
        Curve[i,] <- Coeff[i,1] * rep(1, length(maturity)) +
          Coeff[i,2] * .factorBeta1(Coeff[i,4], maturity) +
          Coeff[i,3] * .factorBeta2(Coeff[i,4], maturity )
      }
    return( Curve )
  } 
