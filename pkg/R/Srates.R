`Srates`<- function( Coeff, maturity, whichRate="Forward" )
  {
    Coeff <- try.xts( Coeff, error=as.matrix )
    Curve <- matrix( 0, nrow(Coeff), length(maturity) )
    colnames(Curve) <- make.names(maturity)

    switch(whichRate,
      Forward =
      {
        colnames(CurveForward) <- maturity
        for(i in 1:nrow(Coeff))
          {
            Curve[i,] <- Coeff[i,1] +
              Coeff[i,2] * .beta1Forward( maturity, Coeff[i,5] ) +
              Coeff[i,3] * .beta2Forward( maturity, Coeff[i,5] ) +
              Coeff[i,4] * .beta2Forward( maturity, Coeff[i,6] )
          }
      },
      Spot =
      {
         for(i in 1:nrow(Coeff))
          {
            Curve[i,] <- Coeff[i,1] +
              Coeff[i,2] * .beta1Spot( maturity, Coeff[i,5] ) +
              Coeff[i,3] * .beta2Spot( maturity, Coeff[i,5] ) +
              Coeff[i,4] * .beta2Spot( maturity, Coeff[i,6] )
          }
      })
    reclass( Curve, Coeff )
  }
