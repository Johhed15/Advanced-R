## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
#library(linmod)
devtools::load_all()

## -----------------------------------------------------------------------------
library(linmod)

 

m <- linreg(Petal.Width ~ Species, data = iris)

 


## ---- warning = FALSE, out.width= "70%", fig.align="center", fig.width = 6, fig.height = 4----
m$plot()

 


## -----------------------------------------------------------------------------
m$print()

 


## -----------------------------------------------------------------------------
m$resid()

 


## -----------------------------------------------------------------------------
m$coef()

 


## -----------------------------------------------------------------------------
m$pred()

 


## -----------------------------------------------------------------------------
m$summary()

 


## -----------------------------------------------------------------------------

 

 


