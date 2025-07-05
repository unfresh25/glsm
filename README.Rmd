# glsm

`glsm` implements Generalized Logistic Saturated Models for categorical outcomes with > 2 levels.  
It includes fitting, prediction, confidence intervals, deviance tests and plots.

## Installation

```r
# Not yet on CRAN, install from GitHub:
# devtools::install_github("tu_usuario/glsm")
```

## Example

```r
library(glsm)

modelo <- glsm(prog ~ ses + write, data = Datos, ref = "academic")
summary(modelo)

plot(modelo)
```
