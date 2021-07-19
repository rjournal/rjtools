## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(plotly)


## ----eval = FALSE, echo = TRUE------------------------------------------------
#> x <- 1:10
#> plot(x)


## ----myplot, fig.cap = "A caption should include three elements: 1) a one sentence summary of what the plot is about; 2) the details of elements being plotted; and 3) the messages from the plot."----
x <- 1:10
plot(x)

