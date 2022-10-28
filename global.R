library(shiny)
library(shinyWidgets)
library(plotly)
library(visNetwork)
library(tidyverse)

# assumptions----
ir <- 50 # infection radius
ms <- 20 # max step distance per iteration

# distance function----
dist <- function(x1, y1, x2, y2) {
  return(
    sqrt((x1-x2)^2+(y1-y2)^2)
  )
}

# probabilities of infection function----
prInf <- function(m, v) {
  return(
    case_when(
       m &  v ~ 0.01,
      !m &  v ~ 0.03,
       m & !v ~ 0.05,
      !m & !v ~ 0.20
    )
  )
}

# probabilities of recovery function----
prRec <- function(v) {
  return(
    case_when(
       v ~ 0.0,
      !v ~ 0.0
    )
  )
}