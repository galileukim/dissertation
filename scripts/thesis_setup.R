# set up ------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  data.table,
  rgdal,
  here,
  R.utils,
  GGally,
  lfe,
  lme4,
  plm,
  broom,
  extrafont,
  rprojroot,
  tmap,
  gridExtra,
  cowplot,
  ggdag,
  scales,
  kableExtra,
  RColorBrewer,
  gghighlight,
  parallel,
  foreach,
  gridExtra,
  naniar,
  knitr,
  gghighlight,
  egg
)

knitr::opts_chunk$set(
  eval = T,
  cache = T,
  cache.lazy = F,
  message = F,
  warning = F,
  echo = F,
  fig.height = 3,
  fig.width = 4,
  fig.align = "center"
  # dev = "pdf"
)

source(
  here("scripts/thesis_funs.R")
)

`%<>%` <- magrittr::`%<>%`
between <- data.table::between
set.seed(1789)