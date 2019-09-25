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
  purrr
)

between <- data.table::between
set.seed(1789)

source(
  here("scripts/thesis_funs.R")
)