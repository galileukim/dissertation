# set-up ------------------------------------------------------------------
source(
  here::here("scripts/thesis_setup.R")
)

source(
  here("scripts/thesis_map.R")
)

# data --------------------------------------------------------------------
# student test scores
saeb_mun <- fread(
  here("data/saeb/saeb_exam_mun.csv")
)

# map ---------------------------------------------------------------------
map_score <- map_br %>% 
  mutate(
    cod_ibge_6 = as.integer(cod_ibge_6)
  ) %>% 
  left_join(
    saeb_mun %>% 
      filter(
        year == 2015
      ),
    by = c("cod_ibge_6")
  )

plot_map(
  map_score,
  fill = "mean_grade_exam_wgt",
  legend_position = "none",
  limits = c(100, 300)
) +
  ggsave(
    here("presentation/presentation_plas/figs/saeb_map.pdf"),
    height = 2.5,
    width = 3
  )
