# set-up ------------------------------------------------------------------
source(
  here::here("scripts/thesis_setup.R")
)

# source(
#   here("scripts/thesis_map.R")
# )

# data --------------------------------------------------------------------
# student test scores
saeb_mun <- fread(
  here("data/saeb/saeb_exam_mun.csv")
)

saeb_hierarchical <- fread(
  here("data/saeb/saeb_hierarchical.csv.gz")
) %>% 
  filter(!is.na(uf))
  

saeb_hierarchical %<>% 
  sample_group(
    n = 100,
    cod_ibge_6
  )

saeb_hierarchical %<>%
    mutate_if(
      is.double,
      rescale
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

# hierarchical model ------------------------------------------------------
mods <- list()

formula_hlm <- as.formula(
  grade_exam ~ as.factor(year) + as.factor(uf) + saeb_principal_female + saeb_principal_education + saeb_principal_year_as_director +
    saeb_principal_appointment + education_teacher + type_contract_teacher + wage_teacher + year_as_teacher + year_working_school_teacher +
    age_cat_student + race_student + mother_home_student + mother_edu_student + father_home_student + father_edu_student +
    log(pop) + rural + lit_rate
)

saeb_hlm <- model.matrix(
  formula_hlm,
  saeb_hierarchical
)

mods$lm_base <- lm(
  formula_hlm,
  data = saeb_hierarchical
)

mods$lmer_base <- lmer(
  grade_exam ~ (1|year) + (1|uf) +
    saeb_principal_female + saeb_principal_education + saeb_principal_year_as_director +
    saeb_principal_appointment + education_teacher + type_contract_teacher + wage_teacher + year_as_teacher + year_working_school_teacher +
    age_cat_student + race_student + mother_home_student + mother_edu_student + father_home_student + father_edu_student +
    log(pop) + rural + lit_rate,
  data = saeb_hierarchical
)
s
plot_hlm <- map(
  mods,
  ~tidycoef(
    .,
    vars = c("year_as_teacher", "year_as_director")
  )
)
