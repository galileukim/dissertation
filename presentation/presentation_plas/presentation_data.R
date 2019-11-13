# global ------------------------------------------------------------------
edu_global <- fread(
  here("data/worldbank/global_edu_indicators.csv")
)

edu_br <- edu_global %>% 
  filter(
    country_code == "bra"
  )

edu_global %<>% 
  filter(country_code != "bra") %>% 
  select_if(is.numeric) %>% 
  group_by(
    year
  ) %>% 
  summarise_all(
    mean,
    na.rm = T
  ) 

# covariates --------------------------------------------------------------
finbra <- fread(
  here("data/finbra/finbra.csv")
)

censo <- import_data(
  here("data/censo_br/"),
  "2000"
) %>% 
  pluck(1)

# censo escolar -----------------------------------------------------------
import_data(
  here("data/censo_escolar"),
  "school_mun|class_mun|class_dep|censo_mun_turnover"
) %>% 
  assign_data

censo_mun_turnover %<>%
  calc_turnover(
    c("cod_ibge_6", "year")
  ) %>% 
  rename(
     n_teachers = n
  )

censo_class_mun %<>% 
  filter(dep == "municipal")

# saeb --------------------------------------------------------------------
saeb_dep <- fread(
  here("data/saeb/saeb_exam_dep.csv")
)

saeb_mun <- fread(
  here("data/saeb/saeb_exam_mun.csv")
) %>% 
  select(-dep)

saeb_mun <- list(
  saeb_mun,
  censo_mun_turnover,
  finbra,
  censo_class_mun
) %>%
  reduce(
    left_join,
    by = c("cod_ibge_6", "year")
  ) %>% 
  left_join(
    censo,
    by = c("cod_ibge_6")
  ) %>% 
  mutate(
    budget_pc = budget_education/sum
  )

saeb_hierarchical <- fread(
  here("data/saeb/saeb_hierarchical.csv.gz")
)

# rais --------------------------------------------------------------------
rais_mun <- fread(
  here("data/rais/rais_mun.csv.gz")
)

rais_edu <- fread(
  here("data/rais/rais_edu_sample.csv.gz")
)
