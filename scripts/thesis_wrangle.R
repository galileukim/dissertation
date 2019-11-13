# set-up ------------------------------------------------------------------
pacman::p_load(
  "tidyverse",
  "purrr",
  "data.table",
  "R.utils",
  "rprojroot",
  "here",
  "parallel"
)

set.seed(1789)

source(
  here("scripts/thesis_funs.R")
)

# wrangle -----------------------------------------------------------------
# elections
election <- fread(
  "~/princeton/R/data/tse/data/wrangle/election_chamber.csv"
  ) %>%
  filter(
    election_year %in% seq(2000, 2016, 4)
  ) %>% 
  group_by(cod_ibge_6) %>%
  mutate(
    mayor_party_lag = dplyr::lag(mayor_party, order_by = election_year),
    mayor_coalition_lag = dplyr::lag(mayor_coalition, order_by = election_year)
  ) %>% 
  ungroup() %>% 
  mutate(
    party_turnover = if_else(mayor_party != mayor_party_lag, 1, 0)
  )

mayor <- gunzip(
  "~/princeton/R/data/tse/data/wrangle/election.csv.gz",
  remove = F,
  temporary = T,
  overwrite = T
) %>% 
  fread %>% 
  filter(
    election_year %in% seq(2000, 2016, 4),
    position == "prefeito"
  )

vereador <- fread(
  "~/princeton/R/data/tse/data/wrangle/election.csv.gz",
  nThread = parallel::detectCores() - 1
) %>% 
  filter(
    election_year %in% seq(2000, 2016, 4),
    position != "prefeito"
  )

# write-out
election %>% 
  fwrite(
    here("data/tse/election.csv")
  )

mayor %>% 
  fwrite(
    here("data/tse/mayor.csv")
  )

vereador %>% 
  fwrite(
    here("data/tse/vereador.csv")
  )

gzip(
  here("data/tse/vereador.csv"),
  overwrite = T
)

# censo -------------------------------------------------------------------
censo_files <- list.files(
  "~/princeton/R/data/censo_br/data/wrangle/ibge",
  pattern = "^censo_mun_20[0|1]0"
)

for(i in seq_along(censo_files)){
  censo <- censo_files[i] %>%
    fread %>%
    transmute(
      cod_ibge_6,
      median_wage,
      rural,
      lit_rate,
      sanitation,
      garbage,
      light,
      pop,
      student_age,
      log_pop = log(pop)
    ) %>%
    setNames(
      c(
        "cod_ibge_6",
        paste0("censo_", names(.)[2:length(names(.))])
      )
    )
  
  # write-out
  censo %>% 
    fwrite(
      here(
        paste0(
          "data/censo_br/censo_", c(2000, 2010)[i], ".csv"
        )
      )
    )
}

# finbra ------------------------------------------------------------------
# budget
finbra <- list.files(
  "~/princeton/R/data/finbra/data/wrangle",
  "^despesa_mun.csv$",
  full.names = T
  ) %>%
  fread %>%
  filter(
    year >= 2000
  ) %>%
  select(
    cod_ibge_6,
    year,
    total = total_expenditure,
    legislative,
    administration,
    health = health_sanitation,
    education = education_culture,
    industry = industry_commerce
  )

# fix names
finbra <- finbra %>%
  setNames(
    c(
      "cod_ibge_6", "year",
      paste0("budget_", names(.)[3:length(names(.))])
    )
  )

# write-out
finbra %>% 
  fwrite(
    here("data/finbra/finbra.csv")
  )

# rais --------------------------------------------------------------------
list.files(
  "~/princeton/R/data/rais/data/wrangle",
  pattern = "^rais_[edu|cabinet].*\\.csv\\.gz$",
  full.names = T
) %>% 
  map(
    . %>% 
      file.copy(
        to = here("data/rais")
      )
  )

file.copy(
  "~/princeton/R/data/rais/data/wrangle/mun",
  here("data/rais"),
  recursive = T
)

# create municipal level data
rais_mun <- fread(
  "~/princeton/R/data/rais/data/wrangle/rais_mun.csv.gz"
)

# electoral years
rais_mun <- rais_mun %>% 
  mutate(
    election_year = case_when(
      between(year, 1993, 1996) ~ 1992,
      between(year, 1997, 2000) ~ 1996,
      between(year, 2001, 2004) ~ 2000,
      between(year, 2005, 2008) ~ 2004,
      between(year, 2009, 2012) ~ 2008,
      between(year, 2013, 2016) ~ 2012
    )
  )

# fix cols
rais_mun <- rais_mun %>% 
  select(
    cod_ibge_6,
    cbo_category,
    year,
    election_year,
    everything()
  )

# write-out
rais_mun %>% 
  fwrite(
    here("data/rais/rais_mun.csv")
  )

gzip(
  here("data/rais/rais_mun.csv"),
  overwrite = T
)

# rais cabinet mun summary table
rais_cabinet_mun <- gunzip(
  "~/princeton/R/data/rais/data/wrangle/rais_cabinet.csv.gz",
  remove = F,
  temporary = T,
  overwrite = T
) %>% 
  fread 

rais_cabinet_mun <- rais_cabinet_mun %>%
  mutate(
    rais_fired = ifelse(
      rais_fired >= 10 & rais_fired <= 22,
      1,
      0
    )
  ) %>%
  group_by(year, cod_ibge_6) %>%
  summarise(
    mean_hired = mean(rais_hired, na.rm = T),
    mean_edu = mean(rais_edu, na.rm = T),
    mean_fired = mean(rais_fired, na.rm = T),
    median_wage = median(rais_wage, na.rm = T),
    mean_time = mean(rais_time, na.rm = T),
    rais_permanent = mean(rais_contract == 30, na.rm = T),
    cabinet_size = n(),
    rais_mun_size = mean(rais_mun_size, na.rm = T),
    prop_higher_edu = mean(rais_edu == 9, na.rm = T),
    mean_edu = mean(rais_edu, na.rm = T)
  ) %>% 
  ungroup()

# arrange
rais_cabinet_mun <- rais_cabinet_mun %>%
  arrange(cod_ibge_6, year)

# write-out
rais_cabinet_mun %>%
  fwrite(
    here("data/rais/rais_cabinet_mun.csv")
  )

# teacher and school principal data
rais_edu <- fread(
  "~/princeton/R/data/rais/data/wrangle/rais_edu.csv.gz",
  nThread = parallel::detectCores() - 1
) %>% 
  filter(
    year >= 2005
  )

# create municipal level data
rais_edu_mun <- rais_edu %>%
  group_by(
    cod_ibge_6,
    rais_category,
    year
  ) %>%
  summarise(
    rais_adm = mean(rais_adm, na.rm = T),
    rais_size = n(),
    rais_higher_edu = mean(rais_edu >= 9, na.rm = T),
    rais_fired = mean(rais_fired, na.rm = T),
    rais_hired = mean(rais_hired, na.rm = T),
    rais_permanent = mean(rais_permanent, na.rm = T),
    rais_rehired = mean(rais_rehired, na.rm = T),
    rais_time = mean(rais_time, na.rm = T),
    rais_wage = median(rais_wage, na.rm = T)
  ) %>%
  ungroup()

# electoral years
rais_edu_mun <- rais_edu_mun %>% 
  mutate(
    election_year = case_when(
      between(year, 2001, 2004) ~ 2000,
      between(year, 2005, 2008) ~ 2004,
      between(year, 2009, 2012) ~ 2008,
      between(year, 2013, 2016) ~ 2012
    )
  )

# fix cols
rais_edu_mun <- rais_edu_mun %>% 
  select(
    cod_ibge_6,
    year,
    election_year,
    rais_category,
    everything()
  )

# write-out
rais_edu_mun %>% 
  fwrite(
    here("data/rais_edu_mun.csv")
  )

# education ---------------------------------------------------------------
# student test scores (saeb)
file.copy(
  "~/princeton/R/data/saeb/data/wrangle/saeb_student.csv.gz",
  here("data/saeb/saeb_student.csv.gz"),
  overwrite = T
)

# teachers (saeb)
file.copy(
  "~/princeton/R/eda/education/data/saeb_teacher.csv.gz",
  here("data/saeb/saeb_teacher.csv.gz"),
  overwrite = T
)

# hierarchical model
file.copy(
  "~/princeton/R/eda/education/data/saeb_hierarchical.csv.gz",
  here("data/saeb/saeb_hierarchical.csv.gz"),
  overwrite = T
)

# school census
file.copy(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/escolas/censo_school.csv.gz",
  here("data/censo_escolar/censo_school.csv.gz"),
  overwrite = T
)

# teacher census
file.copy(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/docentes/docente_mun.csv.gz",
  here("data/censo_escolar/censo_teacher_mun.csv.gz"),
  overwrite = T
)

file.copy(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/docentes/docente_mun_sample.csv.gz",
  here("data/censo_escolar/censo_teacher_mun_sample.csv.gz"),
  overwrite = T
)

file.copy(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/docentes/docente_sample.csv.gz",
  here("data/censo_escolar/censo_teacher_sample.csv.gz"),
  overwrite = T
)
  
# enrollment census
file.copy(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/turmas/turmas.csv.gz",
  here("data/censo_escolar/censo_class.csv.gz"),
  overwrite = T
)

# prova brasil data
list.files(
  "~/princeton/R/data/saeb/data/wrangle/student/",
  "^saeb_"
  ) %>% 
  walk(
    file.copy,
    to = here("data/saeb/"),
    overwrite = T
  )

# ceara data
file.copy(
  "~/princeton/R/data/spaece/data/wrangle/spaece.csv",
  here("data/spaece/spaece.csv"),
  overwrite = T
)

# censo escolar -----------------------------------------------------------
# wrangle
censo_school <- fread(
  here("data/censo_escolar/censo_school.csv.gz"),
  integer64 = "character"
)

# keep only active schools and generate municipal table
censo_school_mun <- censo_school %>% 
  filter(
    str_detect(active, "(1|2|ativ)")
  ) %>% 
  group_by(
    cod_ibge_6,
    year,
    dep
  ) %>% 
  summarise(
    n_school = n(),
    n_room = mean(rooms_existing),
    n_lower_sch = sum(
      elementary_sch,
      elementary_sch_8_9,
      elementary_sch_9,
      na.rm = T
    ),
    n_high_sch = sum(
      high_sch,
      high_sch_normal,
      na.rm = T
    ),
    prop_meal = mean(
      meal,
      na.rm = T
    ),
    prop_electricity = 1 - mean(
      electricity_inexistent,
      na.rm = T
    ),
    prop_internet = mean(
      internet,
      na.rm = T
    ),
    prop_kitchen = mean(
      kitchen, 
      na.rm = T
    ),
    prop_sewer = 1 - mean(
      sewer_inexistent,
      na.rm = T
    ),
    prop_rural = mean(
      location == "rural",
      na.rm = T
    ),
    n_lab_info = sum(
      lab_info,
      na.rm = T
    ),
    n_library = sum(
      library,
      na.rm = T
    ),
    n_staff = sum(
      staff,
      na.rm = T
    ),
    n_teachers = sum(
      num_tchr,
      na.rm = T
    ),
    n_pta = sum(
      association_parents,
      na.rm = T
    ),
    n_tchr_unions = sum(
      association_tchrs,
      na.rm = T
    )
  ) %>% 
  ungroup()

# fix entries
censo_school_mun <- censo_school_mun %>% 
  mutate(
    n_pta = if_else(year == 2004, n_pta, NA_integer_),
    n_tchr_unions = if_else(year == 2004, n_tchr_unions, NA_integer_)
  )

censo_school_mun %>% 
  fwrite(
    here("data/censo_escolar/censo_school_mun.csv")
  )

# teacher
# note: missing data for the year 2009, in the south
# missing data for rio grande do sul and santa catarina

teacher <- fread(
  here("data/censo_escolar/censo_teacher_sample.csv.gz"),
  nThread = parallel::detectCores() - 1,
  integer64 = "character"
)

teacher_agg <- teacher %>%
  group_by(
    year,
    cod_ibge_6,
    dep,
    school_id
  ) %>% 
  summarise(
    n_teacher = n_distinct(teacher_id),
    n_classes = n_distinct(classroom_id, na.rm = T),
    prop_higher_edu = mean(
      edu_desc == "higher edu complete",
      na.rm = T
    ),
    prop_commute = mean(
      cod_ibge_6 != str_sub(mun_residence, 1, 6),
      na.rm = T
    ),
    prop_female = mean(
      gender == "f",na.rm = T
    ),
    mean_age = mean(
      age, na.rm = T
    ),
    mean_n_class_per_teacher = mean(
      n_classes/n_teacher,
      na.rm = T
    )
  ) %>% 
  ungroup()

# student
censo_class <- fread(
  here("data/censo_escolar/censo_class.csv.gz")
)

censo_class_mun <- censo_class %>% 
  group_by(
    cod_ibge_6,
    year,
    dep
  ) %>% 
  summarise_stats(
    num_enroll
  )

censo_class_mun %>% 
  fwrite(
    here("data/censo_escolar/censo_class_mun.csv")
  )
  
censo_class_dep <- censo_class %>% 
  group_by(
    dep,
    year,
    grade
  ) %>% 
  summarise_stats(
    num_enroll
  )

censo_class_dep %>% 
  fwrite(
    here("data/censo_escolar/censo_class_dep.csv")
  )
  
# teacher turnover
files <- list.files(
  "~/princeton/R/data/censo_escolar/data/wrangle/harmonized/docentes",
  pattern = "turnover"
)

names = files %>% 
  basename %>% 
  str_replace_all(
    "docente_mun",
    "censo_teacher"
  )

file.copy(
  files,
  paste0(here("data/censo_escolar/"), names),
  overwrite = T
)

# aggregate
censo_turnover <- fread(
  here("data/censo_escolar/censo_teacher_turnover.csv.gz")
)

censo_turnover_school <- censo_turnover %>% 
  calc_turnover(
    c("state", "cod_ibge_6", "year", "school_id", "grade_level")
  ) 

censo_turnover_school %>% 
  fwrite(
    here("data/censo_escolar/censo_school_turnover.csv")
  )

gzip(
  here("data/censo_escolar/censo_school_turnover.csv"),
  overwrite = T
)

censo_turnover_mun <- censo_turnover %>% 
  calc_turnover(
    c("state", "cod_ibge_6", "year", "grade_level")
  )

censo_turnover_mun %>% 
  fwrite(
    here("data/censo_escolar/censo_mun_turnover.csv")
  )

gzip(
  here("data/censo_escolar/censo_mun_turnover.csv"),
  overwrite = T
)

# saeb --------------------------------------------------------------------
# by municipality-school
saeb_student_mun <- fread(
  here("data/saeb/saeb_exam_mun.csv")
) %>% 
  add_election()

# weight by percentage of students participatings (based on school census data)
censo_class <- fread(
  here("data/censo_escolar/censo_class_mun.csv")
)

saeb_student_mun <- saeb_student_mun %>% 
  left_join(
    censo_class,
    by = c("cod_ibge_6", "year", "grade")
  ) %>% 
  mutate(
    weight = attendees/num_enroll,
    weight = if_else(weight > 1, 1, weight),
    mean_grade_exam_wgt = weight*mean_grade_exam
  )

saeb_student_mun %>% 
  fwrite(
    here("data/saeb/saeb_exam_mun.csv")
  )

# saeb_sample
saeb_student <- fread(
  here("data/saeb/saeb_student.csv.gz")
)

saeb_student %>% 
  sample_frac(0.25) %>% 
  fwrite(
    here("data/saeb/saeb_student_sample.csv")
  )

gzip(
  here("data/saeb/saeb_student_sample.csv")
)
