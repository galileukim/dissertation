# functions ---------------------------------------------------------------
# import data
fread <- partial(
  data.table::fread,
  nThread = parallel::detectCores(),
  integer64 = c("character")
)

list.files <- partial(
  base::list.files,
  full.names = T
)

list_files <- function(path, pattern){
  files <- map2(
    path,
    pattern,
    list.files
  ) %>% 
    flatten_chr
  
  return(files)
}

import_data <- function(path, pattern, names = F){
  files <- list_files(
    path,
    pattern
  )
  
  if(names == F){
    names <- basename(files) %>% 
      str_remove_all(
        "\\.csv|\\.gz"
      )
  }
  
  data <- map(
    files,
    fread
  )
  
  names(data) <- names
  
  return(data)
}

assign_data <- function(data, rm = T){
  walk2(
    names(data),
    data,
    assign,
    envir = globalenv()
  )
}

append_db <- function(path, pattern, conn, names = F){
  data <- import(
    files,
    names
  )
  
  pwalk(
    list(
      name = names,
      value = data
    ),
    RSQLite::dbWriteTable,
    conn = con,
    overwrite = T
  )
}

theme_clean <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "grey75"),
  legend.position = "bottom",
  text = element_text(
    # family = "Roboto",
    color = "grey60",
    size = 9
  ),
  axis.text = element_text(color = "grey60", size = 7),
  axis.title = element_text(size = 8),
  plot.margin = unit(rep(0.25, 4), "cm")
)

theme_set(
  theme_minimal() +
    theme_clean
)

# data manipulation -------------------------------------------------------s
# sample groups
sample_group <- function(data, n, ...){
  grouping_vars <- enquos(...)
  
  data %<>%
    nest(
      -c(!!!grouping_vars)
    ) %>% 
    sample_n(n) %>% 
    unnest
  
  return(data)
}

calc_turnover <- function(data, group_vars){
  years <- data %>% 
    distinct(year) %>% 
    pull
  
  data %<>%
    group_by_at(
      vars(
        group_vars
      )
    ) %>% 
    summarise_at(
      vars(starts_with("turnover_"), n),
      sum,
      na.rm = T
    ) %>% 
    ungroup()
  
  data %<>% 
    mutate(
      implicit = 0
    ) %>% 
    complete(
      year = years,
      fill = list(implicit = 1)
    ) %>% 
    group_by_at(
      vars(
        group_vars,
        -year
      )
    ) %>% 
    mutate(
      n_lag = dplyr::lag(n, order_by = year),
      percent_exit = turnover_exit/n_lag,
      percent_transfer = turnover_transfer/n_lag,
      percent_extinct = turnover_extinct/n_lag,
      percent_entry = turnover_entry/n,
      turnover_index = (turnover_exit + turnover_transfer + turnover_entry)/(n + n_lag)
    ) %>% 
    ungroup() %>% 
    filter(
      implicit != 1
    ) %>% 
    select(
      -implicit
    )
  
  return(data)
}

# tidy and ggcoef
tidycoef <- function(fit, vars = "."){
  tidy(fit) %>%
    filter(
      !str_detect(term, "Intercept|as.factor|Observation.Residual"),
      str_detect(term, vars)
    ) %>%
    mutate(
      conf.low = estimate - 1.96*std.error,
      conf.high = estimate + 1.96*std.error
    ) %>%
    ggcoef(
      vline_color = "grey85",
      vline_linetype = "dotted",
      color = "steelblue3",
      sort = "ascending",
      errorbar_color = "steelblue3"
    ) +
    xlab("") +
    theme_clean
}

# add mandate year vertical lines
mandate_year <- function(years = seq(2005, 2013, 4)){
  geom_vline(
    xintercept = years,
    linetype = "dotted",
    color = "grey65"
  )
}

# summarize props with c.i.
summarise_prop <- function(data, var){
  summarise(
    .data = data,
    prop = mean(get(var), na.rm = T),
    n = n(),
    se = sqrt(prop*(1 - prop)/n),
    upper = prop + 1.96*se,
    lower = prop - 1.96*se
  )
}

# tailored summarise
summarise_stats <- function(data, ...){
  vars <- enquos(...)
  
  data %<>% 
    summarise_at(
      .vars = vars(!!!vars),
      .funs = list(
        ~sum(., na.rm = T),
        ~mean(., na.rm = T),
        ~median(., na.rm = T),
        ~sd(., na.rm = T),
        ~n()
      )
    ) %>% 
    ungroup()
  
  return(data)
}

# plot municipal map
plot_map <- function(
  data, fill, breaks = 6, title = "", label = "", palette = "RdYlBu", legend_position = "bottom",
  limits = NULL
) {
  plot <- ggplot() +
    geom_polygon(
      data = data, 
      aes(
        x = long, 
        y = lat, 
        group = group, 
        fill = get(fill)
      ),
      color = NA
    ) +
    {if(is.factor(data[[fill]])) 
      scale_fill_manual(
        values = rev(
          brewer.pal(n = length(levels(data[[fill]])), name = palette)
        ),
        breaks = levels(data[[fill]]),
        na.value = "gray50"
      )else
        scale_fill_distiller(
          palette = palette,
          breaks = pretty_breaks(breaks),
          direction = -1,
          na.value = "gray50",
          limits = limits
        )
    } +
    guides(fill = guide_legend(reverse = T)) +
    labs(fill = label) +
    theme_void(base_size = 17) +
    xlim(range(data$long)) + ylim(range(data$lat)) +
    coord_quickmap() +
    theme(
      legend.position = legend_position,
      legend.text = element_text(size = 18),
      text = element_text(size = 18),
      legend.title = element_blank(),
      plot.caption = element_text(size = 8)
    ) +
    ggtitle(title)
  
  return(plot)
}

# summarize by mun
summarise_fun <- function(data, var){
  data %>%
    group_by(
      region,
      state,
      mun_id,
      year
    ) %>%
    mutate(
      private_var = ifelse(
        public == 0,
        get(var),
        NA
      )
    ) %>%
    summarise(
      public = mean(
        ifelse(
          public == 1,
          get(var),
          NA
        ),
        na.rm = T
      ),
      private = mean(
        private_var,
        na.rm = T
      )
    )
}

# ols
fit_felm <- function(
  repo,
  dv,
  predictor = c("coalition_share"),
  control = c("mayor_age", "as.factor(mayor_party)", "mayor_coalition_size", "mayor_campaign", "median_wage", "rais_mun_size", "rais_permanent", "mean_edu", "effective_parties"),
  cluster = c("state + year"),
  data
){
  repo <- list()
  
  repo[["ols_basic"]] <- data %>%
    felm(
      formula = formula(
        paste(
          dv, "~", predictor,
          "|", cluster, "| 0"
        ),
        data = .
      )
    )
  
  repo[["ols_full"]] <- data %>%
    felm(
      formula = formula(
        paste(
          dv, "~", predictor, "+", str_c(control, collapse = "+"),
          "|", cluster, "| 0"
        )
      ),
      data = .
    )
  
  return(repo)
}

# logit
fit_logit <- function(
  repo,
  dv,
  predictor = c("coalition_share"),
  control = c("rais_edu", "rais_wage", "rais_permanent", "censo_log_pop", "mayor_reelected", "censo_median_wage", "mayor_campaign", "mayor_coalition_size", "as.factor(year)", "as.factor(state)"),
  data
){
  repo <- list()
  
  # repo[["logit_basic"]] <- data %>%
  #   glm(
  #     formula = formula(
  #       paste(dv, "~", predictor)
  #     ),
  #     family = binomial(),
  #     data = .
  #   )
  # 
  repo[["logit_full"]] <- data %>%
    glm(
      formula = formula(
        paste(dv, "~", predictor, "+", str_c(control, collapse = "+"))
      ),
      family = binomial(),
      data = .
    )
  
  return(repo)
}

# election years
add_election <- function(data){
  data %>% 
    mutate(
      election_year = case_when(
        between(year, 2001, 2004) ~ 2000,
        between(year, 2005, 2008) ~ 2004,
        between(year, 2009, 2012) ~ 2008,
        between(year, 2013, 2016) ~ 2012
      )
    )
}

# covariates
join_covariate <- function(data){
  data %>% 
    left_join(
      fread(
        here("data/finbra/finbra.csv")
      ),
      by = c("cod_ibge_6", "year")
    ) %>% 
    left_join(
      fread(
        here("data/censo_br/censo_2000.csv")
      ),
      by = c("cod_ibge_6")
    ) %>% 
    add_election() %>% 
    left_join(
      fread(
        here("data/tse/election.csv")
      ),
      by = c("cod_ibge_6", "election_year")
    )
}

## ggplot aux
gg_point_smooth <- function(data, mapping = aes(),...){
  ggplot(
    data = data,
    mapping
  ) +
    geom_point(
      alpha = 0.25
    ) +
    geom_smooth(
      method = "lm",
      col = "coral3"
    ) 
}

# missingness
gg_miss_var <- partial(
  naniar::gg_miss_var,
  show_pct = T
)

# hex
gg_hex <- function(data, mapping = aes(), n_bin = 30, ...){
  ggplot(
    data = data,
    mapping
  ) +
    geom_hex(
      bins = n_bin,
      ...
    ) +
    scale_fill_distiller(
      palette = "RdYlBu",
      direction = -1
    )
}
  
# histogram
gg_histogram <- function(data, mapping = aes(), ...){
  ggplot(
    data = data,
    mapping
  ) +
    geom_histogram(
      col = "#1C52A2",
      fill = "steelblue3",
      ...
    )
}

gg_point <- function(data, mapping = aes(), ...){
  ggplot(
    data = data,
    mapping
  ) +
    geom_point(...)
}

# change default ggplot settings
scale_colour_discrete <- function(...) scale_color_brewer(palette="Set2")
scale_fill_discrete <- function(...) scale_fill_brewer(palette="Set2")

update_geom_defaults(
  "point",
  list(color = "#375b7c", size = 1.5)
)

update_geom_defaults(
  "line",
  list(color = "steelblue3", size = 1)
)

# reset
rm(
  list = setdiff(ls(), c(lsf.str(),"theme_clean"))
)