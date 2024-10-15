## World Values Survey Instruction Dataset Construction

### Purpose -- extract relevant modules from raw data for instruction

  ## 1. Setup ----
  
    ### Packages
  
  if(!require(pacman)) install.packages("pacman")
  
  pacman::p_load(tidyverse, data.table, janitor)

  ## 2. Import Data ----
  
  raw_data <- fread(
      "data/raw/wvs_data_2021.csv", na.strings = ""
  )
  
  country_continent_data <- data.table::fread(
      "data/raw/country_continent.csv", na.strings = ""
  )
  
  ## 3. Construct Dataset ----
  
# Session 1 -- Use Module on "Social Values, Norms, Stereotypes (Q1-Q45)"
  
  norms_values_data <- raw_data %>%
      select(
          D_INTERVIEW, matches("B_COUNTRY"), A_YEAR, J_INTDATE,
          matches("^N_"), matches("^G_"), matches("^H_"), matches("^Q[1-9]$"),
          matches("^Q[1-3][0-9](_3){0,1}$"), matches("^Q4[0-5]$")
      ) %>%
      rename(
          Q01_life_family                  = Q1,
          Q02_life_friends                 = Q2,
          Q03_life_leisure                 = Q3,
          Q04_life_politics                = Q4,
          Q05_life_work                    = Q5,
          Q06_life_religion                = Q6,
          Q07_child_manners                = Q7,
          Q08_child_independence           = Q8,
          Q09_child_hard_work              = Q9,
          Q10_child_responsibility         = Q10,
          Q11_child_imagination            = Q11,
          Q12_child_tolerance              = Q12,
          Q13_child_thrift                 = Q13,
          Q14_child_determined             = Q14,
          Q15_child_faith                  = Q15,
          Q16_child_unselfish              = Q16,
          Q17_child_obedient               = Q17,
          Q18_neighbor_drugs               = Q18,
          Q19_neigbor_race                 = Q19,
          Q20_neighbor_aids                = Q20,
          Q21_neighbor_foreign             = Q21,
          Q22_neighbor_homosexual          = Q22,
          Q23_neighbor_religion            = Q23,
          Q24_neighbor_alcohol             = Q24,
          Q25_neighbor_unmarried           = Q25,
          Q26_neighbor_language            = Q26,
          Q27_agree_parents_proud          = Q27,
          Q28_agree_mom_work               = Q28,
          Q29_agree_men_politics           = Q29,
          Q30_agree_boy_university         = Q30,
          Q31_agree_men_business           = Q31,
          Q32_agree_women_housewife        = Q32,
          Q33_agree_men_jobs               = Q33,
          Q33_3_men_jobs                   = Q33_3,
          Q34_agree_nation_jobs            = Q34,
          Q34_3_agree_nation_jobs          = Q34_3,
          Q35_agree_women_income_problem   = Q35,
          Q35_3_agree_women_income_problem = Q35_3,
          Q36_agree_homosexual_parents     = Q36,
          Q37_agree_procreate_duty         = Q37,
          Q38_agree_old_parent_duty        = Q38,
          Q39_agree_lazy_unemployed        = Q39,
          Q40_agree_work_duty              = Q40,
          Q41_agree_work_first             = Q41,
          Q42_society_attitudes            = Q42,
          Q43_future_changes               = Q43,
          Q44_technology                   = Q44,
          Q45_authority                    = Q45
      )
  
  ## 4. Construct Secondary Datasets ----
  
    ### Session 2 — Data Visualization ----
  
      #### Politics/Religion Comparison Dataset (Plot) ----
  
  politics_religion_plot_data <- norms_values_data %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA,
          Q04_life_politics, Q06_life_religion
      ) %>%
      mutate(
          across(
              Q04_life_politics:Q06_life_religion,
              ~ case_when(
                  .x < 0 ~ NA_real_,
                  .x == 4 ~ 1, # 4 is 'strongly disagree' and 1 is 'strongly agree'.
                  # I like bigger = better
                  .x == 3 ~ 2,
                  .x == 2 ~ 3,
                  .x == 1 ~ 4
              )
          )
      ) %>%
      # We want to visualize the relationship between the politics and religion variables, but there
      # are 83,000 observations (too many). So aggregate at the country level
      group_by(B_COUNTRY_ALPHA) %>%
      dplyr::summarize(
          across(
              Q04_life_politics:Q06_life_religion,
              ~ mean(.x, na.rm = TRUE)
          )
      ) %>%
      ungroup() %>%
      # Add continent data to the politics/religion dataset to compare continent statistics
      left_join(
          country_continent_data,
          by = c("B_COUNTRY_ALPHA" = "country")
      ) %>%
      select(
          country_long, continent, everything()
      ) %>%
      arrange(continent, country_long)
  
      #### Politics/Religion Comparison Dataset (Plot) (Long) ----
  
  politics_religion_plot_data_long <- politics_religion_plot_data %>%
      pivot_longer(
          cols = c(Q04_life_politics, Q06_life_religion),
          names_to  = "variable",
          values_to = "life_importance"
      ) %>%
      mutate( # So that it looks good in the plot
          variable = case_when(
              variable == "Q04_life_politics" ~ "Politics",
              variable == "Q06_life_religion" ~ "Religion"
          )
      )
  
      ### Politics/Religion Comparison Dataset by Continent (Plot) (Long)
  
  politics_religion_plot_data_long_continent <- politics_religion_plot_data_long %>%
      mutate( # South and North America are too long strings
          continent = case_when(
              continent == "South America" ~ "South\nAmerica", # Line break
              continent == "North America" ~ "North\nAmerica",
              TRUE                         ~ continent
          )
      ) %>%
      group_by(continent, variable) %>%
      dplyr::summarize(
          life_importance = mean(life_importance, na.rm = TRUE)
      ) %>%
      ungroup()
  
      #### Politics/Religion Comparison Dataset (Table) ----
  
  politics_religion_table_data <- politics_religion_plot_data %>%
      select(-B_COUNTRY_ALPHA) %>% # Don't need it
      mutate( # Too many digits in our numeric variables
          across(
              Q04_life_politics:Q06_life_religion,
              ~ round(.x, digits = 3)
          )
      )
  
      #### Parent/Child Values Dataset ----
  
  parent_child_data <- norms_values_data %>%
      mutate(
          across(
              c(Q07_child_manners:Q17_child_obedient, Q27_agree_parents_proud),
              ~ case_when(
                  .x < 0 ~ NA_real_,
                  .x == 4 ~ 1, # 4 is 'strongly disagree' and 1 is 'strongly agree'.
                  # I like bigger = better
                  .x == 3 ~ 2,
                  .x == 2 ~ 3,
                  .x == 1 ~ 4
              )
          )
      ) %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA,
          Q07_child_manners:Q17_child_obedient, Q27_agree_parents_proud
      )
  
    #### Child Values Country Dataset ----
  
  child_data_country <- norms_values_data %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA,
          matches("^Q(0[7-9]|1[0-7])") # See how much quicker this is?
      ) %>%
      mutate(
          across(
              Q07_child_manners:Q17_child_obedient,
              ~ case_when(
                  .x < 0  ~ NA_integer_,
                  .x == 2 ~ 0,
                  TRUE    ~ .x
              )
          )
      ) %>%
      group_by(B_COUNTRY_ALPHA) %>%
      dplyr::summarize(
          across(
              Q07_child_manners:Q17_child_obedient,
              ~ mean(.x, na.rm = TRUE)
          )
      ) %>%
      ungroup() %>%
      left_join(
          country_continent_data,
          by = c("B_COUNTRY_ALPHA" = "country")
      ) %>%
      select(
          country_long, continent, everything()
      ) %>%
      arrange(continent, country_long)
  
    ### Child Values Continent Dataset ----
  
  child_data_continent <- norms_values_data %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA,
          matches("^Q(0[7-9]|1[0-7])") # See how much quicker this is?
      ) %>%
      left_join(
          country_continent_data,
          by = c("B_COUNTRY_ALPHA" = "country")
      ) %>%
      mutate(
          across(
              Q07_child_manners:Q17_child_obedient,
              ~ case_when(
                  .x < 0  ~ NA_integer_,
                  .x == 2 ~ 0,
                  TRUE    ~ .x
              )
          )
      ) %>%
      group_by(continent) %>%
      dplyr::summarize(
          across(
              Q07_child_manners:Q17_child_obedient,
              ~ mean(.x, na.rm = TRUE)
          )
      ) %>%
      ungroup() %>%
      select(
          continent, everything()
      ) %>%
      arrange(continent)
  
    ### Year-by-Year Norms/Values Dataset ----
  
  yearly_norms_values_data <- norms_values_data %>%
      group_by(A_YEAR) %>%
      group_split()
  
  ## 5. Data Export ----
  
  fwrite(
      norms_values_data,
      "data/final/wvs_norms_values_data.csv", row.names = FALSE, na = ""
  )
  
  yearly_norms_values_data %>%
      map2(
          norms_values_data %>% select(A_YEAR) %>% distinct() %>% pull(),
          ~ write.csv(
              .x,
              paste0("data/final/wvs_norms_values_data_", .y, ".csv"),
              row.names = FALSE, na = ""
          )
      )
  
  fwrite(
      politics_religion_plot_data,
      "data/final/politics_religion_plot.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      politics_religion_plot_data_long,
      "data/final/politics_religion_plot_long.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      politics_religion_plot_data_long_continent,
      "data/final/politics_religion_plot_long_continent.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      politics_religion_table_data,
      "data/final/politics_religion_table.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      parent_child_data,
      "data/final/parent_child.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      child_data_country,
      "data/final/child_values_country.csv", row.names = FALSE, na = ""
  )
  
  fwrite(
      child_data_continent,
      "data/final/child_values_continent.csv", row.names = FALSE, na = ""
  )
  