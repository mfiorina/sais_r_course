## World Values Survey Instruction Dataset Construction

### Purpose -- extract relevant modules from raw data for instruction

  ## 1. Setup ----
  
    ### Packages
  
  if(!require(pacman)) install.packages("pacman")
  
  pacman::p_load(tidyverse, data.table, janitor)
  
    ### File Paths
  
# Set User
  
  # 1 -- Marc-Andrea Fiorina
  
  # 2 -- Enter here if needed
  
  user <- 1
  
  if(user == 1) {
      
      main_filepath <- "/Users/marc-andreafiorina/Dropbox/SAIS R Course/"
      
  }
  
  data_filepath <- paste0(main_filepath, "data/")

  ## 2. Import Data ----
  
  raw_data <- fread(
      
      paste0(data_filepath, "raw/wvs_data_2021.csv"), na.strings = ""
      
  )
  
  ## 3. Construct Dataset ----
  
# Session 1 -- Use Module on "Social Values, Norms, Stereotypes (Q1-Q45)"
  
  values_norms_data <- raw_data %>%
      
      select(
          
          D_INTERVIEW, matches("B_COUNTRY"), A_YEAR, J_INTDATE,
          
          matches("^N_"), matches("^G_"), matches("^H_"), matches("^Q[1-9]$"), matches("^Q[1-3][0-9](_3){0,1}$"),
          
          matches("^Q4[0-5]$")
          
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
  
  ## 4. Data Export ----
  
  fwrite(
      
      values_norms_data,
      
      paste0(data_filepath, "session_1/wvs_values_norms_data.csv"), row.names = FALSE, na = ""
      
  )
  
  fwrite(
      
      values_norms_data,
      
      paste0(data_filepath, "session_2/wvs_values_norms_data.csv"), row.names = FALSE, na = ""
      
  )
  
  fwrite(
      
      values_norms_data,
      
      paste0(data_filepath, "session_3/wvs_values_norms_data.csv"), row.names = FALSE, na = ""
      
  )
  
    