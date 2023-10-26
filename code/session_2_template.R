## Programming for Professional Research Using R -- Session 2

  ### Session Description

    # (Continuing from last week) Using Basic Tidyverse Functions – how to subset, mutate, summarize,
    # and reshape data
    # Data Cleaning -- Simple checks and manipulations to clean raw data
    # Introduction to "tidy" datasets and how to get to them

  ### Resources

    # DIME, “Data Cleaning” (https://dimewiki.worldbank.org/Data_Cleaning). Instructions on how to clean
    # raw household data for use in a development setting.

    # Wickham and Grolemund, R for Data Science Chapter 12 – Tidy Data (https://r4ds.had.co.nz/tidy-data.html).
    # How to structure (“tidy”) your dataset for flexible use in data analysis.

  ### Session Objectives

    # Learn how to:
      # Filter, mutate, group, and summarize data using Tidyverse functions
      # Reshape data using Tidyverse functions
      # Check for duplicates and encode missing values
      
    # Be introduced to:
      # "Tidy" datasets and how to create them using pivot_longer() and pivot_wider()

    # Practice the above!

  ## 1. Setup ----
  
    ### Packages
  
# NOTE -- Unlike using library(), the 'pacman::p_load()' function installs the package if it is already
# not present in the user's R environment.
  
  if(!require(pacman)) install.packages("pacman") 
  
  pacman::p_load(tidyverse, data.table, janitor)
  
    ### File Paths
  
      # Set User (this allows us to use fixed file paths but to adapt them for
      # multiple possible users)
  
      # 1 -- Marc-Andrea Fiorina
  
      # 2 -- Enter here if needed
  
  user <- 1
  
  if(user == 1) {
      
      main_filepath <- "/Users/marc-andreafiorina/Dropbox/SAIS R Course/"
      
  }
  
  if(user == 2) {
      
      main_filepath <- "Your own filepath"
      # Modify this to refer to your file path. Don't forget the '/' at the
      # end!
      
  }
  
  data_filepath <- paste0(main_filepath, "data/")
  # Then we can dynamically add file paths that work for everyone!
  
  ## 2. Import Data ----
 
  norms_values_data <- read.csv(
      
      paste0(data_filepath, "session_2/wvs_values_norms_data.csv"),
      
      na.strings = ""
      
  )

  ## 3. SESSION 1 -- Data 'Wrangling' ----
  
# Goal -- Determine what European countries feel is important in their life
# -> create a dataset summarizing each European country's opinion on different
# life subjects
  
    ### Step 1 -- Subset dataset to keep only European countries ----
  
# This dataset doesn't have a "continent" variable, so I will have to create
# one
  
# Check the names of the variables in my dataset
  
  norms_values_data %>%
      
      names()
  
# Check which countries are in my dataset
  
  norms_values_data %>%
      
      janitor::tabyl(B_COUNTRY_ALPHA)
  
# Use ISO codes to distinguish European countries
  
  european_iso_codes <- c( # This creates a character list
      
      "AND", "CYP", "DEU", "GRC", "RUS", "SRB", "TUR", "UKR"
      
  )
  
  norms_values_data <- norms_values_data %>%
      
      dplyr::mutate(
          
          european = dplyr::case_when( # This creates a dummy variable
              
              B_COUNTRY_ALPHA %in% european_iso_codes ~ 1,
              
              TRUE                                    ~ 0
              
          )
          
      )
  
# Check that it worked
  
  norms_values_data %>%
      
      janitor::tabyl(european, B_COUNTRY_ALPHA)
  
# Now subset using filter()
  
  european_data <- norms_values_data %>%
      
      dplyr::filter(european == 1)
  
    ### Step 2 -- Select relevant variables ----
  
# We want to look at what people find important in life. Those are questions
# Q1-Q6.
  
# So we keep those questions, as well as D_INTERVIEW (unique ID, always keep)
# and B_COUNTRY_ALPHA
  
  european_data <- european_data %>%
      
      dplyr::select(
          
          D_INTERVIEW, B_COUNTRY_ALPHA, dplyr::matches("^Q0[1-6]")
          
          # matches() allows us to select multiple variables
          # at once using a common string in their name
          
      )
  
    ### Step 3 -- Clean variables ----
  
# Will explore this further in next week's session -- need to encode answers as
# NA to continue using numerically
  
  european_data <- european_data %>%
      
      dplyr::mutate(
          
          dplyr::across( # Across is magic and allows you to modify multiple
                         # variables at once
              
              matches("^Q0[1-6]"),
              # Same as for select() above. IMPORTANT -- all variables need to
              # be of the same 'type' (e.g. character, numeric)
              
              ~ case_when(
                  # across() basically loops over every relevant variable.
                  # '.x' refers to each variable being treated
                  
                  .x %in% c(-1, -2, -4, -5) ~ NA_integer_,
                  
                  # Key to understand what NA_ to use. Here the possible
                  # answers will be 1, 2, 3, 4 so those are integers
                  
                  TRUE                      ~ .x
                  # We've created NAs and are leaving the other values alone
                  
              )
              
          )
          
      )
  
    ### Step 4 -- Summarize variables at the country level ----
  
# We want a dataset where each observation (row) is a country, not a household.
# To do this, we use group_by() and summarize()
  
  european_country_data <- european_data %>% # New observation level so new
                                             # dataset
      
      dplyr::group_by(B_COUNTRY_ALPHA) %>% # We're telling R at which level to
                                           # do the grouping
      
      dplyr::summarize(
          
          # Summarize aggregated values based on what we instruct it to do. If
          # we didn't use group_by(), it would summarize to one single value.
          # Here, it will output one value per country.
          
          dplyr::across( # Using across() again to summarize multiple variables
                         # at once
              
              matches("^Q0[1-6]"),
              
              ~ mean(.x, na.rm = TRUE) # Removing NAs and taking the mean of
                                       # each variable
              
          )
          
      ) %>%
      
      dplyr::ungroup() # Always remember to do this! Otherwise your future code
                       # will do weird things
  
    ### Step 5 -- Create a question-level dataset ----
  
# The above is useful. But what if we want to sum up countries' values for all
# of these questions, or look at each country's 'average' enthusiasm, it'll be
# easier with a 'long' dataset than a 'wide' one.
  
  european_country_data_wide <- european_country_data %>%
      
      tidyr::pivot_longer(
          
          cols      = matches("^Q0[1-6]"), # Variables whose data we want to be
                                           # in a single, 'long' variable
          
          names_to  = "topic", # Creates a variable named 'topic' that saves
                               # the variable names
          
          values_to = "score" # Creates a 'long' variable named 'score' that
                              # holds all of the original values
          
      ) %>%
      
      dplyr::mutate( # I don't like how 'topic' has more information than
                     # necessary
          
          topic = stringr::str_replace( # stringr is the best package for
                                        # string manipulation
              
              topic,
              
              "^Q0[1-6]_life_",
              
              ""
              
          )
          
      )
  
# Check that it worked
  
  european_country_data_wide %>% janitor::tabyl(topic) # It did!
  
# Now for example I can look at average 'enthusiasm' by country
  
  average_country_enthusiasm <- european_country_data_wide %>%
      
      dplyr::group_by(B_COUNTRY_ALPHA) %>%
      
      dplyr::summarize(
          
          average_score = mean(score, na.rm = TRUE)
          
      ) %>%
      
      dplyr::ungroup() %>%
      
      dplyr::arrange(average_score) # Order them from highest to lowest
                                    # enthusiasm (NOTE -- smaller
                                    # number means more enthusiasm)
  
# check what we created:
  
  average_country_enthusiasm %>% head()
      
  ## 4. SESSION 2 -- Data Cleaning/Tidy Data ----
  
## This will show you some basic tasks that you can do with raw data to remove
## possible errors prior to analysis:
  
  ## Check for duplicate observations -> i.e. rows should be uniquely
  ## identified
  ## Encode missing values -> answers such as "don't know" or "refused to
  ## respond" shouldn't be negative but NA
  ## Check and confirm numerical outliers (not covered here)
  
    ### Step 1 -- Check for duplicate observations ----
  
# In the norms_values_data dataset, the unique identifier for each observation
# is "D_INTERVIEW". It should be the case that each ID is only used once in
# the dataset. To check for duplicates, this is the preferred method:
  
  duplicates <- norms_values_data %>%
      
      group_by(D_INTERVIEW) %>%
      
      # Grouping at the ID level, we'll be able to check how many times it
      # shows up
      
      dplyr::summarize(
          
          num = n()
          
          # n() counts the number of instances of each ID! Just run up to
          # summarize() and add %>% head() to see what it does
          
      ) %>%
      
      filter(num > 1)
  
# 36 duplicates, uh oh. They all seem to be similar in "order" so I'll check in
# which countries they occurred:
  
  duplicates <- duplicates %>%
      
      left_join( # left_join() allows us to 'merge' datasets, in this case
                 # merging norms_values_data into the duplicates dataset to
                 # add countries
          
          norms_values_data %>%
              
              select(D_INTERVIEW, B_COUNTRY_ALPHA) %>%
              # Need to have a common variable (D_INTERVIEW) between
              # norms_values_data and duplicates, and we only want to add
              # country name
              
              distinct() # Need to do this BECAUSE of the duplicate IDs
          
      )
  
# NOTE -- "left_join" means that only the observations from the first (left)
# dataset (based on value of D_INTERVIEW), here "duplicates", are kept. We
# could have used "right_join" if we wanted to only keep the observations from
# the second (right) dataset, here "norms_values_data", "full_join" if we
# wanted to keep both, or "inner_join" if we only wanted to keep observations
# that appear in both datasets
  
# Check countries:
  
  duplicates %>%
      
      janitor::tabyl(B_COUNTRY_ALPHA) # Uh oh, Mongolia!
  
# There is an issue with the Mongolian IDs. Usually we would have to contact
# the data collection team to determine what the problem is. In this case we
# can't do that, so... Just remove all observations from Mongolia because this
# is strange
  
  norms_values_data <- norms_values_data %>%
      
      filter(B_COUNTRY_ALPHA != "MNG")

    ### Step 2 -- Encode missing values ----
  
# Going to focus on the "child" variables here (Q07-Q17). Extract those first:
  
# 'Easy' selection method would be this:
  
  child_data <- norms_values_data %>%
      
      select(
          
          D_INTERVIEW, B_COUNTRY_ALPHA, Q07_child_manners,
          Q08_child_independence, Q09_child_hard_work,
          Q10_child_responsibility, Q11_child_imagination, Q12_child_tolerance,
          Q13_child_thrift, Q14_child_determined, Q15_child_faith,
          Q16_child_unselfish, Q17_child_obedient # Ugh
          
      )
  
  child_data <- norms_values_data %>%
      
      select(
          
          D_INTERVIEW, B_COUNTRY_ALPHA,
          
          matches("^Q(0[7-9]|1[0-7])") # See how much quicker this is?
          
      )
  
# Check it worked:
  
  child_data %>% names()
  
# We explore what possible values these variables can take. For example:
  
  child_data %>%
      
      tabyl(Q07_child_manners)

# These negative values don't seem great. If we look at the codebook, we see
# that they're different versions of not receiving a quantifiable answer (e.g.
# don't know, refused to respond). We need to encode those as NA
  
# Annoying way to do this:
  
  child_data_sucky_mutate <- child_data %>%
      
      mutate(
          
          Q07_child_manners = case_when(
              
              Q07_child_manners < 0 ~ NA_integer_,
              # Notice that they're always negative so this is an easy
              # simplification
              
              TRUE                  ~ Q07_child_manners
              
          ),
          
          Q08_child_independence = case_when(
              
              Q08_child_independence < 0 ~ NA_integer_,
              
              TRUE                   ~ Q08_child_independence
              
          ),
          
          Q09_child_hard_work = case_when(
              
              Q09_child_hard_work < 0 ~ NA_integer_,
              
              TRUE                    ~ Q09_child_hard_work
              
          ),
          
          Q10_child_responsibility = case_when(
              
              Q10_child_responsibility < 0 ~ NA_integer_,
              
              TRUE                     ~ Q10_child_responsibility
              
          ),
          
          Q11_child_imagination = case_when(
              
              Q11_child_imagination < 0 ~ NA_integer_,
              
              TRUE                      ~ Q11_child_imagination
              
          ),
          
          Q12_child_tolerance = case_when(
              
              Q12_child_tolerance < 0 ~ NA_integer_,
              
              TRUE                    ~ Q12_child_tolerance
              
          ),
          
          Q13_child_thrift = case_when(
              
              Q13_child_thrift < 0 ~ NA_integer_,
              
              TRUE                 ~ Q13_child_thrift
              
          ),
          
          Q14_child_determined = case_when(
              
              Q14_child_determined < 0 ~ NA_integer_,
              
              TRUE                     ~ Q14_child_determined
              
          ),
          
          Q15_child_faith = case_when(
              
              Q15_child_faith < 0 ~ NA_integer_,
              
              TRUE                ~ Q15_child_faith
              
          ),
          
          Q16_child_unselfish = case_when(
              
              Q16_child_unselfish < 0 ~ NA_integer_,
              
              TRUE                    ~ Q16_child_unselfish
              
          ),
          
          Q17_child_obedient = case_when(
              
              Q17_child_obedient < 0 ~ NA_integer_,
              
              TRUE                   ~ Q17_child_obedient
              
          )
          
      )
  
# That sucked. Look how much easier the below is:
  
  child_data <- child_data %>%
      
      mutate(
          
          across(
              
              Q07_child_manners:Q17_child_obedient,
              
              ~ case_when(
                  
                  .x < 0 ~ NA_integer_,
                  
                  TRUE   ~ .x
                  
              )
              
          )
          
      ) # Same result!!! You can compare child_data and child_data_sucky_mutate
        # to see that
  
# Note -- in this dataset, '1' is that the subject was mentioned, '2' that the
# subject wasn't. This is akin to 'yes' and 'no' which are traditionally
# encoded respectively as '1' and '0'. So:
  
  child_data <- child_data %>%
      
      mutate(
          
          across(
              
              Q07_child_manners:Q17_child_obedient,
              
              ~ case_when(
                  
                  .x == 2 ~ 0,
                  
                  TRUE    ~ as.numeric(.x) # Note that we need to change class
                                           # from integer to numeric because
                                           # of the '0'
                  
              )
              
          )
          
      )
  
    ### Step 3 -- Creating a 'tidy' version of this ----
  
# We want a dataset that doesn't have:
  
  # One variable spread across multiple columns, or
  # One observation spread across multiple rows
  
# In this case, it seems that the answers to the question of what is important
# to teach to a child is spread across multiple variables, one for each answer
# basically. This would make it difficult to e.g. check what the
# most popular 5 answers are. Solution: pivot_longer
  
  child_data_long <- child_data %>%
  # Note -- good to create a new dataset when modifying the level of
  # observation
      
      pivot_longer(
          
          cols         = Q07_child_manners:Q17_child_obedient,
          
          names_to     = "child_quality",
          
          names_prefix = "Q[0-9]{2}_child_", # Allows us to only keep what
                                             # follows the variable prefix
          
          values_to    = "child_quality_value" # For now
          
      )
  
# Check what it looks like:
  
  child_data_long %>% head()
  
# Small changes to look nicer
  
  child_data_long <- child_data_long %>%
      
      mutate(
          
          child_quality = str_replace_all(child_quality, "_", " "),
          
          child_quality = str_to_title(child_quality)
          
      )
  
  child_data_long %>%
      
      tabyl(child_quality) # Looks good!
  
# Challenge: Try to determine what the five most popular answers to this
# question were. Hint: "arrange(-var)" orders a dataset from the largest
# value of 'var' to the small value of 'var'.
  
  child_data_long %>%
      
      group_by(child_quality) %>%
      
      dplyr::summarize(
          
          child_quality_value = sum(child_quality_value, na.rm = TRUE)
          
      ) %>%
      
      arrange(-child_quality_value) %>%
      
      head(n = 5)
  