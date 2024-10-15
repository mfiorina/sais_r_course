## R MIEF Skills Workshop — Session 3

  ### Session Description

    # Basic R Use – Introduction to R and RStudio, How to import/export data
    # Using Basic Tidyverse Functions – how to subset, mutate, summarize, and reshape data

  ### Resources

    # Dominic Royé, “A very short introduction to Tidyverse”
    # (https://dominicroye.github.io/en/2020/a-very-short-introduction-totidyverse/). Blog post
    # covering the basics of Tidyverse use in R.

    # tidyr, “Pivoting” (https://tidyr.tidyverse.org/articles/pivot.html). Vignette explaining how
    # to reshape datasets using pivot_longer and pivot_longer.

  ### Session Objectives

    # Learn how to:
      # Filter, mutate, group, and summarize data using Tidyverse functions
      # Reshape data using Tidyverse functions

    # Be introduced to:
      # The concept of a "tidy" dataset

  ## 1. Setup ----
  
      ### Packages ----
  
  # NOTE — Unlike using library(), the 'pacman::p_load()' function installs the package if it is
  # already not present in the user's R environment.
  
  if(!require(pacman)) install.packages("pacman")
  
  pacman::p_load(dplyr, tidyr, stringr, ggplot2, purrr, data.table, janitor)
  
  ## 2. Import Data ----
  
  norms_values_raw <- read.csv( # Other options are base R's read.csv() and data.table::fread()
      "data/final/wvs_norms_values_data.csv", na.strings = ""
  )
  
  ## 3. Data Wrangling ----
  
# Goal — Determine what European countries feel is important in their life -> create a dataset
# summarizing each European country's opinion on different life subjects
  
    ### Step 1 — Subset dataset to keep only European countries ----
  
# This dataset doesn't have a "continent" variable, so I will have to create one
  
# Check the names of the variables in my dataset
  
  norms_values_raw %>%
      names()
  
# Check which countries are in my dataset
  
  norms_values_raw %>%
      tabyl(B_COUNTRY_ALPHA)
  
# Use ISO codes to distinguish European countries
  
  european_iso_codes <- c( # This creates a character list
      "AND", "CYP", "DEU", "GRC", "RUS", "SRB", "TUR", "UKR"
  )
  
  norms_values_data <- norms_values_raw %>%
      mutate(
          european = case_when( # This creates a dummy variable
              B_COUNTRY_ALPHA %in% european_iso_codes ~ 1,
              TRUE                                    ~ 0
          )
      )
  
# Check that it worked
  
  norms_values_data %>%
      tabyl(european, B_COUNTRY_ALPHA)
  
# Now subset using filter()
  
  european_data <- norms_values_data %>%
      filter(european == 1)
  
  ### Step 2 — Select relevant variables ----
  
# We want to look at what people find important in life. Those are questions Q1-Q6.
  
# So we keep those questions, as well as D_INTERVIEW (unique ID, always keep) and B_COUNTRY_ALPHA
  
  european_data <- european_data %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA, matches("^Q0[1-6]")
      )
    # matches() allows us to select multiple variables at once using a common string in
    # their name. The string within matches uses a regular expression. For more about
    # regular expressions, see:
    # https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
  
  ### Step 3 — Clean variables ----
  
# Will explore this further in next week's session — need to encode answers as "NA" to continue
# using numerically
  
  european_data <- european_data %>%
      mutate(
          Q01_life_family = case_when(
              Q01_life_family %in% c(-1, -2, -4, -5) ~ NA_integer_, # Key to understand what NA_ to use.
                                                                    # Here the possible values are 1, 2, 3, 4
                                                                    # so those are integers
              TRUE                                   ~ Q01_life_family
          ),
          Q02_life_friends = case_when(
              Q02_life_friends %in% c(-1, -2, -4, -5) ~ NA_integer_,
              TRUE                                    ~ Q02_life_friends
          ),
          Q03_life_leisure = case_when(
              Q03_life_leisure %in% c(-1, -2, -4, -5) ~ NA_integer_,
              TRUE                                    ~ Q03_life_leisure
          ),
          Q04_life_politics = case_when(
              Q04_life_politics %in% c(-1, -2, -4, -5) ~ NA_integer_,
              TRUE                                     ~ Q04_life_politics
          ),
          Q05_life_work = case_when(
              Q05_life_work %in% c(-1, -2, -4, -5) ~ NA_integer_,
              TRUE                                 ~ Q05_life_work
          ),
          Q06_life_religion = case_when(
              Q06_life_religion %in% c(-1, -2, -4, -5) ~ NA_integer_,
              TRUE                                     ~ Q06_life_religion
          )
      )
# NOTE — The above is super long and annoying code, requiring a lot of copy/pasting.
# Next week, we will look at how to write this code much more efficiently using the across() function.
  
  ### Step 4 — Summarize variables at the country level ----
  
  # We want a dataset where each observation (row) is a country, not a household. To do this, we use
  # group_by() and summarize()
  
  european_country_data <- european_data %>% # New observation level so new dataset
      group_by(B_COUNTRY_ALPHA) %>% # We're telling R at which level to do the grouping
      summarize( # Summarize aggregated values based on what we instruct it to do. If we
                 # didn't use group_by(), it would summarize to one single value. Here,
                 # it will output one value per country.
          Q01_life_family   = mean(Q01_life_family,   na.rm = TRUE),
          Q02_life_friends  = mean(Q02_life_friends,  na.rm = TRUE),
          Q03_life_leisure  = mean(Q03_life_leisure,  na.rm = TRUE),
          Q04_life_politics = mean(Q04_life_politics, na.rm = TRUE),
          Q05_life_work     = mean(Q05_life_work,     na.rm = TRUE),
          Q06_life_religion = mean(Q06_life_religion, na.rm = TRUE)
      ) %>%
      ungroup() # Always remember to do this! Otherwise your future code will do weird
                # things
  
  ### Step 5 — Create a question-level dataset ----
  
  # The above is useful. But what if we want to sum up countries' values for all of these questions,
  # or look at each country's 'average' enthusiasm, it'll be easier with a 'long' dataset than a
  # 'wide' one.
  
  european_country_data_long <- european_country_data %>%
      pivot_longer(
          cols      = matches("^Q0[1-6]"), # Variables whose data we want to be in a single,
          # 'long' variable
          names_to  = "topic", # Creates a variable named 'topic' that saves the variable names
          values_to = "score" # Creates a 'long' variable named 'score' that holds all of the
          # original values
      ) %>%
      mutate( # I don't like how 'topic' has more information than necessary
          topic = str_replace( # stringr is the best package for string manipulation
              topic,
              "^Q0[1-6]_life_",
              ""
          )
      )
  
  # Check that it worked
  
  european_country_data_long %>% tabyl(topic) # It did!
  
  # Now for example I can look at average 'enthusiasm' by country
  
  european_average_country_enthusiasm <- european_country_data_long %>%
      group_by(B_COUNTRY_ALPHA) %>%
      summarize(
          average_score = mean(score, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(average_score) # Order them from highest to lowest enthusiasm (NOTE —
                             # smaller number means more enthusiasm)
  
  # check what we created:
  
  european_average_country_enthusiasm %>% head()
  
  ## 4. Export Data ----
  
  write.csv(
      european_country_data, "data/final/european_country_data.csv",
      row.names = FALSE, na = ""
  ) # row.names = FALSE ensures that the first column of the dataset isn't the row number in the .csv file,
    # and na = "" ensures that NA (missing) values are converted to empty cells ("") in the .csv file
  
  write.csv(
      european_average_country_enthusiasm, "data/final/eu_enthusiasm.csv",
      row.names = FALSE, na = ""
  )
  
  ## 5. Challenge ----
  
  # Create your own script and do the following:
      
    # 1. Find mean values for 'importance in life' variables (Q1-6) for countries in another region
    #    than Europe
  
    # 2. Calculate average 'enthusiasm' for these life subjects in countries in that non-Europe region
  
    # 3. Perform the same analysis, either on European countries or other countries, for one of
    #    the following group of indicators in the dataset:
           # Important child qualities: Q7-18
           # Neighbors: Q19-26
           # Statements to agree with: Q27-41
  
    # 4. Save one dataset for each of the tasks above.
  