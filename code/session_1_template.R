## Programming for Professional Research Using R -- Session 1

  ### Session Description

    # Basic R Use – Introduction to R and RStudio, How to import/export data
    # Using Basic Tidyverse Functions – how to subset, mutate, summarize, and reshape data
    # Using R in a Collaborative Setting – Introduction to downloading and publishing data using
    # GitHub

  ### Resources

    # For those who are fully new to R, I strongly recommend: Thomas Mock, “A Gentle Introduction
    # to Tidy Statistics in R” – blog post
    # (https://themockup.blog/posts/2018-12-10-a-gentle-guide-to-tidy-statistics-inr/) and video
    # (https://www.rstudio.com/resources/webinars/a-gentleintroduction-to-tidy-statistics-in-r/).
   # We won’t have much time to review the basics of R programming during the first session.

    # Dominic Royé, “A very short introduction to Tidyverse”
    # (https://dominicroye.github.io/en/2020/a-very-short-introduction-totidyverse/). Blog post
    # covering the basics of Tidyverse use in R.

    # tidyr, “Pivoting” (https://tidyr.tidyverse.org/articles/pivot.html). Vignette explaining how
    # to reshape datasets using pivot_longer and pivot_wider.

    # Hadley Wickham, “dplyr 1.0.0: working across columns”
    # (https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-colwise/). Explains the basics for
    # flexible column-wise operations using across in R.

  ### Session Objectives

    # Learn how to:
      # Import data in an efficient and reproducible manner
      # Filter, mutate, group, and summarize data using Tidyverse functions
      # Reshape data using Tidyverse functions
      
    # Be introduced to:
      # Code and data collaboration -- GitHub and data project reproducibility

    # Practice the above!

  ## 1. Setup ----
  
    ### Packages
  
# NOTE -- Unlike using library(), the 'pacman::p_load()' function installs the package if it is
# already not present in the user's R environment.
  
  if(!require(pacman)) install.packages("pacman") 
  
  pacman::p_load(tidyverse, data.table, janitor)
  
  ## 2. Import Data ----
 
  norms_values_data <- data.table::fread( # Other options are base R's read.csv() and data.table::fread()
      "data/final/wvs_values_norms_data.csv", na.strings = ""
  )

  ## 3. Data 'Wrangling' ----
  
# Goal -- Determine what European countries feel is important in their life -> create a dataset
# summarizing each European country's opinion on different life subjects
  
    ### Step 1 -- Subset dataset to keep only European countries ----
  
# This dataset doesn't have a "continent" variable, so I will have to create one
  
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
  
# We want to look at what people find important in life. Those are questions Q1-Q6.
  
# So we keep those questions, as well as D_INTERVIEW (unique ID, always keep) and B_COUNTRY_ALPHA
  
  european_data <- european_data %>%
      dplyr::select(
          D_INTERVIEW, B_COUNTRY_ALPHA, dplyr::matches("^Q0[1-6]")
          # matches() allows us to select multiple variables at once using a common string in
          # their name
      )
  
    ### Step 3 -- Clean variables ----
  
# Will explore this further in next week's session -- need to encode answers as "NA" to continue
# using numerically
  
  european_data <- european_data %>%
      dplyr::mutate(
          dplyr::across( # Across is magic and allows you to modify multiple variables at once
              matches("^Q0[1-6]"), # Same as for select() above. IMPORTANT -- all variables need
                                   # to be of the same 'type' (e.g. character, numeric)
              ~ case_when( # across() basically loops over every relevant variable. '.x' refers to
                           # each variable being treated
                  .x %in% c(-1, -2, -4, -5) ~ NA_integer_,
                  # Key to understand what NA_ to use.Here the possible answers will be 1, 2, 3,
                  # 4 so those are integers
                  TRUE                      ~ .x # We've created NAs and are leaving the other
                                                 # values alone
              )
          )
      )
  
    ### Step 4 -- Summarize variables at the country level ----
  
# We want a dataset where each observation (row) is a country, not a household. To do this, we use
# group_by() and summarize()
  
  european_country_data <- european_data %>% # New observation level so new dataset
      dplyr::group_by(B_COUNTRY_ALPHA) %>% # We're telling R at which level to do the grouping
      dplyr::summarize( # Summarize aggregated values based on what we instruct it to do. If we
                        # didn't use group_by(), it would summarize to one single value. Here,
                        # it will output one value per country.
          dplyr::across( # Using across() again to summarize multiple variables at once
              matches("^Q0[1-6]"),
              ~ mean(.x, na.rm = TRUE) # Removing NAs and taking the mean of each variable
          )
      ) %>%
      dplyr::ungroup() # Always remember to do this! Otherwise your future code will do weird
                       # things
  
    ### Step 5 -- Create a question-level dataset ----
  
# The above is useful. But what if we want to sum up countries' values for all of these questions,
# or look at each country's 'average' enthusiasm, it'll be easier with a 'long' dataset than a
# 'wide' one.
  
  european_country_data_wide <- european_country_data %>%
      tidyr::pivot_longer(
          cols      = matches("^Q0[1-6]"), # Variables whose data we want to be in a single,
                                           # 'long' variable
          names_to  = "topic", # Creates a variable named 'topic' that saves the variable names
          values_to = "score" # Creates a 'long' variable named 'score' that holds all of the
                              # original values
      ) %>%
      dplyr::mutate( # I don't like how 'topic' has more information than necessary
          topic = stringr::str_replace( # stringr is the best package for string manipulation
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
      dplyr::arrange(average_score) # Order them from highest to lowest enthusiasm (NOTE --
                                    # smaller number means more enthusiasm)
      
# check what we created:
  
  average_country_enthusiasm %>% head()
  