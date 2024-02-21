## Programming for Professional Research Using R -- Session 4

  ### Session Description

    # Exporting Summary/Regression Tables – How to produce:
      # (i) regression tables and
      # (ii) descriptive summary tables for academic or policy audiences

    # Introduction to R Data Visualization – How to produce beautiful and informative visualizations:
    # scatter plots, density plots, and more

  ### Resources

    # Tables

      # Marek Hlavac, “stargazer: beautiful LATEX, HTML and ASCII tables from R statistical output”
      # (https://cran.rproject.org/web/packages/stargazer/vignettes/stargazer.pdf). Vignette for the
      # stargazer package, main tool to export regression tables to LateX

      # Thomas Mock, “gt - a (G)rammar of (T)ables”
      # (https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/).
      # Introduction to the gt package, a more flexible instrument to export tables in PNG, PDF, or HTML formats.

    # Plots

      # Alicia Horsch, “A quick introduction to ggplot2”
      # (https://towardsdatascience.com/a-quick-introduction-to-ggplot2-d406f83bb9c9). Introduction to the
      # ggplot2 package, the main instrument for plot creation in R.

  ### Session Objectives

    # Learn how to:
      # Create simple academic-standard regression output tables using the stargazer package
      # Create flexible and easy-to-read tables of any dataset using the gt package
      # Create a scatter plot, density plot, and bar chart using the ggplot2 package

    # Practice the above!

  ## 1. Setup ----
  
    ### Packages
  
# NOTE -- Unlike using library(), the 'pacman::p_load()' function installs the package if it is already
# not present in the user's R environment.
  
  options(scipen=999)
  
  if(!require(pacman)) install.packages("pacman") 
  
  pacman::p_load(dplyr, tidyr, ggplot2, purrr, data.table, janitor, usethis, stargazer, huxtable, gt, paletteer)
  
  ## 2. Import Data ----
  
  if(!(file.exists("data/final/wvs_values_norms_data.csv"))) { # Checks whether the data has been
                                                               # downloaded already
      usethis::use_zip(
          "https://www.dropbox.com/scl/fo/vnxjbqyq1g9z368coh1vq/h?rlkey=zc66o2ll7613b5e9ipp915ynk&dl=1"
      )
      
  }
 
  norms_values_data <- data.table::fread( # Other options are base R's read.csv() and readr::read_csv(), but
                                          # data.table::fread() is considered to be the fastest
      "data/final/wvs_values_norms_data.csv", na.strings = ""
  )
  
  ## Country Continent Data
  
  country_continent_data <- data.table::fread(
      "data/raw//country_continent.csv", na.strings = ""
  )

  ## 3. SESSION 1 -- Data 'Wrangling' ----
  
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
  
    ### Step 6 — BONUS — Using purrr::map() ----
  
  # We want to figure out the class of every variable in the european_country_data data frame.
  # To do this, we use the map() function.
  
  european_country_data %>%
      purrr::map_df(
          ~ data.frame(class = class(.x))
      )
  
  # This is nice, but now I'd like to see the names of the variable too... map() has an option for this!
  # You specify the argument .id and set it equal to the name of the variable you want to create.
  
  european_country_data %>%
      purrr::map_df(
          ~ data.frame(class = class(.x)),
          .id = "variable"
      )
  
  # Challenge — We want to figure out how many distinct values each variable has. We can do this
  # using the dplyr::n_distinct() function. Add a new "n_distinct" column to the data frame created
  # within the map_df() call above.
  
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
  # value of 'var' to the small value of 'var'. Solution below.
  
  popular_answers <- child_data_long %>%
      group_by(child_quality) %>%
      dplyr::summarize(
          num = sum(child_quality_value, na.rm = TRUE) # Notice that "sum" works because of coding (yes = 1, no = 0)
      ) %>%
      arrange(desc(num)) # Manners, Responsibility, Tolerance, Hard Work, Independence, Faith
  
  ## 5. Session 3 -- Data Visualization using Tables and Graphs ----
  
    ### Step 1 -- Simple Regression Table ----
  
# Purpose -- I want to accomplish two tasks:
  
  # Assess the relationship between a respondent's relationship with their parents
  # (Q27_agree_parents_proud) and what they think is important for their child (Q07-Q17)
  # -> Regression analysis and table production
  
# We've already cleaned Q07-Q17, check Q27
  
  norms_values_data %>% tabyl(Q27_agree_parents_proud)
  
# Properly encode the missing values
  
  norms_values_data <- norms_values_data %>%
      mutate(
          Q27_agree_parents_proud = case_when(
              Q27_agree_parents_proud < 0 ~ NA_real_,
              Q27_agree_parents_proud == 4 ~ 1, # 4 is 'strongly disagree' and 1 is 'strongly agree'.
                                                # I like bigger = better
              Q27_agree_parents_proud == 3 ~ 2,
              Q27_agree_parents_proud == 2 ~ 3,
              Q27_agree_parents_proud == 1 ~ 4
          )
      )
  
  parent_child_dataset <- norms_values_data %>%
      select(
          D_INTERVIEW, B_COUNTRY_ALPHA,
          Q07_child_manners:Q17_child_obedient, Q27_agree_parents_proud
      )
  
  parent_child_regression <- lm(
      data    = parent_child_dataset,
      formula = Q27_agree_parents_proud ~ Q08_child_independence + Q09_child_hard_work +
          Q10_child_responsibility + Q11_child_imagination + Q12_child_tolerance + Q13_child_thrift +
          Q14_child_determined + Q15_child_faith + Q16_child_unselfish + Q17_child_obedient
  )
  
# Look at what the results of the regression are
  
  parent_child_regression %>% summary()
  
# Lots of interesting relationships here! But we want to present this data easily to other people.
  
# Stargazer outputs a simple LateX script
  
  parent_child_sg <- parent_child_regression %>%
      stargazer()
# Looks super ugly -- you'd want to add labels to replace your variable names in the table.
# stargazer() is very customizable, just use help(stargazer) to see how to add labels
  
# You can then save to LateX using the writeLines() function
  
  writeLines(parent_child_sg, "output/regression_table_sg.tex")
  
# You can then either import the .tex file into a software like Overleaf, or use the
# pdflatex() function from the tinytex package to export to PDF.
  
# Huxtable transforms a regression output into a table described in 'LateX' script.
  
  parent_child_names <- c(
      "Independence"   = "Q08_child_independence",
      "Hard Work"      = "Q09_child_hard_work",
      "Responsibility" = "Q10_child_responsibility",
      "Imagination"    = "Q11_child_imagination",
      "Tolerance"      = "Q12_child_tolerance",
      "Thriftiness"    = "Q13_child_thrift",
      "Determination"  = "Q14_child_determined",
      "Faith"          = "Q15_child_faith",
      "Selflessness"   = "Q16_child_unselfish",
      "Obedience"      = "Q17_child_obedient"
  )
  
  parent_child_hux <- parent_child_regression %>%
      huxtable::huxreg(
          coefs = parent_child_names
      )
  
# Some saving options:
  
  huxtable::quick_latex(
      parent_child_hux, file = "output/regression_table.tex"
  )
  
  quick_pdf(
      parent_child_hux, file = "output/regression_table.pdf"
  )
  
  quick_html(
      parent_child_hux, file = "output/regression_table.html"
  )
  
    ### Step 2 -- Descriptive Statistics Table ----
  
  # I want to observe how people on different continents think about politics and religion as
  # parts of their life -> descriptive statistics table and various plots
  
# First check if our variables are okay
  
  norms_values_data %>% tabyl(Q04_life_politics)
  
  norms_values_data %>% tabyl(Q06_life_religion)
  
  politics_religion_dataset <- norms_values_data %>%
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

# Use the gt() package to create a descriptive statistics table out of this. Using gt() is a very
# iterative process, I'd recommend just trying out the basic one shown below and then adding
# components one by one.
  
  simple_desc_gt_table <- politics_religion_dataset %>%
      select(-B_COUNTRY_ALPHA) %>% # Don't need it
      gt()
  
# Looks pretty rough. Time to make it look nicer:
  
  politics_religion_gt_table <- politics_religion_dataset %>%
      select(-B_COUNTRY_ALPHA) %>% # Don't need it
      mutate( # Too many digits in our numeric variables
          across(
              Q04_life_politics:Q06_life_religion,
              ~ round(.x, digits = 3)
          )
      ) %>%
      group_by(continent) %>% # See what this does
      gt() %>%
      cols_label( # Lets you assign names to columns
          country_long      = "Country",
          Q04_life_politics = "Politics",
          Q06_life_religion = "Religion"
      ) %>%
      tab_header( # Add title/subtitle
          title    = "World Values Survey",
          subtitle = "Importance in Life -- Politics vs. Religion"
      ) %>%
      data_color( # Adding some color scales to make the numbers easier to parse
          columns = Q04_life_politics,
          colors  = scales::col_numeric(
              palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
              domain = NULL
          )
      ) %>%
      data_color( # Adding some color scales to make the numbers easier to parse
          columns = Q06_life_religion,
          colors  = scales::col_numeric(
              palette = as.character(paletteer::paletteer_d("ggsci::blue_material", n = 5)),
              domain = NULL
          )
      )
  
# Could do a lot more but this is enough for now. Save:
  
  gtsave(
      politics_religion_gt_table,
      "output/politics_religion_gt.png"
  )

    ### Step 3 -- Descriptive Statistics Plot(s) ----
  
# We're going to compare politics and religion across continents using different plot formats in the
# ggplot2 package.
  
# KEY -- Once you use the function ggplot(), REPLACE %>% with +
  
# First -- Let's use a simple density plot to assess the distribution of both variables
  
  politics_religion_density_plot <- politics_religion_dataset %>%
      ggplot() +
      geom_density(
          aes( # aesthetics -- variables always go inside of this
              x = Q04_life_politics # geom_density only requires an 'x'
          ),
          color = "red"
      ) +
      geom_density(
          aes( # aesthetics -- variables always go inside of this
              x = Q06_life_religion # geom_density only requires an 'x'
          ),
          color = "blue"
      ) +
      xlab("Importance in Life (1 - 4)")
  
# No legend though. Solution: Use pivot_longer so that each variable is a group
  
  politics_religion_dataset_long <- politics_religion_dataset %>%
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
  
  politics_religion_density_plot2 <- politics_religion_dataset_long %>%
      ggplot() +
      geom_density(
          aes(
              x = life_importance, color = variable
          )
      ) +
      xlab("Importance in Life (1-4)") +
      theme_minimal() + # Always looks better
      theme(
          legend.position = "bottom", # I prefer this
          plot.background = element_rect(color = "white") # Without this the graph's background is
                                                          # transparent
      )
  
# Second -- Bar chart, grouping by continent
  
  politics_religion_bar_chart <- politics_religion_dataset_long %>%
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
      ungroup() %>%
      ggplot() +
      geom_bar(
          aes(
              x = continent, y = life_importance, fill = variable
          ),
          position = "dodge", stat = "identity"
      ) +
      xlab("Continent") +
      ylab("Importance in Life (1-4)") +
      scale_fill_discrete(name = "") + # An easy way to get rid of the legend title
      theme_minimal() + # Always looks better
      theme(
          legend.position = "bottom",
          plot.background = element_rect(color = "white") # Without this the graph's background is
                                                          # transparent
      )
      
# Finally -- Scatter plot of all countries
  
# Scatter plots allow us to visualize data in two dimensions, i.e. along two variables.
# Here it's politics and religion's importance in life
  
  politics_religion_scatter_plot <- politics_religion_dataset %>%
      ggplot() +
      geom_point(
          aes(
              x = Q04_life_politics, y = Q06_life_religion, color = continent
          )
      ) +
      xlab("Importance of Politics") +
      ylab("Importance of Religion") +
      scale_x_continuous( # Make sure scale is 1-4
          limits = c(1, 4)
      ) +
      scale_y_continuous(
          limits = c(1, 4)
      ) +
      theme_minimal() +
      theme(
          legend.position = "bottom",
          plot.background = element_rect(color = "white") # Without this the graph's background is
                                                          # transparent
      )
  
# Fun alternative -- replace points with country abbreviation
  
  politics_religion_scatter_plot2 <- politics_religion_dataset %>%
      ggplot() +
      geom_text(
          aes(
              x = Q04_life_politics, y = Q06_life_religion,
              label = B_COUNTRY_ALPHA, color = continent
          ),
          check_overlap = TRUE
      ) +
      xlab("Importance of Politics") +
      ylab("Importance of Religion") +
      scale_x_continuous( # Make sure scale is 1-4
          limits = c(1, 4)
      ) +
      scale_y_continuous(
          limits = c(1, 4)
      ) +
      theme_minimal() +
      theme(
          legend.position = "bottom",
          plot.background = element_rect(color = "white") # Without this the graph's background is
                                                          # transparent
      )
  
# Save everything important
  
  ggsave(
      "output/politics_religion_density.png",
      politics_religion_density_plot2
  )
  
  ggsave(
      "output/politics_religion_bar.png",
      politics_religion_bar_chart
  )
  
  ggsave(
      "output/politics_religion_scatter.png",
      politics_religion_scatter_plot
  )
  
  ggsave(
      "output/politics_religion_scatter2.png",
      politics_religion_scatter_plot2
  )
  
# Challenges for today:
  
  # Using child_data_long, create a gt() table showing the five most important child qualities
  # for a given country, along with either the number or %age of respondents in each country who
  # called it important (hint: you'll have to use filter() and summarize() for this)
  
  # Then create a density plot showing the distribution of importance of these five child qualities
  # across countries (i.e. summarize at the country level then use ggplot() and geom_density())
  
  # Create a scatter plot showing an interesting comparison between two child qualities across
  # countries/continents
  