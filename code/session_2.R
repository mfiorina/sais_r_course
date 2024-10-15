## R MIEF Skills Workshop — Session 2

  ### Session Description

    # Introduction to R Data Visualization – How to produce beautiful and informative visualizations:
      # scatter plots, density plots, and more

    # Exporting Summary/Regression Tables – How to produce:
      # (i) descriptive summary tables for academic or policy audiences and
      # (ii) regression tables

  ### Resources

  # Plots

    # Alicia Horsch, “A quick introduction to ggplot2”
      # (https://towardsdatascience.com/a-quick-introduction-to-ggplot2-d406f83bb9c9). Introduction to the
      # ggplot2 package, the main instrument for plot creation in R.

  # Tables
  
    # Marek Hlavac, “stargazer: beautiful LATEX, HTML and ASCII tables from R statistical output”
      # (https://cran.rproject.org/web/packages/stargazer/vignettes/stargazer.pdf). Vignette for the
      # stargazer package, main tool to export regression tables to LateX

      # Thomas Mock, “gt - a (G)rammar of (T)ables”
      # (https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/).
      # Introduction to the gt package, a more flexible instrument to export tables in PNG, PDF, or HTML formats.

  ### Session Objectives

    # Learn how to:
      # Create a scatter plot, density plot, and bar chart using the ggplot2 package
      # Create flexible and easy-to-read tables of any dataset using the gt package
      # Create simple academic-standard regression output tables using the stargazer package

  ## 1. Setup ----
  
    ### Packages
  
# NOTE — Unlike using library(), the 'pacman::p_load()' function installs the package if it is already
# not present in the user's R environment.
  
  options(scipen = 999)
  
  if(!require(pacman)) install.packages("pacman") 
  
  pacman::p_load(dplyr, tidyr, ggplot2, stringr, purrr, data.table, janitor, usethis, stargazer, huxtable, gt, paletteer)
  
  ## 2. Import Data ----
 
  norms_values_raw <- data.table::fread( # Other options are base R's read.csv() and readr::read_csv(), but
                                         # data.table::fread() is considered to be the fastest
      "data/final/wvs_norms_values_data.csv", na.strings = ""
  )
  
  ## Country Continent Data
  
  country_continent_raw <- data.table::fread(
      "data/final/country_continent.csv", na.strings = ""
  )
  
  ## Politics/Religion Country Data (Plot)
  
  politics_religion_plot_data <- data.table::fread(
      "data/final/politics_religion_plot.csv", na.strings = ""
  )
  
  ## Politics/Religion Country Data (Plot) (Long)
  
  politics_religion_plot_data_long <- data.table::fread(
      "data/final/politics_religion_plot_long.csv", na.strings = ""
  )
  
  ## Politics/Religion Continent Data (Plot) (Long)
  
  politics_religion_plot_data_long_continent <- data.table::fread(
      "data/final/politics_religion_plot_long_continent.csv", na.strings = ""
  )
  
  ## Politics/Religion Country Data (Table)
  
  politics_religion_table_data <- data.table::fread(
      "data/final/politics_religion_table.csv", na.strings = ""
  )
  
  ## Parent/Child Values Data
  
  parent_child_data <- data.table::fread(
      "data/final/parent_child.csv", na.strings = ""
  )
  
  ## Child Values Country Data
  
  child_values_country_data <- data.table::fread(
      "data/final/child_values_country.csv", na.strings = ""
  )
  
  ##  Child Values Continent Data
  
  child_values_continent_data <- data.table::fread(
      "data/final/child_values_continent.csv", na.strings = ""
  )
  
  ### Step 1 — Descriptive Statistics Plot(s) ----
  
# We're going to compare politics and religion across continents using different plot formats in the
# ggplot2 package.
  
# First — Let's use a simple density plot to assess the distribution of both variables
  
  politics_religion_density_plot <- ggplot(politics_religion_plot_data) +
      geom_density(
          aes( # aesthetics — variables always go inside of this
              x = Q04_life_politics # geom_density only requires an 'x'
          ),
          color = "red"
      ) +
      geom_density(
          aes( # aesthetics — variables always go inside of this
              x = Q06_life_religion # geom_density only requires an 'x'
          ),
          color = "blue"
      ) +
      xlab("Importance in Life (1 - 4)")
  
  # No legend though. Solution: Use long version so that we have a "category" variable
  # of religion or politics
  
  politics_religion_density_plot2 <- ggplot(politics_religion_plot_data_long) +
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
  
  # Second — Bar chart, grouping by continent
  
  politics_religion_bar_chart <- ggplot(politics_religion_plot_data_long_continent) +
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
  
  # Finally — Scatter plot of all countries
  
  # Scatter plots allow us to visualize data in two dimensions, i.e. along two variables.
  # Here it's politics and religion's importance in life
  
  politics_religion_scatter_plot <- ggplot(politics_religion_plot_data) +
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
  
  # Fun alternative — replace points with country abbreviation
  
  politics_religion_scatter_plot2 <- ggplot(politics_religion_plot_data) +
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
  
    ### Step 2 — Descriptive Statistics Table ----
  
  # I want to observe how people on different continents think about politics and religion as
  # parts of their life -> descriptive statistics table and various plots
  
# First check if our variables are okay
  
  tabyl(norms_values_raw, Q04_life_politics)
  
  tabyl(norms_values_raw, Q06_life_religion)

# Use the gt() package to create a descriptive statistics table out of this. Using gt() is a very
# iterative process, I'd recommend just trying out the basic one shown below and then adding
# components one by one.
  
  simple_desc_gt_table <- gt(politics_religion_table_data)
  
  simple_desc_gt_table
  
# Looks pretty rough. Time to make it look nicer:
  
# NOTE — We will explore the use of `%>%` in Session 3. For now, just know that
# it allows us to "stack" our code in a more intuitive manner.
  
  politics_religion_gt_table <- gt(
      politics_religion_table_data, groupname_col = "continent"
  ) %>%
      cols_label( # Lets you assign names to columns
          country_long      = "Country",
          Q04_life_politics = "Politics",
          Q06_life_religion = "Religion"
      ) %>%
      tab_header( # Add title/subtitle
          title    = "World Values Survey",
          subtitle = "Importance in Life — Politics vs. Religion"
      ) %>%
      data_color( # Adding some color scales to make the numbers easier to parse
          columns = Q04_life_politics,
          fn      = scales::col_numeric(
              palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
              domain = NULL
          )
      ) %>%
      data_color( # Adding some color scales to make the numbers easier to parse
          columns = Q06_life_religion,
          fn      = scales::col_numeric(
              palette = as.character(paletteer::paletteer_d("ggsci::blue_material", n = 5)),
              domain = NULL
          )
      )
  
  politics_religion_gt_table
  
# Could do a lot more but this is enough for now. Save:
  
  gtsave(
      politics_religion_gt_table,
      "output/politics_religion_gt.png"
  )
  
  ### Step 3 — Simple Regression Table ----
  
  # Purpose — I want to accomplish two tasks:
  
    # Assess the relationship between a respondent's relationship with their parents
    # (Q27_agree_parents_proud) and what they think is important for their child (Q07-Q17)
    # -> Regression analysis and table production
  
  parent_child_regression <- lm(
      data    = parent_child_data,
      formula = Q27_agree_parents_proud ~ Q08_child_independence + Q09_child_hard_work +
          Q10_child_responsibility + Q11_child_imagination + Q12_child_tolerance + Q13_child_thrift +
          Q14_child_determined + Q15_child_faith + Q16_child_unselfish + Q17_child_obedient
  )
  
# Look at what the results of the regression are
  
  summary(parent_child_regression)
  
# Lots of interesting relationships here! But we want to present this data easily to other people.
  
# Stargazer outputs a simple LateX script
  
  parent_child_sg <- stargazer(parent_child_regression)
  
# Looks super ugly — you'd want to add labels to replace your variable names in the table.
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
  
  parent_child_hux <- huxtable::huxreg(
      parent_child_regression,
      coefs = parent_child_names
  )
  
  parent_child_hux
  
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
  
# Challenges for today:
  
  # Using child_values_country_data, create a scatter plot showing an interesting comparison between
  # two child values across countries.
  
  # Using child_values_continent_data, create a bar plot comparing a specific child value across
  # continents.
  
  # Using child_values_continent_data, create a gt table showcasing the same data as in your bar plot.
  
  