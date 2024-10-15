## R MIEF Skills Workshop — R Template

  ## Purpose

# NOTE — This is the section within which you describe the purpose of your script. Remember to put a "#"
# before the description, turning the text into a comment, as you don't want R to run this code.

# TYPE YOUR PURPOSE HERE:



  ## 1. Setup ----
  
    ### Packages
  
# NOTE — This is the section within which you install/load the required packages for your script.
# Two examples of doing so are shown for you in the comment directly below:
  
  # Method 1
  # install.packages("janitor") # Installs the janitor package
  # library(janitor)            # Loads the janitor package
  
  # Method 2
  # if(!require(pacman)) install.packages("pacman") # installs the pacman package if not already present,
                                                    # loads it otherwise
  # pacman::p_load(janitor) # installs the janitor package if not alrady present, loads it otherwise
  
# INSTALL/LOAD YOUR PACKAGES HERE:
  
  
  
  ## 2. Import Data ----
  
# NOTE — This is the section within which you import the data you require. Remember to open your
# .rproj file within your project folder first, or to create an R Project within your project folder
# if you haven't already. Otherwise, R won't know within which folder to look for the data!
# An example of doing so is shown for you in the comment directly below:
  
  # raw_data <- read.csv("data/raw/xample_dataset.csv", na.strings = "")
  # Remember, you can use help(read.csv) to see what the additional arguments above do.
  
# IMPORT YOUR DATA HERE:
  
  
  
  ## 3. Code! ----
  
# NOTE — This is where you would put your data cleaning, wrangling, or visualization work.
  
# CODE HERE:
  
  
  
  ## 4. Export Data/Visualizations ----
  
# NOTE — This is where you would export the datasets or visualizations you have created.
# An example of doing so is shown for you in the comment directly below:
  
  # write.csv(final_data, "data/final/final_dataset.csv", row.names = FALSE, na ="")
  # Remember, you can use help(write.csv) to see what the additional arguments above do.
  
# EXPORT YOUR DATA/VISUALIZATIONS HERE:
  
  
  
  
  
  
  