#TU059/TU060/TU256/PhD School of Computer Science, TU Dublin
#MATH9102 - Week 1 Lab solutions

#------------Preliminaries-------------------------------
#SECTION ONE: INSTALL AND LOAD packages

# Define a helper function `quiet_library`:
# - Takes a package name as input
# - Installs the package if it is not already installed
# - Loads the package while suppressing warnings and messages
quiet_library <- function(pkg) {
  suppressMessages(suppressWarnings({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE) # Install without messages
    }
    library(pkg, character.only = TRUE) # Load silently
  }))
}

# Load required packages (install if missing)
quiet_library("tidyverse")   # for data wrangling and visualization
quiet_library("gtsummary")   # for creating publication-ready summary tables
quiet_library("flextable")   # for customizable tables in reports
quiet_library("officer")     # needed for flextable advanced customization


#SECTION TWO: SETUP DATAFRAME
#Load the dataset msleep from the ggplot2 

#Load it into a dataframe 
msleepdata<-ggplot2::msleep

# Get the names of the variables in the dataset
names(msleepdata)

#-----------------------------------------------------------------------------------------------


#SECTION THREE: LOOK AT THE DATA

#Get a list of all variables in the dataset using colnames
names(msleepdata)

#You can look at the data in a variable by entering its name at the command prompt
msleepdata$sleep_total #total amount of sleep in hours

#You can use the str function to get an overview of the variable
str(msleepdata$brainwt) #brain weight
str(msleepdata$bodywt) #body weight

#Use summary to get a relevant statistical summary for a variable
summary(msleepdata$vore) #carnivore,herbivore etc

#For numerical data - this will give use median, mean, IQR, max and min for the salary as it is a scale
summary (msleepdata$brainwt)

#Or get a summary of all the variables in the dataset
summary(msleepdata)


#The distribution of observations across mammalian foraging types
table(msleepdata$vore)

#To see investigate the conservation status by vore type 

#We start by creating a table
table(msleepdata$conservation, msleepdata$vore)

#Then we can look at the proportions
proportions(table(msleepdata$conservation, msleepdata$vore))

# Show the proportion of each conservation status for each vore type (Row wise proportions)
# e.g. Critically Endagered Carnivore: 50%, Domesticated Herbivore: 70%
proportions(table(msleepdata$conservation, msleepdata$vore),margin = 1)

# Columnwise proportions
proportions(table(msleepdata$vore, msleepdata$conservation),margin = 1)


#Using the gtsummary package using pipes to create a prettier version of the table
msleepdata |>
  select(conservation, vore) |>
  gtsummary::tbl_summary()

#Showing proportions
msleepdata |>
  select(conservation, vore) |>
  gtsummary::tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)") # Show counts and proportions
  )

#Using flextable to create a version that can be saved to a file
msleepdata |>
  select(conservation, vore) |>
  gtsummary::tbl_summary(
    by = conservation, # Group by conservation
    statistic = list(all_categorical() ~ "{n} ({p}%)")) |> # Show counts and proportions 
  gtsummary::as_flex_table() |> # Convert the table to a format that can be exported
  flextable::save_as_docx(path = "my_table.docx") # Save the file
#By default this will be saved in your default working directory
#To check where this is in the Console pane enter the command getwd()




# Create styled flextable
msleepdata |>
  select(conservation, vore) |>
  tbl_summary(
    by = vore, # group by vore type
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) |>
  as_flex_table() |>
  theme_vanilla() |> # start with a clean theme
  border_outer(part = "all", border = fp_border(color = "black", width = 2)) |> # thick outer border
  border_inner_h(part = "all", border = fp_border(color = "darkblue", width = 1)) |> # horizontal lines
  border_inner_v(part = "all", border = fp_border(color = "darkblue", width = 1)) |> # vertical lines
  color(color = "white", part = "header") |> # white text in header
  bg(bg = "darkblue", part = "header") |> # dark blue header background
  bold(part = "header") |> # bold header text
  autofit() |> 
  save_as_docx(path = "my_fancy_table.docx")
#By default this will be saved in your default working directory
#To check where this is in the Console pane enter the command getwd()

-------------------------------------------------------------------------------
  # SECTION FOUR: MEASURES OF CENTRAL TENDENCY
  #Median
  
  median(msleepdata$sleep_total) #total hours sleep

#Mean
mean(msleepdata$sleep_total)

#You can assign the outcome to a variable
meansal <- mean(msleepdata$sleep_total)

#and then display it on screen
meansal



#Mode
#R doesn't have a built in function to compute the mode but there is a simple function which you can write and include in your code  that will do it for you")

getmode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


#Use the function to get the mode of total sleep hours
getmode(msleepdata$sleep_total)

------------------------------------------------------------------------------------------------
  
#SECTION FOUR: MEASURES OF DISPERSION
#Range
range(msleepdata$sleep_total)


#Quantiles
quantile(msleepdata$sleep_total)

#to get 1st quantile
x=quantile(msleepdata$sleep_total); x[1] 

#Interquartile Range
IQR(msleepdata$sleep_total)

#Variance
var(msleepdata$sleep_total)


#Standard deviation
sd(msleepdata$sleep_total)
#Rounded
round(sd(msleepdata$sleep_total,2)) #rounded to 2 decimal places





