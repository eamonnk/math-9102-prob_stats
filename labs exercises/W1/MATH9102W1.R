#TU059/TU060/TU256/PhD School of Computer Science, TU Dublin
#MATH9102 - Week 1 Example

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
quiet_library("carData")     # companion datasets for the 'car' package
quiet_library("gtsummary")   # for creating publication-ready summary tables
quiet_library("flextable")   # for customizable tables in reports
quiet_library("officer")     # needed for flextable advanced customization


#SECTION TWO: SETUP DATAFRAME
#Load the dataset salaries from the package carData

#Load it into a dataframe we call acad_salary (you can change it to whatever you like)
acad_salary<-carData::Salaries


# This dataset contains data from a 2008-09 nine-month academic salary for Assistant Professors,
# Associate Professors and Professors in a college in the U.S.
# 
# rank: a factor with levels AssocProf AsstProf Prof
# 
# discipline: a factor with levels A ("theoretical" departments) or B ("applied" departments).
#
# yrs.since.phd: years since PhD.
# 
# yrs.service: years of service.
# 
# sex: a factor with levels Female Male
# 
# salary: nine-month salary, in dollars.

# Get the names of the variables in the dataset
names(acad_salary)
#-----------------------------------------------------------------------------------------------


#SECTION THREE: LOOK AT THE DATA

#Get a list of all variables in the dataset using colnames
names(acad_salary)

#You can look at the data in a variable by entering its name at the command prompt
acad_salary$salary #salary

#You can use the str function to get an overview of the variable
str(acad_salary$discipline) #Academic discipline
str(acad_salary$rank) #Academic Rank

#Use summary to get a relevant statistical summary for a variable
summary(acad_salary$discipline) #discipline
summary(acad_salary$rank) #rank

#For salary - this will give use median, mean, IQR, max and min for the salary as it is a scale
summary (acad_salary$salary)

#Or get a summary of all the variables in the dataset
summary(acad_salary)


# Distribution of a particular variable
table(acad_salary$rank)
table(acad_salary$discipline)

#To see investigate the academic ranks by academic discipline 

#We start by creating a table
table(acad_salary$discipline, acad_salary$rank)

#Then we can look at the proportions
proportions(table(acad_salary$discipline, acad_salary$rank))

# Show the proportion of each rank within its respective discipline (Row wise proportions)
# e.g. Discipline A: 13.25967% AsstProf 14.36464% AssocProf 72.37569% Prof
proportions(table(acad_salary$discipline, acad_salary$rank),margin = 1)

# Show the proportion of each discipline within its respective rank (Columnwise proportions)
# e.g. AssistProf: 35.82090% in Discipline A,  64.17910% in Discipline B
proportions(table(acad_salary$discipline, acad_salary$rank),margin = 2)


#Using the gtsummary package using pipes to create a prettier version of the table
acad_salary |>
  select(rank, discipline) |>
  gtsummary::tbl_summary()

#Showing proportions
acad_salary |>
  select(rank, discipline) |>
  gtsummary::tbl_summary(
  by = discipline, # Group by discipline if you want proportions within each discipline
  statistic = list(all_categorical() ~ "{n} ({p}%)") # Show counts and proportions
)

#Using flextable to create a version that can be saved to a file
acad_salary |>
    select(rank, discipline) |>
    gtsummary::tbl_summary(
    by = discipline, # Group by discipline if you want proportions within each discipline
    statistic = list(all_categorical() ~ "{n} ({p}%)")) |> # Show counts and proportions 
    gtsummary::as_flex_table() |> # Convert the table to a format that can be exported
    flextable::save_as_docx(path = "my_table.docx") # Save the file
#By default this will be saved in your default working directory
#To check where this is in the Console pane enter the command getwd()




# Create styled flextable
acad_salary |>
  select(rank, discipline) |>
  tbl_summary(
    by = discipline,
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

median(acad_salary$salary)

#Mean
mean(acad_salary$salary)

#You can assign the outcome to a variable
meansal <- mean(acad_salary$salary)

#and then display it on screen
meansal



#Mode
#R doesn't have a built in function to compute the mode but there is a simple function which you can write and include in your code  that will do it for you")

getmode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


#Use the function to get the mode of salary
getmode(acad_salary$salary)

------------------------------------------------------------------------------------------------

#SECTION FIVE: MEASURES OF DISPERSION
#Range
range(acad_salary$salary)


#Quantiles
quantile(acad_salary$salary)

#to get 1st quantile
x=quantile(acad_salary$salary); x[1] 

#Interquartile Range
IQR(acad_salary$salary)

#Variance
var(acad_salary$salary)


#Standard deviation
sd(acad_salary$salary)
#Rounded
round(sd(acad_salary$salary,2)) #rounded to 2 decimal places





