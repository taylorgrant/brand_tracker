
# PROCESSING CINT DATA ----------------------------------------------------

# 1. SPECIFY LOCAITON OF THE DATA FILE
file_location <- "~/R/bmw/brand_tracker/cint_data/BMW Deliverable File Updated Oct 30.csv"

# 2. CHOOSE YOUR BRAND (BMW, TESLA, MERCEDES, AUDI, LEXUS)
brand <- "BMW"

# 3. DO YOU WANT TO FILTER DOWN TO A SPECIFIC GROUP
my_groups <- NULL
my_groups = c("genz_millen")

# 4. SOURCE THE WRAPPER FUNCTION 
source(here::here("R", "process_cint_tracker.R"))

# 5. RUN THE FUNCTION (DATA IS PROCESS AND ALL DIRECTORIES ARE CREATED)
cint_wrapper(file_location, brand = brand, my_groups = my_groups)

