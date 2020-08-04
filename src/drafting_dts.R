library(readxl)
infrastructure <- read_excel("Data/Indicators.xlsx", sheet = 1, skip = 1)
food_systems <- read_excel("Data/Indicators.xlsx", sheet = 2, skip = 1)
learn_earn <- read_excel("Data/Indicators.xlsx", sheet = 3, skip = 1)
living <- read_excel("Data/Indicators.xlsx", sheet = 4, skip = 1)

datatable(infrastructure[,1:9], rownames = FALSE)
datatable(food_systems[,1:9], rownames = FALSE)
datatable(learn_earn[,1:9], rownames = FALSE)
datatable(living[,1:9], rownames = FALSE)

## Food systems map
datatable(food_systems[c(1,4:5),], rownames = FALSE)

## Food insecurity
datatable(food_systems[2,], rownames = FALSE)

## Lunch
datatable(food_systems[3,], rownames = FALSE)

## Crop data layer
#source for this

# Water
datatable(infrastructure[2,], rownames = FALSE)

# Education
datatable(learn_earn[1:6,], rownames = FALSE)

# Flows
#source for this

# Sectors
datatable(learn_earn[8,], rownames = FALSE)

# Median income
datatable(living[1,], rownames = FALSE)

# Poverty
datatable(living[2,], rownames = FALSE)

# Affordable housing
datatable(living[3,], rownames = FALSE)

# Racial diversity
datatable(living[4,], rownames = FALSE)

# Family stability
datatable(living[5,], rownames = FALSE)

# Education level
datatable(living[6,], rownames = FALSE)

