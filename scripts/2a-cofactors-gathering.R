## The purpose of this script is to show all the code used to edit the data of
## the cofactors used in the study.
data_prn <- read.table("data/data-prn", header = T, sep = "\t")
states_names <- read.table("files/states-names.txt", header = T, sep = "\t")

#### Population density ####

## We load the file containing the population census. The data comes from the
## U.S. Census Bureau website [1]. Only the data for the states represented in
## the Lefrancq data frame are included in the file.
data_census <- read.table("data/raw/data-census", header = T, sep = "\t")
data_census <- data_census[data_census$state %in% data_prn$state, ]

## Since census are lead on a decennial basis, we must infer the population
## sizes for each state for missing years. We do this with considering a simple
## linear growth of the population between two censuses.
pop <- c()
for (s in unique(data_census$state)) {
  for (y in unique(data_census$year)[2:3]) {
    pop_sy <- with(data_census, seq(pop[year == y - 10 & state == s],
                                    pop[year == y & state == s], 
                                    length.out = 11))
    pop <- c(pop, pop_sy)
  }
}
pop <- unique(pop)
state <- rep(unique(data_census$state), each = length(pop) / length(unique(data_census$state)))
year <- rep(min(data_census$year):max(data_census$year), times = length(unique(data_prn$state)))
pop <- pop[year %in% data_prn$year]

## Now that we have the population per state for each year of interest, we must
## calculate the corresponding population density. To do that, we need the area
## of each state, an information available on the website of the U.S. Census
## Bureau [2].
data_area <- read.table("data/raw/data-area", header = T, sep = "\t")
data_area <- data_area[data_area$state %in% data_prn$state, ]

## We calculate the density as the ratio of the population size on the area of
## each state.
density <- pop / rep(data_area$area, each = length(unique(data_prn$year)))

## Finally, we build and save the data frame containing the population density
## for each state and year.
data_density <- data.frame(state = state[year %in% data_prn$year], 
                           year = year[year %in% data_prn$year], 
                           density)
data_density <- data_density[order(data_density$year), ]
write.table(data_dens, file = "data/cofactors/data-density", quote = F, row.names = F, sep = "\t")

#### Temperature ####

## We load the file containing the information on all the meteorological
## stations available. The data set comes from the GHCNd data portal [3] The 
## downloaded version dates from 2021-04-2022.
list_stations <- readLines("files/ghcnd-stations.txt")

## We filter to keep the US-based stations only.
list_stations <- list_stations[grep("US", substr(list_stations, 1, 11))]

## And we keep only the stations from states where strains were isolated in the
## data frame we use.
list_stations <- list_stations[substr(list_stations, 39, 40) %in% data_prn$state]

## Another data set, also available on the GHCNd data portal [4], is used to
## filter the stations we should keep.
inventory <- read.table("files/ghcnd-inventory.txt", 
                        col.names = c("id", "lat", "lon", "var", "start", "end"))

## We filter the stations to keep those that measured the maximum temperature
## (TMAX) and the minimum temperature (TMIN) between 2007 (first year where
## PRN- strains were isolated in the data) and 2019 (last year reported in
## the data).
start = min(data_prn$year)
end = max(data_prn$year)
inventory <- inventory[inventory$var %in% c("TMAX", "TMIN"), ]
inventory <- inventory[inventory$id %in% names(table(inventory$id)[table(inventory$id) == 2]), ]
inventory <- inventory[inventory$end >= start, ]
inventory <- inventory[inventory$start <= end, ]

## We can now have the final list of stations that interest us. 
list_stations <- list_stations[substr(list_stations, 1, 11) %in% inventory$id]
id_stations <- substr(list_stations, 1, 11)

## Now, we download the files containing the information on the temperatures
## by sending requests to the HTTPS portal of the NOAA. This method is 
## pretty time-consuming, and because the servers are not made to support such 
## an automated approach it may be necessary to run again and again this loop.
## Note: An API [5] is avalaible to do pretty much the same thing, but we
## discovered it later, and it is not 100% stable either.
for (id in id_stations[!id_stations %in% list.files("data/raw/temp-us-ghcnd/")]) {
  url <- paste0("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&startDate=",
                start, "-01-01",  
                "&endDate=", 
                end, "-12-31",
                "&stations=", 
                id, 
                "&format=csv")
  try({
    df_ghcnd <- read.csv(url)
    df_ghcnd <- df_ghcnd[, c("STATION", "DATE", "TMAX", "TMIN")]
    write.table(df_ghcnd, paste0("data/raw/temp-us-ghcnd/", id), 
                quote = F, row.names = F, sep = "\t")
  })
}

## After downloading the files, we summarize and merge them. First, we get the
## average temperature per year per station. The average temperature is first
## calculated each day for each station as the mean between the TMAX value and
## the TMIN value.
list_files_ghcnd <- list.files("data/raw/temp-us-ghcnd/")
list_temp <- list()
for (file in list.files("data/raw/temp-us-ghcnd/")) {
  xx <- read.table(paste0("data/raw/temp-us-ghcnd/", file), header = T, sep = "\t")
  xx <- na.omit(xx) # For some dates/stations, temperatures were not recorded.
  year <- substr(xx$DATE, 1, 4)
  temp <- (xx$TMAX + xx$TMIN) / 20
  list_temp[[file]] <- tapply(temp, year, mean)
}

## We then merge the temperature, the stations id, the state of origin and the
## year into one data frame.
temp <- unlist(list_temp)
stations <- gsub("\\..*", "", names(temp))
state <- rep(substr(list_stations[id_stations %in% stations], 39, 40), 
             times = table(stations))
year <- gsub(".*\\.", "", names(temp))
data_temperature <- data.frame(stations, state, year, temp, row.names = NULL)

## Since we do not need to keep the "per station" information, we summarize the
## data by averaging all stations for each state and each year and thus produce
## another data frame, which we save.
temp <- tapply(data_temperature$temp, interaction(data_temperature$state, data_temperature$year), mean)
year <- gsub(".*\\.", "", names(temp))
state <- gsub("\\..*", "", names(temp))
data_temperature <- data.frame(state, year, temp, row.names = NULL)
write.table(data_temperature, file = "data/cofactors/data-temperature", quote = F, row.names = F, sep = "\t")

#### Household size ####

## The data on the household size was obtained "by hand" from the FTP server of
## the U.S. Census Bureau, directory "summary_file". Using the documentation
## and files available, we were able to retrieve the average household size in
## each state of interest for each year of the study. The results were saved in
## the file "data-household".
data_households <- read.table("data/cofactors/data-households", header = T)

#### Vaccine coverage ####

## The vaccine coverage data comes from the ChildVaxView portal of the CDC [7].
## The raw data set is pretty large, so we need to filter it to keep only the
## rows and columns that interest us. The file was downloaded the 15th of May
## 2023. The applied filters are detailled below.
data_coverage <- read.table("data/raw/data-cov-cdc", header = T, sep = ",")

## We first need to keep only the rows concerning the DTaP vaccine.
data_coverage <- data_coverage[data_coverage$Vaccine == "DTaP", ]

## Since the WHO recommends children to be vaccinated with three doses of
## vaccine, we focus on the proportion of individuals who received at least
## three doses.
data_coverage <- data_coverage[data_coverage$Dose == "≥3 Doses", ]

## The data is classified per age category and birth cohort. The U.S. vaccine
## schedule recommends the third dose of DTaP to be taken at six months of age,
## but some parents vaccinate their children later in their first year. Thus,
## we focus on children in the "13 Months" category.
data_coverage <- data_coverage[data_coverage$Dimension == "13 Months", ]

## We consider that the "13 Months" category represents the vaccine coverage of
## the year corresponding to the birth year plus one. Thus, we only need to 
## keep the years from from 2011 to 2018 (earlier years are not available).
data_coverage <- data_coverage[data_coverage$Birth.Year.Birth.Cohort %in% (data_prn$year - 1), ]

## We finally build the data frame containing the data we need. We must adapt
## the year of vaccine coverage (birth date + 1), adjust the names of the
## states, and remove the rows of states excluded from the analysis.
data_coverage <- data_coverage[data_coverage$Geography %in% states_names$long, ]
data_coverage <- data_coverage[, c(4, 5, 8)]
colnames(data_coverage) <- c("state", "year", "coverage")
for (state in states_names$long) {
  data_coverage$state[data_coverage$state == state] <- states_names$short[states_names$long == state]
}
data_coverage <- data_coverage[data_coverage$state %in% data_prn$state, ]

## The problem is that the data starts in 2012, while we need it from 2007.
## To get around this problem, we decided to consider the vaccine coverage to
## be constant state-wise, i.e., the vaccine coverage does not change with
## years. We used the mean vaccine coverage per state.
coverage <- tapply(data_coverage$coverage, data_coverage$state, mean)
data_coverage <- data.frame(state = rep(sort(unique(data_prn$state)), 
                                        times = length(unique(data_prn$year))),
                            year = rep(seq(min(data_prn$year), max(data_prn$year)), 
                                       each = length(unique(data_prn$state))),
                            coverage = rep(coverage, times = length(unique(data_prn$year))))

## The data frame that will be used in the analyzes is now ready.
write.table(data_coverage, file = "data/cofactors/data-coverage", quote = F, row.names = F, sep = "\t")

#### Hospitals density ####

## The number of hospitals per state in the U.S. can be found on the website of
## the U.S. Bureau of Labor Statistics for the year 2019 [8]. Since we did not
## have access to data for previous years, we considered the number of
## hospitals to be constant over years.
data_hospitals <- read.table("data/raw/data-hospitals", header = T, sep = "\t")

## We filter the data we must keep.
for (state in states_names$long) {
  data_hospitals$state[data_hospitals$state == state] <- states_names$short[states_names$long == state]
}
data_hospitals <- data_hospitals[data_hospitals$state %in% data_prn$state, ]
data_hospitals <- data.frame(state = rep(data_hospitals$state, 
                                        each = length(unique(data_prn$year))),
                             year = rep(seq(min(data_prn$year), max(data_prn$year)), 
                                        times = length(unique(data_hospitals$state))),
                             n.hospitals = rep(data_hospitals$n.hospitals, 
                                               each = length(unique(data_prn$year))))
data_hospitals <- data_hospitals[order(data_hospitals$year, data_hospitals$state), ]

## What we use in our analyzes is not the raw number of hospitals but the
## density of them, i.e., the number of hospitals per 100,000 inhabitants in
## each state. 
state <- rep(sort(unique(data_prn$state)), each = length(unique(data_prn$year)))
year <- rep(min(data_prn$year):max(data_prn$year), times = length(unique(data_prn$state)))
data_pop <- data.frame(state, year, pop)
data_pop <- data_pop[order(data_pop$year), ]
data_hospitals$hospitals <- data_hospitals$n.hospitals * 1e5 / data_pop$pop
data_hospitals <- data_hospitals[, c(1, 2, 4)]

## We save the data set.
write.table(data_hospitals, file = "data/cofactors/data-hospitals", quote = F, row.names = F, sep = "\t")

#### Income ####

## Data on the median income was gathered in a similar fashion than data on the
## household size, resulting in a file made "by hand".
data_income <- read.table("data/raw/data-income", header = T, sep = "\t")
data_income$med.inc <- data_income$med.inc / 1000
write.table(data_income, "data/cofactors/data-income", quote = F, row.names = F, sep = "\t")

## Since the meaning of an income is inflation-dependent, we corrected the raw
## values to adjust them to the value of the dollar in 2021. To do so, we used
## the Consumer Price Index provided by the U.S. Bureau of Labor Statistics
## [9]. To obtain the median income in 2021 dollars, we multiplied the median
## income on year X with the ratio of the CPI of 2021 and the CPI on year X.
cpi <- read.table("files/cpi", header = T, sep = "\t")
for (year in cpi$year) {
  data_income$income[data_income$year == year] = 
    data_income$med.inc[data_income$year == year] * cpi$cpi[cpi$year == 2021] / cpi$cpi[cpi$year == year]
}
data_income <- data_income[, c(1, 2, 4)]

## We save the data set.
write.table(data_income, file = "data/cofactors/data-income", quote = F, row.names = F, sep = "\t")

#### Physicians density ####

## Data on the number of physicians per 10,000 inhabitants is available on the
## website of the CDC [10].
data_physicians <- read.table("data/raw/data-physicians", header = T, sep = "\t")
for (state in states_names$long) {
  data_physicians$state[data_physicians$state == state] <- states_names$short[states_names$long == state]
}
data_physicians <- data_physicians[data_physicians$state %in% data_prn$state
                                   & data_physicians$year >= 2000, ]

## The problem is that the density of physicians is only available for a few
## selected years. Thus, we use the same approach as for the population sizes
## to estimate the density of physicians for missing years.
phy <- c()
available_years <- unique(data_physicians$year)
for (s in unique(data_physicians$state)) {
  for (y in 2:3) {
    phy_sy <- with(data_physicians, seq(physicians[year == available_years[y - 1] & state == s],
                                        physicians[year == available_years[y] & state == s], 
                                        length.out = available_years[y] - available_years[y - 1] + 1))
    phy <- c(phy, phy_sy[-1])
  }
}
state <- rep(unique(data_physicians$state), each = length(phy) / length(unique(data_physicians$state)))
year <- rep(min(data_physicians$year):max(data_physicians$year), times = length(unique(data_prn$state)))
year <- year[year != min(year)]
phy <- phy[year %in% data_prn$year]

## We can build the data set that will be used in the analyzes.
data_physicians <- data.frame(state = state[year %in% data_prn$year],
                              year = year[year %in% data_prn$year],
                              physicians = phy)
data_physicians <- data_physicians[order(data_physicians$year, data_physicians$state), ]
write.table(data_physicians, file = "data/cofactors/data-physicians", quote = F, row.names = F, sep = "\t")

#### Age ####

## Data on the median age of the populations was obtained a similar way that
## data for the household size and income. The file was thus made by hand.
data_age <- read.table("data/cofactors/data-age", header = T, sep = "\t")


## [1] https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/population-change-data-table.pdf
## [2] https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
## [3] https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
## [4] https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt
## [5] https://www.ncei.noaa.gov/support/access-data-service-api-user-documentation
## [6] https://www2.census.gov/programs-surveys/acs/summary_file/
## [7] https://www.cdc.gov/vaccines/imz-managers/coverage/childvaxview/
## [8] https://www.bls.gov/opub/ted/2020/number-of-hospitals-and-hospital-employment-in-each-state-in-2019.htm
## [9] https://www.bls.gov/cpi/data.htm
## [10] https://www.cdc.gov/nchs/data/hus/2020-2021/DocSt.pdf