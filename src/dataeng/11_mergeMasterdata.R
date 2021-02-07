# Merge the dependent variable and all independent variables in a combined 
# dataframe
###############################################################################


# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "COUNTRYCODES2.csv")
COUNTRYCODES2 <- read.table(infile
                           , header = TRUE
                           , sep = "\t"
                           , quote = "\"'"
                           , dec = "."
                           , check.names = TRUE
                           , na.strings = "NA"
)

infile <- list(here("data", "interim", "FORAREA.CHANGE.csv"), #1
               here("data", "interim", "FAOCROPSMEADOWS.csv"), #2
               here("data", "interim", "CATTLEPERHA_edt.csv"), #3
               here("data", "interim", "POPULATION_edt.csv"), #4
               here("data", "interim", "BIOENERGYSHARE_edt.csv"), #5
               here("data", "interim", "FUELSHARE_edt.csv"), #6
               here("data", "raw", "SOLIDFUELUSE.csv"), #7
               here("data", "interim", "FOREXSHARE.csv"), #8
               here("data", "interim", "roads_paved_edt.csv"), #9
               here("data", "interim", "GDP_growth_edt.csv"), #10
               here("data", "raw", "TI_corruption_2012.csv") #11
)

data.list <- lapply(infile, read.table
                    , header = TRUE
                    , sep = "\t"
                    , quote = "\"'"
                    , dec = "."
                    , check.names = TRUE
)
str(data.list)


# Transform data ----------------------------------------------------------

# ISO3, COUNTRY, ANDIFF.00.10.rel
masterdata <- data.frame(ISO3 = data.list[[1]]$ISO3
                         , COUNTRY = data.list[[1]]$COUNTRY
                         , ANDIFF.00.10.rel = data.list[[1]]$ANDIFF.00.10.rel
                         )

# + CROPAREADIF.00.10
masterdata <-
  merge(masterdata, data.list[[2]][, c(1, 13)], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,4]))
table(is.na(data.list[[2]][,c(13)]))

# + MEADOWAREADIF.00.10
masterdata <-
  merge(masterdata, data.list[[2]][, c(1, 14)], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,5]))
table(is.na(data.list[[2]][,c(14)]))

# + CATT.DIFF.00.10.rel
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, COUNTRY = COUNTRYCODES2$FAOCOUNTRY)
aux.df <- merge(aux.df, data.list[[3]][,c(1,5)], by = "COUNTRY", all.x = TRUE)
masterdata <- merge(masterdata, aux.df[,-1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,6]))
table(is.na(data.list[[3]][,c(5)]))

# + POPDIF.00.10.rel + RURPOPDIF.00.10.rel + URBPOPDIF.00.10.rel
# + AGRIPOPDIF.00.10.rel
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, COUNTRYCODE = COUNTRYCODES2$FAOSTATCODE)
aux.df <-
  merge(aux.df, data.list[[4]][, c(2, 5, 6, 7, 8)], by = "COUNTRYCODE", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

# + BIOENDIFF.00.09.rel
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, COUNTRYCODE = COUNTRYCODES2$FAOSTATCODE)
aux.df <-
  merge(aux.df, data.list[[5]][, c(2, 6)], by = "COUNTRYCODE", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[, 11]))
table(is.na(data.list[[5]][, c(6)]))

# + FUELSHAREDIF.00.10
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, COUNTRY = COUNTRYCODES2$FAOCOUNTRY)
aux.df <-
  merge(aux.df, data.list[[6]][, c(1, 8)], by = "COUNTRY", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[, 12]))
table(is.na(data.list[[6]][, c(8)]))

# + SOLIDFUELUSE.2010 
# (Daten liegen fuer 2000 nicht vor, daher keine Differenzbetrachtung)
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, UNSDCODE = COUNTRYCODES2$UNSDNUMCODE)
aux.df <-
  merge(aux.df, data.list[[7]][, c(2, 3)], by = "UNSDCODE", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[, 13]))
table(is.na(data.list[[7]][, c(3)]))

# + FOREXSHAREDIF.00.10
masterdata <-
  merge(masterdata, data.list[[8]][, c(1, 9)], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,14]))
table(is.na(data.list[[8]][,c(9)]))

# + ROAD.CHANGE.00.10.rel
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, COUNTRYCODE = COUNTRYCODES2$ISO3)
aux.df <-
  merge(aux.df, data.list[[9]][, c(2, 14)], by = "COUNTRYCODE", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[, 15]))
table(is.na(data.list[[9]][, c(14)]))

# + GDPGROWTHAV.00.10
aux.df <-
  data.frame(ISO3 = COUNTRYCODES2$ISO3, Country.Code = COUNTRYCODES2$ISO3)
aux.df <-
  merge(aux.df, data.list[[10]][, c(2, 14)], by = "Country.Code", all.x = TRUE)
masterdata <-
  merge(masterdata, aux.df[, -1], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,16]))
table(is.na(data.list[[10]][,c(14)]))

# + CPI2012 (Daten liegen fuer 2000 nicht vor, daher keine Differenzbetrachtung)
masterdata <-
  merge(masterdata, data.list[[11]][, c(2, 3)], by = "ISO3", all.x = TRUE)

table(is.na(masterdata[,17]))
table(is.na(data.list[[11]][,c(3)]))


# Save data ---------------------------------------------------------------

outfile <- here("data", "processed", "MASTERDATA2.csv")
write.table(
  masterdata,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
