#Les dades provenen de kaggle.com
#https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report.csv
#https://www.kaggle.com/statchaitya/countrycontinent

# Carrega de dades desde la carpeta en local.
whr_df <- read.csv("world-happiness-report.csv")
countries <-read.csv("countryContinent.csv")

#
data <- merge(x = whr_df, y = countries, by.x ="Country.name", by.y = "country", all.x = TRUE)

# Selecció de columnes
data <- data[,c(1:11,16,17)]

# Valors nulls de continents i regions
summary(data$continent)
summary(data$sub_region)


# Correccions manuals per aliminar els valors nulls de continents i sub_regions
data$continent[data$Country.name=="Bolivia"] <- data$continent[data$Country.name=="Argentina"]
data$sub_region[data$Country.name=="Bolivia"] <- data$sub_region[data$Country.name=="Argentina"]
data$continent[data$Country.name=="Congo (Brazzaville)"] <- data$continent[data$Country.name=="Angola"]
data$sub_region[data$Country.name=="Congo (Brazzaville)"] <- data$sub_region[data$Country.name=="Angola"]
data$continent[data$Country.name=="Congo (Kinshasa)"] <- data$continent[data$Country.name=="Angola"]
data$sub_region[data$Country.name=="Congo (Kinshasa)"] <- data$sub_region[data$Country.name=="Angola"]
data$continent[data$Country.name=="Hong Kong S.A.R. of China"] <- data$continent[data$Country.name=="Japan"]
data$sub_region[data$Country.name=="Hong Kong S.A.R. of China"] <- data$sub_region[data$Country.name=="Japan"]
data$continent[data$Country.name=="Iran"] <- data$continent[data$Country.name=="Afghanistan"]
data$sub_region[data$Country.name=="Iran"] <- data$sub_region[data$Country.name=="Afghanistan"]
data$continent[data$Country.name=="Ivory Coast"] <- data$continent[data$Country.name=="Ghana"]
data$sub_region[data$Country.name=="Ivory Coast"] <- data$sub_region[data$Country.name=="Ghana"]
data$continent[data$Country.name=="Kosovo"] <- data$continent[data$Country.name=="Croatia"]
data$sub_region[data$Country.name=="Kosovo"] <- data$sub_region[data$Country.name=="Croatia"]
data$continent[data$Country.name=="Laos"] <- data$continent[data$Country.name=="Thailand"]
data$sub_region[data$Country.name=="Laos"] <- data$sub_region[data$Country.name=="Thailand"]
data$continent[data$Country.name=="Moldova"] <- data$continent[data$Country.name=="Ukraine"]
data$sub_region[data$Country.name=="Moldova"] <- data$sub_region[data$Country.name=="Ukraine"]
data$continent[data$Country.name=="North Cyprus"] <- data$continent[data$Country.name=="Cyprus"]
data$sub_region[data$Country.name=="North Cyprus"] <- data$sub_region[data$Country.name=="Cyprus"]
data$continent[data$Country.name=="North Macedonia"] <- data$continent[data$Country.name=="Greece"]
data$sub_region[data$Country.name=="North Macedonia"] <- data$sub_region[data$Country.name=="Greece"]
data$continent[data$Country.name=="Palestinian Territories"] <- data$continent[data$Country.name=="Israel"]
data$sub_region[data$Country.name=="Palestinian Territories"] <- data$sub_region[data$Country.name=="Israel"]
data$continent[data$Country.name=="Russia"] <- data$continent[data$Country.name=="Kazakhstan"]
data$sub_region[data$Country.name=="Russia"] <- data$sub_region[data$Country.name=="Kazakhstan"]
data$continent[data$Country.name=="Somaliland region"] <- data$continent[data$Country.name=="Somalia"]
data$sub_region[data$Country.name=="Somaliland region"] <- data$sub_region[data$Country.name=="Somalia"]
data$continent[data$Country.name=="South Korea"] <- data$continent[data$Country.name=="Japan"]
data$sub_region[data$Country.name=="South Korea"] <- data$sub_region[data$Country.name=="Japan"]
data$continent[data$Country.name=="Syria"] <- data$continent[data$Country.name=="Iraq"]
data$sub_region[data$Country.name=="Syria"] <- data$sub_region[data$Country.name=="Iraq"]
data$continent[data$Country.name=="Taiwan Province of China"] <- data$continent[data$Country.name=="China"]
data$sub_region[data$Country.name=="Taiwan Province of China"] <- data$sub_region[data$Country.name=="China"]
data$continent[data$Country.name=="Tanzania"] <- data$continent[data$Country.name=="Kenya"]
data$sub_region[data$Country.name=="Tanzania"] <- data$sub_region[data$Country.name=="Kenya"]
data$continent[data$Country.name=="United Kingdom"] <- data$continent[data$Country.name=="Ireland"]
data$sub_region[data$Country.name=="United Kingdom"] <- data$sub_region[data$Country.name=="Ireland"]
data$continent[data$Country.name=="United States"] <- data$continent[data$Country.name=="Canada"]
data$sub_region[data$Country.name=="United States"] <- data$sub_region[data$Country.name=="Canada"]
data$continent[data$Country.name=="Venezuela"] <- data$continent[data$Country.name=="Peru"]
data$sub_region[data$Country.name=="Venezuela"] <- data$sub_region[data$Country.name=="Peru"]
data$continent[data$Country.name=="Vietnam"] <- data$continent[data$Country.name=="Japan"]
data$sub_region[data$Country.name=="Vietnam"] <- data$sub_region[data$Country.name=="Japan"]
