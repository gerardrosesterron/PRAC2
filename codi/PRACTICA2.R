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
data$continent[data$Country.name=="Bolivia"] <- data$continent[data$Country.name=="Argentina"][1]
data$sub_region[data$Country.name=="Bolivia"] <- data$sub_region[data$Country.name=="Argentina"][1]
data$continent[data$Country.name=="Congo (Brazzaville)"] <- data$continent[data$Country.name=="Angola"][1]
data$sub_region[data$Country.name=="Congo (Brazzaville)"] <- data$sub_region[data$Country.name=="Angola"][1]
data$continent[data$Country.name=="Congo (Kinshasa)"] <- data$continent[data$Country.name=="Angola"][1]
data$sub_region[data$Country.name=="Congo (Kinshasa)"] <- data$sub_region[data$Country.name=="Angola"][1]
data$continent[data$Country.name=="Hong Kong S.A.R. of China"] <- data$continent[data$Country.name=="Japan"][1]
data$sub_region[data$Country.name=="Hong Kong S.A.R. of China"] <- data$sub_region[data$Country.name=="Japan"][1]
data$continent[data$Country.name=="Iran"] <- data$continent[data$Country.name=="Afghanistan"][1]
data$sub_region[data$Country.name=="Iran"] <- data$sub_region[data$Country.name=="Afghanistan"][1]
data$continent[data$Country.name=="Ivory Coast"] <- data$continent[data$Country.name=="Ghana"][1]
data$sub_region[data$Country.name=="Ivory Coast"] <- data$sub_region[data$Country.name=="Ghana"][1]
data$continent[data$Country.name=="Kosovo"] <- data$continent[data$Country.name=="Croatia"][1]
data$sub_region[data$Country.name=="Kosovo"] <- data$sub_region[data$Country.name=="Croatia"][1]
data$continent[data$Country.name=="Laos"] <- data$continent[data$Country.name=="Thailand"][1]
data$sub_region[data$Country.name=="Laos"] <- data$sub_region[data$Country.name=="Thailand"][1]
data$continent[data$Country.name=="Moldova"] <- data$continent[data$Country.name=="Ukraine"][1]
data$sub_region[data$Country.name=="Moldova"] <- data$sub_region[data$Country.name=="Ukraine"][1]
data$continent[data$Country.name=="North Cyprus"] <- data$continent[data$Country.name=="Cyprus"][1]
data$sub_region[data$Country.name=="North Cyprus"] <- data$sub_region[data$Country.name=="Cyprus"][1]
data$continent[data$Country.name=="North Macedonia"] <- data$continent[data$Country.name=="Greece"][1]
data$sub_region[data$Country.name=="North Macedonia"] <- data$sub_region[data$Country.name=="Greece"][1]
data$continent[data$Country.name=="Palestinian Territories"] <- data$continent[data$Country.name=="Israel"][1]
data$sub_region[data$Country.name=="Palestinian Territories"] <- data$sub_region[data$Country.name=="Israel"][1]
data$continent[data$Country.name=="Russia"] <- data$continent[data$Country.name=="Kazakhstan"][1]
data$sub_region[data$Country.name=="Russia"] <- data$sub_region[data$Country.name=="Kazakhstan"][1]
data$continent[data$Country.name=="Somaliland region"] <- data$continent[data$Country.name=="Somalia"][1]
data$sub_region[data$Country.name=="Somaliland region"] <- data$sub_region[data$Country.name=="Somalia"][1]
data$continent[data$Country.name=="South Korea"] <- data$continent[data$Country.name=="Japan"][1]
data$sub_region[data$Country.name=="South Korea"] <- data$sub_region[data$Country.name=="Japan"][1]
data$continent[data$Country.name=="Syria"] <- data$continent[data$Country.name=="Iraq"][1]
data$sub_region[data$Country.name=="Syria"] <- data$sub_region[data$Country.name=="Iraq"][1]
data$continent[data$Country.name=="Taiwan Province of China"] <- data$continent[data$Country.name=="China"][1]
data$sub_region[data$Country.name=="Taiwan Province of China"] <- data$sub_region[data$Country.name=="China"][1]
data$continent[data$Country.name=="Tanzania"] <- data$continent[data$Country.name=="Kenya"][1]
data$sub_region[data$Country.name=="Tanzania"] <- data$sub_region[data$Country.name=="Kenya"][1]
data$continent[data$Country.name=="United Kingdom"] <- data$continent[data$Country.name=="Ireland"][1]
data$sub_region[data$Country.name=="United Kingdom"] <- data$sub_region[data$Country.name=="Ireland"][1]
data$continent[data$Country.name=="United States"] <- data$continent[data$Country.name=="Canada"][1]
data$sub_region[data$Country.name=="United States"] <- data$sub_region[data$Country.name=="Canada"][1]
data$continent[data$Country.name=="Venezuela"] <- data$continent[data$Country.name=="Peru"][1]
data$sub_region[data$Country.name=="Venezuela"] <- data$sub_region[data$Country.name=="Peru"][1]
data$continent[data$Country.name=="Vietnam"] <- data$continent[data$Country.name=="Japan"][1]
data$sub_region[data$Country.name=="Vietnam"] <- data$sub_region[data$Country.name=="Japan"][1]

# Ordenament de les columnes
data <- data[,c(1,13,12,2,4,5,6,7,8,9,10,11,3)]

# Canvi de noms de les columnes
names(data)[names(data) == "Country.name"] <- "Country"
names(data)[names(data) == "sub_region"] <- "Region"
names(data)[names(data) == "continent"] <- "Continent"
names(data)[names(data) == "year"] <- "Year"
names(data)[names(data) == "Log.GDP.per.capita"] <- "GDP per capita"
names(data)[names(data) == "Social.support"] <- "Social support"
names(data)[names(data) == "Healthy.life.expectancy.at.birth"] <- "Healthy life expectancy at birth"
names(data)[names(data) == "Freedom.to.make.life.choices"] <- "Freedom to make life choices"
names(data)[names(data) == "Perceptions.of.corruption"] <- "Perceptions of corruption"
names(data)[names(data) == "Positive.affect"] <- "Positve affect"
names(data)[names(data) == "Negative.affect"] <- "Negative affect"
names(data)[names(data) == "Life.Ladder"] <- "Happiness rate"

# Visio generalitzada del joc de dades
summary(data)
str(data)

# Tractament de valors nulls
data_not_nulls <- data[complete.cases(data),c(4:13)]


#Correlacions
#install.packages("Hmisc")
#install.packages(("ggplot2"))
#install.packages("rlang")
library("rlang")
library("ggplot2")
library("Hmisc")


#res <- rcorr(as.matrix(data),type="spearman")
correlation <-rcorr(as.matrix(data_not_nulls),type = "spearman")
print(correlation)

#install.packages("corrplot")
library("corrplot")

corrplot(cor(data_not_nulls), method = "number", type="upper", tl.col = "black", tl.cex= .6, number.cex = .7, title="Correlacions")

install.packages("PerformanceAnalytics")
#library("PerformanceAnalytics")
chart.Correlation(data_not_nulls, histogram=TRUE, pch=19)
