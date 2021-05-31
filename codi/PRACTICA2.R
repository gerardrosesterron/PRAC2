#Les dades provenen de kaggle.com.
#https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report.csv
#https://www.kaggle.com/statchaitya/countrycontinent


# Carrega de dades desde la carpeta en local.
whr_df <- read.csv("world-happiness-report.csv")
countries <-read.csv("countryContinent.csv")


# Consolidació de dades en un únic joc de dades.
data <- merge(x = whr_df, y = countries, by.x ="Country.name", by.y = "country", all.x = TRUE)


# Selecció de columnes.
data <- data[,c(1:11,16,17)]


# Correccions manuals per eliminar els valors nulls de continents i sub_regions
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


# Ordenament de les columnes.
data <- data[,c(1,13,12,2,4,5,6,7,8,9,10,11,3)]


# Canvi de nom de les columnes.
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


# Tractament de valors nulls. Substitució de valors nulls per la mitjana agrupada per pais.
library(dplyr)
data_without_nulls <- data %>% group_by(`Country`) %>% 
  mutate(`GDP per capita` = ifelse(is.na(`GDP per capita`), mean(`GDP per capita`, na.rm=TRUE),`GDP per capita`),
         `Social support` = ifelse(is.na(`Social support`), mean(`Social support`, na.rm=TRUE),`Social support`),
         `Healthy life expectancy at birth` = ifelse(is.na(`Healthy life expectancy at birth`), mean(`Healthy life expectancy at birth`, na.rm=TRUE),`Healthy life expectancy at birth`),
         `Freedom to make life choices` = ifelse(is.na(`Freedom to make life choices`), mean(`Freedom to make life choices`, na.rm=TRUE),`Freedom to make life choices`),
         `Generosity` = ifelse(is.na(`Generosity`), mean(`Generosity`, na.rm=TRUE),`Generosity`),
         `Perceptions of corruption` = ifelse(is.na(`Perceptions of corruption`), mean(`Perceptions of corruption`, na.rm=TRUE),`Perceptions of corruption`),
         `Positve affect` = ifelse(is.na(`Positve affect`), mean(`Positve affect`, na.rm=TRUE),`Positve affect`),
         `Negative affect` = ifelse(is.na(`Negative affect`), mean(`Negative affect`, na.rm=TRUE),`Negative affect`)
         )

# Per alguns casos on no existeix cap valor i per tant no es possible aplicar el metode anterior.
# Es procedeix a eliminar les files que contenen valors nulls.
data_without_nulls <- data_without_nulls[complete.cases(data_without_nulls),]


# Tractament de valors extrems.
# Analisi de les dades numèriques. es seleccionen només els valors numerics.
data_numerical_without_nulls <- data_without_nulls[c(5:13)]

# Considerem 3 desviacions tipiques per considerar un valor com a valor extrem.
# Funció que dona la quantitat de valors extrems amb més de 3 desviacions típiques per un donat dataframe
ourliers_function <- function(df){
  qty_outliers_3std <- vector()
  for(i in 1:ncol(df)) {
    qty_outliers_3std <- c(qty_outliers_3std, sum(abs(scale(df[,i])) > 3))
  }
  df_out <- as.data.frame(cbind(colnames(df),qty_outliers_3std))
  names(df_out)[names(df_out) == "V1"] <- "Atributs numerics"
  return(df_out)
}

# Crida de la funció ourliers_function().
qty_outliers <- ourliers_function(data_numerical_without_nulls)


# Es comproba que existeixen valors extrems per aquest joc de dades. De totes maneres com que els països son molt diversos,
# i en aquest joc de dades estan tots inclosos es decideix mantenir tots els registres.


# Analisi de les dades.
summary(data_without_nulls)
str(data_without_nulls,give.attr = FALSE)


#############################################################################################################


# Comprobació de la normalitat dels atributs.
apply(data_numerical, 2, shapiro.test)

shapiro.test(data_without_nulls$`GDP per capita`)
plot(density(data_without_nulls$`GDP per capita`))

shapiro.test(data_without_nulls$`Social support`)
plot(density(data_without_nulls$`Social support`))
qqnorm(data_without_nulls$`Social support`);qqline(data_without_nulls$`Social support`, col = 2)

ks.test(data_without_nulls$`Social support`, pnorm, mean=mean(data_without_nulls$`Social support`), sd=sd(data_without_nulls$`Social support`))

ks.test(data_without_nulls$`Social support`, pnorm,)

shapiro.test(data_without_nulls$`Healthy life expectancy at birth`)
plot(density(data_without_nulls$`Healthy life expectancy at birth`))

shapiro.test(data_without_nulls$`Freedom to make life choices`)
plot(density(data_without_nulls$`Freedom to make life choices`))

shapiro.test(data_without_nulls$Generosity)
plot(density(data_without_nulls$Generosity))

shapiro.test(data_without_nulls$`Perceptions of corruption`)
plot(density(data_without_nulls$`Perceptions of corruption`))

shapiro.test(data_without_nulls$`Positve affect`)
plot(density(data_without_nulls$`Positve affect`))

shapiro.test(data_without_nulls$`Negative affect`)
plot(density(data_without_nulls$`Negative affect`))

shapiro.test(data_without_nulls$`Happiness rate`)
plot(density(data_without_nulls$`Happiness rate`))




shapiro.test(data_without_nulls$`GDP per capita`[data_without_nulls$Country=='Italy'])
plot(density(data_without_nulls$`GDP per capita`[data_without_nulls$Country=='Italy']))

shapiro.test(data_without_nulls$`GDP per capita`[data_without_nulls$Region=='South America'])
plot(density(data_without_nulls$`GDP per capita`[data_without_nulls$Region=='South America']))


shapiro.test(data_numerical$`Social support`)
plot(density(data_numerical$`Social support`))

shapiro.test(data_without_nulls$`Healthy life expectancy at birth`[data_without_nulls$Country=='Sweden' | data_without_nulls$Country=='Norway'])
plot(density(data_without_nulls$`Healthy life expectancy at birth`[data_without_nulls$Country=='Sweden' | data_without_nulls$Country=='Norway']))




# Seleccio d'atributs per processar correlacions
data_without_nulls <- data_without_nulls[,c(4:13)]


#Correlacions
#install.packages("Hmisc")
#install.packages(("ggplot2"))
#install.packages("rlang")
library("rlang")
library("ggplot2")
library("Hmisc")


#res <- rcorr(as.matrix(data),type="spearman")
correlation <-rcorr(as.matrix(data_without_nulls),type = "spearman")
print(correlation)

#install.packages("corrplot")
library("corrplot")

corrplot(cor(data_without_nulls), method = "number", type="upper", tl.col = "black", tl.cex= .6, number.cex = .7, title="Correlacions")

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(data_without_nulls, histogram=TRUE, pch=19)
