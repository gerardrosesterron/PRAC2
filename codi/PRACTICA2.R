#Les dades provenen de kaggle.com.
#https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report.csv
#https://www.kaggle.com/statchaitya/countrycontinent


# Carrega de dades desde la carpeta en local.
whr_df <- read.csv("world-happiness-report.csv", fileEncoding="UTF-8-BOM")
countries <-read.csv("countryContinent.csv")


# Consolidació de dades en un únic joc de dades.
data <- merge(x = whr_df, y = countries, by.x ="Country.name", by.y = "country", all.x = TRUE)


# Selecció de columnes. (S'eliminen les dades que no ens interessen dels països. Per exempple el ISO code)
data <- data[,c(1:11,16,17)]


# Correccions manuals per eliminar els valors nuls de continents i sub_regions.
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


# Tornem a ordenar les columnes.
data <- data[,c(1,13,12,2,4,5,6,7,8,9,10,11,3)]


# Canvi de nom de les columnes.
names(data)[names(data) == "Country.name"] <- "Country"
names(data)[names(data) == "sub_region"] <- "Region"
names(data)[names(data) == "continent"] <- "Continent"
names(data)[names(data) == "year"] <- "Year"
names(data)[names(data) == "Log.GDP.per.capita"] <- "GDP"
names(data)[names(data) == "Social.support"] <- "Social"
names(data)[names(data) == "Healthy.life.expectancy.at.birth"] <- "Health_birth"
names(data)[names(data) == "Freedom.to.make.life.choices"] <- "Freedom"
names(data)[names(data) == "Perceptions.of.corruption"] <- "Corruption"
names(data)[names(data) == "Positive.affect"] <- "Positve_affect"
names(data)[names(data) == "Negative.affect"] <- "Negative_affect"
names(data)[names(data) == "Life.Ladder"] <- "Happiness_rate"


# Funció que dona la quantitat de valors nulls per un donat dataframe.
nulls_function <- function(df){
  qty_nulls <- vector()
  for(i in 1:ncol(df)) {
    qty_nulls <- c(qty_nulls, sum(is.na(df[,i])))
  }
  
  df_out <- as.data.frame(cbind(colnames(df),qty_nulls))
  names(df_out)[names(df_out) == "V1"] <- "Atributs"
  df_out$qty_nulls <- as.numeric(as.character(df_out$qty_nulls))
  return(df_out)
}


# Crida de la funció nulls_function().
qty_nulls_before_treatment <- nulls_function(data)


# Tractament de valors nulls. Substitució de valors nulls per la mitjana agrupada per país.
library(dplyr)
data_without_nulls_AVG.imp <- data %>% group_by(`Country`) %>% 
  mutate(`GDP` = ifelse(is.na(`GDP`), mean(`GDP`, na.rm=TRUE),`GDP`),
         `Social` = ifelse(is.na(`Social`), mean(`Social`, na.rm=TRUE),`Social`),
         `Health_birth` = ifelse(is.na(`Health_birth`), mean(`Health_birth`, na.rm=TRUE),`Health_birth`),
         `Freedom` = ifelse(is.na(`Freedom`), mean(`Freedom`, na.rm=TRUE),`Freedom`),
         `Generosity` = ifelse(is.na(`Generosity`), mean(`Generosity`, na.rm=TRUE),`Generosity`),
         `Corruption` = ifelse(is.na(`Corruption`), mean(`Corruption`, na.rm=TRUE),`Corruption`),
         `Positve_affect` = ifelse(is.na(`Positve_affect`), mean(`Positve_affect`, na.rm=TRUE),`Positve_affect`),
         `Negative_affect` = ifelse(is.na(`Negative_affect`), mean(`Negative_affect`, na.rm=TRUE),`Negative_affect`)
  )


# Crida de la funció nulls_function() per coprobar si existeixen valors nuls.
qty_nulls_after_treatment_AVG.imp <- nulls_function(data_without_nulls_AVG.imp)


# Es pot comprobar que es redueix la quantitat de nulls de 373 a 106.
print(paste("Abans del tractament hi han: ",sum(qty_nulls_before_treatment$qty_nulls), "valors nulls."))
print(paste("Després del tractament hi han: ",sum(qty_nulls_after_treatment_AVG.imp$qty_nulls), "valors nulls."))


# S'ha reduït el nombre de nulls però no ha estat possible eliminar tots els valors nulls.
# Perque per alguns casos on no existeix cap valor i per tant no es possible aplicar el metode anterior.
# Es procedeix a eliminar les files que contenen valors nulls.
data_without_nulls_AVG.imp <- data_without_nulls_AVG.imp[complete.cases(data_without_nulls_AVG.imp),]


# Tractament de valors nulls amb KNN de la libreria VIM.
library(VIM)
data_without_nulls_KNN.imp <- kNN(data)


# Crida de la funció nulls_function() per coprobar si existeixen valors nulls.
qty_nulls_after_treatment_KNN.imp <- nulls_function(data_without_nulls_KNN.imp)


# Es pot comprobar que es redueix la quantitat de nulls de 373 a 0.
print(paste("Abans del tractament hi han: ",sum(qty_nulls_before_treatment$qty_nulls), "valors nulls."))
print(paste("Després del tractament hi han: ",sum(qty_nulls_after_treatment_KNN.imp$qty_nulls), "valors nulls."))


# Selecció de les columnes amb valors ignorant així les etiquetes creades per KNN().
data_without_nulls_KNN.imp <- data_without_nulls_KNN.imp[,c(1:13)]


# Comparació dels dos mètodes
summary(data_without_nulls_AVG.imp)
summary(data_without_nulls_KNN.imp)


# S'ha assolit l'objectiu d'eliminar tots els valors nulls amb els dos mètodes
# Amb KNN() es mantenen el nombre de registres. Mentre que pel mètode de la mitjana es perdien 71 registres.
# Amb l'objectgiu de mantenir la majoria d'informació es tria el mètode de KNN() per la resta de l'exercici.


# Tractament de valors extrems.
# Analisi de les dades numèriques. es seleccionen només els valors numerics.
data_without_nulls_KNN.imp_numerical <- data_without_nulls_KNN.imp[c(5:13)]


# Considerem 3 desviacions típiques per considerar un valor com a valor extrem.
# Funció que dona la quantitat de valors extrems per columna amb més de 3 desviacions típiques per un donat dataframe.
ourliers_function <- function(df){
  qty_outliers_3std <- vector()
  for(i in 1:ncol(df)) {
    qty_outliers_3std <- c(qty_outliers_3std, sum(abs(scale(df[,i])) > 3))
  }
  df_out <- as.data.frame(cbind(colnames(df),qty_outliers_3std))
  names(df_out)[names(df_out) == "V1"] <- "Atributs numerics"
  df_out$qty_outliers_3std <- as.numeric(as.character(df_out$qty_outliers_3std))
  return(df_out)
}


# Crida de la funció ourliers_function().
qty_outliers <- ourliers_function(data_without_nulls_KNN.imp_numerical)


# La quantitat total de valors extrems (mes de 3 desviacions tipiques). 
print(paste("Abans del tractament hi han: ",sum(qty_outliers$qty_outliers_3std), "valors extrems"))


# Es comproba que existeixen 96 valors extrems per aquest joc de dades. De totes maneres com que els països son molt diversos,
# i en aquest joc de dades estan tots inclosos es decideix mantenir tots els registres.


#Desem el conjunt de dades inicial, abans d'afegir noves variables per utilitzar-lo després en la regressió lineal
initial_dataset_KNN <- data_without_nulls_KNN.imp


#Comprovem els valors únics de les variables categòriques:
unique(data_without_nulls_KNN.imp$Country)
unique(data_without_nulls_KNN.imp$Continent)
unique(data_without_nulls_KNN.imp$Region)


# Creem una nova columna que indiqui si el valor de happiness és low o high

data_without_nulls_KNN.imp$Happiness <- ifelse(data_without_nulls_KNN.imp$Happiness_rate >= 0 & data_without_nulls_KNN.imp$Happiness_rate <= 4, 0,
                                               ifelse(data_without_nulls_KNN.imp$Happiness_rate >=5 & data_without_nulls_KNN.imp$Happiness_rate <=8, 1, 2))


#Creem una nova columna que indiqui si les dades són d'abans o de despres del 2010
data_without_nulls_KNN.imp$Years <- ifelse(data_without_nulls_KNN.imp$Year >= 2004 & data_without_nulls_KNN.imp$Year < 2010, "Before 2010",
                                           ifelse(data_without_nulls_KNN.imp$Year >="2010" & data_without_nulls_KNN.imp$Year <=2050, "2010 or after", 2))


#Factoritzem Continent, Region, Year i Years
data_without_nulls_KNN.imp$Continent <- as.factor(data_without_nulls_KNN.imp$Continent)
data_without_nulls_KNN.imp$Region <- as.factor(data_without_nulls_KNN.imp$Region)
data_without_nulls_KNN.imp$Year <- as.factor(data_without_nulls_KNN.imp$Year)
data_without_nulls_KNN.imp$Years <- as.factor(data_without_nulls_KNN.imp$Years)


summary(data_without_nulls_KNN.imp)


#4.1 SELECCIÓ GRUPS DE DADES. 


#Agrupem les dades per Continent ja que es necessitarà pel contrast d'hipòtesis
levels(data_without_nulls_KNN.imp$Continent)
data.Africa <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Continent == "Africa",]
data.Americas <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Continent == "Americas",]
data.Asia <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Continent == "Asia",]
data.Europe <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Continent == "Europe",]
data.Oceania <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Continent == "Oceania",]


#Agrupem les dades segons siguin abans o despres del 2010
data.before_2010 <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Years == "Before 2010",]
data.after_2010 <- data_without_nulls_KNN.imp[data_without_nulls_KNN.imp$Years == "2010 or after",]


#2.4.2. COMPROVACIÓ DE LA NORMALITAT I HOMOGENEÏTAT DE LA VARIANÇA


#Comprovació de la normalització dels atributs: 
#Atributs normalitzats: valors de p superior a 0.05
shapiro.test(data_without_nulls_KNN.imp$GDP)
shapiro.test(data_without_nulls_KNN.imp$Social)
shapiro.test(data_without_nulls_KNN.imp$Health_birth)
shapiro.test(data_without_nulls_KNN.imp$Freedom)
shapiro.test(data_without_nulls_KNN.imp$Generosity)
shapiro.test(data_without_nulls_KNN.imp$Corruption)
shapiro.test(data_without_nulls_KNN.imp$Positve_affect)
shapiro.test(data_without_nulls_KNN.imp$Negative_affect)
shapiro.test(data_without_nulls_KNN.imp$Happiness)


#Comprovació de la homogeneïtat de la variança:
#test de Fligner-Killeen


# Hipotesi nulla amb un nivell de significança alpha = 0.05; si el valor p és superior a alpha, les variançes son igual, per tant hi ha homogeneïtat.
library(car)

fligner.test(Happiness ~ GDP, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Social, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Health_birth, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Freedom, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Generosity, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Corruption, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Positve_affect, data = data_without_nulls_KNN.imp)
fligner.test(Happiness ~ Negative_affect, data = data_without_nulls_KNN.imp)
#No Homogenies: Totes excepte Positive_affect


# 4.3 CONTRAST D'HIPOTESI


# Plantegem el contrast paramètric d’hipòtests de dos mostres sobre la diferència de les mitjes:  
# HIP1: mu_2009_Afr - mu_2009_Eur = 0  // HIP2:  mu_2009Afri - mu_2019Eur < 0

levels (data_without_nulls_KNN.imp$Continent)

data_happiness_Eur <- data.Europe$Happiness
data_happiness_Afr <- data.Africa$Happiness

t.test(data_happiness_Afr,  data_happiness_Eur, alternative = "less")


data_happiness_Amr <- data.Americas$Happiness
data_happiness_Afr <- data.Africa$Happiness

t.test(data_happiness_Afr,  data_happiness_Amr, alternative = "less")


data_happiness_Asia <- data.Asia$Happiness
data_happiness_Afr <- data.Africa$Happiness

t.test(data_happiness_Afr,  data_happiness_Asia, alternative = "less")


data_happiness_Oce <- data.Oceania$Happiness
data_happiness_Afr <- data.Africa$Happiness

t.test(data_happiness_Afr,  data_happiness_Oce, alternative = "less")


# Obtenim un p val menor que alpha (0.05), per tant rebutgem la hipòtesi nul·la i veiem que sí afecta negativament viure a Africa. A europa més hapiness. 


# CORRELATION MATRIX

res <- cor(data_without_nulls_KNN.imp[5:14])
round(res, 2)


# REGRESSIO

GDP = initial_dataset_KNN$GDP
Social = initial_dataset_KNN$Social
healthy = initial_dataset_KNN$Health_birth
Freedom = initial_dataset_KNN$Freedom
Positive_affect = initial_dataset_KNN$Positve_affect
Negative_affect = initial_dataset_KNN$Negative_affect
Corruption = initial_dataset_KNN$Corruption
Continent = initial_dataset_KNN$Continent
Year = initial_dataset_KNN$Year
Region = initial_dataset_KNN$Region


# Variable a predir

hap_rate = initial_dataset_KNN$Happiness_rate


model1 <- lm(hap_rate ~ Social  + Freedom + Region + Positive_affect + Year +  Continent + Negative_affect + Corruption, data = initial_dataset_KNN)
model2 <- lm(hap_rate ~ GDP + Social + Freedom + Region + Positive_affect + Year + Corruption, data = initial_dataset_KNN)
model3 <- lm(hap_rate ~ GDP + Freedom + Region + Positive_affect + Negative_affect, data = initial_dataset_KNN)


summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared


# El segon model és amb el que obtenim millors prediccions. 

newdata1 <- data.frame(GDP = 8 , Social = 0.7 , Freedom = 0.75 , Region = "Southern Europe", Positive_affect = 0.75, Year = 2010 , Corruption = 0.85 )
newdata2 <- data.frame(GDP = 10 , Social = 0.7 , Freedom = 0.75 , Region = "Southern Europe", Positive_affect = 0.75, Year = 2010 , Corruption = 0.85 )
newdata3 <- data.frame(GDP = 8 , Social = 0.7 , Freedom = 0.75 , Region = "Southern Europe", Positive_affect = 0.75, Year = 2019 , Corruption = 0.85 )
newdata4 <- data.frame(GDP = 10 , Social = 0.7 , Freedom = 0.75 , Region = "Southern Europe", Positive_affect = 0.75, Year = 2019 , Corruption = 0.85 )


predict(model2, newdata1)
predict(model2, newdata2)
predict(model2, newdata3)
predict(model2, newdata4)



# REPRESENTACIÓ DELS RESULTATS:

happiness_continent.plot <-ggplot(data_without_nulls_KNN.imp,aes(x = Continent,y = Happiness_rate, fill = Continent))
happiness_continent.plot + geom_bar(stat="identity", position="identity") 


gdp.plot  <-ggplot(data_without_nulls_KNN.imp,aes(x = GDP, y = Happiness_rate, color = Happiness_rate))
gdp.plot+geom_point()

social.plot  <-ggplot(data_without_nulls_KNN.imp,aes(x = Social,  y = Happiness_rate, color = Happiness_rate))
social.plot+geom_point()

healthy.plot  <-ggplot(data_without_nulls_KNN.imp,aes(x = healthy, y = Happiness_rate, color = Happiness_rate))
healthy.plot+geom_point()

generosity.plot  <-ggplot(data_without_nulls_KNN.imp,aes(x = Generosity, y = Happiness_rate, color = Happiness_rate))
generosity.plot+geom_point()


#Desem el conjunt de dades net:
write.csv(data_without_nulls_KNN.imp, row.names = TRUE, file = "dades_finals.csv")
