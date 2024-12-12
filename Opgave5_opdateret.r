library(restatapi)
library(dplyr)
library(tidyr)
library(ggplot2)

# Retrival - external
allTabs <- get_eurostat_toc()


#Søg for variabler i title
#colnames(allTabs)
#test <- allTabs %>% select(c(title,code)) %>% filter(str_detect(title,"household")) %>%
  #filter(str_detect(title,"consumption"))

#Find metadata
chMeta <- get_eurostat_dsd("namq_10_fcs")
unique(chMeta$concept)
chMeta %>% filter(concept=="geo") %>% select(code) %>% unique()

#Get data from eurostat
#first create list
myFilter <- list(
              geo=c("DK","BE","NE","SE","AT","DE","FR","IT","ES"),
              na_item="P31_S14", #Final consumption expenditure of households
              unit="CP_MEUR", #Current prices, million euro
              s_adj="SCA" #Seasonally and calendar adjusted data
)

chDF <- get_eurostat_data("namq_10_fcs",
                          filters = myFilter,
                          date_filter = "1999":"2024"
                          )
chDF$pct_growth <- NA

for (i in 5:nrow(chDF)) {
  
  current_geo <- chDF$geo[i -4]
  previous_geo <- chDF$geo[i]
  
  if (current_geo == previous_geo) {
    Værdi_1 <- chDF$values[i - 4]
    Værdi_2 <- chDF$values[i]
    
    # Beregn procentvis vækst
    pct_growth <- ((Værdi_2 - Værdi_1) / Værdi_1) * 100
    
    # Debugging: Print værdierne og pct_growth
    cat("Current Value:", Værdi_1, "Future Value:", Værdi_2, "Pct Growth:", pct_growth, "\n")
    
    chDF$pct_growth[i] <- pct_growth
  }
}



#=================================================================#
#Opgave 2

#Frasortering af data
chDF <- chDF[,c(-2,-3,-4,-6)]

chDF_wide <- pivot_wider(chDF, 
                       names_from = time,
                       values_from = pct_growth)

chDF_wide$Mean <- NA

for (i in 1:nrow(chDF_wide)) {
  mean_værdi <- mean(as.numeric(chDF_wide[i, 6:103]), na.rm = TRUE)  
  chDF_wide$Mean[i] <- mean_værdi  
}

meandf <- chDF_wide[,c(1,104)]

ggplot(meandf, aes(x = reorder(geo, Mean), y = Mean, fill = Mean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = geo), vjust = 0.5, hjust = -0.2, color = "black") +  # Tilføj landekoder som etiketter
  coord_flip() +  # Vend søjlediagrammet horisontalt
  labs(title = "Spanien har haft den største gennemsnitlige realvækst i perioden 2000q1 - 2024q2", x = "Landekode", y = "Gennemsnitsvækst") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Grøn gradient
  theme_minimal()

#==================================================================#
#Opgave 3

#Fjern coronakrisen og find gennemsnit fra 2000 til 2024 igen
#Definere coronakrisen fra q1 2020 til q3 2021

chDF_corona <- chDF_wide[,c(1,6:85,93:103)]

chDF_corona$Mean <- NA

for (i in 1:nrow(chDF_corona)) {
  mean_værdi <- mean(as.numeric(chDF_corona[i, 2:92]), na.rm = TRUE)  
  chDF_corona$Mean[i] <- mean_værdi  
}

meandf_corona <- chDF_corona[,c(1,93)]
meandf_corona$mean_fuld_periode <- meandf$Mean
meandf_corona$ændring <- NA

for (i in 1:nrow(meandf_corona)){
  forskel <- meandf_corona[i,2]-meandf[i,2]
  meandf_corona$ændring[i] <- forskel
}

meandf_corona$ændring <- as.numeric(meandf_corona$ændring)

ggplot(meandf_corona, aes(x = reorder(geo, ændring), y = ændring, fill = ændring)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(ændring, 2), sep = ": ")), 
            vjust = -0.2, hjust = -0.2, color = "black") +
  coord_flip() +
  labs(title = "Sverige i tilbagegang, trods fremgang hos naboerne - 2000-Q1 - 2024-Q2", 
       x = "Landekode", y = "Gennemsnitsvækst") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()
#==================================================================#
#Opgave 4

#Størst fald i coronaperioden 2020-2024

Alle_lande <- list(
  geo=c("*"),
  na_item="P31_S14",
  unit="CP_MEUR",
  s_adj="SCA"
)

AlleDF <- get_eurostat_data("namq_10_fcs",
                          filters = Alle_lande,
                          date_filter = "2019":"2024"
)

AlleDF$pct_growth <- NA

for (i in 5:nrow(AlleDF)) {
  
  current_geo <- AlleDF$geo[i -4]
  previous_geo <- AlleDF$geo[i]
  
  if (current_geo == previous_geo) {
    Værdi_1 <- AlleDF$values[i - 4]
    Værdi_2 <- AlleDF$values[i]
    
    # Beregn procentvis vækst
    pct_growth <- ((Værdi_2 - Værdi_1) / Værdi_1) * 100
    
    # Debugging: Print værdierne og pct_growth
    cat("Current Value:", Værdi_1, "Future Value:", Værdi_2, "Pct Growth:", pct_growth, "\n")
    
    AlleDF$pct_growth[i] <- pct_growth
  }
}

AlleDF <- AlleDF[,c(-2,-3,-4,-6)]

AlleDF_wide <- pivot_wider(AlleDF, 
                         names_from = time,
                         values_from = pct_growth)

AlleDF_wide <- AlleDF_wide[-32,c(1,6:23)]

AlleDF_wide$Mean <- NA

for (i in 1:nrow(AlleDF_wide)){
  gennemsnit <- mean(as.numeric(AlleDF_wide[i,2:19]))
  AlleDF_wide$Mean[i] <- gennemsnit
}

Gennemsnitfald <- AlleDF_wide[,c(1,20)]

ggplot(Gennemsnitfald, aes(x = reorder(geo, Mean), y = Mean, fill = Mean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = geo), vjust = 0.5, hjust = -0.2, color = "black") +  # Tilføj landekoder som etiketter
  coord_flip() +  # Vend søjlediagrammet horisontalt
  labs(title = "Norge har haft den mindste gennemsnitlige realvækst i perioden 2020q1 - 2024q2", x = "Landekode", y = "Gennemsnitsvækst") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Grøn gradient
  theme_minimal()

#__________________________________________________________________________#
#Fortolkning på opgaven

AlleCorona <- AlleDF_wide[,1:8]

AlleCorona$Mean <- NA

for (i in 1:nrow(AlleCorona)){
  gennemsnit <- mean(as.numeric(AlleCorona[i,2:8]))
  AlleCorona$Mean[i] <- gennemsnit
}

GennemsnitCorona <- AlleCorona[,c(1,9)]

ggplot(GennemsnitCorona, aes(x = reorder(geo, Mean), y = Mean, fill = Mean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = geo), vjust = 0.5, hjust = -0.2, color = "black") +  # Tilføj landekoder som etiketter
  coord_flip() +  # Vend søjlediagrammet horisontalt
  labs(title = "Grækenland har haft den mindste gennemsnitlige realvækst i perioden 2020q1 - 2021q3 (Coronaperioden)", x = "Landekode", y = "Gennemsnitsvækst") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Grøn gradient
  theme_minimal()
