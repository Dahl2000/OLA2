###########2,1###################

# Indlæs de nødvendige pakker
library(dkstat)
library(danstat)
library(devtools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

###################################################################################
### OPG. 2.1 – Opdatering af DI’s forbrugertillidsindikator ###
####################

# Hent metadata via API
meta_data <- dst_meta("FORV1")
str(meta_data)

# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Filtrer data
FORV1_filter <- list(
  INDIKATOR = "*",  # Brug "*" for at inkludere alle indikatorer
  Tid = "*"         # Brug "*" for at inkludere alle tidspunkter
)
FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# Konverter data til bred format
FORV1Data_wide <- FORV1Data %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Vis de første rækker af det bredere dataframe
head(FORV1Data_wide)

# Sørg for, at TID-kolonnen er i kvartalsformat
FORV1Data_wide$TID <- paste0(format(as.Date(FORV1Data_wide$TID), "%Y"), quarters(as.Date(FORV1Data_wide$TID)))

# Summér alle kolonner gruppevis baseret på samme kvartal
FORV1Data_wide <- aggregate(. ~ TID, data = FORV1Data_wide, FUN = sum, na.rm = TRUE)

# subset fra 2000Q1 - Find rækken, hvor TID er "2000Q1"
start_row <- which(FORV1Data_wide$TID == "2000Q1")

# Subset fra "2000Q1" til slutningen
FORV1Data_wide <- FORV1Data_wide[start_row:nrow(FORV1Data_wide), ]

# Opret et nyt dataframe med DI-FTI relaterede kolonner
DI_FTI_data <- FORV1Data_wide %>%
  select(TID,
         "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
         "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
         "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
         "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")

# Beregn DI's forbrugertillidsindikator
DI_FTI_data$DI_forbrugertillidsindikator <- rowMeans(
  DI_FTI_data[, -1], na.rm = TRUE
)

# Ryd data op ved at fjerne NA'er
DI_FTI_data_clean <- DI_FTI_data %>%
  filter(!is.na(DI_forbrugertillidsindikator))

# Sørg for, at relevante kolonner er numeriske
DI_FTI_data_clean$DI_forbrugertillidsindikator <- as.numeric(DI_FTI_data_clean$DI_forbrugertillidsindikator)

# Opret og ryd FORV1Data_wide
FORV1Data_wide_clean <- FORV1Data_wide %>%
  filter(!is.na(Forbrugertillidsindikatoren))

# Sørg for, at relevante kolonner er numeriske
FORV1Data_wide_clean$Forbrugertillidsindikatoren <- as.numeric(FORV1Data_wide_clean$Forbrugertillidsindikatoren)

# Konverter TID til en factor
DI_FTI_data_clean$TID <- factor(DI_FTI_data_clean$TID, levels = unique(DI_FTI_data_clean$TID))
FORV1Data_wide_clean$TID <- factor(FORV1Data_wide_clean$TID, levels = unique(FORV1Data_wide_clean$TID))

# Beregn ymin og ymax for at lave korrekt skala i plot
ymin <- min(DI_FTI_data_clean$DI_forbrugertillidsindikator, FORV1Data_wide_clean$Forbrugertillidsindikatoren, na.rm = TRUE)
ymax <- max(DI_FTI_data_clean$DI_forbrugertillidsindikator, FORV1Data_wide_clean$Forbrugertillidsindikatoren, na.rm = TRUE)

# Merge DI forbrugertillid og DST forbrugertillid
combined_data <- merge(DI_FTI_data_clean[, c("TID", "DI_forbrugertillidsindikator")], 
                       FORV1Data_wide_clean[, c("TID", "Forbrugertillidsindikatoren")], 
                       by = "TID", all = TRUE)

# Fjern duplikater efter sammenlægning
combined_data <- combined_data[!duplicated(combined_data$TID), ]

# Plot udviklingen af forbrugertilliden
ggplot(combined_data, aes(x = TID)) +
  geom_line(aes(y = DI_forbrugertillidsindikator, color = "DI", group = 1)) +
  geom_line(aes(y = Forbrugertillidsindikatoren, color = "DST", group = 2)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 7)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +  # Øger skriftstørrelsen til 12
  labs(x = "Kvartal", y = "Forbrugertillid", color = "Indikator") +
  ggtitle("Forbruger tilliderne er parallelle - men DST mest positiv")

#####################################################
# indlæs data via API
# Hent dataen
NKHCO21 <- dst_meta(table = "NKHCO21", lang = "da")

# Filtrer data
realforbrug.filter <- list( 
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKHC021_data <- dst_get_data(table = "NKHC021", query = realforbrug.filter, lang = "da")

NKHC021_data<- NKHC021_data[, -c(1:3)]  # Fjern de tre første kolonner med metadata, da der er na værdier i value

# Beregning af realvækst
NKHC021_data$Realvækst <- (NKHC021_data$value / dplyr::lag(NKHC021_data$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# Sørg for, at TID-kolonnen er i kvartalsformat
NKHC021_data$TID <- paste0(format(as.Date(NKHC021_data$TID), "%Y"), quarters(as.Date(NKHC021_data$TID)))

# subset fra 2000Q1 - Find rækken, hvor TID er "2000Q1"
start_row <- which(NKHC021_data$TID == "2000Q1")

# Subset fra "2000Q1" til slutningen
realforbrug_data_filtreret <- NKHC021_data[start_row:nrow(NKHC021_data), ]
realforbrug_data_filtreret$TID <- as.factor(realforbrug_data_filtreret$TID)

# komniner forbrugerforventninger og realvækst
new_row <- realforbrug_data_filtreret[100,]
realforbrug_data_filtreret[100,] <- new_row
combined_data_NKHCO21 <- cbind(combined_data[, 1], Realvækst = realforbrug_data_filtreret$Realvækst, combined_data[, -1])
# Ændre navnet på den første kolonne til "TID"
colnames(combined_data_NKHCO21)[1] <- "TID"

# Korrelationskoefficient mellem DI og DST forbrugertillidsindikatorer
cor(combined_data$DI_forbrugertillidsindikator, combined_data$Forbrugertillidsindikatoren, use = "complete.obs")
correlation1 <- cor(combined_data_NKHCO21$DI_forbrugertillidsindikator, 
                    combined_data_NKHCO21$Realvækst, 
                    use = "complete.obs")
correlation2 <- cor(combined_data_NKHCO21$Forbrugertillidsindikatoren, 
                    combined_data_NKHCO21$Realvækst, 
                    use = "complete.obs")
print(correlation1)
print(correlation2)

# Opret linær regression og vis summary
model1 <- lm(combined_data_NKHCO21$DI_forbrugertillidsindikator ~ combined_data_NKHCO21$Realvækst, data = combined_data_NKHCO21)
model2 <- lm(combined_data_NKHCO21$Forbrugertillidsindikatoren ~ combined_data_NKHCO21$Realvækst, data = combined_data_NKHCO21)
summary(model1)
summary(model2)

#For at se om samme tal kommer fra Baums periode 2000Q1 - 2016Q2
{
  #For at se om samme tal kommer fra Baums periode 2000Q1 - 2016Q2
  test <- combined_data_NKHCO21[1:66,]
  test_cor_di <- cor(test$DI_forbrugertillidsindikator, 
                      test$Realvækst, 
                      use = "complete.obs")
  print(test_cor_di)

  test_model_di <- lm(DI_forbrugertillidsindikator ~ Realvækst, data = test)
  summary(test_model)
  
  #For DST
  test_cor_dst <- cor(test$Forbrugertillidsindikatoren, 
                     test$Realvækst, 
                     use = "complete.obs")
  print(test_cor_dst)
  
  test_model_dst <- lm(Forbrugertillidsindikatoren ~ Realvækst, data = test)
  summary(test_model_dst)
}

ggplot(combined_data_NKHCO21, aes(x = TID)) +
  geom_bar(aes(y = Realvækst * 10, fill = "DST Realvækst"), stat = "identity") +  # Tegn søjlerne først
  geom_line(aes(y = DI_forbrugertillidsindikator, color = "DI", group = 1)) +
  geom_line(aes(y = Forbrugertillidsindikatoren, color = "DST", group = 2)) +
  scale_fill_manual(values = "grey") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)]) +
  scale_y_continuous(
    name = "Nettotal",
    sec.axis = sec_axis(~ . / 10, name = "Realvækst (%)")  # Invers af skalaen fra Realvækst
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    legend.position = "bottom"
  ) +
  labs(x = "Kvartal", color = "Indikator", fill = "Realvækst") +
  ggtitle("DI's Forbrugertillid er historisk set mindre positiv end DST's")


##########################################################################################
# OPG. 2.2
#########################################################

# opret linær regression
lmtest_DI <- lm(combined_data_NKHCO21$Realvækst ~ combined_data_NKHCO21$DI_forbrugertillidsindikator, data = combined_data_NKHCO21)
lmtest_DST <- lm(combined_data_NKHCO21$Realvækst ~ combined_data_NKHCO21$Forbrugertillidsindikatoren, data = combined_data_NKHCO21)

summary(lmtest_DI)
summary(lmtest_DST)

# Opret data frame for forudsigelser
new_data <- data.frame(Tid = combined_data_NKHCO21$TID,
                       Realvækst = combined_data_NKHCO21$Realvækst,
                       DI_forbrugertillidsindikator = combined_data_NKHCO21$DI_forbrugertillidsindikator, 
                       Forbrugertillidsindikatoren = combined_data_NKHCO21$Forbrugertillidsindikatoren)

# # Forudsig realvæksten i forbruget for Q4 2024 baseret på DI's forbrugertillidsindikator
DI_forudsigelse <- predict(lmtest_DI, newdata = new_data)
DST_forudsigelse <- predict(lmtest_DST, newdata = new_data)

# opret forudsigelser
new_data$DI_forudsigelse <- predict(lmtest_DI, newdata = new_data)
new_data$DST_forudsigelse <- predict(lmtest_DST, newdata = new_data)

print(DI_forudsigelse)
print(DST_forudsigelse)

# Tilføj en besked for at gøre resultatet mere forståeligt
cat("DI's forudsagte realvækst i husholdningernes forbrug for Q4 2024 er:", round(new_data$DI_forudsigelse, 2), "%\n")
cat("DST's forudsagte realvækst i husholdningernes forbrug for Q4 2024 er:", round(new_data$DST_forudsigelse, 2), "%\n")

########################################
# OPG. 2.3
########################################

new_data$Realvækst <- round(new_data$Realvækst, digits = 2)
new_data$DI_forudsigelse <- round(new_data$DI_forudsigelse, digits = 2)
new_data$DST_forudsigelse <- round(new_data$DST_forudsigelse, digits = 2)

ggplot(new_data, aes(x = Tid)) +
  geom_line(aes(y = DI_forudsigelse, color = "DI", group = 1)) +
  geom_line(aes(y = DST_forudsigelse, color = "DST", group = 2)) +
  # Tilføj labels for sidste punkt
  geom_text(
    data = tail(new_data, 1),  # Brug det sidste datapunkt
    aes(y = DI_forudsigelse, label = round(DI_forudsigelse, 3), color = "DI"),
    vjust = -0.5,  # Juster tekstens vertikale placering
    size = 4
  ) +
  geom_text(
    data = tail(new_data, 1),  # Brug det sidste datapunkt
    aes(y = DST_forudsigelse, label = round(DST_forudsigelse, 3), color = "DST"),
    vjust = -0.5,  # Juster tekstens vertikale placering
    size = 4
  ) +
  scale_x_discrete(breaks = function(x) rev(x)[seq(1, length(x), by = 2)]) +
  scale_y_continuous(
    name = "Realvækst (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    legend.position = "bottom"
  ) +
  labs(x = "Kvartal", color = "Indikator", fill = "Realvækst") +
  ggtitle("DI's Forbrugertillid er historisk set mindre positiv end DST's")


###########################################
# OPG. 2.4
####################################################

# Angiv forudsigelser fra opgave 2.3
forudsigelse_DI <- new_data[100,5]
forudsigelse_DST <- new_data[100,6]

prognose_DI <- 2.1 # https://www.danskindustri.dk/arkiv/analyser/2024/5/di-prognose-tilbagegangen-i-beskaftigelsen-er-aflyst/
prognose_Nationalbanken <- 1.6 # tabel 4 - https://www.nationalbanken.dk/da/viden-og-nyheder/publikationer-og-taler/analyse/2024/inflationen-er-paa-rette-kurs-men-der-er-fortsat-et-vist-inflationspres#chapter-01

# Udskriv begge andele ##
cat("Forudsigelse fra opgave 2.3 (DI):", round(forudsigelse_DI, 2), "%\n")
cat("Forudsigelse fra opgave 2.3 (DST):", round(forudsigelse_DST, 2), "%\n")
cat("Seneste prognose fra DI:", round(prognose_DI, 2), "%\n")
cat("Seneste prognose fra Nationalbanken:", round(prognose_Nationalbanken, 2), "%\n")

##samling##
sammenligning <- data.frame(
  Kilde = c("Forudsigelse DI", "Forudsigelse DST", "Prognose DI", "Prognose Nationalbanken"),
  Vækst = c(forudsigelse_DI, forudsigelse_DST, prognose_DI, prognose_Nationalbanken)
)

# Plot sammenligning af forudsigelser og prognoser

ggplot(sammenligning, aes(x = Kilde, y = Vækst, fill = Kilde)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Vækst, 2)), vjust = -0.5) +  # Tilføj tekstetiketter over søjlerne
  labs(title = "DI forventer den største vækst i privatforbruget for 2024",
       x = "Prognoser",
       y = "pct.") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +  # Definerer farver for søjlerne
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Juster x-aksens tekst
  theme(legend.position = "none")
