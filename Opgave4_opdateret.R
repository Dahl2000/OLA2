library(dkstat)
library(devtools)
library(tidyverse)
library(ggplot2)
library(dplyr)


# Byg en forespørgsel for at hente data fra tabellen FORV1
query <- list(
  "INDIKATOR" = c("Forbrugertillidsindikatoren"),   # Hent alle indikatorer
  "TID" = c("*")          # Hent alle tilgængelige år
)

# Hent data ved hjælp af forespørgslen, hvor labels=FALSE sørger for, at vi får nøgler
forbrugertillid_data <- dst_get_data("FORV1", query = query, lang = "da", labels = FALSE)

# Tilføj kolonner for år og kvartal
forbrugertillid_data <- forbrugertillid_data %>%
  mutate(År = format(as.Date(TID), "%Y"),               # Ekstraher år fra dato
         Måned = as.numeric(format(as.Date(TID), "%m")), # Ekstraher måneden som tal
         Kvartal = ceiling(Måned / 3))                   # Omdan måneder til kvartaler

# Filtrér data for perioden 2000 til 3. kvartal 2024
filtered_data <- forbrugertillid_data %>%
  filter(År >= 1996 & År <= 2024, !(År == 2024 & Kvartal > 3))

# Beregn kvartalsvise gennemsnit
kvartalsvis_gennemsnit <- filtered_data %>%
  group_by(År, Kvartal) %>%                            # Gruppér efter år og kvartal
  summarise(Forbrugertillid_gns = mean(value, na.rm = TRUE))  # Beregn gennemsnit, fjern NA

# Kombinér År og Kvartal til en dato (den første dag i kvartalet)
kvartalsvis_gennemsnit$TID <- as.Date(paste(kvartalsvis_gennemsnit$År, 
                                            (kvartalsvis_gennemsnit$Kvartal - 1) * 3 + 1, "01", sep = "-"), 
                                      format = "%Y-%m-%d")

# Plot med korrekt datoformat og kun årstal på x-aksen
ggplot(kvartalsvis_gennemsnit, aes(x = TID, y = Forbrugertillid_gns, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Kvartalsvis Forbrugertillid fra 1996 til 2024",
       x = "År",
       y = "Gennemsnitlig Forbrugertillid") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Viser kun årstal
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Drej x-aksen for bedre læsbarhed


# Opg 4.2 

# Byg en forespørgsel for at hente data fra tabellen FORV1
query <- list(
  "INDIKATOR" = c("Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"),   # Hent alle indikatorer
  "TID" = c("*")          # Hent alle tilgængelige år
)

# Hent data ved hjælp af forespørgslen, hvor labels=FALSE sørger for, at vi får nøgler
anskaffelses_data <- dst_get_data("FORV1", query = query, lang = "da", labels = FALSE)

# Tilføj kolonner for år og kvartal
anskaffelses_data <- anskaffelses_data %>%
  mutate(År = format(as.Date(TID), "%Y"),               # Ekstraher år fra dato
         Måned = as.numeric(format(as.Date(TID), "%m")), # Ekstraher måneden som tal
         Kvartal = ceiling(Måned / 3))                   # Omdan måneder til kvartaler

# Filtrér data for perioden 2000 til 3. kvartal 2024
filtered_data <- anskaffelses_data %>%
  filter(År >= 2000 & År <= 2024, !(År == 2024 & Kvartal > 3))

# Beregn kvartalsvise gennemsnit
kvartalsvis_gennemsnit <- filtered_data %>%
  group_by(År, Kvartal) %>%                            # Gruppér efter år og kvartal
  summarise(anskaffelse_gns = mean(value, na.rm = TRUE))  # Beregn gennemsnit, fjern NA

# Konverter År og Kvartal til en datoformat for at kunne plotte kontinuerligt
kvartalsvis_gennemsnit$TID <- as.Date(paste(kvartalsvis_gennemsnit$År, kvartalsvis_gennemsnit$Kvartal * 3, "01", sep = "-"), "%Y-%m-%d")

# Beregn det samlede gennemsnit for hele perioden
samlet_gns <- mean(filtered_data$value, na.rm = TRUE)

# Plot kvartalsdata med det samlede gennemsnit
ggplot(kvartalsvis_gennemsnit, aes(x = TID, y = anskaffelse_gns)) +
  geom_line(color = "blue", size = 1) +  # Kvartalsvise gennemsnit
  geom_hline(yintercept = samlet_gns, color = "red", linetype = "dashed", size = 1) +  # Samlet gennemsnit
  labs(title = "Kvartalsvis Anskaffelse af Større Forbrugsgoder fra 2000 til 2024",
       x = "År",
       y = "Gennemsnitlig Anskaffelsesindikator") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Viser kun årstal
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Drej x-aksen for bedre læsbarhed



# Opg 4.3

# Indlæs nødvendige pakker
library(dkstat)

# Opret query til at hente alle værdier for hver variabel
query <- list(
  FORMAAAL = "*",        # Hent alle værdier for formål
  PRISENHED = "*",        # Hent alle værdier for prisenhed
  SÆSON = "*",            # Hent både sæsonkorrigerede og ikke-sæsonkorrigerede data
  Tid = "*"               # Hent alle år/tidsperioder
)

# Hent data fra NKHC21-tabellen med den specificerede query
kategorier11_data <- dst_get_data("NKHC21", query = query, lang = "da")

# Sørg for at indlæse dplyr-pakken
library(dplyr)

kategorier11_filtered <- kategorier11_data %>%
  filter(
    year(TID) >= 2020 & year(TID) <= 2023,   # Filtrer tid mellem 2020 og 2023
    grepl("kædede værdier", PRISENHED),      # Filtrer på PRISENHED med "kædede værdier"
    SÆSON == "Ikke sæsonkorrigeret",         # Filtrer på SÆSON med "Ikke sæsonkorrigeret"
    FORMAAAL != "I alt",                     # Filtrer på FORMAAAL med alt andet end "I alt"
  ) 

# Fjern kolonnerne PRISENHED og SÆSON fra kategorier11_filtered
kategorier11_filtered <- kategorier11_filtered %>%
  select(-PRISENHED, -SÆSON)

# Filtrer data for det sidste kvartal i 2020 og 2023
kategorier_2020_2023_Q4 <- kategorier11_filtered %>%
  filter(quarter(TID) == 4 & year(TID) %in% c(2020, 2023))

# Summér forbrug i Q4 2020
forbrug_Q4_2020 <- kategorier_2020_2023_Q4 %>%
  filter(year(TID) == 2020) %>%
  group_by(FORMAAAL) %>%
  summarise(Total_Forbrug_2020_Q4 = sum(value, na.rm = TRUE))

# Summér forbrug i Q4 2023
forbrug_Q4_2023 <- kategorier_2020_2023_Q4 %>%
  filter(year(TID) == 2023) %>%
  group_by(FORMAAAL) %>%
  summarise(Total_Forbrug_2023_Q4 = sum(value, na.rm = TRUE))

# Kombinér Q4 2020 og Q4 2023 data for at beregne væksten
vækst_data_Q4 <- forbrug_Q4_2023 %>%
  inner_join(forbrug_Q4_2020, by = "FORMAAAL") %>%
  mutate(Vækst_Q4 = (Total_Forbrug_2023_Q4 - Total_Forbrug_2020_Q4) / Total_Forbrug_2020_Q4 * 100)

# Kombinér data for 2020 og 2023 for at beregne vækstraten
vækst_data_Q4 <- forbrug_Q4_2023 %>%
  inner_join(forbrug_Q4_2020, by = "FORMAAAL") %>%
  mutate(Procentvis_Vækst = (Total_Forbrug_2023_Q4 - Total_Forbrug_2020_Q4) / Total_Forbrug_2020_Q4 * 100)

# Se den procentvise ændring for hver gruppe
print(vækst_data_Q4)

# Opret en ny tabel for væksten i hver kategori fra 2020 til 2023
category_growth <- kategorier11_filtered %>%
  group_by(FORMAAAL) %>%  # Gruppér efter kategori
  summarise(
    Value_2020 = sum(value[year(TID) == 2020]),  # Sum værdierne for 2020
    Value_2023 = sum(value[year(TID) == 2023])   # Sum værdierne for 2023
  ) %>%
  mutate(Percentage_Growth = ((Value_2023 - Value_2020) / Value_2020) * 100)  # Beregn procentvis vækst

# Tilføj en kolonne for farve (blå for stigning, grøn for fald)
category_growth <- category_growth %>%
  mutate(Color = ifelse(Percentage_Growth >= 0, "blue", "green"))

# Opret bar grafen med farve og procentsatser
ggplot(category_growth, aes(x = reorder(FORMAAAL, Percentage_Growth), y = Percentage_Growth, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Drej akserne, så kategorierne vises vertikalt
  labs(title = "Procentvis Vækst/Fald per Kategori (2020-2023)", x = "Kategori", y = "Procentvis Vækst/Fald") +
  scale_fill_identity() +  # Brug den specificerede farve (grøn for fald, blå for vækst)
  geom_text(aes(label = paste0(round(Percentage_Growth, 1), "%")), hjust = -0.2) +
  theme_minimal()







# Opg 4.4

library(lubridate)
library(dplyr)

# Konverter TID fra formatet YYYYQn til datoformat YYYY-MM-DD
combined_data_new <- combined_data %>%
  mutate(TID = case_when(
    grepl("Q1", TID) ~ paste0(sub("Q1", "", TID), "-01-01"),
    grepl("Q2", TID) ~ paste0(sub("Q2", "", TID), "-04-01"),
    grepl("Q3", TID) ~ paste0(sub("Q3", "", TID), "-07-01"),
    grepl("Q4", TID) ~ paste0(sub("Q4", "", TID), "-10-01")
  )) %>%
  mutate(TID = as.Date(TID))  # Konverter til datoformat

# Filtrer data kun for årene 2020 til 2023
combined_data_filtered <- combined_data_new %>%
  filter(TID >= as.Date("2020-01-01") & TID <= as.Date("2023-12-31"))


# Merge combined_data_filtered og kategorier11_filtered på TID
merged_data <- merge(kategorier11_filtered, combined_data_filtered, by = "TID")

# Find unikke forbrugskategorier fra kolonnen FORMAAAL
forbrugskategorier <- unique(merged_data$FORMAAAL)

# Opret lister til at gemme resultaterne af regressionerne for både DI og DST
di_regressioner <- list()
dst_regressioner <- list()

# Løkke gennem hver forbrugskategori
for (kat in forbrugskategorier) {
  
  # Filtrer data for hver forbrugskategori
  data_kat <- merged_data %>% filter(FORMAAAL == kat)
  
  # Lineær regression for DI's forbrugertillidsindikator mod forbrug (value)
  di_model <- lm(value ~ DI_forbrugertillidsindikator, data = data_kat)
  di_regressioner[[kat]] <- summary(di_model)
  
  # Lineær regression for DST's forbrugertillidsindikator mod forbrug (value)
  dst_model <- lm(value ~ Forbrugertillidsindikatoren, data = data_kat)
  dst_regressioner[[kat]] <- summary(dst_model)
}

# Eksempel på at se resultaterne for en specifik kategori (f.eks. "Fødevarer")
di_regressioner[["Fødevarer"]]  # DI regression for Fødevarer
dst_regressioner[["Fødevarer"]]  # DST regression for Fødevarer