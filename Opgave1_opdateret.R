library(devtools)
library(dkstat)
library(dst)
library(danstat)
library(dplyr)
library(ggplot2)

################################
# OPG. 1.1
#########################
# Indlæs nødvendige biblioteker
# devtools: hjælper med udvikling af pakker
# dkstat, dst, danstat: bruges til at hente data fra Danmarks Statistik API

# Indlæs metadata fra Danmarks Statistik for tabel "POSTNR1"
POSTNR1 <- dst_meta(table = "POSTNR1", lang = "da")

# Definer filter til dataudtræk - "*" betyder, at vi ønsker alle postnumre og alle tidsperioder
POSTNR1_filter <- list(
  PNR20 = "*",
  Tid = "*"
)

# Hent data fra Danmarks Statistik baseret på ovenstående filter
POSTNR1_data <- dst_get_data(table = "POSTNR1", query = POSTNR1_filter, lang = "da")

# Ændrer navnet på den 3. kolonne fra "value" til "Indbyggere"
colnames(POSTNR1_data)[3] <- "Indbyggere"

# Fjern rækker, hvor postnummeret er "hele landet", da vi ikke er interesseret i nationale data
POSTNR1_data <- subset(POSTNR1_data, PNR20 != "Hele landet")

# brug kun seneste dato 2024
POSTNR1_data_filtreret <- subset(POSTNR1_data, TID == "2024-01-01")

# Split postnummer og bynavn i separate kolonner
# Første 4 tegn er postnummer, resten er bynavn
POSTNR1_data_filtreret$Postnummer <- substr(POSTNR1_data_filtreret$PNR20, 1, 4)
POSTNR1_data_filtreret$By <- substr(POSTNR1_data_filtreret$PNR20, 6, nchar(POSTNR1_data_filtreret$PNR20))

# Fjern kolonnen "PNR20", da den nu er splittet i to separate kolonner
POSTNR1_data_filtreret <- POSTNR1_data_filtreret[,-1]

# Fjern tekst i parentes (kommunenavne) fra by-kolonnen
POSTNR1_data_filtreret$By <- gsub("\\(.*\\)", "", POSTNR1_data_filtreret$By)

# Erstat kun " Ø", " SV", eller " N" efter et mellemrum med en tom streng
#POSTNR1_data_filtreret$By <- gsub(" (Ø|Øst|K|V|C|SV|SØ|N|NV)$", "", POSTNR1_data_filtreret$By)

POSTNR1_samlet <- aggregate(Indbyggere ~ Postnummer + By, data = POSTNR1_data_filtreret, FUN = sum)

#############################
## Opgave 1.2 – Kategori-variabel ##

# Tilføj en ny kolonne "Bykategori", der vil indeholde byernes kategorier baseret på indbyggertal
POSTNR1_samlet$Bykategori <- NA

# Definer kategorier for bystørrelser baseret på antal indbyggere
bycat <- c("landet" = 0, "landsby" = 250, "lille by" = 1000, 
           "almindelig by" = 2500, "større by" = 10000, "storby" = 50000)

# Brug funktionen 'cut' til at opdele byerne i de definerede kategorier
POSTNR1_samlet$Bykategori <- cut(POSTNR1_samlet$Indbyggere,
                               breaks = c(bycat, Inf), 
                               labels = names(bycat),
                               right = FALSE)

###########################################
## Opgave 1.3 – Merge de to dataframes
####################

# Indlæs ekstern CSV-fil med data om boliger
boligsiden <- read.csv("boligsiden.csv")

# Ændrer kolonnen "postnr" i boligsiden til "Postnummer" for at kunne merge på denne kolonne
names(boligsiden)[names(boligsiden) == "postnr"] <- "Postnummer"

# Merge POSTNR1_data med boligsiden-data på kolonnen "Postnummer"
POSTNR1_merge <- merge(POSTNR1_samlet, boligsiden, by = "Postnummer")

# Tjek for duplikerede rækker efter merge
dupletter <- which(duplicated(POSTNR1_merge))
POSTNR1_merge[dupletter, ]

# Fjern de duplikerede rækker manuelt ved at angive de specifikke rækker, der skal fjernes
POSTNR1_merge <- POSTNR1_merge[-dupletter, ]

################################
## Opgave 1.4 – Plot
################################

POSTNR1_clean <- na.omit(POSTNR1_merge)

# kvmpris i t.kr.
POSTNR1_clean$kvmpris <- POSTNR1_clean$kvmpris*1000

# Konverter kolonnen "By" til en faktor, så den kan bruges i analyser
POSTNR1_clean$By <- as.factor(POSTNR1_clean$By)

# Opret et simpelt scatterplot med base R for at visualisere forholdet mellem størrelse og kvmpris
plot(POSTNR1_clean$kvmpris, POSTNR1_clean$værelser,
     main = "Scatterplot af kvmpris mod størrelse",
     xlab = "Størrelse (kvm)",
     ylab = "Kvmpris",
     las = 2)  # Roter x-aksen labels for bedre læsbarhed

z <- (POSTNR1_clean$kvmpris - mean(POSTNR1_clean$kvmpris)) / sd(POSTNR1_clean$kvmpris)
outliers <- which(abs(z) > 3)

POSTNR1_final <- POSTNR1_clean[-outliers,]

POSTNR1_final$Bykategori <- as.factor(POSTNR1_final$Bykategori)

# Opret et søjlediagram over kvadratmeterpris fordelt på bykategorier ved brug af ggplot2
ggplot(POSTNR1_final, aes(x = Bykategori, y = kvmpris, fill = Bykategori)) +
  geom_col(position = "dodge") +
  labs(x = "Bykategori", y = "Kvmpris", title = "Større byer, højere kvmpriser") +
  theme_minimal()

# Boliger til salg
antal_boliger <- data.frame(table(POSTNR1_final$Bykategori))
colnames(antal_boliger) <- c("Bykategori", "Til_salg")

ggplot(antal_boliger, aes(x = Bykategori, y = Til_salg, fill = Bykategori)) +
  geom_col() +
  geom_text(aes(label = Til_salg), vjust = -0.5) +  # Brug Til_salg som tekstetiketter
  labs(x = "Bykategori", y = "Antal boliger") +
  labs(title = "Flest boliger til salg i de større byer") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12))

# gns. af boligens alder
gennemsnit_alder <- aggregate(opført ~ Bykategori, data = POSTNR1_final, FUN = mean)
gennemsnit_alder$opført <- round(gennemsnit_alder$opført, digits = 0)

############################
# subset pr. postnummer for udvælge et område

# postnumre i region sjælland
udvalgte_postnumre <- c(2670, 2680, 2690, 4000, 4030, 4040, 4060, 4070, 4100, 4130, 4140, 4160, 4171, 4173,4174, 4180, 
                        4190, 4200, 4220, 4230, 4241, 4242, 4243, 4244, 4245, 4250, 4261, 4262, 4270, 4281, 4291, 
                        4293, 4295, 4296, 4300, 4305, 4320, 4330, 4340, 4350, 4360, 4370, 4390, 4400, 4420, 4440, 
                        4450, 4460, 4470, 4480, 4490, 4500, 4520, 4532, 4534, 4540, 4550, 4560, 4571, 4572, 4573, 
                        4581, 4583, 4591, 4592, 4593, 4600, 4621, 4622, 4623, 4632, 4640, 4652, 4653, 4654, 4660, 
                        4671, 4672, 4673, 4681, 4682, 4683, 4684, 4690, 4700, 4720, 4733, 4735, 4736, 4750, 4760, 
                        4771, 4772, 4773, 4780, 4791, 4792, 4793, 4800, 4840, 4850, 4862, 4863, 4871, 4872, 4873, 
                        4874, 4880, 4891, 4892, 4894, 4895, 4900, 4912, 4913, 4920, 4930, 4941, 4942, 4943, 4944, 
                        4945, 4951, 4952, 4953, 4960, 4970, 4983, 4990)

# subset ud fra postnumre vector
postnumre_subset <- POSTNR1_final[POSTNR1_final$Postnummer %in% udvalgte_postnumre, ]

# beregn gennemsnits kvmpris for hver bykategori i datasættet
gennemsnit_pr_kategori <- postnumre_subset %>%
  group_by(Bykategori) %>%
  summarize(
    gns_kvmpris = mean(kvmpris),
    antal_boliger = n() 
  )

gennemsnit_pr_kategori$Bykategori <- as.factor(gennemsnit_pr_kategori$Bykategori)

# Opret et søjlediagram over kvadratmeterpris fordelt på bykategorier ved brug af ggplot2
ggplot(gennemsnit_pr_kategori, aes(x = Bykategori, y = gns_kvmpris, fill = Bykategori)) +
  geom_col() +
  geom_text(aes(label = round(gns_kvmpris, 1)), vjust = -0.3) +
  labs(x = "Bykategori",  # Her angives teksten på x-aksen
       y = "Gns. kvmpris i t.kr.",
       title = "Markant forskel i kvm priser mellem by og land i Region Sjælland") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

# Korrelation
gennemsnit_pr_kategori$Bykategori <- as.numeric(factor(gennemsnit_pr_kategori$Bykategori))
korrelation <- cor(gennemsnit_pr_kategori$Bykategori, gennemsnit_pr_kategori$gns_kvmpris)
print(korrelation)

# 
ggplot(gennemsnit_pr_kategori, aes(x = Bykategori, y = antal_boliger, fill = Bykategori)) +
  geom_col() +
  geom_text(aes(label = antal_boliger), vjust = -0.5) +  # Placerer teksten lige over søjlen
  labs(x = "Bykategori", y = "Antal byer") +
  labs( title = "Byer tilsalg pr. kategori - Region Sjælland") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12))

antal_boliger <- data.frame(gennemsnit_pr_kategori$Bykategori,gennemsnit_pr_kategori$antal_boliger)
colnames(antal_boliger) <- c("Bykategori", "Antalboliger")