library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(shinydashboard)
options(scipen = 999)


df_żywe_urodzenia <- read_excel("./Urodzenia żywe w Polsce 2007-2023.xlsx")
df_żywe_urodzenia$Województwo[17]="Polska"
colnames(df_żywe_urodzenia)[-1] = as.integer(colnames(df_żywe_urodzenia)[-1])
df_żywe_urodzenia_longer = df_żywe_urodzenia %>%
  pivot_longer(-1, names_to="rok",values_to="zywe_urodzenia")


df_pozostawione <- read_excel("./Noworodki pozostawione w szpitalu 2007-2023.xlsx",
                                        range = "A4:R25")
colnames(df_pozostawione)[-1] = substring(colnames(df_pozostawione)[-1],
                                          nchar(colnames(df_pozostawione)[-1])-3)
df_pozostawione <- df_pozostawione %>% filter(!(row_number()%in%c(1,2,3,4)))
df_pozostawione_longer <- df_pozostawione %>%
  pivot_longer(-1, names_to="rok", values_to = "pozostawione_noworodki")


df_noworodki <- df_żywe_urodzenia_longer %>%
  inner_join(df_pozostawione_longer,by = c("Województwo"="Województwa","rok"))%>%
  mutate(odsetek_pozostawien = pozostawione_noworodki/zywe_urodzenia)
#------------------------------------------------------
df_ludnosc_do_24 <- read_excel("./Liczba osób w wieku 0-24 lata w Polsce, 2014-2023.xlsx",
                              range = "A1:K18")
colnames(df_ludnosc_do_24)[-1] <- as.integer(colnames(df_ludnosc_do_24)[-1])
df_ludnosc_do_24[17,1] = "Polska"
df_ludnosc_do_24 <- df_ludnosc_do_24 %>% pivot_longer(!Województwo,
                                                     names_to = "rok",
                                                     values_to = "wszyscy_do_24")

#------------------------------------------------------
df_ilosc_all_piecza <- read_excel("./Wychowankowie (0-24 lata) w pieczy zastępczej 2014-2023.xlsx",
                                  range = "A1:K18")
colnames(df_ilosc_all_piecza)[1] = "Województwo"
colnames(df_ilosc_all_piecza)[-1] = as.integer(colnames(df_ilosc_all_piecza)[-1])
df_ilosc_all_piecza[17,1] = "Polska"
df_ilosc_all_piecza <- df_ilosc_all_piecza %>% pivot_longer(!Województwo,
                                                            names_to = "rok",
                                                            values_to = "piecza_do_24")
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2016 <- read_excel("./2016_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.2",range = "A9:F27")
df_ilosc_inst_2016 <- df_ilosc_inst_2016[-1,-2]
colnames(df_ilosc_inst_2016) <- c("Województwo","ilosc_placowek",
                                       "ilosc_miejsc", "wychowankowie_31_12",
                                       "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2016$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2016 <- read_excel("./2016_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.7",range = "A6:L26")
df_opusz_inst_2016 <- df_opusz_inst_2016[-c(1,2,3),-2]
colnames(df_opusz_inst_2016) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2016[1,1] = "Polska"
df_opusz_inst_2016[df_opusz_inst_2016=="–"]="0"
df_opusz_inst_2016[-1] <- lapply(df_opusz_inst_2016[-1],as.integer)

df_opusz_inst_przed_i_18_2016 <- pivot_longer(df_opusz_inst_2016[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2016 <- df_opusz_inst_2016[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2016)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2016 <- pivot_longer(df_opusz_inst_po_18_2016,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2016 <- rbind(df_opusz_inst_przed_i_18_2016,
                            df_opusz_inst_po_18_2016)
rm("df_opusz_inst_przed_i_18_2016")
rm(df_opusz_inst_po_18_2016)
#----------------------------------------------------------------------
df_rodzinna_2016 <- read_excel("./2016_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.11",range = "A7:C24")
df_rodzinna_2016=df_rodzinna_2016[,-2]
colnames(df_rodzinna_2016) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2016[1,1]="Polska"

df_ilosc_inst_2016 <- df_ilosc_inst_2016 %>% mutate(rok = 2016)
df_opusz_inst_2016 <- df_opusz_inst_2016 %>% mutate(rok = 2016)
df_rodzinna_2016 <- df_rodzinna_2016 %>% mutate(rok=2016)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2017 <- read_excel("./2017_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.2",range = "A9:F27")
df_ilosc_inst_2017 <- df_ilosc_inst_2017[-1,-2]
colnames(df_ilosc_inst_2017) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2017$Województwo[1] = "Polska"
#--------------------------------------------------
df_opusz_inst_2017 <- read_excel("./2017_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.7",range = "A6:L26")
df_opusz_inst_2017 <- df_opusz_inst_2017[-c(1,2,3),-2]
colnames(df_opusz_inst_2017) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2017[1,1] = "Polska"
df_opusz_inst_2017[df_opusz_inst_2017=="–"]="0"
df_opusz_inst_2017[-1] <- lapply(df_opusz_inst_2017[-1],as.integer)

df_opusz_inst_przed_i_18_2017 <- pivot_longer(df_opusz_inst_2017[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2017 <- df_opusz_inst_2017[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2017)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2017 <- pivot_longer(df_opusz_inst_po_18_2017,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2017 <- rbind(df_opusz_inst_przed_i_18_2017,
                            df_opusz_inst_po_18_2017)
rm("df_opusz_inst_przed_i_18_2017")
rm(df_opusz_inst_po_18_2017)
#----------------------------------------------------------------------
df_rodzinna_2017 <- read_excel("./2017_PieczaZastepcza.xlsx",
                               sheet = "TABL.III.11",range = "A7:C24")
df_rodzinna_2017=df_rodzinna_2017[,-2]
colnames(df_rodzinna_2017) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2017[1,1]="Polska"

df_ilosc_inst_2017 <- df_ilosc_inst_2017 %>% mutate(rok = 2017)
df_opusz_inst_2017 <- df_opusz_inst_2017 %>% mutate(rok = 2017)
df_rodzinna_2017 <- df_rodzinna_2017 %>% mutate(rok=2017)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2018 <- read_excel("./2018_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.2",range = "A9:F27")
df_ilosc_inst_2018 <- df_ilosc_inst_2018[-1,-2]
colnames(df_ilosc_inst_2018) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2018$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2018 <- read_excel("./2018_PieczaZastepcza.xlsx",
                                 sheet = "TABL.III.7",range = "A6:L26")
df_opusz_inst_2018 <- df_opusz_inst_2018[-c(1,2,3),-2]
colnames(df_opusz_inst_2018) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2018[1,1] = "Polska"
df_opusz_inst_2018[df_opusz_inst_2018=="-"]="0"
df_opusz_inst_2018[-1] <- lapply(df_opusz_inst_2018[-1],as.integer)

df_opusz_inst_przed_i_18_2018 <- pivot_longer(df_opusz_inst_2018[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2018 <- df_opusz_inst_2018[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2018)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2018 <- pivot_longer(df_opusz_inst_po_18_2018,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2018 <- rbind(df_opusz_inst_przed_i_18_2018,
                            df_opusz_inst_po_18_2018)
rm("df_opusz_inst_przed_i_18_2018")
rm(df_opusz_inst_po_18_2018)
#----------------------------------------------------------------------
df_rodzinna_2018 <- read_excel("./2018_PieczaZastepcza.xlsx",
                               sheet = "TABL.III.11",range = "A8:C25")
df_rodzinna_2018=df_rodzinna_2018[,-2]
colnames(df_rodzinna_2018) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2018[1,1]="Polska"

df_ilosc_inst_2018 <- df_ilosc_inst_2018 %>% mutate(rok = 2018)
df_opusz_inst_2018 <- df_opusz_inst_2018 %>% mutate(rok = 2018)
df_rodzinna_2018 <- df_rodzinna_2018 %>% mutate(rok=2018)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2019 <- read_excel("./2019_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.2",range = "A8:F26")
df_ilosc_inst_2019 <- df_ilosc_inst_2019[-1,-2]
colnames(df_ilosc_inst_2019) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2019$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2019 <- read_excel("./2019_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.7",range = "A6:L26")
df_opusz_inst_2019 <- df_opusz_inst_2019[-c(1,2,3),-2]
colnames(df_opusz_inst_2019) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2019[1,1] = "Polska"
df_opusz_inst_2019[df_opusz_inst_2019=="-"]="0"
df_opusz_inst_2019[-1] <- lapply(df_opusz_inst_2019[-1],as.integer)

df_opusz_inst_przed_i_18_2019 <- pivot_longer(df_opusz_inst_2019[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2019 <- df_opusz_inst_2019[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2019)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2019 <- pivot_longer(df_opusz_inst_po_18_2019,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2019 <- rbind(df_opusz_inst_przed_i_18_2019,
                            df_opusz_inst_po_18_2019)
rm("df_opusz_inst_przed_i_18_2019")
rm(df_opusz_inst_po_18_2019)
#----------------------------------------------------------------------
df_rodzinna_2019 <- read_excel("./2019_PieczaZastepcza.xlsx",
                               sheet = "TABL.I.11",range = "A8:C25")
df_rodzinna_2019=df_rodzinna_2019[,-2]
colnames(df_rodzinna_2019) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2019[1,1]="Polska"

df_ilosc_inst_2019 <- df_ilosc_inst_2019 %>% mutate(rok = 2019)
df_opusz_inst_2019 <- df_opusz_inst_2019 %>% mutate(rok = 2019)
df_rodzinna_2019 <- df_rodzinna_2019 %>% mutate(rok=2019)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2020 <- read_excel("./2020_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.2",range = "A13:F31")
df_ilosc_inst_2020 <- df_ilosc_inst_2020[-1,-2]
colnames(df_ilosc_inst_2020) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2020$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2020 <- read_excel("./2020_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.7",range = "A6:L30")
df_opusz_inst_2020 <- df_opusz_inst_2020[-(1:7),-2]
colnames(df_opusz_inst_2020) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2020[1,1] = "Polska"
df_opusz_inst_2020[df_opusz_inst_2020=="-"]="0"
df_opusz_inst_2020[-1] <- lapply(df_opusz_inst_2020[-1],as.integer)

df_opusz_inst_przed_i_18_2020 <- pivot_longer(df_opusz_inst_2020[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2020 <- df_opusz_inst_2020[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2020)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2020 <- pivot_longer(df_opusz_inst_po_18_2020,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2020 <- rbind(df_opusz_inst_przed_i_18_2020,
                            df_opusz_inst_po_18_2020)

rm("df_opusz_inst_przed_i_18_2020")
rm(df_opusz_inst_po_18_2020)
#----------------------------------------------------------------------
df_rodzinna_2020 <- read_excel("./2020_PieczaZastepcza.xlsx",
                               sheet = "TABL.I.11",range = "A11:C28")
df_rodzinna_2020=df_rodzinna_2020[,-2]
colnames(df_rodzinna_2020) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2020[1,1]="Polska"

df_ilosc_inst_2020 <- df_ilosc_inst_2020 %>% mutate(rok = 2020)
df_opusz_inst_2020 <- df_opusz_inst_2020 %>% mutate(rok = 2020)
df_rodzinna_2020 <- df_rodzinna_2020 %>% mutate(rok=2020)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2021 <- read_excel("./2021_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.2",range = "A14:F32")
df_ilosc_inst_2021 <- df_ilosc_inst_2021[-1,-2]
colnames(df_ilosc_inst_2021) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2021$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2021 <- read_excel("./2021_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.7",range = "A7:L31")
df_opusz_inst_2021 <- df_opusz_inst_2021[-(1:7),-2]
colnames(df_opusz_inst_2021) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2021[1,1] = "Polska"
df_opusz_inst_2021[df_opusz_inst_2021=="-"]="0"
df_opusz_inst_2021[-1] <- lapply(df_opusz_inst_2021[-1],as.integer)

df_opusz_inst_przed_i_18_2021 <- pivot_longer(df_opusz_inst_2021[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2021 <- df_opusz_inst_2021[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2021)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2021 <- pivot_longer(df_opusz_inst_po_18_2021,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2021 <- rbind(df_opusz_inst_przed_i_18_2021,
                            df_opusz_inst_po_18_2021)

rm("df_opusz_inst_przed_i_18_2021")
rm(df_opusz_inst_po_18_2021)
#----------------------------------------------------------------------
df_rodzinna_2021 <- read_excel("./2021_PieczaZastepcza.xlsx",
                               sheet = "TABL.I.11",range = "A12:C29")
df_rodzinna_2021=df_rodzinna_2021[,-2]
colnames(df_rodzinna_2021) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2021[1,1]="Polska"

df_ilosc_inst_2021 <- df_ilosc_inst_2021 %>% mutate(rok = 2021)
df_opusz_inst_2021 <- df_opusz_inst_2021 %>% mutate(rok = 2021)
df_rodzinna_2021 <- df_rodzinna_2021 %>% mutate(rok=2021)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2022 <- read_excel("./2022_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.2",range = "A12:F30")
df_ilosc_inst_2022 <- df_ilosc_inst_2022[-1,-2]
colnames(df_ilosc_inst_2022) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2022$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2022 <- read_excel("./2022_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.7",range = "A7:L28")
df_opusz_inst_2022 <- df_opusz_inst_2022[-(1:4),-2]
colnames(df_opusz_inst_2022) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali","do_rodziny_nat_po_18",
                                 "wlasne_gospodarstwo_po_18","pozostali_po_18")
df_opusz_inst_2022[1,1] = "Polska"
df_opusz_inst_2022[df_opusz_inst_2022=="—"]="0"
df_opusz_inst_2022[-1] <- lapply(df_opusz_inst_2022[-1],as.integer)

df_opusz_inst_przed_i_18_2022 <- pivot_longer(df_opusz_inst_2022[,1:8],-1,
                                              names_to = "rodzaj",
                                              values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2022 <- df_opusz_inst_2022[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2022)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")

df_opusz_inst_po_18_2022 <- pivot_longer(df_opusz_inst_po_18_2022,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2022 <- rbind(df_opusz_inst_przed_i_18_2022,
                            df_opusz_inst_po_18_2022)

rm("df_opusz_inst_przed_i_18_2022")
rm(df_opusz_inst_po_18_2022)
#----------------------------------------------------------------------
df_rodzinna_2022 <- read_excel("./2022_PieczaZastepcza.xlsx",
                               sheet = "TABL.I.11",range = "A9:C26")
df_rodzinna_2022=df_rodzinna_2022[,-2]
colnames(df_rodzinna_2022) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2022[1,1]="Polska"

df_ilosc_inst_2022 <- df_ilosc_inst_2022 %>% mutate(rok = 2022)
df_opusz_inst_2022 <- df_opusz_inst_2022 %>% mutate(rok = 2022)
df_rodzinna_2022 <- df_rodzinna_2022 %>% mutate(rok=2022)
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
df_ilosc_inst_2023 <- read_excel("./2023_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.2",range = "A10:F28")
df_ilosc_inst_2023 <- df_ilosc_inst_2023[-1,-2]
colnames(df_ilosc_inst_2023) <- c("Województwo","ilosc_placowek",
                                  "ilosc_miejsc", "wychowankowie_31_12",
                                  "wychowankowie_w_ciagu_roku")
df_ilosc_inst_2023$Województwo[1] = "Polska"
#------------------------------------------------------------
df_opusz_inst_2023 <- read_excel("./2023_PieczaZastepcza.xlsx",
                                 sheet = "TABL.I.7",range = "A7:L28")
df_opusz_inst_2023 <- df_opusz_inst_2023[-(1:4),-2]
colnames(df_opusz_inst_2023) = c("Województwo","razem","do_rodziny_nat",
                                 "do_adopcji","do_rodzinnej_pieczy","do_innej_instytucjonalnej",
                                 "do_domy_pomocy","pozostali")
df_opusz_inst_2023[1,1] = "Polska"
df_opusz_inst_2023[df_opusz_inst_2023=="—"]="0"

df_opusz_inst_2023[-1] <- lapply(df_opusz_inst_2023[-1],as.integer)

df_opusz_inst_przed_i_18_2023 <- pivot_longer(df_opusz_inst_2023[,1:8],-1,
                                            names_to = "rodzaj",
                                            values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = F)


df_opusz_inst_po_18_2023 <- df_opusz_inst_2023[,c(1,9,10,11)]
colnames(df_opusz_inst_po_18_2023)[-1] <- c("do_rodziny_nat",
                                            "wlasne_gospodarstwo","pozostali")
                                            
df_opusz_inst_po_18_2023 <- pivot_longer(df_opusz_inst_po_18_2023,-1,
                                         names_to = "rodzaj",
                                         values_to = "ilosc_wychowanków")%>%
  mutate(czy_po_18 = T)

df_opusz_inst_2023 <- rbind(df_opusz_inst_przed_i_18_2023,
                            df_opusz_inst_po_18_2023)
rm("df_opusz_inst_przed_i_18_2023")
rm(df_opusz_inst_po_18_2023)


#----------------------------------------------------------------------
df_rodzinna_2023 <- read_excel("./2023_PieczaZastepcza.xlsx",
                               sheet = "TABL.I.11",range = "A9:C26")
df_rodzinna_2023=df_rodzinna_2023[,-2]
colnames(df_rodzinna_2023) = c("Województwo","wychowankowie_rodzinna")
df_rodzinna_2023[1,1]="Polska"

df_ilosc_inst_2023 <- df_ilosc_inst_2023 %>% mutate(rok = 2023)
df_opusz_inst_2023 <- df_opusz_inst_2023 %>% mutate(rok = 2023)
df_rodzinna_2023 <- df_rodzinna_2023 %>% mutate(rok=2023)

df_ilosc_inst <- rbind(df_ilosc_inst_2016,df_ilosc_inst_2017,df_ilosc_inst_2018,
                  df_ilosc_inst_2019,df_ilosc_inst_2020,df_ilosc_inst_2021,
                  df_ilosc_inst_2022,df_ilosc_inst_2023)
rm(df_ilosc_inst_2016,df_ilosc_inst_2017,df_ilosc_inst_2018,
        df_ilosc_inst_2019,df_ilosc_inst_2020,df_ilosc_inst_2021,
        df_ilosc_inst_2022,df_ilosc_inst_2023)

df_opusz_inst <- rbind(df_opusz_inst_2016,df_opusz_inst_2017,df_opusz_inst_2018,
                       df_opusz_inst_2019,df_opusz_inst_2020,df_opusz_inst_2021,
                       df_opusz_inst_2022,df_opusz_inst_2023)
rm(df_opusz_inst_2016,df_opusz_inst_2017,df_opusz_inst_2018,
      df_opusz_inst_2019,df_opusz_inst_2020,df_opusz_inst_2021,
      df_opusz_inst_2022,df_opusz_inst_2023)

df_rodzinna <- rbind(df_rodzinna_2016,df_rodzinna_2017,df_rodzinna_2018,
                     df_rodzinna_2019,df_rodzinna_2020,df_rodzinna_2021,
                     df_rodzinna_2022,df_rodzinna_2023)
rm(df_rodzinna_2016,df_rodzinna_2017,df_rodzinna_2018,
         df_rodzinna_2019,df_rodzinna_2020,df_rodzinna_2021,
         df_rodzinna_2022,df_rodzinna_2023)
rm(df_pozostawione,df_żywe_urodzenia)




ui <- dashboardPage(
  dashboardHeader(title = "Opuszczone Noworodki i dzieci w pieczy zastępczej",
                  titleWidth = 0),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Opuszczone noworodki", tabName = "strona_noworodki",
               icon = icon("baby")),
      menuItem("Piecza zastępcza", tabName = "strona_piecza",
               icon = icon("child")),
      menuItem("Info", tabName = "strona_info", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
        #text {
            word-wrap: break-word;
            white-space: normal;
        }
    ")),
    tabItems(
      
      
      tabItem(tabName = "strona_noworodki",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("noworodki_cols_year",
                              "Lata:",
                              min = 2007,
                              max = 2023,
                              value = c(2007,2023))
                ),

                mainPanel(
                  plotOutput("noworodki_cols")
                )
              ),
              br(),
              plotOutput("noworodki_violin"),
              textOutput("noworodki_text")
              
      ),
      
      
      tabItem(tabName = "strona_piecza",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("piecza_odsetek_year",
                              "Lata:",
                              min = 2016,
                              max = 2023,
                              value = c(2016,2023))
                ),
                
                mainPanel(
                  plotOutput("piecza_odsetek")
                )),
              textOutput("piecza_odsetek_text"),
                br(),
                
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("piecza_inst_rodz_year",
                                "Lata:",
                                min = 2016,
                                max = 2023,
                                value = c(2016,2023))
                  ),
                  
                  mainPanel(
                    plotOutput("piecza_inst_rodz")
                  )
                  
                ),
                textOutput("piecza_inst_rodz_text"),
                br(),
                br(),
              
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("piecza_inst_por_year",
                                "Lata:",
                                min = 2016,
                                max = 2023,
                                value = c(2016,2023))
                  ),
                  
                  mainPanel(
                    plotOutput("piecza_inst_por")
                  )
                  
                ),
                textOutput("piecza_inst_por_text"),
                br(),br(),
              
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("opuszcz_inst_year",
                                "Lata:",
                                min = 2016,
                                max = 2023,
                                value = c(2016,2023)),
                    checkboxGroupInput("opuszcz_inst_g_wiekowa",
                                       "Grupy wiekowe:",
                                       c("do 18 lat"="do_18",
                                         "od 18 do 24 lat" = "nad_18"))
                  ),
                  mainPanel(
                    plotOutput("opuszcz_inst")
                  )
                ),
                textOutput("opuszcz_inst_text")
                
              
              
              ),
            
      tabItem(tabName ="strona_info",
              textOutput("info_text")
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$info_text <- renderText(
      "W analizie zostały wykorzystane dane dotyczęce ilości noworodków 
      pozostawanych w szpitalach nie ze względów zdrowotnych(2007-2023), 
      ilości żywych urodzeń(2007-2023). Wykorzystane były też dane dotyczące 
      ilości wychowanków pieczy rodzinnej(2016-2023), instytucjonalnej(2016-2023),
      dowolnej pieczy zastepczej(2014-2023), a także ludności do 24 roku życia(2014-2023) w poszczególnych województwach,dane dotyczące tego gdzie
      trafiały osoby po opuszczeniu intucjonalnej pieczy zastępczej z podziałem 
      na województwa(2016-2023)"
    )  
  
    output$noworodki_cols <- renderPlot({
      
        df_noworodki %>% 
        filter(rok %in% input$noworodki_cols_year[1]:input$noworodki_cols_year[2]) %>%
        group_by(Województwo) %>% summarise(odsetek_pozostawien = mean(odsetek_pozostawien))%>%
        mutate(Województwo = fct_reorder(.$Województwo, .$odsetek_pozostawien),
               highlighted = case_when(Województwo == "Polska" ~ T,
                                       .default = F))%>%
        ggplot(aes(x = Województwo, y = odsetek_pozostawien,fill = odsetek_pozostawien)) + 
        scale_fill_gradient(low = "#8c2a64", high = "#c594b1") + 
        geom_col() +
        geom_col(
          data = . %>% filter(highlighted),
          aes(x = Województwo, y = odsetek_pozostawien),
          fill = "#303174"
        ) +
        scale_x_discrete(guide = guide_axis(angle = 25))+
        labs(title = "Średni odsetek noworodków pozostawionych w szpitalach nie ze względów zdrowotnych w wybranych latach",
             fill = "Odsetek pozostawień",
             y = "Średni odsetek pozostawień",
             x = "Województwo")+
        scale_y_continuous(limits = c(0,0.0095),expand = c(0,0))+
        theme(plot.title = element_text(hjust=0.5))
    })
    
    output$noworodki_violin <- renderPlot({
      df_noworodki %>% ggplot(aes( y = odsetek_pozostawien, x= Województwo,
                                   fill = Województwo)) + 
        geom_violin() + scale_y_log10() +
        scale_x_discrete(guide = guide_axis(angle = 25))+
        labs(title = "Rozkład odsekta pozostawień noworodków w poszczególnych województwach",
             y="odsetek(skala logarytmiczna)") +
        theme(plot.title = element_text(hjust=0.5))
    })
    
    output$noworodki_text <- renderText({
      "Odsetek noworodków pozostawionych w szpitalach nie ze względów zdrowotych jest bardzo zróżnicowany w zależności województw, a także w obrębie tego samego województwa w różnych latach, czego bardzo dobrym przykładem jest np.województwo zachodniopomorskie.
Odsetek ten wydaje się w skali Polski w przybliżeniu stały i wynosi ok. 1.8 promila wszytkich żywo urodzonych noworodków."
    })
    
    output$piecza_odsetek <- renderPlot({
      
      df_temp_1 <- df_ilosc_all_piecza %>% 
        filter(rok%in%(input$piecza_odsetek_year[1]:input$piecza_odsetek_year[2]))
      df_temp_2 <- df_ludnosc_do_24 %>% 
        filter(rok%in%(input$piecza_odsetek_year[1]:input$piecza_odsetek_year[2]))
      
      df_temp_1 %>% inner_join(df_temp_2, by=c("Województwo","rok"))%>%
        mutate(odsetek = piecza_do_24/wszyscy_do_24)%>%
        group_by(Województwo)%>% summarise(odsetek = mean(odsetek))%>%
        ggplot(aes(x = fct_reorder(Województwo,odsetek), y = odsetek, fill = Województwo)) +
        geom_col() + scale_x_discrete(guide = guide_axis(angle = 25)) +
        scale_y_continuous(expand = c(0,0),limits = c(0,0.0105)) + 
        labs(x = "Województwo",
             title = "Średni odsetek osób do 24 roku życia w pieczy zastępczej w wybranych latach",
             y = "Średni odsetek") + theme(plot.title = element_text(hjust=0.5))
        
    })
    
    output$piecza_odsetek_text <- renderText({
      "Odsetek osób w pieczy zastępczej rośnie z upływem lat(w skali kraju wzrosła od ok. 5.5 promila w 2016 ok 6 promili w 2023), ale hierarchia województw pod względem tego odsetka jest w przybliżeniu stała na przestrzeni lat."
    })
    
    output$piecza_inst_rodz <- renderPlot({
      df_ilosc_inst %>% 
        inner_join(df_rodzinna,by=c("Województwo","rok")) %>% 
        group_by(Województwo,rok)%>% 
        mutate(suma = wychowankowie_31_12+wychowankowie_rodzinna)%>%
        filter(rok %in% input$piecza_inst_rodz_year[1]:input$piecza_inst_rodz_year[2])%>%
        mutate(odsetek_inst = wychowankowie_31_12/suma,
               odsetek_rodz = wychowankowie_rodzinna/suma)%>%
        group_by(Województwo)%>%summarise(odsetek_inst = mean(odsetek_inst),
                                          odsetek_rodz = mean(odsetek_rodz),)%>%
        pivot_longer(-1,names_to = "typ", values_to = "odsetek")%>%
        ggplot(aes(x=Województwo, y = odsetek, fill = typ))+geom_col(position="dodge")+
        scale_y_continuous(limits = c(0,0.8), expand = c(0,0)) + 
        scale_x_discrete(guide = guide_axis(angle = 25)) +
        scale_fill_discrete(labels = c("instutucjonalna","rodzinna"))+
        labs(fill="Rodzaj pieczy",y = "Średni odsetek wychowanków(stan na 31 XII)",
             title = "Średni odsetek wychowanków z podziałem na rodzaj piechy w wybranych latach")+
        theme(plot.title = element_text(hjust=0.5))
    })
    
    output$piecza_inst_rodz_text<- renderText({
      "Można zauważyć że w wszystkich województwach piecza rodzinna przeważa i wychowanków w tej pieczy jest często nawet 2 razy więcej niż w pieczy intycjonalnej."
    })
    
    output$piecza_inst_por <- renderPlot({
      df_ilosc_inst %>% 
        filter(rok %in% input$piecza_inst_por_year[1]:input$piecza_inst_por_year[2])%>%
        group_by(Województwo)%>%summarise(wychowankowie_31_12 = mean(wychowankowie_31_12),
                                          wychowankowie_w_ciagu_roku = mean(wychowankowie_w_ciagu_roku))%>%
        pivot_longer(-1,names_to="rodzaj", values_to = "wychowankowie")%>%
        filter(Województwo != "Polska")%>%
        ggplot(aes(x = Województwo, y = wychowankowie, fill = rodzaj)) +
        geom_col(position = "dodge")+
        scale_fill_manual(values=c("#8c2a64","#c594b1"), 
                          labels = c("w dniu 31 XII", "w ciągu roku"))+
        scale_x_discrete(guide = guide_axis(angle = 25))+
        labs(title = "średnia ilość wychowanków w pieczy instytucjonlanej w wybranych latach",
             y = "ilość wychowanków",
             fill = "ilość wychowanków")+
        theme(plot.title = element_text(hjust=0.5))+
        scale_y_continuous(limits = c(0,3800), expand = c(0,0))
    })
    
    output$piecza_inst_por_text <- renderText({
      "W każdym województwie każdego roku znacząca część osób która znajdowała 
      się w instytucjonalnej pieczy zastępczej przed końcem roku trafia gdzie 
      idziej. Nasuwa się pytanie gdzie?"
    })
    
    output$opuszcz_inst <- renderPlot({

        selected <- input$opuszcz_inst_g_wiekowa
      
        if (is.null(selected)) {
         return(data.frame(Message = "Proszę wybrać grupę wiekową"))
        }
        print(input$opuszcz_inst_g_wiekowa)
          df_opusz_inst %>%
          filter(rodzaj!="razem")%>%
          filter(rok %in% input$opuszcz_inst_year[1]:input$opuszcz_inst_year[2])%>%
          mutate(czy_po_18 = case_when(czy_po_18 ~ "nad_18",
                                       .default = "do_18"))%>%
          filter(czy_po_18 %in% selected) %>% group_by(Województwo,rodzaj)%>%
          summarise(ilosc_wychowanków = sum(ilosc_wychowanków)) %>%
          group_by(Województwo) %>% mutate(razem = sum(ilosc_wychowanków)) %>%
          mutate(odsetek = ilosc_wychowanków/razem)%>%
          mutate(rodzaj = case_when(rodzaj == "wlasne_gospodarstwo" ~ "zakłada własne gospodarstwo",
                                    rodzaj == "do_rodziny_nat" ~ "powraca do rodziny naturalnej",
                                    rodzaj == "do_rodzinnej_pieczy" ~ "umieszczony w rodzinnej pieczy",
                                    rodzaj == "do_innej_instytucjonalnej" ~ "umieszczony w innej inst. pieczy",
                                    rodzaj == "do_domy_pomocy" ~ "umieszczony w domu pomocy społecznej",
                                    rodzaj == "do_adopcji" ~ "przekazany adopcji",
                                    rodzaj == "pozostali" ~ "pozostali"))%>%
          ggplot(aes(x = Województwo, y = rodzaj, fill = odsetek)) + 
          geom_tile()+coord_fixed()+scale_x_discrete(guide=guide_axis(angle = 25))+
          labs(y = "wychowanek po opuszczeniu instytucjonalej pieczy:",
               title = "Gdzie trafiają osoby po opuszczeniu pieczy zastępczej?")+
          theme(plot.title = element_text(hjust=0.5))
    })
    
    output$opuszcz_inst_text <- renderText({
      "Najwięcej osób do 18 roku życia po opuszczeniu instytucjonalnej pieczy 
      zastępczej powraca do rodziny naturalnej, spora część jest umieszczana w
      pieczy rodzinnej lub w innej pieczy instytucjonalnej, a mała cześć jest 
      adoptowana lub trafia do domu pomocy społeczenej. Ta zależnośc jest 
      z grubsza prawdzina dla wszytkich województw na przestrzeli lat.

      U osób powyżej 18 roku życia w latach 2016-2019 porównywalna część osób po
      opuszczeniu instytucjonalnej opieki zastępczej powracała do rodzin 
      naturalnych co zakładła własne gospodarstwa. Jednak od 2020 do 2023 roku widać, że
      w większości województw większa część osób zaczęła zakładać własne gospodarstwa domowe."
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



