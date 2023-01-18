library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(stringr)
library(ggridges)

setwd("")
# Load The Data
democ <- read_excel("democ.xlsx", col_types = c("text", 
                    "text", "numeric", "numeric", "numeric", 
                    "numeric", "numeric", "numeric", "numeric", 
                    "numeric", "numeric", "numeric", "numeric", 
                    "numeric")) %>% mutate

gdrppercap <- read_excel("gdrppercap.xlsx", col_types = c("text", 
                        "text", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric"))

gini <- read_excel("gini.xlsx", col_types = c("text", 
                   "text", "numeric", "numeric", "numeric", 
                   "numeric", "numeric"))

indo <-  st_read("")


#Descriptive Statistics 
### GDP Per Capita
gdrplong <- gdrppercap %>% pivot_longer("2020":"2010",
                                        names_to = "Tahun", values_to = "GDRP Per Kapita")

gdrplong <- gdrplong %>% mutate(Pulau =case_when(str_detect(Provinsi,"PAPUA")~ "Papua",
                                                 str_detect(Provinsi,"SUMATERA|RIAU|KEP|ACEH|JAMBI|BENGKULU|LAMPUNG")~"Sumatera",
                                                 str_detect(Provinsi, "JAWA|BANTEN|BALI|YOGYAKARTA|DKI") ~ "Jawa dan Bali",
                                                 str_detect(Provinsi, "MALUKU|NUSA")~ "Maluku dan Nusa Tenggara",
                                                 str_detect(Provinsi, "KALIMANTAN")~"Kalimantan",
                                                 str_detect(Provinsi, "SULAWESI")~"Sulawesi",
                                                 T~"Indonesia"))
gdrplong %>% filter(Tahun <= 2019 & Tahun >= 2015 & !(Pulau %in% c("Papua", "Indonesia"))) %>% ggplot(aes(`GDRP Per Kapita`, Pulau, fill = Pulau)) + geom_density_ridges(jittered_points = F) + scale_x_continuous(trans = "log2") + facet_wrap(.~Tahun) + theme(legend.position= "None") + ggtitle("Distribusi GDRP Per Kapita Provinsi berbagai Pulau di Indonesia")  

gdrplong %>% filter(Pulau == "Kalimantan" & Tahun == 2019) %>% arrange(desc(`GDRP Per Kapita`))


###Democracy Map Making
indo_democ <- left_join(indo, democ, by = c("HASC_1" = "kode"))
indo_democ <-indo_democ %>%  pivot_longer("2020":"2009", names_to = "Tahun", values_to = "Indeks Demokrasi") %>% filter(Tahun >= 2015 & Tahun <= 2019)

br_democ <- c(0,60,80,100)
tm_shape(indo_democ)+ tm_borders()+ tm_fill(col = "Indeks Demokrasi", breaks = br_democ)  +tm_facets(by = "Tahun", nrow = 3)
1+1

