
require(data.table)
require(ggplot2)
require(readxl)

# Qualitative color schemes by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

### agricultural area

# https://agrarbericht.ch/de/produktion/pflanzliche-produktion/flaechennutzung
#https://agrarbericht.ch/de/service/dokumentation/download-center
temp = tempfile(fileext = ".xlsx")
# dataURL <- "https://agrarbericht.ch/download/documents/b2/0t7lqpkqyscuou5vbtv8x3ellxjrhb/ab21_1_landwirtschaftliche_nutzflaeche_nach_nutzungsarten_datenreihe_d.xlsx"
dataURL <- "https://agrarbericht.ch/download/documents/ad/kghstgb35yyxhvsfw1keekcv2xsf99/ab22_1_landwirtschaftliche_nutzflaeche_nach_nutzungsarten_datenreihe_d.xlsx"

download.file(dataURL, destfile=temp, mode='wb')

Area <- read_excel(temp,sheet=1, na = "-",skip = 1)
# Area <- read_excel("ab22_1_landwirtschaftliche_nutzflaeche_nach_nutzungsarten_datenreihe_d.xlsx",sheet=1, na = "-",skip = 1)
Area <-setDT(Area[1:62,])

FodderArea <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Futtergetreide","Silo- und Grünmais","Futtererbsen (Eiweisserbsen)","Futterrüben (Runkeln, Halbzuckerrüben)")])) # Futtergetreide inkl. Hafer, Gerste, Weizen etc.
FodderArea

LegumeArea <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Hülsenfrüchte")])) 
LegumeArea

CropArea <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Offenes Ackerland")]))

FoodArea <- CropArea-FodderArea
FoodArea

CropArea_Temp_grasslands <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Ackerland Total")]))
9368/CropArea_Temp_grasslands
FoodArea/CropArea_Temp_grasslands

PermanentPerm_grasslands <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Naturwiesen, Weiden")]))
# 46230/PermanentPerm_grasslands ## 7.6% less Perm_grasslands
# MaxLegumeArea_max2inCropRotation <- 47536 + 100680 + 15070
Temp_grassland_area <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Kunstwiesen")]))

SiloMais <- sum(as.numeric(Area$`2020`[Area$Produkt%in%c("Silo- und Grünmais")]))
824816/SiloMais




###########
temp = tempfile(fileext = ".xlsx")
# dataURL <- "https://agrarbericht.ch/download/documents/bf/8ihu06g8zhtql7t7ntna8sbjbjlz6w/ab21_3_produktion_datenreihe_d.xlsx"
dataURL <- "https://2021.agrarbericht.ch/download/documents/bf/8ihu06g8zhtql7t7ntna8sbjbjlz6w/ab21_3_produktion_datenreihe_d.xlsx"

download.file(dataURL, destfile=temp, mode='wb')
Production <- read_excel(temp,sheet=1, na = "-",skip = 1)

CerealProduction <- as.numeric(Production$`2020`[Production$Produkt%in%c("Getreide")])-as.numeric(Production$`2020`[Production$Produkt%in%c("Brotweizen")]) #inkl mais

Production_Mais_HumanConsumption01 <- as.numeric(Production$`2020`[Production$Produkt%in%c("Körnermais")])*0.1
CerealFodder <- CerealProduction-Production_Mais_HumanConsumption01

Production_Mais_Feed <- 824816 # t TS https://www.sbv-usp.ch/fileadmin/user_upload/04_SES2021_Versorgungsbilanzen.pdf



#####
MeatConsumption <- 447482 #https://agrarbericht.ch/de/markt/tierische-produkte/fleisch-und-eier

#######
#https://www.proviande.ch/de/der-fleischmarkt-in-zahlen

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.proviande.ch/sites/proviande/files/2023-03/Entwicklung%20des%20Pro-Kopf-Verbrauchs_2022_4.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
MeatConsumptionDetailed <- read_excel(temp,sheet=1, na = "-",skip = 4)
# MeatConsumptionDetailed <- read_excel("Entwicklung des Pro-Kopf-Verbrauchs_2021_1.xlsx",skip=4)

MeatConsumptionDetailed <- t(MeatConsumptionDetailed)
MeatConsumptionDetailed <- as.data.frame(MeatConsumptionDetailed)
names(MeatConsumptionDetailed) <- MeatConsumptionDetailed[1,]
MeatConsumptionDetailed <- MeatConsumptionDetailed[-1,1:10]
MeatConsumptionDetailed <- as.data.frame(MeatConsumptionDetailed)
MeatConsumptionDetailed$'2020' <- as.numeric(MeatConsumptionDetailed$'2020')
sum(MeatConsumptionDetailed$'2020',na.rm = T)
MeatConsumptionDetailed$Animal <- rownames(MeatConsumptionDetailed)
MeatConsumptionDetailed <- na.omit(MeatConsumptionDetailed)

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.proviande.ch/sites/proviande/files/2022-03/Entwicklung%20der%20Inlandanteile%20am%20Gesamtkonsum_2021.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
MeatImport <- read_excel(temp,sheet=1, na = "-",skip = 3)
# MeatImport <- read_excel("Entwicklung der Inlandanteile am Gesamtkonsum_2021.xlsx",skip=3)
MeatImport <- t(MeatImport)
MeatImport <- as.data.frame(MeatImport)
names(MeatImport) <- MeatImport[1,]
MeatImport <- MeatImport[-1,1:10]
MeatImport <- as.data.frame(MeatImport)
MeatImport$Import2020Proportion <- 1-as.numeric(MeatImport$'2020')/100
MeatImport$Animal <- rownames(MeatImport)
Export <- data.frame(Kalb=2,Rind=5119,Schwein=2577, Schaf=1,Geflügel=1545, Pferd=1, Übrige=8 )
sum(Export)
Export <- t(Export)
Export <- as.data.frame(Export)
names(Export) <- "Export"
Export$Animal <- row.names(Export)

MeatConsumptionDetailed <- merge(MeatConsumptionDetailed, MeatImport[,c("Import2020Proportion","Animal")], by="Animal", all.x = T)
MeatConsumptionDetailed$Animal <- gsub("\\*","", MeatConsumptionDetailed$Animal)
MeatConsumptionDetailed <- merge(MeatConsumptionDetailed, Export, by="Animal", all.x = T)
MeatConsumptionDetailed$Import2020Proportion[is.na(MeatConsumptionDetailed$Import2020Proportion)] <- 0

SwissPopulation <-  8.7*10^6
MeatConsumptionDetailed$'2020' <- MeatConsumptionDetailed$'2020' *SwissPopulation/1000

RuminantConsumption <- sum(MeatConsumptionDetailed$'2020'[MeatConsumptionDetailed$Animal%in%c("Rind","Kalb","Pferd","Kaninchen","Ziege","Schaf","Wild")],na.rm = T)
RuminantConsumption
MonogastricConsumption <- sum(MeatConsumptionDetailed$'2020'[MeatConsumptionDetailed$Animal%in%c("Schwein","Geflügel","Übrige")],na.rm = T)
MonogastricConsumption



MeatConsumptionDetailed$SwissProducedMeat2020 <- MeatConsumptionDetailed$'2020'*(1-MeatConsumptionDetailed$Import2020Proportion)
MeatConsumptionDetailed$Export[is.na(MeatConsumptionDetailed$Export)] <- 0
MeatConsumptionDetailed$SwissMeatImport2020 <- MeatConsumptionDetailed$'2020'*(MeatConsumptionDetailed$Import2020Proportion)
sum(MeatConsumptionDetailed$SwissMeatImport2020)
sum(MeatConsumptionDetailed$Export)

sum(MeatConsumptionDetailed$SwissMeatImport2020-MeatConsumptionDetailed$Export)

MeatConsumptionDetailed$SwissProducedMeat2020 <- MeatConsumptionDetailed$SwissProducedMeat2020 +MeatConsumptionDetailed$Export
SwissRuminantProduction <- sum(MeatConsumptionDetailed$SwissProducedMeat2020[MeatConsumptionDetailed$Animal%in%c("Rind","Kalb","Pferd","Kaninchen","Ziege","Schaf","Wild")],na.rm = T)
SwissRuminantProduction
SwissMonogastricProduction <- sum(MeatConsumptionDetailed$SwissProducedMeat2020[MeatConsumptionDetailed$Animal%in%c("Schwein","Geflügel","Übrige")],na.rm = T)
SwissMonogastricProduction




(MonogastricConsumption+RuminantConsumption) - MeatConsumption
SwissRuminantProteinProduction <- SwissRuminantProduction * 0.214
SwissMonogastricProteinProduction <- SwissMonogastricProduction * 0.216
SwissMeatProteinProduction <- SwissMonogastricProteinProduction+SwissRuminantProteinProduction


RuminantProteinConsumption <- RuminantConsumption * 0.214
MonogastricProteinConsumption <- MonogastricConsumption * 0.216
MeatProteinConsumption <- MonogastricProteinConsumption+RuminantProteinConsumption

MeatProteinConsumption-SwissMeatProteinProduction

NettoImportedMeatProtein <- MeatProteinConsumption-SwissMeatProteinProduction

### reduce Perm_grasslands

lay_yield <- 12
reduce_lay_maize_prop <- 0.7
Fodder_Mais_half <- Production_Mais_Feed*reduce_lay_maize_prop



## areas for potential legume production
areas_raw <- read.csv("areas_gdd.csv")

## legume yield Switzerland
yield_legumes_CH <- read.csv("FAOSTAT_data_en_6-22-2023.csv")
yield_legumes_CH_sub <- subset(yield_legumes_CH, Year%in%c(2016:2020)&Element=="Yield"&Item!="Beans")
yield_legumes_CH_sub$value <- yield_legumes_CH_sub$Value/10000
yield_legumes_CH_sub <- setDT(yield_legumes_CH_sub)[,list(min_prot_tha=min(value),max_prot_tha=max(value)),by=Item]

soybean_min_prot_tha <- 0.396* yield_legumes_CH_sub$min_prot_tha[yield_legumes_CH_sub$Item=="Soya beans"] # prot * yield 
soybean_max_prot_tha <- 0.396* yield_legumes_CH_sub$max_prot_tha[yield_legumes_CH_sub$Item=="Soya beans"]
soybean_prot_tha <- seq(soybean_min_prot_tha, soybean_max_prot_tha, length.out=10)

fababean_min_prot_tha <- .296*yield_legumes_CH_sub$min_prot_tha[yield_legumes_CH_sub$Item=="Broad beans and horse beans, dry"] 
fababean_max_prot_tha <- .296*yield_legumes_CH_sub$max_prot_tha[yield_legumes_CH_sub$Item=="Broad beans and horse beans, dry"]
fababean_prot_tha <-seq(fababean_min_prot_tha, fababean_max_prot_tha, length.out=10)

pea_min_prot_tha <- 0.214*yield_legumes_CH_sub$min_prot_tha[yield_legumes_CH_sub$Item=="Peas, dry"] 
pea_max_prot_tha <- 0.214*yield_legumes_CH_sub$max_prot_tha[yield_legumes_CH_sub$Item=="Peas, dry"] 
pea_prot_tha <-seq(pea_min_prot_tha, pea_max_prot_tha, length.out=10)
prot_tha_variation <- data.frame(soybean_prot_tha, fababean_prot_tha, pea_prot_tha )

## correct for protein quality based on DIAAS (see https://doi.org/10.1002/fsn3.1809)
prot_tha_variation$soybean_prot_tha <- prot_tha_variation$soybean_prot_tha
prot_tha_variation$fababean_prot_tha <- prot_tha_variation$fababean_prot_tha
prot_tha_variation$pea_prot_tha <- prot_tha_variation$pea_prot_tha


prot_tha_DIAAS <- lapply(1:10, function(x) unlist(areas_raw[2,1:3])*prot_tha_variation[x,]*c(1,0.6,0.8))
prot_tha_DIAAS <- rbindlist(prot_tha_DIAAS,idcol=F)
# prot_tha_DIAAS <- as.data.frame(prot_tha_DIAAS)
prot_tha_DIAAS$Total_DIAAS <- rowSums(prot_tha_DIAAS)

prot_tha <- lapply(1:10, function(x) unlist(areas_raw[2,1:3])*prot_tha_variation[x,])
prot_tha <- rbindlist(prot_tha,idcol=F)
# prot_tha_DIAAS <- as.data.frame(prot_tha_DIAAS)
# prot_tha$Total <- rowSums(prot_tha)

prot_tha_DIAAS$Total <- rowSums(prot_tha)

prot_tha_DIAAS$Frac <- prot_tha_DIAAS$Total_DIAAS/ prot_tha_DIAAS$Total
# average_DIAAS_correction_factor <- mean(prot_tha_DIAAS$Frac)
DIAAS_correction_factor <- prot_tha_DIAAS$Frac


prot_tha_frac <-prot_tha
prot_tha_frac$Total <- rowSums(prot_tha_frac)
prot_tha_frac <- sapply(prot_tha_frac, function(x) x/prot_tha_frac$Total )
prot_tha_frac <- as.data.frame(prot_tha_frac)
prot_tha_frac$Total <- NULL

get_areas_and_protein_production <- function(areas_raw, MeatProteinConsumption, reduce_lay_maize_prop, LegumeArea, Temp_grassland_area, lay_yield, SiloMais, prot_tha ){
  Fodder_Temp_grassland_half <- Temp_grassland_area*lay_yield*reduce_lay_maize_prop
  
  
  areas_scenario1 <- data.frame(Crop=c("Soybean","Faba bean","Pea"),Cultivation_break=c(3,3,6),Scenario="Scenario I",Potential_area= unlist(areas_raw[1,1:3]), Protein_yield=prot_tha)
  areas_scenario2 <- data.frame(Crop=c("Soybean","Faba bean","Pea"),Cultivation_break=c(3,3,6),Scenario="Scenario II",Potential_area= unlist(areas_raw[2,1:3]), Protein_yield=prot_tha)
  
  areas <- rbind(areas_scenario1,areas_scenario2)
  areas <- setDT(areas)
  areas$Production_area_single <- areas$Potential_area/(areas$Cultivation_break+1)
  areas$Protein_production_single <- areas$Production_area_single*areas$Protein_yield
  
  areas$DIAAS_correction <- 1
  areas$DIAAS_correction[areas$Crop=="Pea"] <- 0.8
  areas$DIAAS_correction[areas$Crop=="Faba bean"] <- 0.6
  
  areas$Replacing_meat_protein_single <- areas$Protein_production_single*  areas$DIAAS_correction/MeatProteinConsumption*100
  
  areas[,Potential_area_combined:=Potential_area-shift(Potential_area),by=Scenario]
  areas
  
  areas$Potential_area_combined[areas$Crop=="Soybean"] <- areas$Potential_area[areas$Crop=="Soybean"]
  areas$Production_area_combined_legume_1 <- areas$Potential_area_combined/(areas$Cultivation_break+1)
  areas$Production_area_combined_legume_2 <- shift(areas$Potential_area_combined*(1/8))
  areas$Production_area_combined_legume_2[is.na(areas$Production_area_combined_legume_2)] <- 0
  # areas$Production_area_combined_legume_2[areas$Crop=="Rotation"] <- NA
  
  
  areas$Production_area_combined <- areas$Production_area_combined_legume_1+areas$Production_area_combined_legume_2
  areas$Protein_production_combined <- areas$Protein_yield*as.numeric(areas$Production_area_combined) ## in tons
  
  
  
  ############ replace Perm_grasslands
  
  
  Legume_production_area_combined <- max(areas$Production_area_combined) ## scenario II inclusive suitable Perm_grassland and Temp_grassland
  
  Area_Perm_grassland_for_legumes <- Legume_production_area_combined-LegumeArea-reduce_lay_maize_prop*Temp_grassland_area-reduce_lay_maize_prop*SiloMais
  Area_Perm_grassland_for_legumes
  
  reduce_lay_maize_prop*SiloMais
  reduce_lay_maize_prop*Temp_grassland_area
  
  
  areas$Potential_area_rotation[8] /508207
  
  areas$Potential_area_rotation[8] /Temp_grassland_area
  areas$Potential_area_rotation[8] /(SiloMais*reduce_lay_maize_prop+Temp_grassland_area*reduce_lay_maize_prop+Area_Perm_grassland_for_legumes )## should be 1
  
  areas$Protein_production_combined_legume_1 <-areas$Production_area_combined_legume_1*areas$Protein_yield
  areas$Protein_production_combined_legume_2 <-areas$Production_area_combined_legume_2*areas$Protein_yield
  
 
  areas$Protein_production_combined_DIAAS_corrected <- areas$Protein_production_combined*  areas$DIAAS_correction
  areas$Replacing_meat_protein <- areas$Protein_production_combined_DIAAS_corrected/MeatProteinConsumption*100
  areas$Temp_grassland_yield <- lay_yield
  areas$Area_Perm_grassland_for_legumes <- Area_Perm_grassland_for_legumes
  
  return(areas)
}


areas_all_yield <- lapply(c(12), function(i_Temp_grassland) rbindlist(idcol = T,lapply(1:10, function(i_prot) get_areas_and_protein_production(areas_raw, MeatProteinConsumption, reduce_lay_maize_prop, LegumeArea, Temp_grassland_area, lay_yield=i_Temp_grassland, SiloMais, prot_tha=unlist(prot_tha_variation[i_prot,]) ))))
areas_all_yield <- rbindlist(areas_all_yield, idcol = F)
areas_all_yield$DIAAS_correction <- NULL

areas_average_yield <- areas_all_yield[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x),digits = 0), big.mark=","),")")), by=.(Scenario, Crop, Cultivation_break)]
dummy_digits <- areas_all_yield[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 2),")")), by=.(Scenario, Crop, Cultivation_break)]
# areas_average_yield$Replacing_meat_protein <- dummy_digits$Replacing_meat_protein
# areas_average_yield$Replacing_meat_protein_single <- dummy_digits$Replacing_meat_protein_single
areas_average_yield$Protein_yield <- dummy_digits$Protein_yield

areas_average_yield[] <- lapply(areas_average_yield, function(x) gsub(" \\(\u00B10\\)","", x))
areas_average_yield$Temp_grassland_yield <- NULL
areas_average_yield$.id <- NULL

areas_average_yield

Sum_digits <- areas_all_yield[,list(Protein_yield=sum(Protein_yield), Potential_area_combined=sum(Potential_area_combined, na.rm=T),Production_area_combined_legume_1=sum(Production_area_combined_legume_1, na.rm=T),Production_area_combined_legume_2=sum(Production_area_combined_legume_2, na.rm=T),Protein_production_combined_legume_1=sum(Protein_production_combined_legume_1, na.rm=T),Protein_production_combined_legume_2=sum(Protein_production_combined_legume_2, na.rm=T),Production_area_combined=sum(Production_area_combined, na.rm=T),Protein_production_combined=sum(Protein_production_combined, na.rm=T), Protein_production_combined_DIAAS_corrected=sum(Protein_production_combined_DIAAS_corrected,na.rm = T)), by=.(Scenario, Temp_grassland_yield,.id)]
Sum_digits$Replacing_meat_protein <- Sum_digits$Protein_production_combined_DIAAS_corrected/MeatProteinConsumption*100


p <- melt.data.table(Sum_digits, measure.vars = c("Replacing_meat_protein"))

ggplot(p, aes(y=value, x=Protein_yield) ) + #ylab("Temperature difference (°C)")+ xlab("Height binned (m.a.s.l.)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.size = unit(1, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=2)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(values = tol10qualitative )+
  # scale_y_continuous(labels = scales::scientific)+
  facet_wrap(variable~Scenario,switch="y", scales = "free",nrow=1)

Sum_digits$Protein_yield <- NULL
Sum_digits$Temp_grassland_yield <- NULL
Sum_digits$.id <- NULL
dummy_digits_rot <- Sum_digits[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 2),")")), by=.(Scenario)]
Rotation <- Sum_digits[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x),digits = 0), big.mark=","),")")), by=.(Scenario)]
# Rotation$Replacing_meat_protein <- dummy_digits_rot$Replacing_meat_protein
Rotation[] <- lapply(Rotation, function(x) gsub(" \\(\u00B10\\)","", x))


Rotation$Crop <- "Total"
Rotation$Cultivation_break <- NA
require(plyr)
areas <- rbind.fill(areas_average_yield, Rotation)
areas <- areas[order(areas$Scenario),]
areas



Protein_legume_total <- subset(Sum_digits, Scenario=="Scenario II")$Protein_production_combined
Protein_legume_total


####

TableArea <- areas
TableArea <- setDT(TableArea)
TableArea$Production_area_combined_legume_1 <- as.numeric(gsub(",","",TableArea$Production_area_combined_legume_1))
TableArea$Production_area_single <- as.numeric(gsub(",","",TableArea$Production_area_single))
TableArea$Production_on_perm_grasslands_single <- TableArea$Production_area_single-shift(TableArea$Production_area_single, n=4)
TableArea$Production_on_perm_grasslands_combined <- TableArea$Production_area_combined_legume_1-shift(as.numeric(TableArea$Production_area_combined_legume_1), n=4)
Production_on_perm_grasslands_combined <- TableArea$Production_on_perm_grasslands_combined[TableArea$Crop=="Total"&TableArea$Scenario=="Scenario II"] ## legume area on Perm_grasslands combined
round(Production_on_perm_grasslands_combined,digits = 0)

TableArea$Production_area_combined_legume_1 <- format(TableArea$Production_area_combined_legume_1, big.mark = ",")
TableArea$Production_area_single <- format(TableArea$Production_area_single, big.mark = ",")
TableArea$Production_area_single[c(4,8)] <- NA

TableArea_Small <- TableArea
names(TableArea_Small)

names(TableArea_Small) <- gsub("_"," ",names(TableArea_Small))
names(TableArea_Small) <- gsub(" \\[%\\] "," ",names(TableArea_Small))
names(TableArea_Small) <- gsub(" \\[ha\\]","",names(TableArea_Small))
names(TableArea_Small) <- gsub(" \\[t\\]","",names(TableArea_Small))
TableArea_Small$Scenario <- gsub("Scenario ", "",TableArea_Small$Scenario)

p <- TableArea_Small[,c(1:3,5,4,6:13,17)]

units <- data.frame(" "," ","(a)","(t/ha)","(ha)","(ha)","(t)","(\\%)","(ha)","(ha)","(ha)","(ha)","(t)","(\\%)")
names(units) <- names(p)
p <- rbind(units,p)
p
require(xtable)
options(xtable.sanitize.text.function=identity)
print(xtable(p, type = "latex", align = rep("p{1.5cm}",times=(ncol(p)+1)), digits=c(0)), file = "Areas_scenarios1_2.tex",include.rownames=F, vline.after=8, hline.after=c(-1,1,4,5,8,nrow(p)))
####################################
names(TableArea_Small)
p <- TableArea_Small[,c(1:3,5,4,6:8,15,9,19,10:11, 14:15,12:13,17)]
p$`Production on perm grasslands combined` <- format(p$`Production on perm grasslands combined`,big.mark = ",")


p <- p[,c(1:5,10:18)]
units <- data.frame(" "," ","(a)","(t/ha)","(ha)","(ha)","(ha)","(ha)","(ha)","(t)","(t)","(ha)","(t)","(\\%)")
names(units) <- names(p)
p <- rbind(units,p)
p
require(xtable)
options(xtable.sanitize.text.function=identity)
print(xtable(p, type = "latex", align = rep("p{1.5cm}",times=(ncol(p)+1)), digits=c(0)), file = "Areas_scenarios1_2_big.tex",include.rownames=F, vline.after=8, hline.after=c(-1,1,4,5,8,nrow(p)))


##########

### egg
ProteinContentEgg <- 0.14  # 14g/100g protein and 648 kj pro 100 g  https://naehrwertdaten.ch/en/search/#/food/340661
NetEggImportedJoule <- (252) * 10^12 #in Joule
EggProducedJoule <- (327+20) * 10^12 #in Joule
NetEggImportedJoule/648000/10*0.14 /1000
EggProducedJoule/648000/10*0.14 /1000
TotalEggConsumptionKG <-(EggProducedJoule+NetEggImportedJoule)/648000/10
TotalEggproteinConsumption <- TotalEggConsumptionKG*0.14 /1000 # in tons
TotalEggproteinConsumption
SwissEggProteinProduction <- TotalEggproteinConsumption*347/(347+252)

NettoImportedEggProtein <- TotalEggproteinConsumption-SwissEggProteinProduction

### milk
# 120 086 TS  https://www.sbv-usp.ch/fileadmin/user_upload/04_SES2021_Versorgungsbilanzen.pdf  
# SwissMilkProteinProduction <- 3400000 *0.032 # 3.4 Mio t Verkehrsmilk https://www.sbv-usp.ch/fileadmin/user_upload/MISTA2020_def_online.pdf

ProteinContentMilk <- 0.032  # 3.2g/100g protein and 284 kj pro 100 ml  https://naehrwertdaten.ch/de/search/#/food/341028
NetMilkImportedJoule <- (-1992+ 1244) * 10^12 #in Joule
MilkProducedJoule <- (6594+1992) * 10^12 #in Joule

TotalMilkConsumptionKG <-(MilkProducedJoule+NetMilkImportedJoule)/284000/10 *1.075#density
TotalMilkproteinConsumption <- TotalMilkConsumptionKG*ProteinContentMilk /1000 # in tons
TotalMilkproteinConsumption
SwissMilkProteinProduction <- TotalMilkproteinConsumption* (6594+1992)/(6594+1992-1992+ 1244)

NettoImportedMilkProtein <- TotalMilkproteinConsumption-SwissMilkProteinProduction


SwissRuminantProteinProductionMilk <- SwissMilkProteinProduction+SwissRuminantProteinProduction
SwissRuminantProteinProductionMilk
SwissMonogastricProteinProductionEgg <-SwissMonogastricProteinProduction+SwissEggProteinProduction
SwissMonogastricProteinProductionEgg

Protein_food_total <- SwissMeatProteinProduction+SwissMilkProteinProduction+SwissEggProteinProduction+NettoImportedEggProtein+NettoImportedMeatProtein+NettoImportedMilkProtein

####
Area_Perm_grassland_for_legumes <- abs(unique(areas_all_yield$Area_Perm_grassland_for_legumes))

get_balance <- function(Total_feed_raw, Import_feed_raw,Temp_grassland_area,lay_yield, reduce_lay_maize_prop, Area_Perm_grassland_for_legumes, PermanentPerm_grasslands, Protein_food_total, Protein_legume_total,Fodder_Mais_half,SwissRuminantProteinProduction,SwissMonogastricProteinProduction,SwissEggProteinProduction,NettoImportedMeatProtein,NettoImportedEggProtein,NettoImportedMilkProtein,DIAAS_correction_factor){
  Fodder_Temp_grassland <- Temp_grassland_area*lay_yield
  Fodder_Temp_grassland_half <- Temp_grassland_area*lay_yield*reduce_lay_maize_prop
  
  Fodder_Temp_grasslandMais <- Fodder_Mais_half+Fodder_Temp_grassland_half
  
  SwissMeatEggProteinProduction <- SwissMeatProteinProduction+SwissEggProteinProduction
  NettoImportedProtein <- NettoImportedEggProtein+NettoImportedMeatProtein+NettoImportedMilkProtein
  ## https://www.sbv-usp.ch/fileadmin/user_upload/04_SES2021_Versorgungsbilanzen.pdf
  # Total_feed_raw <- 5876765
  Total_feed_other <- 351954 
  # Conc_feed_laying_hens <- 3487972*365*0.15/1000 # 0.15 kg per day
  Total_feed_conc <-1523395 #marktfähig
  Total_feed_all <- Total_feed_raw+Total_feed_conc+Total_feed_other
  Total_feed_all
  
  (-286463-57613-288424)/Total_feed_conc # approx import prop. # see https://www.sbv-usp.ch/de/agristat-aktuell-04-19-futtermittelbilanz-2017/
  
  ratio_herb_mono_all_2017 <-  7251404/1255657 #https://www.sbv-usp.ch/de/agristat-aktuell-04-19-futtermittelbilanz-2017/
  ratio_herb_mono_conc_2017 <- 601150/1016709
  # ratio_herb_mono_Rough_2017 <-(7251404-601150)/(1255657 -1016709)
  
  601150/( 601150+1016709)
  ratio_herb_mono_conc_2020 <- 0.369 #https://www.sbv-usp.ch/de/agristat-aktuell-04-22-futtermittelbilanz-2020/
  
  # Total_feed_conc <- Total_feed_conc- Conc_feed_laying_hens
  Conc_monogastrics_total <- Total_feed_conc*(1-ratio_herb_mono_conc_2020)# https://www.sbv-usp.ch/fileadmin/sbvuspch/00_Bilder/04_Medien/Agristat_aktuell/2019/Aktuell_AGRISTAT_2019-04.pdf
  Conc_ruminats_total <- Total_feed_conc*(ratio_herb_mono_conc_2020)
  
  1255657*0.067
  Rough_monogastrics_total <- Conc_monogastrics_total/0.81*0.067 # https://www.sbv-usp.ch/fileadmin/sbvuspch/00_Bilder/04_Medien/Agristat_aktuell/2019/Aktuell_AGRISTAT_2019-04.pdf
  Rough_ruminants_total <- Total_feed_raw-Rough_monogastrics_total
  Total_Monogastrics_feed_other <- Conc_monogastrics_total/0.81*0.123
  Total_herbivor_feed_other <- Total_feed_other-Total_Monogastrics_feed_other
  
  Total_herbivor_feed <- Rough_ruminants_total+Conc_ruminats_total+Total_herbivor_feed_other
  Total_Monogastrics_feed <- Rough_monogastrics_total+Conc_monogastrics_total+Total_Monogastrics_feed_other
  
  ratio_herb_mono_Rough_2020 <- Rough_ruminants_total/Rough_monogastrics_total
  
  Total_feed_conc-Conc_ruminats_total-Conc_monogastrics_total
  Total_feed_raw-Rough_ruminants_total-Rough_monogastrics_total
  Total_feed_all-Total_herbivor_feed-Total_Monogastrics_feed
  
  PropOfHerbIn_raw <- Rough_ruminants_total/Total_feed_raw
  
  #######
  PermanentPerm_grasslandsInclAlpweiden <- PermanentPerm_grasslands+ 465000 
  ## remove artifical intensive Perm_grassland with 10 t/ha TS # 
  
  Fodder_pasture <- Total_feed_raw - Fodder_Temp_grassland - Import_feed_raw#####Rough_ruminants_total-Fodder_Temp_grassland
  Fodder_pasture/PermanentPerm_grasslandsInclAlpweiden # average pasture yield
  Reduction_Perm_grassland_TS <- Fodder_pasture*(Area_Perm_grassland_for_legumes/PermanentPerm_grasslandsInclAlpweiden) ##  reduction in fodder from pasture due to legumes on pasture
  Fodder_Perm_grassland_reduced <- Fodder_pasture*(1-Area_Perm_grassland_for_legumes/PermanentPerm_grasslandsInclAlpweiden)
  
  Fodder_Perm_grassland_reduced/Rough_ruminants_total
  
  Reduced_Herbivors_TS_Proportion <- (Fodder_Temp_grassland+Reduction_Perm_grassland_TS)/Rough_ruminants_total## reduction in TS
  Reduced_Herbivors_TS_Proportion
  #####
  
  
  
  
  Table_feed1 <- data.frame(Scenario=c("Scenario II"),Animal=c("Ruminants"), Feed=c("Cereal","Concentrated","Side Products","Conc others","Others","Roughage"), Total_feed= c(Total_herbivor_feed), Proportion=c(0.031, 0.083, 0.043,NA,0.043,0.874))
  Table_feed1$Quantity <- Table_feed1$Total_feed*Table_feed1$Proportion
  Table_feed1$Quantity[is.na(Table_feed1$Quantity)] <- Conc_ruminats_total- sum(Table_feed1$Quantity[Table_feed1$Feed%in%c("Cereal","Side Products")])
  Table_feed1$Proportion <- Table_feed1$Quantity/Table_feed1$Total_feed
  
  Table_feed1$ReducedTo_Proportion <- 1-Reduced_Herbivors_TS_Proportion
  Table_feed1$Feed_reduction[Table_feed1$Feed=="Roughage"] <-  Fodder_Temp_grassland+Reduction_Perm_grassland_TS
  
  
  Table_feed_Ruminants <- rbind(Table_feed1)#, Table_feed2)
  Table_feed_Ruminants$Feed_reduction[is.na(Table_feed_Ruminants$Feed_reduction)] <- 0
  
  Table_feed_Ruminants$Feed_reduced <- Table_feed_Ruminants$Quantity*Table_feed_Ruminants$ReducedTo_Proportion 
  
  Table_feed_Ruminants$Feed_surplus_Ruminants <- Table_feed_Ruminants$Quantity -Table_feed_Ruminants$Feed_reduced 
  Table_feed_Ruminants$Feed_surplus_Ruminants_LessRawFodderArea  <- Table_feed_Ruminants$Feed_surplus_Ruminants-Table_feed_Ruminants$Feed_reduction 
  
  
  Total_feed_conc-Table_feed1$Quantity[Table_feed1$Feed=="Concentrated"]
  Conc_ruminats_total
  Total_Ruminants_feed<- sum(Table_feed1$Quantity[Table_feed1$Feed%in%c("Cereal","Side Products","Conc others")])
  ######
  propConc <- (Total_feed_conc-Total_Ruminants_feed) /Total_Monogastrics_feed
  propConc
  
  Table_feed_Monogastrics <- data.frame(Animal=c("Monogastrics"), Feed=c("Cereal","Concentrated","Side Products","Conc others","Others","Roughage"), Total_feed= c(Total_Monogastrics_feed), Proportion=c(50.2/100, propConc , 25.6/100,NA ,12.3/100, 6.7/100 )) #81/100
  
  Table_feed_Monogastrics$Quantity <- Table_feed_Monogastrics$Total_feed*Table_feed_Monogastrics$Proportion
  Table_feed_Monogastrics$Quantity[is.na(Table_feed_Monogastrics$Quantity)] <- Conc_monogastrics_total - sum(Table_feed_Monogastrics$Quantity[Table_feed_Monogastrics$Feed%in%c("Cereal","Side Products")])
  Table_feed_Monogastrics$Proportion <- Table_feed_Monogastrics$Quantity/Table_feed_Monogastrics$Total_feed
  
  
  Table_feed_Monogastrics$Import <- 0
  Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed=="Side Products"] <- -288424 ## https://www.sbv-usp.ch/fileadmin/user_upload/04_SES2021_Versorgungsbilanzen.pdf
  Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed=="Cereal"] <- -286463-57613
  
  if(Import_feed_raw==0){
    print("Calculating net self-sufficiency")
    Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed=="Side Products"] <- -369067
    Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed=="Cereal"] -388755

    }
  Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed=="Concentrated"] <-sum(Table_feed_Monogastrics$Import[Table_feed_Monogastrics$Feed%in%c("Cereal","Side Products")])
  Table_feed_Monogastrics$Feed_reduced <- Table_feed_Monogastrics$Quantity+Table_feed_Monogastrics$Import
  Table_feed_Monogastrics <- merge(Table_feed_Monogastrics, Table_feed_Ruminants[,c("Scenario","Feed","Feed_surplus_Ruminants_LessRawFodderArea")], by=c("Feed"))
  
  Table_feed_Monogastrics$Feed_MonoRed_surplusRum <- Table_feed_Monogastrics$Feed_reduced+Table_feed_Monogastrics$Feed_surplus_Ruminants_LessRawFodderArea
  
  
  
  ####
  Protein2OilCake <- 2#1/0.8
  ##
  
  ##### feedback
  p_scenario <- subset(Table_feed_Monogastrics, Scenario=="Scenario II")
  p_scenario$Scenario <- "Scenario II"
  
  
  # ##    -> equation 2
  Conc_monogastrics_netto <- Conc_monogastrics_total- abs(p_scenario$Import[p_scenario$Feed=="Concentrated"])
  
  
  
  ###########
  
  # ## 1 -> equation 3
  # Protein_food_total <- SwissMeatEggProteinProduction+SwissMilkProteinProduction+NettoImportedProtein
  
  # 
  # 
  # ## 2   -> equation 4
  # Protein_legume_food <- (Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor
  # 
  # 
  # ### 3  -> equation 5
  # Conc_legume_feed <- (Protein_legume_total -Protein_legume_food) * Protein2OilCake
  # Conc_legume_feed
  #
  #
  # ### 4 (2 in 3)
  # Conc_legume_feed <- (Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake
  # Conc_legume_feed <- (Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake
  ### 5  -> equation 6
  # Conc_monogastrics_new <- Conc_monogastrics_netto+Conc_legume_feed+Conc_ruminats_total*(1-r_ruminants) 
  # Conc_monogastrics_new
  # 
  # ### 6 (4 in 5)
  # 
  # Conc_monogastrics_new <- Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1-r_ruminants) 
  # Conc_monogastrics_new <- Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1-r_ruminants) 
  #
  # ### 7  -> equation 7
  #
  # r_monogastrics <- Conc_monogastrics_new/Conc_monogastrics_total
  #
  # ### 8 (6 in 7)
  #
  # r_monogastrics <- (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1-r_ruminants) ) /Conc_monogastrics_total
  # r_monogastrics <- (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1-r_ruminants) ) /Conc_monogastrics_total
  #
  # ### 9 -> equation 8
  #
  # r_ruminants <- (Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total
  
  # ### 10 (9 in 8)
  #
  # r_monogastrics <- (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)* ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) )/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1- ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) ) ) /Conc_monogastrics_total
  # r_monogastrics <- (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)* ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) )/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1- ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) ) ) /Conc_monogastrics_total
  
  #####
  library(rootSolve)
  f <- function(r_monogastrics) {
    r_monogastrics - (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)* ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) )/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1- ((Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total) ) ) /Conc_monogastrics_total
    }
  # p <- optimize(f, interval = c(0.45,0.75), maximum = F)
  solutions <- uniroot.all(f, interval = c(0.4, 0.8))
  print(solutions)
  r_monogastrics <- solutions[1]
  r_monogastrics
  ##
  
  r_ruminants <- (Rough_ruminants_total+Rough_monogastrics_total*(1-r_monogastrics)-Fodder_Temp_grasslandMais-Reduction_Perm_grassland_TS)/Rough_ruminants_total
  
  print(DIAAS_correction_factor)
  print(paste("r_ruminants:", r_ruminants))
  print(paste("r_monogastrics:",r_monogastrics))
  
  Protein_legume_food <- (Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor
  # print(paste("Protein_legume_food:",Protein_legume_food))
 
  Conc_monogastrics_new <- Conc_monogastrics_netto+( (Protein_legume_total -Protein_legume_food) * Protein2OilCake)+Conc_ruminats_total*(1-r_ruminants) 
  

  
  # print(r_monogastrics)
  r_monogastrics1 <- (Conc_monogastrics_netto+ ((Protein_legume_total - ((Protein_food_total-r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor) ) * Protein2OilCake) +Conc_ruminats_total*(1-r_ruminants) ) /Conc_monogastrics_total
  # print(r_monogastrics1)
  
  # r_monogastrics2 <- Conc_monogastrics_new/Conc_monogastrics_total
  Protein_legume_food2 <- (Protein_food_total-r_monogastrics1*(SwissEggProteinProduction+SwissMonogastricProteinProduction)-(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants)/DIAAS_correction_factor

  Protein_food_total2 <- Protein_legume_food*DIAAS_correction_factor+r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)+(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants
  Protein_food_total3 <- Protein_legume_food*DIAAS_correction_factor+r_monogastrics1*(SwissEggProteinProduction+SwissMonogastricProteinProduction)+(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants
  
  Conc_legume_feed <- (Protein_legume_total -Protein_legume_food) * Protein2OilCake
  Conc_legume_feed

  
  ####
  Protein_legume_food
  FractionForPlantProtein <- Protein_legume_food/SwissMeatEggProteinProduction
  FractionForPlantProtein
  
  
  SwissRuminantProteinProduction_new <- SwissRuminantProteinProduction*(r_ruminants)
  SwissMonogastricProteinProduction_new <- SwissMonogastricProteinProduction*(r_monogastrics)
  SwissEggProteinProduction_new <-  SwissEggProteinProduction*(r_monogastrics)
  
  SwissRuminantProteinProduction_new+SwissMonogastricProteinProduction_new-SwissRuminantProteinProduction-SwissMonogastricProteinProduction
  
  SwissMilkProduction_new <- SwissMilkProteinProduction*(r_ruminants)
  
  # print(Protein_food_total-(r_monogastrics*(SwissEggProteinProduction+SwissMonogastricProteinProduction)+(SwissMilkProteinProduction+SwissRuminantProteinProduction)*r_ruminants))
  # print((1-r_monogastrics)*(SwissEggProteinProduction+SwissMonogastricProteinProduction)+(SwissMilkProteinProduction+SwissRuminantProteinProduction)*(1-r_ruminants)+NettoImportedProtein)
  
  print( paste("New plant-based protein:",(SwissMeatProteinProduction-SwissMonogastricProteinProduction_new-SwissRuminantProteinProduction_new)+(SwissMilkProteinProduction-SwissMilkProduction_new)+(SwissEggProteinProduction-SwissEggProteinProduction_new)+NettoImportedEggProtein+NettoImportedMeatProtein+NettoImportedMilkProtein))
  print(data.frame(Protein_food_total, Protein_food_total2, Protein_food_total3, Protein_legume_total, Protein_legume_food, Protein_legume_food2, Conc_monogastrics_new,r_monogastrics, r_monogastrics1, r_ruminants))
  
  ##########
  
  
  protein_df <- NULL
  protein_df <- data.frame(Source=c("Milk"),Animal="Ruminant",Scenario="Milk",Protein=SwissMilkProduction_new)
  protein_df <- rbind(protein_df, data.frame(Source=c("Milk"),Animal="Legume",Scenario="Plant",Protein=SwissMilkProteinProduction-SwissMilkProduction_new+NettoImportedMilkProtein))
  # protein_df <- rbind(protein_df, data.frame(Source=c("Plant"),Scenario="Used",Protein=x))
  protein_df <- rbind(protein_df, data.frame(Source=c("Meat"),Animal="Ruminant",Scenario="Meat",Protein=SwissRuminantProteinProduction_new))
  protein_df <- rbind(protein_df, data.frame(Source=c("Meat"),Animal="Monogastrics",Scenario="Meat",Protein=SwissMonogastricProteinProduction_new))
  protein_df <- rbind(protein_df, data.frame(Source=c("Meat"),Animal="Legume",Scenario="Plant",Protein=SwissMeatProteinProduction-SwissMonogastricProteinProduction_new-SwissRuminantProteinProduction_new))
  protein_df <- rbind(protein_df, data.frame(Source=c("Meat-import"),Animal="Legume",Scenario="Plant",Protein=NettoImportedMeatProtein))
  protein_df <- rbind(protein_df, data.frame(Source=c("Egg-import"),Animal="Legume",Scenario="Plant",Protein=NettoImportedEggProtein))
  protein_df <- rbind(protein_df, data.frame(Source=c("Milk-export"),Animal="Legume",Scenario="Plant",Protein=NettoImportedMilkProtein*-1))
  protein_df <- rbind(protein_df, data.frame(Source=c("Egg"),Animal="Monogastrics",Scenario="Egg",Protein=SwissEggProteinProduction_new))
  protein_df <- rbind(protein_df, data.frame(Source=c("Egg"),Animal="Legume",Scenario="Plant",Protein=SwissEggProteinProduction-SwissEggProteinProduction_new))
  Protein_substituted_from_animal <- sum(protein_df$Protein[protein_df$Scenario=="Plant"])
  # print(Protein_substituted_from_animal/Protein_legume_food)
  protein_df <- rbind(protein_df, data.frame(Source=c("Supplement"),Animal="Legume",Scenario="Plant",Protein=Protein_legume_food-Protein_substituted_from_animal))
  
  
  ######
  Table_feed_Ruminants$ReducedTo_Proportion <- r_ruminants
  
  Table_feed_Monogastrics$Legume_feed_new <-  Conc_legume_feed
  Table_feed_Monogastrics$Legume_feed_new[!(Table_feed_Monogastrics$Feed%in%c("Concentrated","Side Products"))] <- 0
  
  Table_feed_Monogastrics$ReducedTo_Proportion <- r_monogastrics
  FeedData <- rbind.fill(Table_feed_Ruminants,Table_feed_Monogastrics)
  
  FeedDataDetail <- FeedData[!FeedData$Feed%in%c("Concentrated","Roughage"),]
  FeedDataDetail$Import[is.na(FeedDataDetail$Import)] <- 0
  FeedDataDetail$Feed_reduced <- FeedDataDetail$Quantity*FeedDataDetail$ReducedTo_Proportion
  FeedDataDetail$Reduction_in_conc_feed <- FeedDataDetail$Feed_reduced-FeedDataDetail$Quantity
  
  FeedDataDetail$Feed_item <- FeedDataDetail$Feed
  FeedDataDetail$Feed <- "Concentrated"
  FeedDataDetail$Feed[FeedDataDetail$Feed_item=="Others"] <- "Roughage"
  
  TableFeed_Small <- subset(FeedDataDetail, Scenario=="Scenario II"&Feed_item!="Others")#TableFeed[order(TableFeed$Scenario),]
  TableFeed_Small2 <- setDT(TableFeed_Small)[,list(Feed_baseline=sum(Quantity),Import=sum(Import),Legume_feed_new=sum(Legume_feed_new,na.rm=T),Reduction_in_conc_feed=sum(Reduction_in_conc_feed,na.rm = T),Feed_reduced=sum(Feed_reduced)),by=.(Scenario,Animal,Feed,Feed_item,ReducedTo_Proportion)]
  table_reduced_concentrated_feed <- TableFeed_Small2
  
  
  
  ########
  ######
  Reduced_Roughage_TS <- data.frame(Fodder_maize=-Fodder_Mais_half, Temp_grassland=-Fodder_Temp_grassland_half, Perm_grassland=-Reduction_Perm_grassland_TS) #, Add_Roughage_from_monogastric= ReducedMonogastricRoughage
  Total=sum(Reduced_Roughage_TS)
  Others=0
  Reduced_Roughage_TS <- rbind(t(Reduced_Roughage_TS), Others, Total)
  Reduced_Roughage_TS <- as.data.frame(Reduced_Roughage_TS)
  names(Reduced_Roughage_TS)[1] <- "Reduction_in_roughage"
  Reduced_Roughage_TS$Reduction_in_roughage_area[rownames(Reduced_Roughage_TS)=="Fodder_maize"] <- -SiloMais*reduce_lay_maize_prop
  Reduced_Roughage_TS$Reduction_in_roughage_area[rownames(Reduced_Roughage_TS)=="Temp_grassland"] <- -Temp_grassland_area*reduce_lay_maize_prop
  Reduced_Roughage_TS$Reduction_in_roughage_area[rownames(Reduced_Roughage_TS)=="Perm_grassland"] <- -Area_Perm_grassland_for_legumes
  Reduced_Roughage_TS$Reduction_in_roughage_area[rownames(Reduced_Roughage_TS)=="Total"] <-sum(Reduced_Roughage_TS$Reduction_in_roughage_area, na.rm = T)
  
  Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Total"] <- Total_feed_raw#Rough_ruminants_total
  Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Fodder_maize"] <- Production_Mais_Feed#*(PropOfHerbIn_raw)
  Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Temp_grassland"] <- Fodder_Temp_grassland#*(PropOfHerbIn_raw)
  Perm_grasslands_Temp_grasslands_TS <- Total_feed_raw-970313## 4906452 #mehrjahrigerFutterbau TS
  Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Perm_grassland"] <- (Perm_grasslands_Temp_grasslands_TS-Fodder_Temp_grassland)#*(PropOfHerbIn_raw) # https://www.sbv-usp.ch/fileadmin/user_upload/04_SES2021_Versorgungsbilanzen.pdf
  Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Others"] <- Total_feed_raw-sum(Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)!="Total"],na.rm=T)
  sum(Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)!="Total"],na.rm=T)-Total_feed_raw
  
  Reduced_Roughage_TS$Proportion[rownames(Reduced_Roughage_TS)=="Total"] <- Reduced_Roughage_TS$Reduction_in_roughage[rownames(Reduced_Roughage_TS)=="Total"] /Reduced_Roughage_TS$Feed_baseline[rownames(Reduced_Roughage_TS)=="Total"] 
  Reduced_Roughage_TS$Proportion <- round(Reduced_Roughage_TS$Proportion, digits = 3)
  Reduced_Roughage_TS$Reduction_in_roughage <- round(Reduced_Roughage_TS$Reduction_in_roughage, digits = 1)
  Reduced_Roughage_TS$Animal <- "Ruminants"
  Reduced_Roughage_TS$Feed_item <- rownames(Reduced_Roughage_TS)
  Reduced_Roughage_TS$Feed <- "Roughage"
  
  ###
  Reduced_Herbivors_TS <- Reduced_Roughage_TS[,c("Animal","Feed","Feed_item","Reduction_in_roughage","Reduction_in_roughage_area","Feed_baseline")]
  Reduced_Herbivors_TS$Feed_baseline <- Reduced_Herbivors_TS$Feed_baseline*PropOfHerbIn_raw
  
  Reduced_Mono_TS <- Reduced_Roughage_TS[,c("Feed","Feed_item","Reduction_in_roughage","Reduction_in_roughage_area","Feed_baseline")]
  Reduced_Mono_TS$Animal <- "Monogastrics"
  Reduced_Mono_TS$Feed_baseline <- Reduced_Mono_TS$Feed_baseline*(1-PropOfHerbIn_raw)
  Reduced_Mono_TS$Reduction_in_roughage_area <- 0#Reduced_Mono_TS$Reduction_in_roughage_area*(1-PropOfHerbIn_raw)
  Reduced_Mono_TS$Reduction_in_roughage <- -Reduced_Mono_TS$Feed_baseline*(1-r_monogastrics)
  
  
  require(plyr)
  Reduced_TS <- rbind.fill(Reduced_Herbivors_TS,Reduced_Mono_TS)
  Reduced_TS <- Reduced_TS[Reduced_TS$Feed_item!="Total",c("Animal","Feed","Feed_item","Feed_baseline","Reduction_in_roughage_area","Reduction_in_roughage")]
  Reduced_TS$Feed_baseline[is.na(Reduced_TS$Feed_baseline)] <- 0
  Reduced_TS$Reduction_in_roughage[is.na(Reduced_TS$Reduction_in_roughage)] <- 0
  Reduced_TS$Reduction_in_roughage_monogastrics[Reduced_TS$Animal=="Ruminants"] <- -Reduced_TS$Reduction_in_roughage[Reduced_TS$Animal=="Monogastrics"] 
  Reduced_TS$Reduction_in_roughage_monogastrics[Reduced_TS$Animal=="Monogastrics"] <- 0
  
  Reduced_TS$Feed_reduced <-Reduced_TS$Feed_baseline+Reduced_TS$Reduction_in_roughage+Reduced_TS$Reduction_in_roughage_monogastrics
  Reduced_TS$Import_CH <- "CH_production"
  Reduced_TS$Quantity <- Reduced_TS$Feed_baseline
  Reduced_TS$Feed_baseline <- Reduced_TS$Feed_baseline
  
  Reduced_TS$Reduction_in_roughage_area[is.na(Reduced_TS$Reduction_in_roughage_area)] <- 0
  table_reduced_roughage <-  Reduced_TS
  
  table_reduced_roughage$Proportion <- table_reduced_roughage$Feed_reduced/table_reduced_roughage$Feed_baseline
  
  return(list(table_reduced_roughage, table_reduced_concentrated_feed,protein_df))
}

####

Total_feed_Rough_variation <- seq.int(5937313, 5849755,length.out=10)

balance <- lapply(Protein_legume_total, function(i_prot_legume) lapply(1:10, function(i_feed) get_balance(Total_feed_raw=Total_feed_Rough_variation[i_feed],Import_feed_raw=143448, Temp_grassland_area,lay_yield, reduce_lay_maize_prop, Area_Perm_grassland_for_legumes, PermanentPerm_grasslands, Protein_food_total, i_prot_legume,Fodder_Mais_half,SwissRuminantProteinProduction,SwissMonogastricProteinProduction,SwissEggProteinProduction,NettoImportedMeatProtein,NettoImportedEggProtein,NettoImportedMilkProtein,DIAAS_correction_factor=DIAAS_correction_factor[i_feed])))


balanceNet <- lapply(Protein_legume_total, function(i_prot_legume) lapply(1:10, function(i_feed) get_balance(Total_feed_raw=(Total_feed_Rough_variation[i_feed]-143448),Import_feed_raw=0, Temp_grassland_area,lay_yield, reduce_lay_maize_prop, Area_Perm_grassland_for_legumes, PermanentPerm_grasslands, Protein_food_total, Protein_legume_total= i_prot_legume,Fodder_Mais_half,SwissRuminantProteinProduction,SwissMonogastricProteinProduction,SwissEggProteinProduction,NettoImportedMeatProtein,NettoImportedEggProtein,NettoImportedMilkProtein,DIAAS_correction_factor=DIAAS_correction_factor[i_feed]) ))

iterations <- length(balance)

for (iteration_outer in Protein_legume_total){
  print(iteration_outer)}

get_results <- function(balance, n_results){
  table_reduced_concentrated_feed <- NULL
  iterations <- length(balance)
  for (iteration_outer in 1:iterations){
    for (iteration2 in 1:iterations){
      p <-   balance[[iteration_outer]][[iteration2]][[n_results]]
      p$Total_feed_raw <- Total_feed_Rough_variation[iteration2]
      p$Protein_legume_total <- Protein_legume_total[iteration_outer]
      table_reduced_concentrated_feed <- rbind(table_reduced_concentrated_feed,p)
    }}
  return(as.data.table(table_reduced_concentrated_feed))
  
}
table_reduced_roughage <- get_results(balance, 1)



table_reduced_concentrated_feed <- get_results(balance, 2)



# ggsave("InOutFeedFluxes.pdf",  width = 200, height = 120, units = "mm", dpi = 300, bg="white",first_row)


p_conc <- table_reduced_concentrated_feed
p_conc$ReducedTo_Proportion <- p_conc$Feed_reduced/ p_conc$Feed_baseline

areas_average_yield <- p_conc[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x),digits = 0), big.mark=","),")")), by=.(Scenario, Animal, Feed, Feed_item)]
dummy_digits <- p_conc[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Scenario, Animal, Feed,Feed_item)]
areas_average_yield$ReducedTo_Proportion <- dummy_digits$ReducedTo_Proportion

areas_average_yield[] <- lapply(areas_average_yield, function(x) gsub(" \\(\u00B10\\)","", x))
average_yield_conc_digits <- p_conc[,lapply(.SD, mean), by=.(Scenario, Animal, Feed, Feed_item)]


Sum_digits <- setDT(p_conc)[,list(Feed_baseline=sum(Feed_baseline),Import=sum(Import),Legume_feed_new=sum(Legume_feed_new,na.rm=T),Reduction_in_conc_feed=sum(Reduction_in_conc_feed,na.rm = T),Feed_reduced=sum(Feed_reduced)),by=.(Scenario,Animal,Feed,Protein_legume_total,Total_feed_raw)]
Sum_digits$Import[Sum_digits$Import==0] <- NA
Sum_digits$Legume_feed_new[Sum_digits$Legume_feed_new==0] <- NA
Sum_digits$ReducedTo_Proportion <- Sum_digits$Feed_reduced/ Sum_digits$Feed_baseline


####

p <- melt.data.table(Sum_digits, measure.vars = c("ReducedTo_Proportion"))

ggplot(p, aes(y=value,x=Protein_legume_total, color=Total_feed_raw )) + ylab("Reduction to")+ xlab("Legume protein produced in CH (t/year)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.size = unit(1, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=3)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_gradientn(colors=tol5qualitative[c(1:5)] )+
  # scale_y_continuous(labels = scales::scientific)+
  facet_wrap(Animal~.,switch="y", scales = "free",nrow=1)

p <- melt.data.table(Sum_digits, measure.vars = c("ReducedTo_Proportion"))
p <- subset(p, Animal%in%c("Ruminants"))#,"Total Monogastrics"
p$Protein_legume_total <- p$Protein_legume_total/10^6
p$Total_feed_raw <- p$Total_feed_raw/10^6

ggR_ruminants <- ggplot(p, aes(y=Total_feed_raw,x=Protein_legume_total, color=(value) ))+ ylab("Roughage produced (Mt DM/year)")+ xlab("Legume protein produced (Mt DM/year)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(1.75, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=3)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_gradientn(name=expression(r[ruminant]),colors= tol6qualitative[c(2,3)])
# scale_y_continuous(labels = scales::scientific)+
# facet_wrap(variable~.,switch="y", scales = "free",nrow=1)
ggR_ruminants

p <- melt.data.table(Sum_digits, measure.vars = c("ReducedTo_Proportion"))
p <- subset(p, Animal%in%c("Monogastrics"))#,"Total Monogastrics"
p$Protein_legume_total <- p$Protein_legume_total/10^6
p$Total_feed_raw <- p$Total_feed_raw/10^6

ggR_monogastric <- ggplot(p, aes(y=Total_feed_raw,x=Protein_legume_total, color=(value) )) + ylab("Roughage produced (Mt DM/year)")+ xlab("Legume protein produced (Mt DM/year)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(1.75, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=3)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_gradientn(name=expression(r[monogastric]),colors= tol3qualitative[c(2,3,1)])
# scale_y_continuous(labels = scales::scientific)+
# facet_wrap(variable~.,switch="y", scales = "free",nrow=1)
ggR_monogastric

require(cowplot)
first_row <- plot_grid(ggR_ruminants, ggR_monogastric, rel_widths =   c(1,1), ncol = 2, labels="AUTO")  #,vjust=0.5+
first_row


#####
Sum_digits$Protein_legume_total <- NULL
Sum_digits$Total_feed_raw <- NULL


dummy_digits_rot <- Sum_digits[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Animal, Feed, Scenario)]
total_animal <- Sum_digits[,lapply(.SD, function(x) paste0(format(round(mean(x,na.rm=T),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x,na.rm=T),digits = 0), big.mark=","),")")), by=.(Animal, Feed, Scenario)]
total_animal$ReducedTo_Proportion <- dummy_digits_rot$ReducedTo_Proportion
total_animal[] <- lapply(total_animal, function(x) gsub(" \\(\u00B10\\)","", x))
total_animal[] <- lapply(total_animal, function(x) gsub("NaN \\(\u00B1NA\\)","0", x))
total_animal$Animal <- paste("Total",total_animal$Animal)
total_animal$Feed_item <- "All"

Sum_digits <- setDT(p_conc)[,list(Feed_baseline=sum(Feed_baseline),Import=sum(Import),Legume_feed_new=sum(Legume_feed_new,na.rm=T),Reduction_in_conc_feed=sum(Reduction_in_conc_feed,na.rm = T),Feed_reduced=sum(Feed_reduced)),by=.(Scenario,Feed,Protein_legume_total,Total_feed_raw)]
Sum_digits$Import[Sum_digits$Import==0] <- NA
Sum_digits$Legume_feed_new[Sum_digits$Legume_feed_new==0] <- NA
Sum_digits$ReducedTo_Proportion <- Sum_digits$Feed_reduced/ Sum_digits$Feed_baseline
dummy_digits_rot <- Sum_digits[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Feed, Scenario)]

total <- Sum_digits[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x,na.rm=T),digits = 0), big.mark=","),")")), by=.(Feed, Scenario)]
total$ReducedTo_Proportion <- dummy_digits_rot$ReducedTo_Proportion
total$Import <- total_animal$Import[2]
total$Legume_feed_new <- total_animal$Legume_feed_new[2]
total[] <- lapply(total, function(x) gsub(" \\(\u00B10\\)","", x))
total$Animal <- "Total"
total$Feed_item <- "All"


table_reduced_concentrated_feed_total <- rbind.fill(areas_average_yield[,names(areas_average_yield)%in%names(total_animal),with=F], total_animal, total )
table_reduced_concentrated_feed_total$Scenario <- NULL
table_reduced_concentrated_feed_total

p <- table_reduced_concentrated_feed_total
p <- p[,c(1:3,5:9,4)]

names(p) <- gsub("ReducedTo_","",names(p))
names(p) <- gsub("_"," ",names(p))

p$Scenario <- NULL

units <- data.frame("","","","(t DM)","(t DM)","(t DM)","(t DM)","(t DM)","")
names(units) <- names(p)
p <- rbind(units,p)
p
require(xtable)
print(xtable(p, type = "latex"), file = "table_reduced_concentrated_feed_total.tex",include.rownames=F, hline.after=c(-1,1,nrow(p)-3,nrow(p)-1,nrow(p)))



########

p <- table_reduced_roughage
p$ReducedTo_Proportion <- p$Proportion
p$Scenario <- "II"

areas_average_yield <- p[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x),digits = 0), big.mark=","),")")), by=.(Animal, Feed, Feed_item)]
dummy_digits <- p[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Animal, Feed,Feed_item)]
areas_average_yield$ReducedTo_Proportion <- dummy_digits$ReducedTo_Proportion

areas_average_yield[] <- lapply(areas_average_yield, function(x) gsub(" \\(\u00B10\\)","", x))
average_yield_Rough_digits <- p[,lapply(.SD, mean), by=.(Import_CH,Scenario,Animal, Feed, Feed_item)]


Sum_digits <- setDT(p)[,list(Feed_baseline=sum(Feed_baseline),Reduction_in_roughage_area=sum(Reduction_in_roughage_area), Reduction_in_roughage=sum(Reduction_in_roughage),Feed_reduced=sum(Feed_reduced)), by=.(Scenario,Animal,Feed,Protein_legume_total,Total_feed_raw)]
Sum_digits$Protein_legume_total <- NULL
Sum_digits$Total_feed_raw <- NULL
Sum_digits$Reduction_in_roughage_area[Sum_digits$Reduction_in_roughage_area==0] <- NA
Sum_digits$ReducedTo_Proportion <- Sum_digits$Feed_reduced/ Sum_digits$Feed_baseline

Sum_digits[,list(min(ReducedTo_Proportion), max(ReducedTo_Proportion)),by=Animal]

dummy_digits_rot <- Sum_digits[,lapply(.SD, function(x) paste0(round(mean(x),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Animal, Feed, Scenario)]
total_animal <- Sum_digits[,lapply(.SD, function(x) paste0(format(round(mean(x,na.rm=T),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x,na.rm=T),digits = 0), big.mark=","),")")), by=.(Animal, Feed, Scenario)]
total_animal$ReducedTo_Proportion <- dummy_digits_rot$ReducedTo_Proportion

total_animal[] <- lapply(total_animal, function(x) gsub(" \\(\u00B10\\)","", x))
total_animal[] <- lapply(total_animal, function(x) gsub("NaN \\(\u00B1NA\\)","0", x))
total_animal$Animal <- paste("Total",total_animal$Animal)
total_animal$Feed_item <- "All"

Sum_digits <- setDT(p)[,list(Feed_baseline=sum(Feed_baseline),Reduction_in_roughage_area=sum(Reduction_in_roughage_area), Reduction_in_roughage=sum(Reduction_in_roughage),Feed_reduced=sum(Feed_reduced)), by=.(Scenario,Feed,Protein_legume_total,Total_feed_raw)]
Sum_digits$ReducedTo_Proportion <- Sum_digits$Feed_reduced/ Sum_digits$Feed_baseline

dummy_digits_rot <- Sum_digits[,lapply(.SD, function(x) paste0(round(mean(x,na.rm=T),digits = 2)," (\u00B1",round(sd(x),digits = 3),")")), by=.(Feed, Scenario)]
total$ReducedTo_Proportion <- dummy_digits_rot$ReducedTo_Proportion
total <- Sum_digits[,lapply(.SD, function(x) paste0(format(round(mean(x),digits = 0), big.mark=",")," (\u00B1",format(round(sd(x,na.rm=T),digits = 0), big.mark=","),")")), by=.(Feed, Scenario)]
total$ReducedTo_Proportion <- dummy_digits_rot$ReducedTo_Proportion
total$Reduction_in_roughage_area <- total_animal$Reduction_in_roughage_area[1]
total[] <- lapply(total, function(x) gsub(" \\(\u00B10\\)","", x))
total$Animal <- "Total"
total$Feed_item <- "All"


table_reduced_roughage_total <- rbind.fill(areas_average_yield, total_animal, total )
table_reduced_roughage_total


p <- table_reduced_roughage_total

p <- p[,c(1:6,8,14)]
p$Feed_item <- gsub("Fodder_","Fodder ",p$Feed_item)
p$Feed_item <- gsub("_","\\. ",p$Feed_item)

names(p) <- gsub("_"," ",names(p))
names(p) <- gsub("ReducedTo","",names(p))
p
# p[,4:7] <- lapply(p[,4:7], round )
# p$Proportion <- round(p$Proportion, digits=2)

units <- data.frame("","","","(t DM)","(ha)","(t DM)","(t DM)","")
names(units) <- names(p)
p <- rbind(units,p)


p
require(xtable)
# options(xtable.sanitize.text.function=identity)
print(xtable(p, type = "latex", digits=c(1)), file = "table_reduced_roughage_total.tex",include.rownames=F, hline.after=c(-1,1,nrow(p)-3,nrow(p)-1,nrow(p)))


##########

df_protein_all <- get_results(balance, 3)

df_protein_all_net <- get_results(balanceNet, 3)


p <- melt.data.table(df_protein_all, measure.vars = c("Protein"))

ggplot(p, aes(y=Total_feed_raw,x=Protein_legume_total, color=value )) + ylab("Reduction to")+ xlab("Legume protein produced in CH (t/year)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.size = unit(1, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=3)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_gradientn(colors=tol5qualitative[c(1:5)] )+
  # scale_y_continuous(labels = scales::scientific)+
  facet_wrap(Animal~.,switch="y", scales = "free",nrow=1)


p <- df_protein_all[, list(Sum_Protein=sum(Protein,na.rm = T)),by=.(Total_feed_raw,Protein_legume_total,Animal)]

p <- dcast.data.table(p, Total_feed_raw+Protein_legume_total~Animal, value.var = "Sum_Protein")

ggplot(p, aes(y=Monogastrics,x=Legume, color=Ruminant )) + ylab("Monogastrics")+ xlab("Legume protein produced in CH (t/year)")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.size = unit(1, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=4)+
  # geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_gradientn(colors=tol5qualitative[c(1:5)] )
# scale_y_continuous(labels = scales::scientific)+
# facet_wrap(Source~.,switch="y", scales = "free",nrow=1)
#########

# https://www.sbv-usp.ch/fileadmin/user_upload/07_SES2021_Nahrungsmittelbilanz.pdf
FoodAnimalsTJ <- data.frame(Item=c("Meat","Egg","Fish","Milk","AnimalFat"),FoodProducedTJ=c(3131,327,8,6594,1413), FoodConsumedTJ=c(3723,579,336,5872,1657),ConsumedPerDayPerPersonKJ=c(1157,180,104,1825,515),ConsumedPerCapitaKG=c(47,13,8,239,6))
# FoodMeatTJ = 3723, 
# FoodEggTJ = 579 ,
FoodFishTJ = 336 
# FoodMilkTJ = 5872 ,
# FoodAnimalFatTJ = 1657)
# sum(FoodAnimalsTJ)
# FoodAnimalsTJ <-12167
TotalFoodAnimalsTJ <-sum(FoodAnimalsTJ$FoodConsumedTJ)


(TotalFoodAnimalsTJ-FoodFishTJ)/TotalFoodAnimalsTJ

# MeatJoulesperCapita <- FoodAnimalsTJ$FoodMeatTJ * 10^12 / SwissPopulation  #Joules per capita
# MeatKGperCaptita <- 48 #kg per capita
# MeatJoulesperCapita/MeatKGperCaptita/1000 # kj of meat
# 1190/(47/365)


FoodAnimalsTJ$KJperKG <- FoodAnimalsTJ$ConsumedPerDayPerPersonKJ/(FoodAnimalsTJ$ConsumedPerCapitaKG/365)

SwissFoodAnimalsTJ <- 11473 


Protein_legume_Food <- unique(df_protein_all$Protein_legume_total)

prot_tha_frac$Protein_legume_Food <- Protein_legume_Food

prot_tha_frac$Soybean_TJ <- prot_tha_frac$Protein_legume_Food * prot_tha_frac$soybean_prot_tha*1600*10000/10^9 #https://naehrwertdaten.ch/en/ 
prot_tha_frac$Fababean_TJ <- prot_tha_frac$Protein_legume_Food * prot_tha_frac$fababean_prot_tha*1425*10000/10^9 # Dhull et al. 2021
prot_tha_frac$FPea_TJ <- prot_tha_frac$Protein_legume_Food * prot_tha_frac$pea_prot_tha*1400*10000/10^9 #https://naehrwertdaten.ch/en/ 
prot_tha_TJ <- prot_tha_frac[5:7]
prot_tha_TJ$LegumeProductionTJ <- rowSums(prot_tha_TJ)

prot_tha_TJ$FoodPlantsTJ <- 28679  
prot_tha_TJ$SwissFoodPlantsTJ <- 11228 

prot_tha_TJ$FoodProductionTJ <- prot_tha_TJ$SwissFoodPlantsTJ + prot_tha_TJ$LegumeProductionTJ

prot_tha_TJ$Selfsufficiency <- prot_tha_TJ$FoodProductionTJ  /prot_tha_TJ$FoodPlantsTJ 

get_selfsufficiency <- function(df_protein_all){
df_protein_animal <- df_protein_all[, list(Sum_Protein=sum(Protein,na.rm = T)),by=.(Animal,Total_feed_raw,Protein_legume_total)]
df_protein_animal$RuminantProteinProduction <- SwissMilkProteinProduction +SwissRuminantProteinProduction
df_protein_animal$MonogastricProteinProduction <- SwissEggProteinProduction +SwissMonogastricProteinProduction
df_protein_animal <- dcast.data.table(df_protein_animal, ...~Animal, value.var = "Sum_Protein")
df_protein_animal$r_monogastric <- df_protein_animal$Monogastrics/df_protein_animal$MonogastricProteinProduction
df_protein_animal$r_ruminant <- df_protein_animal$Ruminant/df_protein_animal$RuminantProteinProduction


df_protein_meat <- df_protein_all[, list(Sum_Protein=sum(Protein,na.rm = T)),by=.(Scenario,Total_feed_raw,Protein_legume_total)]
df_protein_meat <- dcast.data.table(df_protein_meat, ...~Scenario, value.var = "Sum_Protein")

df_protein_animal$r_meat <- (df_protein_meat$Meat)/(SwissMeatProteinProduction)
df_protein_animal$Iteration <- 1:100
r_food <- melt.data.table(df_protein_animal, id.vars = c("Iteration"),measure.vars = c("r_monogastric","r_ruminant","r_meat"))

FoodAnimalsTJ$FoodProducedTJ_ImportSubstituted <- FoodAnimalsTJ$FoodConsumedTJ
FoodAnimalsTJ$FoodProducedTJ_ImportSubstituted[FoodAnimalsTJ$Item=="Milk" ] <- FoodAnimalsTJ$FoodProducedTJ[FoodAnimalsTJ$Item=="Milk" ]
FoodAnimalsTJ$variable <- c("r_meat","r_monogastric","Unchanged","r_ruminant","r_meat")

SelfsufficiencyMeat <- merge(FoodAnimalsTJ, r_food, by="variable")
SelfsufficiencyMeat$FoodProducedTJ_new <- SelfsufficiencyMeat$FoodProducedTJ_ImportSubstituted*SelfsufficiencyMeat$value

SelfsufficiencyMeat <- setDT(SelfsufficiencyMeat)[,list(FoodProductionTJ=sum(FoodProducedTJ_new)),by=Iteration]
SelfsufficiencyMeat$Source <- "Animal"
SelfsufficiencyMeat$TotalTJ <- SelfsufficiencyMeat$FoodProductionTJ +FoodFishTJ
BaselineMeat <- data.frame(FoodProductionTJ= SwissFoodAnimalsTJ,TotalTJ=TotalFoodAnimalsTJ ,Source="Animal", Iteration=0 )
BaselineMeatNet <- data.frame(FoodProductionTJ= 8914,TotalTJ=TotalFoodAnimalsTJ ,Source="Animal", Iteration=-1 )

SelfsufficiencyMeat <- rbind(SelfsufficiencyMeat,BaselineMeat,BaselineMeatNet)


SelfsufficiencyPlant <- data.frame(FoodProductionTJ=prot_tha_TJ$FoodProductionTJ,Source="Plant")
SelfsufficiencyPlant <- rbind(SelfsufficiencyPlant,SelfsufficiencyPlant,SelfsufficiencyPlant,SelfsufficiencyPlant,SelfsufficiencyPlant)
SelfsufficiencyPlant <- rbind(SelfsufficiencyPlant,SelfsufficiencyPlant)
SelfsufficiencyPlant$Iteration <- 1:100
BaselinePlant <- data.frame(FoodProductionTJ= prot_tha_TJ$SwissFoodPlantsTJ[1] ,Source="Plant", Iteration=0 )
BaselinePlantNet <- data.frame(FoodProductionTJ= prot_tha_TJ$SwissFoodPlantsTJ[1] ,Source="Plant", Iteration=-1 )
SelfsufficiencyPlant <- rbind(SelfsufficiencyPlant,BaselinePlant,BaselinePlantNet)

SelfsufficiencyPlant$TotalTJ <- prot_tha_TJ$FoodPlantsTJ[1]

Selfsufficiency <- rbind(SelfsufficiencyPlant, SelfsufficiencyMeat)
Selfsufficiency$Type <- "Seperate"


SelfsufficiencyOverall <- setDT(Selfsufficiency)[,list(FoodProductionTJ=sum(FoodProductionTJ),TotalTJ=sum(TotalTJ,na.rm = T)),by=.(Iteration)]
SelfsufficiencyOverall$Source <- "Overall"
SelfsufficiencyOverall$Type <- "Overall"



Selfsufficiency_all <- rbind(SelfsufficiencyOverall, Selfsufficiency)
Selfsufficiency_all$Scenario <- Selfsufficiency_all$Source
Selfsufficiency_all$Scenario[Selfsufficiency_all$Iteration==0] <- "Baseline-gross"
Selfsufficiency_all$Scenario[Selfsufficiency_all$Iteration==-1] <- "Baseline-net"

Selfsufficiency_all$Selfsufficiency <- Selfsufficiency_all$FoodProductionTJ/Selfsufficiency_all$TotalTJ*100

return(Selfsufficiency_all)
}

Selfsufficiency_gross <- get_selfsufficiency(df_protein_all)
Selfsufficiency_gross$Scenario[!Selfsufficiency_gross$Scenario%in%c("Baseline","Baseline-net","Baseline-gross")] <- "Improved gross \nself-sufficiency"


Selfsufficiency_net <- get_selfsufficiency(df_protein_all_net)
Selfsufficiency_net$Scenario[!Selfsufficiency_net$Scenario%in%c("Baseline","Baseline-net","Baseline-gross")] <- "Improved net \nself-sufficiency"
 
Selfsufficiency <- rbind(Selfsufficiency_gross,Selfsufficiency_net)
Selfsufficiency$Type <- "Gross"
Selfsufficiency$Type[grepl("net",Selfsufficiency$Scenario)] <- "Net"
p <- Selfsufficiency
p$Improved <- "Baseline"
p$Improved[grepl("Improved",p$Scenario)] <- "Improved"

p$Scenario <- as.factor(p$Scenario)
# p$Scenario <- factor(p$Scenario, levels = levels(p$Scenario)[4:1])
p1 <- p[,list(value=mean(Selfsufficiency),SD=sd(Selfsufficiency)),by=.(Scenario,Type,Source,Improved)]


ggplot(data=p1, aes(y=value,x=Type, color=Source ))+ ylab("Self-sufficiency (%)")+ xlab("Food production")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.text.y = element_blank(),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(1.75, "lines"),legend.position="top", panel.grid.minor.x = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point( size=4, shape= 18)+
  geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey40", width = 0.15)  +
  # geom_boxplot(data=subset(p,Scenario!="Baseline"), outlier.colour = "grey",show.legend = F)+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(name="",values= c(tol4qualitative))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  guides(color = guide_legend(nrow=3))+
  facet_grid(.~Improved,switch="y", scales = "free")

ggplot(data=p1, aes(y=value,x=Improved,color=Improved, shape=Source, alpha=Type ))+ ylab("Self-sufficiency (%)")+ xlab("Food production")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.text.y = element_blank(),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(1.75, "lines"),legend.position="top", panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(size=4)+
  # geom_point(data=subset(p1, Type=="Net"), alpha=0.4, size=4)+
  geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey40", width = 0.15)  +
  # geom_boxplot(data=subset(p,Scenario!="Baseline"), outlier.colour = "grey",show.legend = F)+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(name="",values= c("grey30",tol4qualitative[2]), guide="none")+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_alpha_manual(name="",values = c(1, 0.5))+
  scale_shape_manual(name="",values=19:16)+
  guides(shape = guide_legend(nrow=3), alpha = guide_legend(nrow=3))

ggSelfsufficiency <- ggplot(data=p, aes(y=Selfsufficiency,x=Source,color=Improved, shape=Improved))+ ylab("Self-sufficiency (%)")+ xlab("Food production")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.text.y = element_blank(),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(0.75, "lines"),legend.position="top", panel.grid.minor = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  # geom_point(size=4)+
  # geom_point(data=subset(p1, Type=="Net"), alpha=0.4, size=4)+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey40", width = 0.15)  +
  geom_boxplot(outlier.colour = "grey")+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(name="",values= c("grey30",tol4qualitative[2]))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_shape_manual(name="",values=19:16)+
  guides(shape = guide_legend(nrow=2), color = guide_legend(nrow=2))+
  facet_grid(.~Type,switch="y", scales = "free")
ggSelfsufficiency




p <- melt.data.table(p, measure.vars = c("FoodProductionTJ","Selfsufficiency"))

ggplot(data=p, aes(y=value,x=Source, color=Scenario ))+ ylab("")+ xlab("Source")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=1,color="white"),legend.key.width = unit(1.75, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 45, hjust = 1),text = element_text(size=11),axis.title = element_text(size = 11))+
  geom_point(data=subset(p,Scenario=="Baseline"), size=4, shape= 9)+
  geom_boxplot(data=subset(p,Scenario!="Baseline"), outlier.colour = "grey",show.legend = F)+#+stat_boxplot(geom = "errorbar", width = 0.2)
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(name="",values= c(tol2qualitative,"grey50","grey30"))+
# scale_y_continuous(labels = scales::scientific)+
  facet_grid(variable~Type,switch="y", scales = "free")


# second_row <- plot_grid( ggSelfsufficiency, NULL, rel_widths =   c(1,1), ncol = 2, labels=c("C",""))  #,vjust=0.5+
# o1 <- plot_grid( first_row, second_row, rel_heights =  c(1,0.75), ncol = 1, labels="")  #,vjust=0.5+

first_row <- plot_grid(ggR_ruminants, ggR_monogastric, ggSelfsufficiency, rel_widths =   c(1,1,0.6), ncol = 3, labels="AUTO")  #,vjust=0.5+

# ggsave("Sensitivity_production_on_r.pdf",  width = 210, height = 100, units = "mm", dpi = 300, first_row)

p <- Selfsufficiency[,list(Selfsufficiency=paste0(round(mean(Selfsufficiency),digits = 1)," (\u00B1",round(sd(Selfsufficiency),digits = 2),")"),Food_production=paste0(format(round(mean(FoodProductionTJ),digits = 0), big.mark=",")," (\u00B1",round(sd(FoodProductionTJ),digits = 0),")") ,Food_consumption=paste0(format(round(mean(TotalTJ),digits = 0), big.mark=",")," (\u00B1",round(sd(TotalTJ),digits = 0),")") ),by=.(Type,Scenario,Source)]
p <- p[order(p$Type, p$Scenario,p$Source),]
p$Scenario <- gsub("\n","" ,p$Scenario)
p$Scenario <- gsub("line-","line " ,p$Scenario)
p$Scenario <- gsub(" net","" ,p$Scenario)
p$Scenario <- gsub(" gross","" ,p$Scenario)
names(p) <- gsub("_"," ",names(p))
names(p)[names(p)=="Selfsufficiency"] <- "Self-sufficiency"

units <- data.frame(" "," "," ","(\\%)","(TJ)","(TJ)")
names(units) <- names(p)
p <- rbind(units,p)
p
require(xtable)
options(xtable.sanitize.text.function=identity)
print(xtable(p, type = "latex"), file = "Selfsufficiency.tex",include.rownames=F, hline.after=c(-1,1,7,nrow(p)))
####################################

#######
average_yield_conc_digits
average_yield_Rough_digits

InOutConc <- rbind.fill(average_yield_conc_digits, average_yield_Rough_digits)
InOutConc <- setDT(InOutConc)
InOutConc$Scenario <- NULL
InOutConc$Import_CH <- NULL

InOutConc <- InOutConc[,lapply(.SD,sum), by=.(Animal,Feed,Feed_item)]
InOutConc$Import[is.na(InOutConc$Import)] <- 0
InOutConc$Legume_feed_new[is.na(InOutConc$Legume_feed_new)] <- 0
InOutConc$Saved_total <- InOutConc$Quantity-InOutConc$Feed_reduced
# InOutConc$Legume_feed_new[(InOutConc$Feed_item%in%c("Side Products")&InOutConc$Animal=="Monogastrics")] <- InOutConc$Legume_feed_new[(InOutConc$Feed_item%in%c("Side Products")&InOutConc$Animal=="Monogastrics")]-10000 ## balance to avoid negativ numbers
# InOutConc$Legume_feed_new[(InOutConc$Feed_item%in%c("Conc others")&InOutConc$Animal=="Monogastrics")] <- 10000 ## balance to avoid negativ numbers


InOutConc$Feed_reduced <- InOutConc$Feed_reduced-InOutConc$Legume_feed_new
InOutConc$Import <- abs(InOutConc$Import)
InOutConc$Saved <- InOutConc$Saved_total - InOutConc$Import
InOut_Scenario <- melt.data.table(InOutConc, id.vars = c("Animal","Feed","Feed_item"),measure.vars = c("Import","Feed_reduced","Legume_feed_new","Saved"),variable.name = "Scenario")
InOut_Scenario <- subset(InOut_Scenario, Feed_item!="Add_Roughage_from_monogastric" )


InOut_Scenario[,sum(value,na.rm = T),by=Animal]
InOut_Scenario[,sum(value,na.rm = T),by=Feed]
InOut_Scenario[,sum(value,na.rm = T),by=.(Feed,Animal)]
sum(InOut_Scenario[,sum(value,na.rm = T),by=.(Scenario)]$V1)
sum(InOut_Scenario[,sum(value,na.rm = T),by=.(Feed)]$V1)



InOut<- (InOut_Scenario)
InOut$value[is.na(InOut$value)] <- 0
# InOut$value <- abs(InOut$value)
InOut$value[InOut$Scenario=="Saved"&InOut$Animal=="Monogastrics"]
InOut$value[InOut$Scenario=="Saved"&InOut$Animal=="Ruminants"]

InOut <- InOut[order(InOut$Feed_item),]
# 
balanceConc <- sum(InOut$value[InOut$Scenario=="Saved"&InOut$Feed=="Concentrated"])
# InOut$value[InOut$Scenario=="Saved"&InOut$Feed=="Concentrated"] <- 0

InOut$Scenario <- as.character(InOut$Scenario)
InOut$Scenario[InOut$Scenario=="Feed_reduced"&InOut$Animal=="Ruminants"] <- "Ruminants_new"
InOut$Scenario[InOut$Scenario=="Feed_reduced"&InOut$Animal=="Monogastrics"] <- "Monogastric_new"
InOut$Scenario <- gsub("Import","Saved_imports", InOut$Scenario)
InOut$Scenario[InOut$Scenario=="Saved"] <- "Saved_for_legumes"
InOut$Feed_item[InOut$Feed_item=="Side Products"] <- "Side products"

InOut$cols <- "grey"
  InOut$cols[InOut$Scenario == "Legume_Feed"] <- "#4477AA"
    
  InOut$cols[InOut$Scenario == "Saved_imports"] <- "#117733"
    InOut$cols[InOut$Scenario == "Saved"] <- "#DDCC77"
      
    InOut[,sum(value,na.rm = T),by=Animal]
    InOut[,sum(value,na.rm = T),by=.(Animal,Feed)]
    
    
    p <- InOut[,c("Feed_item","Feed","Animal","Scenario","value")]
    
    p$Feed_item <- as.factor(p$Feed_item)
    p$Feed_item <- factor(p$Feed_item , levels = levels(p$Feed_item)[c(1,7,2,3:6)]  )
    
    p$Scenario <- gsub("_"," ", p$Scenario )
    p$Feed_item <- gsub("_"," ", p$Feed_item)
    
    
    p$Scenario <- as.factor(p$Scenario)
    p$Scenario <- factor(p$Scenario , levels = levels(p$Scenario)[c(2,1,3:5)]  )
    
    p <- p[, value:=0[round(sum(value),digits = 1)==0], by=.(Feed_item,Feed,Scenario)] #cannot display negative values properly
    
    p$value <- p$value/10^6
    
    require(ggalluvial)
    
    ggFeedFluxes <- ggplot(p, aes(y = value, axis1 = Feed_item, axis2 = Feed, axis3=Animal, axis4=Scenario)) + ylab("Feed produced (Mt DM/year)")+
      geom_alluvium(aes(fill=Scenario) ) +
      geom_stratum( width        =  1/6 ,  color        = '#222222') + 
      geom_label(stat = "stratum",aes(label = after_stat(stratum)), size=3) +
      # geom_text(stat = "stratum", aes(label = after_stat(stratum)), angle = 90) +
      scale_x_discrete(limits = c("Baseline item","Baseline feed","Baseline animal","Improved \n self-sufficiency") ) +
      # scale_x_continuous(breaks = 1:2, labels = c("Sex", "Class")) + 
      scale_fill_manual( values=c("darkblue", "#88CCEE", "#117733", "darkred", "#DDCC77"))+
      geom_text(stat  = 'stratum',label.strata =  TRUE)+
      theme_bw()+ theme(legend.position = "none")
    ggFeedFluxes
    # ggsave("InOutFeedFluxes.pdf",  width = 170, height = 140, units = "mm", dpi = 300, bg="white")
    
    
    protein_df <- df_protein_all[, list(Protein=mean(Protein,na.rm = T)),by=.(Source, Scenario, Animal)]
    df_protein_mean <- df_protein_all[, list(Protein=mean(Protein,na.rm = T),SD=sd(Protein)),by=.(Source, Scenario, Animal)]
    df_protein_mean
    df_protein_sum <- df_protein_all[, list(Protein=sum(Protein,na.rm = T),SD=sd(Protein)),by=.(Scenario,Total_feed_raw,Protein_legume_total)]
    df_protein_sum[, list(Protein=mean(Protein,na.rm = T),SD=sd(Protein)),by=.(Scenario)]
    
    df_protein_plant <- df_protein_sum[, list(Prop=Protein[Scenario=="Plant"]/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    df_protein_plant[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    
    df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Scenario!="Plant"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    
    df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Scenario=="Milk"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Scenario=="Meat"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Scenario=="Egg"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    
    # #baseline
    # df_protein_sum <- df_protein_all[, list(Protein=sum(Protein,na.rm = T),SD=sd(Protein)),by=.(Source,Total_feed_raw,Protein_legume_total)]
    # df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Source=="Milk"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    # df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    # df_protein_animal <- df_protein_sum[, list(Prop=sum(Protein[Source!="Milk"])/sum(Protein)),by=.(Total_feed_raw,Protein_legume_total)]
    # df_protein_animal[, list(Protein=mean(Prop,na.rm = T), min=min(Prop), max=max(Prop),SD=sd(Prop))]
    
    
    p <- protein_df
    p$Protein <- p$Protein/10^6
    
    
    p$Animal <- as.factor(p$Animal)
    p$Animal <- factor(p$Animal , levels = levels(p$Animal)[c(2,3,1)]  )
    p$Source <- gsub("-"," ", p$Source )
    
    ggProteinFluxes <- ggplot(p, aes(y = Protein, axis1 = Source, axis2= Scenario)) + ylab("Food protein produced (Mt DM/year)")+
      geom_alluvium(aes(fill=Scenario) ) +
      geom_stratum( width        =  1/6 ,  color        = '#222222') + 
      geom_label(stat = "stratum",aes(label = after_stat(stratum)), size=3) +
      # geom_text(stat = "stratum", aes(label = after_stat(stratum)), angle = 90) +
      scale_x_discrete(limits = c("Baseline","Improved \n self-sufficiency") ) +
      # scale_x_continuous(breaks = 1:2, labels = c("Sex", "Class")) + 
      scale_fill_manual( values=tol4qualitative[c(3,1,2,4)])+
      geom_text(stat  = 'stratum',label.strata =  TRUE)+
      theme_bw()+ theme(legend.position = "none")
    
    require(cowplot)
    
    # first_row <- plot_grid( ggFeedFluxes, ggProteinFluxes, ggSelfsufficiency, rel_widths =   c(1,0.375,0.375), ncol = 3, labels="AUTO")  #,vjust=0.5+
    # ggsave("InOutFeedFluxe2s.png",  width = 260, height = 160, units = "mm", dpi = 300, bg="white",first_row)
    
    first_row <- plot_grid( ggFeedFluxes, ggProteinFluxes, rel_widths =   c(1,0.375), ncol = 2, labels="AUTO")  #,vjust=0.5+
    
    # ggsave("InOutFeedFluxes.pdf",  width = 200, height = 200, units = "mm", dpi = 300, bg="white",first_row)
    
