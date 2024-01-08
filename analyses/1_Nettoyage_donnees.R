

library(XLConnect)


# Métadonnées stations
metadata <- readWorksheetFromFile(file = "data/raw-data/Bennes_SURFSTAT Corse 2022.xlsx", sheet = "Feuil2")
metadata <- metadata[1:(min(which(is.na(metadata$Numero_Echantillon)))-1),]
metadata$Heure_Fichier <- NULL
metadata$Heure_Video <- NULL
metadata$Observation_terrain <- NULL
metadata$Pas_fait <- NULL
metadata$Video_a_verifier <- NULL

metadata$RHO <- toupper(metadata$RHO)
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "-10"] <- "<10%"
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "N/A"] <- NA
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "+90"] <- ">90%"
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "NSP"] <- NA
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "10-50"] <- "10-50%"
metadata$Recouvrement_Rhodolithes_video[metadata$Recouvrement_Rhodolithes_video == "50-90"] <- "50-90%"

# Suppression des échantillons sans coordonnées
metadata <- metadata[metadata$X_Bateau != "N/A",]

# Création ID unique simplifié et suppression des observations inutiles
metadata$Numero_Echantillon <- gsub(" ", "", metadata$Numero_Echantillon)
metadata$Numero_Echantillon <- gsub("Taverna", "", metadata$Numero_Echantillon)
metadata <- metadata[-grep("Observation", metadata$Numero_Echantillon),]
metadata$Sample <- sapply(metadata$Numero_Echantillon, function(x) paste0("T", strsplit(x, "T")[[1]][2]))


# Correction des coordonnées
for (j in 1:ncol(metadata)){
  
  metadata[,j] <- gsub(",", ".", metadata[,j])
  metadata[,j] <- gsub("'", "", metadata[,j])
  
}#eo for j

metadata$Profondeur <- as.numeric(metadata$Profondeur)

# Transformation des coordonnées GPS pour tout mettre en degrés décimaux
metadata[["Latitude"]] <- toDegDec(metadata$Y_Bateau)
metadata[["Longitude"]] <- toDegDec(metadata$X_Bateau)


##################
### Calcimetry ###
##################

# List files
calcimetry_files <- list.files(path = "data/raw-data/Laboratoire", pattern = glob2rx("*calcimetry*.xlsx"), recursive = TRUE, full.names = TRUE)

# Concatenate data
calcimetry_data <- NA

for (file in calcimetry_files){
  
  # Get sheet name
  wb <- loadWorkbook(file)
  sheet_name <- getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  data <- readWorksheetFromFile(file = file, 
                                sheet = sheet_name,
                                header = FALSE,
                                startRow = 1,
                                endRow = 20,
                                startCol = 1,
                                endCol = 20,
                                autofitRow = FALSE, 
                                autofitCol = FALSE)
  
  
  col <- grep("Sample", data)[1]
  row <- grep("Sample", data[, col])[1]
  
  data <- readWorksheetFromFile(file = file, 
                                     sheet = sheet_name,
                                     header = FALSE,
                                     startRow = row + 1,
                                     startCol = col - 1,
                                     endCol = col + 2,
                                     autofitRow = TRUE, 
                                     autofitCol = FALSE)
  
  calcimetry_data <- rbind(calcimetry_data, data)
  
}#eo for file

calcimetry_data <- calcimetry_data[-1, -c(1,3)]
colnames(calcimetry_data) <- c("Sample", "CaCO3")
calcimetry_data$Sample <- gsub(" ", "", calcimetry_data$Sample)

# Averaging over duplicates
calcimetry_data <- aggregate(CaCO3 ~ Sample, calcimetry_data, mean, na.rm = TRUE)



######################
### Organic Matter ###
######################

# List files
organic_matter_files <- list.files(path = "data/raw-data/Laboratoire", pattern = glob2rx("*organic_matter*.xlsx"), recursive = TRUE, full.names = TRUE)

# Concatenate data
organic_matter_data <- NA

for (file in organic_matter_files){
  
  # Get sheet name
  wb <- loadWorkbook(file)
  sheet_name <- getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  data <- readWorksheetFromFile(file = file, 
                                sheet = sheet_name,
                                header = FALSE,
                                startRow = 1,
                                endRow = 20,
                                startCol = 1,
                                endCol = 20,
                                autofitRow = FALSE, 
                                autofitCol = FALSE)
  
  
  col <- grep("Sample", data)[1]
  row <- grep("Sample", data[, col])[1]
  
  data <- readWorksheetFromFile(file = file, 
                                sheet = sheet_name,
                                header = FALSE,
                                startRow = row+1,
                                startCol = col-1,
                                endCol = col+7,
                                autofitRow = TRUE, 
                                autofitCol = FALSE)
  
  organic_matter_data <- rbind(organic_matter_data, data)
  
}#eo for file

organic_matter_data <- organic_matter_data[-1, c(2,ncol(organic_matter_data))]
colnames(organic_matter_data) <- c("Sample","LOI_550")
organic_matter_data$Sample <- gsub(" ", "", organic_matter_data$Sample)

# Averaging over duplicates
organic_matter_data <- aggregate(LOI_550 ~ Sample, organic_matter_data, mean, na.rm = TRUE)



##################
### Grain size ###
##################

# # List files
# grain_size_files <- list.files(path = "1_DONNEES_BRUTES/Laboratoire", pattern = glob2rx("*grain_size*.xlsx"), recursive = TRUE, full.names = TRUE)
# 
# # Concatenate data
# grain_size_data <- NA
# #error = "T6S0038"
# for (file in grain_size_files){
#   
#   # Get sheet name
#   wb <- loadWorkbook(file)
#   sheet_name <- getSheets(wb)[1]
#   
#   # Get position of "Sample" keyword
#   sheet <- readWorksheetFromFile(file = file, 
#                                 sheet = sheet_name,
#                                 header = FALSE,
#                                 startRow = 1,
#                                 endRow = 60,
#                                 startCol = 1,
#                                 endCol = 50,
#                                 autofitRow = FALSE, 
#                                 autofitCol = FALSE)
#   
#   
#   # CLAY
#   col <- grep("CLAY",sheet)[1]
#   row <- grep("CLAY",sheet[,col])[1]
#   
#   data <- readWorksheetFromFile(file = file, 
#                                 sheet = sheet_name,
#                                 header = FALSE,
#                                 startRow = row,
#                                 endRow = row+4,
#                                 startCol = col+2,
#                                 #endCol = col+7,
#                                 autofitRow = FALSE, 
#                                 autofitCol = TRUE)
#   
#   data[2,] <- gsub("[.$ls]","",data[2,])
#   data[1,] <- paste0(data[1,],data[2,])
#   
#   clay <- data.frame("Sample" = as.vector(unlist(data[1,])),
#                      "pc_inf2um" = as.vector(unlist(data[5,])))
#   
#   # if (length(grep(error,clay$Sample))>0){
#   #   
#   #   cat("PRoblem with file : ",file)
#   #   
#   # }
#   
#   # VASE SAND
#   col <- grep("VASE - SAND",sheet)[1]
#   row <- grep("VASE - SAND",sheet[,col])[1]
#   
#   data <- readWorksheetFromFile(file = file, 
#                                 sheet = sheet_name,
#                                 header = FALSE,
#                                 startRow = row,
#                                 endRow = row+9,
#                                 startCol = col+2,
#                                 #endCol = col+7,
#                                 autofitRow = FALSE, 
#                                 autofitCol = TRUE)
#   
#   sand <- data.frame("Sample" = as.vector(unlist(data[1,])),
#                      "pc_2um" = as.vector(unlist(data[5,])),
#                      "pc_63um" = as.vector(unlist(data[6,])),
#                      "pc_125um" = as.vector(unlist(data[7,])),
#                      "pc_250um" = as.vector(unlist(data[8,])),
#                      "pc_500um" = as.vector(unlist(data[9,])),
#                      "pc_1000um" = as.vector(unlist(data[10,])))
#   
#   # GRAVEL
#   col <- grep("GRAVEL",sheet)[1]
#   row <- grep("GRAVEL",sheet[,col])[1]
#   
#   data <- readWorksheetFromFile(file = file, 
#                                 sheet = sheet_name,
#                                 header = FALSE,
#                                 startRow = row,
#                                 endRow = row+4,
#                                 startCol = col+2,
#                                 #endCol = col+7,
#                                 autofitRow = FALSE, 
#                                 autofitCol = TRUE)
#   
#   gravel <- data.frame("Sample" = as.vector(unlist(data[1,])),
#                      "pc_sup2um" = as.vector(unlist(data[5,])))
#   
#   grain_size_data <- rbind(grain_size_data,
#                            cbind(clay,sand[,-1],pc_sup2um=gravel[,-1]))
#   
# }#eo for file
# 
# grain_size_data <- grain_size_data[-1,]
# grain_size_data$Sample <- gsub(" ","",grain_size_data$Sample)
# grain_size_data$Sample <- gsub("Taverna","",grain_size_data$Sample)
# grain_size_data$Sample <- sapply(grain_size_data$Sample, function(x) paste0("T",strsplit(x,"T")[[1]][2]))
# 
# for (j in 2:ncol(grain_size_data)){
#   
#   grain_size_data[,j] <- as.numeric(gsub(",",".",grain_size_data[,j]))
#   
# }




################################
### Grain size detailed data ###
################################

# List files
grain_size_files <- list.files(path = "data/raw-data/Laboratoire", pattern = glob2rx("*.xls"), recursive = TRUE, full.names = TRUE)

# Concatenate data
grain_size_data <- NA
#error = "T6S0038"

for (file in grain_size_files){
  
  # Get sheet name
  wb <- loadWorkbook(file)
  sheet_name <- getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  sheet <- readWorksheetFromFile(file = file, 
                                 sheet = sheet_name,
                                 header = FALSE,
                                 #startRow = 1,
                                 #endRow = 60,
                                 #startCol = 1,
                                 #endCol = 50,
                                 autofitRow = TRUE, 
                                 autofitCol = TRUE)
  
  sheet[,2] <- gsub(",", ".", sheet[,2])
  
  
  sample <- strsplit(file,"/")[[1]]
  sample <- gsub(".xls", "", sample[length(sample)])
  sample <- gsub("Taverna", "", sample)
  sample <- paste0("T", strsplit(sample, "[T]")[[1]][2])
  
  # Statistics
  mean <- as.numeric(sheet[grep("Mean", sheet[,1])[1],2])
  median <- as.numeric(sheet[grep("Median", sheet[,1])[1],2])
  mode <- as.numeric(sheet[grep("Mode", sheet[,1])[1],2])
  SD <- as.numeric(sheet[grep("S.D", sheet[,1])[1],2])
  variance <- as.numeric(sheet[grep("Variance", sheet[,1])[1],2])
  CV <- as.numeric(sheet[grep("C.V", sheet[,1])[1],2])
  skewness <- as.numeric(sheet[grep("Skewness", sheet[,1])[1],2])
  kurtosis <- as.numeric(sheet[grep("Kurtosis", sheet[,1])[1],2])
  d10 <- as.numeric(sheet[grep("d10", sheet[,1])[1],2])
  d50 <- as.numeric(sheet[grep("d50", sheet[,1])[1],2])
  d90 <- as.numeric(sheet[grep("d90", sheet[,1])[1],2])
  
  
  # Particle diameters
  pc_2 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 3, 2])
  pc_4 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 4, 2])
  pc_63 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 5, 2])
  pc_125 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 6, 2])
  pc_250 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 7, 2])
  pc_500 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 8, 2])
  pc_1000 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 9, 2])
  pc_2000 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 10, 2])
  pc_2500 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 11, 2])
  pc_4000 <- as.numeric(sheet[grep("Particle Diameter", sheet[,1])[1] + 12, 2])
  
  # Transform into each category's percentage
  pc_4000 <- pc_4000 - pc_2500
  pc_2500 <- pc_2500 - pc_2000
  pc_2000 <- pc_2000 - pc_1000
  pc_1000 <- pc_1000 - pc_500
  pc_500 <- pc_500 - pc_250
  pc_250 <- pc_250 - pc_125
  pc_125 <- pc_125 - pc_63
  pc_63 <- pc_63 - pc_4
  pc_4 <- pc_4 - pc_2
  
  # Control
  # pc_2 + pc_4 + pc_63 + pc_125 + pc_250 + pc_500 + pc_1000 + pc_2000 + pc_2500 + pc_4000
  
  # if (length(grep(error,sample))>0){
  #   
  #   cat("PRoblem with file : ",file)
  #   
  # }
  
  dat <- data.frame("Sample" = sample,
                    "Mean"= mean,
                    "Median" = median,
                    "Mode" = mode,
                    "SD" = SD,
                    "Variance" = variance,
                    "CV" = CV,
                    "Skewness" = skewness,
                    "Kurtosis" = kurtosis,
                    "d10" = d10,
                    "d50" = d50,
                    "d90" = d90,
                    "pc_2" = pc_2,
                    "pc_4" = pc_4,
                    "pc_63" = pc_63,
                    "pc_125" = pc_125,
                    "pc_250" = pc_250,
                    "pc_500" = pc_500,
                    "pc_1000" = pc_1000,
                    "pc_2000" = pc_2000,
                    "pc_2500" = pc_2500,
                    "pc_4000" = pc_4000)
  
  
  grain_size_data <- rbind(grain_size_data,
                           dat)
  
}#eo for file

grain_size_data <- grain_size_data[-1, ]



###########################
### CHECKS MISSING DATA ###
###########################

# ANALYSES EN TROP
organic_matter_data$Sample[which(!is.element(organic_matter_data$Sample, metadata$Sample))] # OK
calcimetry_data$Sample[which(!is.element(calcimetry_data$Sample, metadata$Sample))] # OK
grain_size_data$Sample[which(!is.element(grain_size_data$Sample, metadata$Sample))] # OK

# ANALYSES MANQUANTES
metadata[which(!is.element(metadata$Sample, organic_matter_data$Sample)),]
metadata[which(!is.element(metadata$Sample, calcimetry_data$Sample)),]
metadata[which(!is.element(metadata$Sample, grain_size_data$Sample)),]

# Missing samples in all lab results
missing_organic <- metadata$Sample[which(!is.element(metadata$Sample, organic_matter_data$Sample))]
missing_calcimetry <- metadata$Sample[which(!is.element(metadata$Sample, calcimetry_data$Sample))]
missing_grain_size <- metadata$Sample[which(!is.element(metadata$Sample, grain_size_data$Sample))]

missing_all <- intersect(missing_grain_size, missing_organic)
missing_all <- intersect(missing_all, missing_calcimetry)

write.table(missing_organic, "data/Missing_samples_organic.csv", sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
write.table(missing_calcimetry, "data/Missing_samples_calcimetry.csv", sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
write.table(missing_grain_size, "data/Missing_samples_grain_size.csv", sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
write.table(missing_all, "data/Missing_samples_all.csv", sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)


#####################
### PRESSURE DATA ###
#####################

pressions <- read.table("data/raw-data/Pressions/bennes_metadata.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
pressions <- pressions[, -c(2:8)]
pressions <- pressions[-which(pressions$Sample %in% missing_all),]



#######################
### BIOCENOSES DATA ###
#######################

# Z:\CartoNAS\ETUDES\MEDTRIX\PLATEFORME MEDTRIX\MEDTRIX_FTP\rep3\data\biocenoses\Biocenoses_Mediterranee_DONIA_expert_sept_2022.shp


############################
### Concatenate all data ###
############################

metadata <- metadata[, c("Sample", "Zone", "Profondeur", "Date", "Longitude", "Latitude", "RHO", "Recouvrement_Rhodolithes_video")]
all_data <- merge(metadata, calcimetry_data, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, organic_matter_data, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, grain_size_data, by = "Sample", all.x = TRUE, all.y = TRUE)


all_data <- merge(all_data, pressions, by = "Sample")

write.table(all_data, "data/derived-data/Donnees_sediments_2022.csv", sep = ";", dec = ".", row.names = FALSE)

