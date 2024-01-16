
# Load functions
source("R/function_folk.R")
source("R/function_shepard.R")
source("R/degmindec2degdec.R")

######################
### CLEAN METADADA ###
######################

# Métadonnées stations
metadata <- XLConnect::readWorksheetFromFile(file = "data/raw-data/Bennes_SURFSTAT Corse 2022.xlsx", sheet = "Feuil2")
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
### CALCIMETRY ###
##################

# List files
calcimetry_files <- list.files(path = "data/raw-data/Laboratoire", pattern = glob2rx("*calcimetry*.xlsx"), recursive = TRUE, full.names = TRUE)

# Concatenate data
calcimetry_data <- NA

for (file in calcimetry_files){
  
  # Get sheet name
  wb <- XLConnect::loadWorkbook(file)
  sheet_name <- XLConnect::getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  data <- XLConnect::readWorksheetFromFile(file = file, 
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
  
  data <- XLConnect::readWorksheetFromFile(file = file, 
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
### ORGANIC MATTER ###
######################

# List files
organic_matter_files <- list.files(path = "data/raw-data/Laboratoire", pattern = glob2rx("*organic_matter*.xlsx"), recursive = TRUE, full.names = TRUE)

# Concatenate data
organic_matter_data <- NA

for (file in organic_matter_files){
  
  # Get sheet name
  wb <- XLConnect::loadWorkbook(file)
  sheet_name <- XLConnect::getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  data <- XLConnect::readWorksheetFromFile(file = file, 
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
  
  data <- XLConnect::readWorksheetFromFile(file = file, 
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
### GRAIN SIZE ###
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
#   wb <- XLConnect::loadWorkbook(file)
#   sheet_name <- XLConnect::getSheets(wb)[1]
#   
#   # Get position of "Sample" keyword
#   sheet <- XLConnect::readWorksheetFromFile(file = file, 
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
#   data <- XLConnect::readWorksheetFromFile(file = file, 
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
#   data <- XLConnect::readWorksheetFromFile(file = file, 
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
#   data <- XLConnect::readWorksheetFromFile(file = file, 
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
  wb <- XLConnect::loadWorkbook(file)
  sheet_name <- XLConnect::getSheets(wb)[1]
  
  # Get position of "Sample" keyword
  sheet <- XLConnect::readWorksheetFromFile(file = file, 
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
  
  # if (length(grep(error,sample)) > 0){
  #   
  #   cat("Problem with file : ", file)
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


##################################
### POINTS FOR DATA EXTRACTION ###
##################################

positions <- metadata[, c("Longitude", "Latitude")]
positions <- sf::st_as_sf(x = positions, 
                   coords = c("Longitude", "Latitude"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#######################
### BIOCENOSES DATA ###
#######################

biocenoses <- sf::st_read(dsn = "Z:/CartoNAS/ETUDES/MEDTRIX/PLATEFORME MEDTRIX/MEDTRIX_FTP/data/biocenoses/latest/Biocenoses_Mediterranee_DONIA_expert_dec_2023.shp")

positions_L93 <- sf::st_transform(x = positions, crs = sf::st_crs(biocenoses))

habitat <- sf::st_join(x = positions_L93, y = biocenoses)
habitat <- sf::st_drop_geometry(habitat)
habitat <- data.frame("Sample" = metadata$Sample, "Habitat" = habitat$HABITAT)

habitat$Habitat[which(habitat$Habitat == "Biocenose du detritique cotier (DC)")] <- "Det. côtier"
habitat$Habitat[which(habitat$Habitat == "Association a Cymodocea nodosa sur SFBC")] <- "Cymodocea SFBC"
habitat$Habitat[which(habitat$Habitat == "Biocenose des sables fins bien calibres (SFBC)")] <- "SFBC"
habitat$Habitat[which(habitat$Habitat == "Biocenose de l herbier a Posidonia oceanica")] <- "P.oceanica"
habitat$Habitat[which(habitat$Habitat == "Zone bathyale")] <- "Zone bathyale"
habitat$Habitat[which(habitat$Habitat == "Biocenose des algues infralittorales")] <- "Algues infra"
habitat$Habitat[which(habitat$Habitat == "Biocenose des fonds detritiques envases (DE)")] <- "Det. Envasé"
habitat$Habitat[which(habitat$Habitat == "Association de la matte morte de Posidonia oceanica")] <- "Matte Morte"
habitat$Habitat[which(habitat$Habitat == "Biocenose Coralligene")] <- "Coralligene"
habitat$Habitat[which(habitat$Habitat == "Habitats artificiels")] <- "Habitats artificiels"
habitat$Habitat[which(habitat$Habitat == "Biocenose des sables et graviers sous influence des courants de fond (SGCF) - infralittoral")] <- "SGCF - infra"
habitat$Habitat[which(habitat$Habitat == "Biocenose des sables et graviers sous influence des courants de fond (SGCF) - circalittoral")] <- "SGCF - circa"


#######################
### BATHYMETRY DATA ###
#######################

raster_reference <- terra::rast(extent = terra::ext(c(1150000, 1250000, 6000000, 6250000)),
                   resolution = 50, 
                   crs = terra::crs(biocenoses))

bathy <- terra::rast("Z:/CartoNAS/BATHY/bathy_andro_gebco_medfr_3395.tif")
bathy <- terra::project(x = bathy, raster_reference)

# Transformation en points
bathy <- terra::as.points(bathy)
bathy <- sf::st_as_sf(bathy)

# Extraction de l'habitat pour chaque centroide du raster
centroids <- sf::st_join(x = bathy, y = biocenoses)

# Remplacement des noms d'habitats
centroids$HABITAT[which(centroids$HABITAT == "Biocenose du detritique cotier (DC)")] <- "Det. côtier"
centroids$HABITAT[which(centroids$HABITAT == "Association a Cymodocea nodosa sur SFBC")] <- "Cymodocea SFBC"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose des sables fins bien calibres (SFBC)")] <- "SFBC"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose de l herbier a Posidonia oceanica")] <- "P.oceanica"
centroids$HABITAT[which(centroids$HABITAT == "Zone bathyale")] <- "Zone bathyale"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose des algues infralittorales")] <- "Algues infra"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose des fonds detritiques envases (DE)")] <- "Det. Envasé"
centroids$HABITAT[which(centroids$HABITAT == "Association de la matte morte de Posidonia oceanica")] <- "Matte Morte"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose Coralligene")] <- "Coralligene"
centroids$HABITAT[which(centroids$HABITAT == "Habitats artificiels")] <- "Habitats artificiels"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose des sables et graviers sous influence des courants de fond (SGCF) - infralittoral")] <- "SGCF - infra"
centroids$HABITAT[which(centroids$HABITAT == "Biocenose des sables et graviers sous influence des courants de fond (SGCF) - circalittoral")] <- "SGCF - circa"

# Suppression des colonnes inutiles et des points sans données
centroids$EU_CD <- NULL
centroids$DONIA <- NULL
centroids$surfstat <- NULL
centroids$NOMMDO <- NULL
centroids$source <- NULL
centroids$date <- NULL
centroids$auteur <- NULL
centroids$contact <- NULL
centroids$SURFSTAT_R <- NULL
centroids$surface <- NULL
colnames(centroids)[1] <- "Profondeur"
colnames(centroids)[2] <- "Habitat"
centroids <- centroids[!is.na(centroids$Habitat),]

# Sauvegarde du fichier bathy avec habitats
sf::st_write(obj = centroids, dsn = "outputs/01_Bathy_habitats.shp", delete_dsn = TRUE, delete_layer = TRUE)


#####################
### PRESSURE DATA ###
#####################

pressions <- read.table("data/raw-data/Pressions/bennes_metadata.csv", sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE)
pressions <- pressions[, -c(2:8)]
pressions <- pressions[-which(pressions$Sample %in% missing_all),]


####################
### CURRENT DATA ###
####################

# Lecture des tif de courantologie
VZ <- terra::rast("Z:/CartoNAS/SIG/ENVIRONMENTAL_DATA/PREVIMER/tiff/VZ_fond_2022_summary.tif") # MIN / MAX / MEAN / SD
UZ <- terra::rast("Z:/CartoNAS/SIG/ENVIRONMENTAL_DATA/PREVIMER/tiff/UZ_fond_2022_summary.tif") # MIN / MAX / MEAN / SD

# Correction des positions qui tombent à côté pour récupérer le centroide le plus proche
positions <- terra::vect(as.matrix(metadata[, c("Longitude", "Latitude")]), crs = "+proj=longlat +datum=WGS84")
result <- terra::extract(x = UZ, y = positions)$UZ_fond_2022_summary_3
outside_mask <- is.na(result)
outside_pts <- positions[outside_mask,]
outside_pts <- terra::geom(outside_pts)[, c("x", "y")]
nearest.cells <- seegSDM::nearestLand(points = outside_pts, raster = raster::raster(UZ), max_distance = 10000)

positions <- terra::geom(positions)[, c("x", "y")]
positions[outside_mask,] <- nearest.cells
positions <- terra::vect(as.matrix(positions), crs = "+proj=longlat +datum=WGS84")

# Extraction des valeurs
UZ_points <- terra::extract(x = UZ, y = positions)$UZ_fond_2022_summary_3


# Correction des positions qui tombent à côté pour récupérer le centroide le plus proche
positions <- terra::vect(as.matrix(metadata[, c("Longitude", "Latitude")]), crs = "+proj=longlat +datum=WGS84")
result <- terra::extract(x = VZ, y = positions)$VZ_fond_2022_summary_3
outside_mask <- is.na(result)
outside_pts <- positions[outside_mask,]
outside_pts <- terra::geom(outside_pts)[, c("x", "y")]
nearest.cells <- seegSDM::nearestLand(points = outside_pts, raster = raster::raster(VZ), max_distance = 10000)

positions <- terra::geom(positions)[, c("x", "y")]
positions[outside_mask,] <- nearest.cells
positions <- terra::vect(as.matrix(positions), crs = "+proj=longlat +datum=WGS84")

# Extraction des valeurs
VZ_points <- terra::extract(x = VZ, y = positions)$VZ_fond_2022_summary_3


courantologie <- data.frame("Sample" = metadata$Sample, "UZ" = UZ_points, "VZ" = VZ_points)
courantologie$Courant <- sqrt(courantologie$UZ^2 + courantologie$VZ^2)
courantologie$UZ <- NULL
courantologie$VZ <- NULL


############################
### Concatenate all data ###
############################


metadata <- metadata[, c("Sample", "Zone", "Profondeur", "Date", "Longitude", "Latitude", "RHO", "Recouvrement_Rhodolithes_video")]
all_data <- merge(metadata, calcimetry_data, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, organic_matter_data, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, grain_size_data, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, habitat, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, pressions, by = "Sample", all.x = TRUE, all.y = TRUE)
all_data <- merge(all_data, courantologie, by = "Sample", all.x = TRUE, all.y = TRUE)


# On comble les profondeurs manquantes avec la bathy
missing_depth <- which(is.na(all_data$Profondeur))
all_data$Profondeur[missing_depth] <- -all_data$mean_bathy[missing_depth]
all_data <- all_data[, -grep("bathy", colnames(all_data))]

# On supprime pour l'instant les variables min_ et max_
all_data <- all_data[, -grep("min_", colnames(all_data))]
all_data <- all_data[, -grep("max_", colnames(all_data))]

# On comble les pc_4000 vides pour ensuite prendre que les complete.cases
all_data$pc_4000[which(is.na(all_data$pc_4000)&!is.na(all_data$pc_2500))] <- 100

# On regroupe la granulométrie en clay, sand et gravel
all_data$pc_clay <- all_data$pc_2
all_data$pc_silt <- all_data$pc_4 + all_data$pc_63
all_data$pc_sand <- all_data$pc_125 + all_data$pc_250 + all_data$pc_500 + all_data$pc_1000 + all_data$pc_2000
all_data$pc_gravel <- all_data$pc_2500 + all_data$pc_4000


######################
### CLASSES DE SOL ###
#####################

# USDA
classes_sol <- data.frame("CLAY" = all_data$pc_clay, "SILT" = all_data$pc_silt, "SAND" = all_data$pc_sand)
classes_sol$TOTAL <- apply(classes_sol, 1, sum, na.rm = TRUE)
classes_sol$CLAY <- classes_sol$CLAY / classes_sol$TOTAL * 100
classes_sol$SILT <- classes_sol$SILT / classes_sol$TOTAL * 100
classes_sol$SAND <- classes_sol$SAND / classes_sol$TOTAL * 100
rownames(classes_sol) <- all_data$Sample
classes_sol <- classes_sol[complete.cases(classes_sol), ]
classes <- soiltexture::TT.points.in.classes(tri.data = classes_sol, class.sys = "USDA.TT")
classes <- sapply(1:nrow(classes), function(i) colnames(classes)[which.max(classes[i, ])])
classes <- data.frame("Sample" = rownames(classes_sol), "classe_USDA" = classes)
# 1 Cl clay
# 2 SiCl silty clay
# 3 SaCl sandy clay
# 4 ClLo clay loam
# 5 SiClLo silty clay loam
# 6 SaClLo sandy clay loam
# 7 Lo loam
# 8 SiLo silty loam
# 9 SaLo sandy loam
# 10 Si silt
# 11 LoSa loamy sand
# 12 Sa sand
classes$classe_USDA[classes$classe_USDA  ==  "LoSa"] <- "Loamy sand"
classes$classe_USDA[classes$classe_USDA  ==  "Sa"] <- "Sand"
classes$classe_USDA[classes$classe_USDA  ==  "SaLo"] <- "Sandy loam"
classes$classe_USDA[classes$classe_USDA  ==  "SiLo"] <- "Silty loam"
classes$classe_USDA[classes$classe_USDA  ==  "Lo"] <- "Loam"

all_data <- merge(all_data, classes, by = "Sample", all.x = TRUE, all.y = TRUE)


# FOLK & SHEPARD
classes_sol <- data.frame("CLAY" = all_data$pc_clay, "SILT" = all_data$pc_silt, "SAND" = all_data$pc_sand)
classes_sol$TOTAL <- apply(classes_sol, 1, sum, na.rm = TRUE)
classes_sol$GRAVEL <- 100 - classes_sol$TOTAL
rownames(classes_sol) <- all_data$Sample
classes_sol <- classes_sol[complete.cases(classes_sol), ]

classes_sol$classe_Shepard_coarse <- sapply(1:nrow(classes_sol), function(i) get_shepard_class(gravel = classes_sol$GRAVEL[i], 
                                                                                               sand = classes_sol$SAND[i], 
                                                                                               silt = classes_sol$SILT[i], 
                                                                                               clay = classes_sol$CLAY[i])$coarse)

classes_sol$classe_Shepard_fine <- sapply(1:nrow(classes_sol), function(i) get_shepard_class(gravel = classes_sol$GRAVEL[i], 
                                                                                             sand = classes_sol$SAND[i], 
                                                                                             silt = classes_sol$SILT[i], 
                                                                                             clay = classes_sol$CLAY[i])$fine)

classes_sol$classe_Folk_coarse <- sapply(1:nrow(classes_sol), function(i) get_folk_class(gravel = classes_sol$GRAVEL[i], 
                                                                                         sand = classes_sol$SAND[i], 
                                                                                         silt = classes_sol$SILT[i], 
                                                                                         clay = classes_sol$CLAY[i])$coarse)

classes_sol$classe_Folk_fine <- sapply(1:nrow(classes_sol), function(i) get_folk_class(gravel = classes_sol$GRAVEL[i], 
                                                                                       sand = classes_sol$SAND[i], 
                                                                                       silt = classes_sol$SILT[i], 
                                                                                       clay = classes_sol$CLAY[i])$fine)

classes_sol <- data.frame("Sample" = rownames(classes_sol), 
                          "classe_Shepard_coarse" = classes_sol$classe_Shepard_coarse, 
                          "classe_Shepard_fine" = classes_sol$classe_Shepard_fine, 
                          "classe_Folk_coarse" = classes_sol$classe_Folk_coarse, 
                          "classe_Folk_fine" = classes_sol$classe_Folk_fine)

all_data <- merge(all_data, classes_sol, by = "Sample", all.x = TRUE, all.y = TRUE)


# Informations qualitatives sur les rhodolithes
all_data$Recouvrement_Rhodolithes_video[all_data$Recouvrement_Rhodolithes_video  ==  "<10%"] <- 5
all_data$Recouvrement_Rhodolithes_video[all_data$Recouvrement_Rhodolithes_video  ==  "10-50%"] <- 30
all_data$Recouvrement_Rhodolithes_video[all_data$Recouvrement_Rhodolithes_video  ==  "50-90%"] <- 70
all_data$Recouvrement_Rhodolithes_video[all_data$Recouvrement_Rhodolithes_video  ==  ">90%"] <- 95
all_data$Recouvrement_Rhodolithes_video <- as.numeric(all_data$Recouvrement_Rhodolithes_video)

rownames(all_data) <- all_data$Sample
data <- all_data

# Export data
write.table(data, "data/derived-data/01_Donnees_sediments_2022.csv", sep = ";", dec = ".", row.names = FALSE)
save(data, file = "outputs/01_data.RData")

# Export shapefile all data
data_sp <- data
sp::coordinates(data_sp) <- ~ Longitude + Latitude
sp::proj4string(data_sp) <- sp::CRS("+init=epsg:4326")
rgdal::writeOGR(obj = data_sp, dsn = "outputs/01_Donnees_classes_sediments_2022.shp", layer = "01_Donnees_classes_sediments_2022", driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################
### KRIGING DATA ###
####################

grain_size <- c("Mean", "Median", "Mode", "SD", "Variance", "CV", "Skewness", "Kurtosis", "d10", "d50", "d90", 
                colnames(data)[grep("pc_", colnames(data))])

# On ne garde que les valeurs complètes
data <- data[complete.cases(data[, c(grain_size, "CaCO3", "LOI_550")]), ]

# On supprime les habitats qui ne comportent que trop peu de données
data <- data[data$Habitat %in% names(which(table(data$Habitat) > 5)), ]

# Lecture de la couche bathy + habitat
bathy <- rgdal::readOGR("outputs/01_Bathy_habitats.shp")
bathy@data$Profondeur <- - bathy@data$Profondeur

# Transformation des données en sp
data_sp <- data
sp::coordinates(data_sp) <- ~ Longitude + Latitude
sp::proj4string(data_sp) <- sp::CRS("+init=epsg:4326")
data_sp <- sp::spTransform(x = data_sp, bathy@proj4string)

# Liste de variables à interpoler
variables <- c("CaCO3", "LOI_550", "Median", "pc_clay", "pc_silt", "pc_sand", "pc_gravel")
habitats <- unique(data_sp@data$Habitat)

for (variable in variables){
  
  cat("Variable : ", variable, "\n")
  # Premier habitat pour pouvoir ensuite faire le rbind
  grid <- bathy[bathy@data$Habitat == habitats[1],]
  results <- eval(parse(text = paste0("automap::autoKrige(formula = ", variable, " ~ Profondeur, input_data = data_sp[data_sp$Habitat == habitats[1],], new_data = grid)")))
  results <- results$krige_output
  results$var1.pred[results$var1.pred < 0 ] <- 0
  
  for (habitat in habitats[2:length(habitats)]){
    
    grid <- bathy[bathy@data$Habitat == habitat,]
    krigeHab <- eval(parse(text = paste0("automap::autoKrige(formula = ", variable, " ~ Profondeur, input_data = data_sp[data_sp$Habitat == habitat,], new_data = grid)")))
    krigeHab <- krigeHab$krige_output
    krigeHab$var1.pred[krigeHab$var1.pred < 0 ] <- 0
    
    # On aggrège les résultats pour les différents habitats
    results <- rbind(results,
                     krigeHab)
  }#eo for habitat
  
  # Transformation des résultats en raster
  results <- sp::spTransform(results, bathy@proj4string)
  results <- data.frame("x" = sp::coordinates(results)[,"coords.x1"], 
                        "y" = sp::coordinates(results)[,"coords.x2"], 
                        "prediction" = results@data[,"var1.pred"])
  
  sp::coordinates(results) <- ~ x + y
  sp::gridded(results) <- TRUE
  results <- raster::raster(results)
  sp::proj4string(results) <- bathy@proj4string
  
  # Export des résultats
  raster::writeRaster(x = results, paste0("outputs/01_Cartographie_", variable, ".tif"), overwrite = TRUE)
  
}#eo for variable
