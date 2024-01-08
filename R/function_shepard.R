

get_shepard_class <- function(gravel = 10, sand = 50, clay = 30, silt = 20){
  
  ##############
  ### GRAVEL ###
  ##############
  
  if (!is.na(gravel)){
    
    # Proportions of total
    total <- sand + clay + silt + gravel
    prop_silt_clay <- (silt + clay) / total
    prop_sand <- sand / total
    prop_gravel <- gravel / total
    
    # Ratios
    gravel_sand_ratio <- gravel / sand
    sand_silt_clay_ratio <- sand / (silt + clay)
    
    if (prop_gravel >= 0.75){
      
      class_1 <- "GRAVEL"
      
    }else if (gravel_sand_ratio >= 1 & prop_silt_clay < 0.2 & sand_silt_clay_ratio >= 1){
      
      class_1 <- "GRAVEL"
      
    }else if (prop_gravel < 0.1){
      
      class_1 <- "SAND SILT CLAY"
      
    }else{
      
      class_1 <- "GRAVELLY SEDIMENT"
      
    }#eo if
    
  }else{
    
    class_1 <- NA
    
  }#if !is.na(gravel)
  
  
  
  ######################
  ### SAND CLAY SILT ###
  ######################
  
  # If not only gravel
  if (sand + clay + silt > 0){
    
    # Proportions of total
    total <- sand + clay + silt
    prop_sand <- sand / total
    prop_clay <- clay / total
    prop_silt <- silt / total
    
    # Ratios
    clay_silt_ratio <- clay / silt
    sand_clay_ratio <- sand / clay
    silt_sand_ratio <- silt / sand
    
    
    
    if (prop_clay >= 0.75){
      
      class_2 <- "CLAY"
      
    }else if (prop_sand >= 0.75){
      
      class_2 <- "SAND"
      
    }else if (prop_silt >= 0.75){
      
      class_2 <- "SILT"
      
    }else if (silt_sand_ratio < 1 & prop_clay < 0.2 & clay_silt_ratio < 1){
      
      class_2 <- "SILTY SAND"
      
    }else if (silt_sand_ratio >= 1 & prop_clay < 0.2 & clay_silt_ratio < 1){
      
      class_2 <- "SANDY SILT"
      
    }else if (sand_clay_ratio >= 1 & prop_silt < 0.2 & clay_silt_ratio >= 1){
      
      class_2 <- "CLAYEY SAND"
      
    }else if (sand_clay_ratio < 1 & prop_silt < 0.2 & silt_sand_ratio < 1){
      
      class_2 <- "SANDY CLAY"
      
    }else if (clay_silt_ratio >= 1 & prop_sand < 0.2 & silt_sand_ratio >= 1){
      
      class_2 <- "SILTY CLAY"
      
    }else if (clay_silt_ratio < 1 & prop_sand < 0.2 & sand_clay_ratio < 1){
      
      class_2 <- "CLAYEY SILT"
      
    }else{
      
      class_2 <- "SAND SILT CLAY"
      
    }
    
  }else{
    
    class_2 <- NA
    
  }#eo if sand + clay + silt > 0
  
  return(list("coarse" = class_1, "fine" = class_2))
  
  
}#eo function