

get_folk_class <- function(gravel = NA, sand = 50, clay = 30, silt = 20){
  
  ##############
  ### GRAVEL ###
  ##############
  
  if (!is.na(gravel)){
    
    total <- gravel + sand + silt + clay
    mud <- clay + silt
    prop_gravel <- gravel / total
    sand_mud_ratio <- sand / mud
    
    # Define class_1
    if (prop_gravel >= 0.8){
      
      class_1 <- "G"
      
    }else if (prop_gravel >= 0.3 & prop_gravel < 0.8){
      
      if (sand_mud_ratio < 1){
        
        class_1 <- "mG"
        
      }else if (sand_mud_ratio >= 1 & sand_mud_ratio < 9){
        
        class_1 <- "msG"
        
      }else{
        
        class_1 <- "sG"
        
      }#eo if sand_mud_ratio
      
    }else if (prop_gravel >= 0.05 & prop_gravel < 0.3){
      
      if (sand_mud_ratio < 1){
        
        class_1 <- "gM"
        
      }else if (sand_mud_ratio >= 1 & sand_mud_ratio < 9){
        
        class_1 <- "gmS"
        
      }else{
        
        class_1 <- "gS"
        
      }#eo if sand_mud_ratio
      
    }else if (prop_gravel >= 0.01 & prop_gravel < 0.05){
      
      if (sand_mud_ratio < 1/9){
        
        class_1 <- "(g)M"
        
      }else if (sand_mud_ratio >= 1/9 & sand_mud_ratio < 1){
        
        class_1 <- "(g)sM"
        
      }else if (sand_mud_ratio >= 1 & sand_mud_ratio < 9){
        
        class_1 <- "(g)mS"
        
      }else{
        
        class_1 <- "(g)S"
        
      }#eo if sand_mud_ratio
      
    }else{
      
      if (sand_mud_ratio < 1/9){
        
        class_1 <- "M"
        
      }else if (sand_mud_ratio >= 1/9 & sand_mud_ratio < 1){
        
        class_1 <- "sM"
        
      }else if (sand_mud_ratio >= 1 & sand_mud_ratio < 9){
        
        class_1 <- "mS"
        
      }else{
        
        class_1 <- "S"
        
      }#eo if sand_mud_ratio
      
    }#eo if prop_sand
    
  }else{
    
    class_1 <- NA
    
  }#if !is.na(gravel)
  
  
  ######################
  ### SAND CLAY SILT ###
  ######################
  
  # If not only gravel
  if (sand + clay + silt > 0){
    
    total <- sand + clay + silt
    prop_sand <- sand / total
    clay_silt_ratio <- clay / silt
    
    # Define class_2
    if (prop_sand >= 0.9){
      
      class_2 <- "S"
      
    }else if (prop_sand >= 0.5 & prop_sand < 0.9){
      
      if (clay_silt_ratio > 2){
        
        class_2 <- "cS"
        
      }else if (clay_silt_ratio < 0.5){
        
        class_2 <- "zS"
        
      }else{
        
        class_2 <- "mS"
        
      }#eo if clay_silt_ratio
      
    }else if (prop_sand >= 0.1 & prop_sand < 0.5){
      
      if (clay_silt_ratio > 2){
        
        class_2 <- "sC"
        
      }else if (clay_silt_ratio < 0.5){
        
        class_2 <- "sZ"
        
      }else{
        
        class_2 <- "sM"
        
      }#eo if clay_silt_ratio
      
    }else{
      
      if (clay_silt_ratio > 2){
        
        class_2 <- "C"
        
      }else if (clay_silt_ratio < 0.5){
        
        class_2 <- "Z"
        
      }else{
        
        class_2 <- "M"
        
      }#eo if clay_silt_ratio
      
    }#eo if prop_sand
    
  }else{
    
    class_2 <- NA
    
  }#eo if sand + clay + silt > 0
  
  return(list("coarse" = class_1, "fine" = class_2))
  
}#eo function