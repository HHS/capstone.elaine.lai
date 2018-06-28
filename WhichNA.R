whichNA = function(dataset){      
  for(i in 1:ncol(dataset)){                  
    is_na = is.na(dataset[, i])               
    if(any(is_na)){                           
      na_ids = which(is_na)                   
      message = paste0(                       
        colnames(dataset)[i],
        "they are",
        na_ids)            
      print(message)                         
    }
    else(print(paste0(colnames(dataset)[i]," has no NAs")))
  }                         
}