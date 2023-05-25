


FormatAnnotations <- function(FiltAnnotations, comb_rows = T, Testing = F){
  
  if(comb_rows == T){
    Annotations <- FiltAnnotations %>%
      mutate(file = paste(filepath, filename, sep = '/'),
             width = imgwidth,
             height = imgheight,
             sp_id = sp_id,
             sp_name = sp_name,
             x_left = x_left_scaled,
             y_top = y_top_scaled,
             boxwidth = boxwidth_scaled,
             boxheight = boxheight_scaled,
             .keep = 'none')# %>%
      ###
      # mutate(sp_id = 1,
      #        sp_name = 'fish') 
    
    sp_index <- Annotations %>%
      select(sp_id, sp_name) %>%
      distinct() %>%
      arrange(sp_id) %>%
      ungroup()
    
    sp_index <- sp_index %>%
      mutate(sp_id = 0:(nrow(sp_index)-1))
    
     Annotations <- Annotations %>%
       group_by(sp_name) %>%
       mutate(id = cur_group_id()) %>%
       ungroup() %>%
       select(-sp_id, -sp_name) %>%
       mutate(id = id-1) %>%
       mutate(id = keras::to_categorical(id, nrow(sp_index)))
    
    # Annotations <- Annotations %>%
    #   select(-sp_id, sp_name) %>%
    #   relocate(file, x_left, y_top, boxwidth, boxheight, id)# %>%
    #   #rename(id = sp_name)

    Annotations$id <- as.data.frame(Annotations$id)

    Annotations$id <- Annotations$id %>%
      rowwise %>%
      mutate(all = list(c(V1, V2, V3, V4))) %>%
      ungroup %>%
      select(all)
    
    Annotations <- Annotations %>%
      rowwise %>%
      mutate(bboxcoords = list(c(x_left, y_top, boxwidth, boxheight))) %>%
      mutate(bbox = list(c(id$all, bboxcoords))) %>%
      #mutate(bbox = list(c(id, bboxcoords))) %>%
      mutate(bbox = list(unlist(bbox))) %>%
      ungroup %>%
      select(file, width, height, bbox) %>%
      group_by(file) %>%
      summarise(annot = list(unlist(bbox))) %>%
      # rowwise %>%
      # mutate(annot = list(array(unlist(annot)))) %>%
      ungroup #%>%
      #unnest_wider(annot, names_sep = '_', simplify = T)
      # rowwise %>%
      # mutate(annot = list(unlist(annot))) %>%
      # mutate(annot = list(array(unlist(annot)))) %>%
      # ungroup
    
    
    
  }
  
  if(Testing == T){

    Annotations <- FiltAnnotations %>%
      mutate(file = paste(filepath, filename, sep = '/'),
             width = imgwidth,
             height = imgheight,
             sp_id = sp_id,
             sp_name = sp_name,
             x_left_sc = x_left_scaled,
             y_top_sc = y_top_scaled,
             width = boxwidth_scaled,
             height = boxheight_scaled,
             xl_orig = x_left,
             yt_orig = y_top,
             yb_orig = y_top+boxheight,
             xr_orig = x_left+boxwidth,
             h_orig = boxheight,
             w_orig = boxwidth,
             .keep = 'none')
    Annotations <- Annotations %>%
      mutate(y_bottom_sc = y_top_sc+height,
             x_right_sc = x_left_sc+width) %>%
      select(-w_orig, -h_orig, -width, -height)
    
    sp_index <- Annotations %>%
      select(sp_id, sp_name) %>%
      distinct() %>%
      arrange(sp_id) %>%
      ungroup()
    
    sp_index <- sp_index %>%
      mutate(sp_id = 0:(nrow(sp_index)-1))
    
    Annotations <- Annotations %>%
      group_by(sp_name) %>%
      mutate(id = cur_group_id()) %>%
      ungroup() %>%
      select(-sp_id) %>%
      rename(sp_id = id)
  }
  

  
  




  
  return(list(Annotations = Annotations, SpeciesIndex = sp_index))
  
}


