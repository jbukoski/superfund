# A function that uses a sparse geometric predicate binary (using st_intersects). 
# The function is used with `lmap` across the SGPB to summarize census block SOVI data.

summarize_data <- function(criteria, blocks, aa = FALSE) {
  
  # criteria: sparse geometric predicate binary of intersecting buffered sites and counties
  # blocks: census blocks with SOVI data
  
  criteria_df <- unlist(criteria)
  site_index <- criteria_df[length(criteria_df)]
  
  if(aa == TRUE) {
  
    dat <- blocks %>%
      mutate(rn = row_number()) %>%
      filter(rn %in% criteria_df) %>%
      summarise(site_index = site_index, 
                avg_rpl_themes = mean(rpl_themes),
                sum_e_totpop = round(sum(e_totpop * perc_itrsct/100), 0),
                pop_wgtd_rpl_themes = round(sum((e_totpop * perc_itrsct/100) * rpl_themes), 0),
                pop_wgtd_rpl_t1 = round(sum((e_totpop * perc_itrsct/100) * rpl_theme1), 0),
                pop_wgtd_rpl_t2 = round(sum((e_totpop * perc_itrsct/100) * rpl_theme2), 0),
                pop_wgtd_rpl_t3 = round(sum((e_totpop * perc_itrsct/100) * rpl_theme3), 0),
                pop_wgtd_rpl_t4 = round(sum((e_totpop * perc_itrsct/100) * rpl_theme4), 0),
                max_rpl_t1 = round(max(rpl_theme1), 4),
                max_rpl_t2 = round(max(rpl_theme2), 4),
                max_rpl_t3 = round(max(rpl_theme3), 4),
                max_rpl_t4 = round(max(rpl_theme4), 4),
                max_rpl_themes = round(max(rpl_themes), 4)) %>%
      list()
    
  } else {
    
    dat <- blocks %>%
      mutate(rn = row_number()) %>%
      filter(rn %in% criteria_df) %>%
      summarise(site_index = site_index,
                avg_rpl_themes = mean(rpl_themes),
                sum_e_totpop = sum(e_totpop),
                pop_wgtd_rpl_themes = sum(e_totpop * rpl_themes),
                pop_wgtd_rpl_t1 = sum(e_totpop * rpl_theme1),
                pop_wgtd_rpl_t2 = sum(e_totpop * rpl_theme2),
                pop_wgtd_rpl_t3 = sum(e_totpop * rpl_theme3),
                pop_wgtd_rpl_t4 = sum(e_totpop * rpl_theme4),
                max_rpl_t1 = round(max(rpl_theme1), 4),
                max_rpl_t2 = round(max(rpl_theme2), 4),
                max_rpl_t3 = round(max(rpl_theme3), 4),
                max_rpl_t4 = round(max(rpl_theme4), 4),
                max_rpl_themes = round(max(rpl_themes), 4)) %>%
      list()
    
  }
  
  return(dat)
  
}

# Function to convert simple features objects to data frames. 
# Tricky part that does not make this easy to handle is specifying the geometry as NULL. 
# The function also selects relevant data from the SOVI objects and drops 
# repeated columns (name, county, state, ...).

sf_to_df <- function(sf_object) {
  
  st_geometry(sf_object) <- NULL
  
  sf_object <- sf_object %>%
    dplyr::select(site_id, matches("avg_rpl_themes*"), matches("sum_e_totpop*"),
                  matches("pop_wgtd_rpl_themes*"), matches("pop_wgtd_f_t*")) %>%
    mutate(site_id = as.numeric(site_id))

  return(sf_object)
  
}

# Summarizes SoVI data based on the Unit-Hazard Coincidence method.

generate_uhc <- function(sovi_data, sites_data) {
  
  m1_dat <- st_join(sites_data, sovi_data)
  
  m1_binary_blcks <- sapply(st_contains(sovi_data, sites_data), function(x) {length(x) != 0})
  m1_binary <- sapply(st_within(sites_data, sovi_data), function(x) {length(x) != 0})
  
  m1_blcks <- sovi_data %>%
    filter(m1_binary_blcks)
  
  m1_sovi <- m1_dat %>%
    filter(m1_binary) %>%
    mutate(pop_wgtd_rpl_themes = e_totpop * rpl_themes,
           pop_wgtd_rpl_t1 = e_totpop * rpl_theme1,
           pop_wgtd_rpl_t2 = e_totpop * rpl_theme2,
           pop_wgtd_rpl_t3 = e_totpop * rpl_theme3,
           pop_wgtd_rpl_t4 = e_totpop * rpl_theme4,
           max_rpl_t1 = round(rpl_theme1, 4),
           max_rpl_t2 = round(rpl_theme2, 4),
           max_rpl_t3 = round(rpl_theme3, 4),
           max_rpl_t4 = round(rpl_theme4, 4),
           max_rpl_themes = round(rpl_themes, 4),
           sum_e_totpop = e_totpop,
           buffer = NA,
           method = "uhc") 

  return(m1_sovi)
  
}

# Summarizes SoVI data based on the Buffered Intersection method.

generate_bi <- function(sovi_data, sites_data, buffer) {
  
  m2_return <- data.frame()
  
  for(i in 1:length(buffer)) {
    
    buff <- buffer[i]
    
    m2_sgbp <- sites_data %>%
      st_buffer(buffer[i]) %>%
      st_intersects(sovi_data)
    
    for(i in 1:length(m2_sgbp)) { 
      m2_sgbp[[i]] = c(m2_sgbp[[i]], i) 
      }
    
    m2_summary <- m2_sgbp %>%
      lmap(summarize_data, blocks = sovi_data)
    
    sovi_itrsct <- m2_summary[[1]]
    
    for(i in 2:length(m2_summary)) { 
      sovi_itrsct <- rbind(sovi_itrsct, m2_summary[[i]]) 
      }
    
    sovi_1km_df <- sovi_itrsct
    st_geometry(sovi_1km_df) = NULL 
    
    m2_sovi <- sites_data %>%
      as.data.frame() %>%
      mutate(site_index = row_number(),
             buffer = buff,
             method = "bi") %>%
      left_join(sovi_1km_df, by = "site_index") %>%
      st_sf()
    
    m2_return <- rbind(m2_return, as.data.frame(m2_sovi))
  
  }
    
  m2_return %>%
    st_sf() %>%
    return()
  
}

# A helper function to generate the M3: Areal Apportionment data correctly.
# This function and "generate_aa()" work together to build the aa data.

m3_dat <- function(sovi_data, site, buffer) {
  
  m3_return <- data.frame()
  
  for(i in 1:length(buffer)) {
    
    buff <- buffer[i]
    
    areal_appor <- sovi_data %>%
      mutate(area_m2 = st_area(.)) %>%
      st_intersection(st_buffer(site, buff)) %>%
      st_cast("MULTIPOLYGON") %>%
      mutate(area_m2 = as.numeric(gsub(" m^2", "", area_m2)),
             int_area_m2 = as.numeric(gsub(" m^2", "", st_area(.))),
             perc_itrsct = round(100 * int_area_m2 / area_m2, 4),
             wgtd_pop = round(e_totpop * perc_itrsct/100, 0))
    
    m3_sovi <- areal_appor
    
    m3_sgbp <- site %>%
      st_buffer(buff) %>%
      st_intersects(m3_sovi)
    
    m3_summary <- m3_sgbp %>%
      lmap(summarize_data, blocks = m3_sovi, aa = TRUE) %>%
      extract2(1) %>%
      st_set_geometry(NULL)
    
    m3_sovi <- site %>%
      as.data.frame() %>%
      cbind(m3_summary) %>%
      mutate(method = "aa",
             buffer = buffer[i],
             method = "aa") %>%
      st_sf()
    
    m3_return <- rbind(m3_return, as.data.frame(m3_sovi))
    
  }
  
  m3_return %>%
    st_sf() %>%
    return()
  
}


# Summarizes SoVI data based on the Areal Apportionment method.
# Performs a loop function over each of the sites with the m3_dat function to
# avoid spatial overlap of multiple NPLs in a block.

generate_aa <- function(sovi_data, sites_data, buffer) {
  
  m3_sovi <- m3_dat(sovi_data, sites_data[1,], buffer)
  
  for(i in 2:nrow(sites_data)) {
    
    m3_sovi <- m3_sovi %>% 
      rbind(m3_dat(sovi_data, sites_data[i,], buffer)) 
    
  }

  return(m3_sovi)
  
}

# Full function that generates the sites data for m1 and m2, can run with different buffer sizes.

generate_all <- function(sovi_data, sites_data, buffer) {

  print("generating uhc data...")
  m1_sovi <- generate_uhc(sovi_data, sites_data)
  
  print("generating bi data...")
  m2_sovi <- generate_bi(sovi_data, sites_data, buffer)
  
  print("generating aa data...")
  m3_sovi <- generate_aa(sovi_data, sites_data, buffer)
  
  # Get column names to jive
  
  print("joining data...")
  
  m2_sovi <- m2_sovi %>%
    select(colnames(m2_sovi)[colnames(m2_sovi) %in% colnames(m1_sovi)])

  m3_sovi <- m3_sovi %>%
    select(colnames(m3_sovi)[colnames(m3_sovi) %in% colnames(m1_sovi)])
  
  m1_sovi <- m1_sovi %>%
    select(colnames(m1_sovi)[colnames(m1_sovi) %in% colnames(m2_sovi)])
  
  # Join the data
  
  sites_sovi <- m1_sovi %>%
    rbind(m2_sovi, m3_sovi) %>%
    arrange(name, desc(method), buffer)

  return(sites_sovi)

}

# A function for generating the intersected shapefiles

m1_polys <- function(sovi_data, sites_data, fld_rank) {

  sites_filt <- sites_102003 %>%
    filter(flood_rank == fld_rank)
  
  m1_binary_blocks <- sapply(st_intersects(sovi_data, sites_filt), 
                            function(x) {length(x) != 0})
  
  m1_blocks <- sovi_data %>%
    filter(m1_binary_blocks) %>%
    mutate(flood_rank = fld_rank)
    
  return(m1_blocks)

}

# A function to generate the intersected shapefiles

m2_polys <- function(sovi_data, sites_data, buffer) {
  
  # This function generates a shapefile of intersected census blocks based on a buffer size
  # Buffer should be supplied in meters, and both the sovi and sites data should be in EPSG:102003 (planar projection)

  m2_itrsct_ids <- sovi_data %>%
    st_intersection(st_buffer(sites_data, buffer)) %>%
    st_cast("MULTIPOLYGON") %>%
    dplyr::select(affgeoid, flood_rank)
  
  m2_itrsct <- sovi_data %>%
    filter(affgeoid %in% m2_itrsct_ids$affgeoid) %>%
    mutate(buffer = buffer,
           method = "bi") %>%
    left_join(dplyr::select(st_set_geometry(m2_itrsct_ids, NULL), flood_rank, affgeoid),
              by = "affgeoid") %>%
    st_as_sf()

  return(m2_itrsct)
  
}

# A function to generate polygons based on the areal apportionment method of joining data

m3_polys <- function(sovi_data, sites_data, buffer) {

  m3_itrsct <- sovi_data %>%
    mutate(area_m2 = st_area(.)) %>%
    st_intersection(st_buffer(sites_data, buffer)) %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(int_area_m2 = st_area(.)) %>%
    mutate(area_m2 = as.numeric(gsub(" m^2", "", area_m2)),
           int_area_m2 = as.numeric(gsub(" m^2", "", int_area_m2))) %>%
    mutate(perc_itrsct = round(100 * int_area_m2 / area_m2, 2),
           method = "aa",
           buffer = buffer,
           wgtd_pop = round(e_totpop * perc_itrsct/100, 0))
  
  return(m3_itrsct)
  
}


