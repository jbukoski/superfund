
# A function that uses a sparse geometric predicate binary (using st_intersects). The function is use with `lmap` across the SGPB to summarize census block SOVI data.

summarize_data <- function(criteria, blocks, aa = FALSE) {
  
  # criteria: sparse geometric predicate binary of intersecting buffered sites and counties
  # blocks: census blocks with SOVI data
  
  criteria_df <- unlist(criteria)
  
  site_index <- criteria_df[length(criteria_df)]
  # criteria_df <- criteria_df[1:length(criteria_df)-1]
  
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

# Function to convert simple features objects to data frames. Tricky part that does not make this easy to handle is specifying the geometry as NULL. The function also selects relevant
# data from the SOVI objects and drops repeated columns (name, county, state, ...)

sf_to_df <- function(sf_object) {
  
  st_geometry(sf_object) <- NULL
  
  sf_object <- sf_object %>%
    dplyr::select(site_id, matches("avg_rpl_themes*"),
           matches("sum_e_totpop*"), matches("pop_wgtd_rpl_themes*"),
           matches("pop_wgtd_f_t*"))
  
  sf_object$site_id <- as.numeric(sf_object$site_id)
  
  return(sf_object)
  
}

# Full function that generates the sites data, can run with different buffer sizes.

generate_data <- function(sovi_data, sites_data, buffer) {

  # Method 1 -- Unit-Hazard Coincidence
  
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
           max_rpl_themes = round(rpl_themes, 4)) %>%
    dplyr::select(site_id, name, county.x, state.x, flood_rank, e_totpop, 
                  rpl_themes, pop_wgtd_rpl_themes, matches("pop_wgtd_rpl_t*"),
                  matches("max_rpl*")) %>%
    rename(sum_e_totpop = e_totpop, 
           avg_rpl_themes = rpl_themes)
  
  # Method 2 -- Buffered Intersection
  
  m2_sgbp <- sites_data %>%
    st_buffer(buffer) %>%
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
    mutate(site_index = row_number()) %>%
    left_join(sovi_1km_df, by = "site_index")
  
# Join the data
  
  m2_sovi <- m2_sovi %>%
    select(colnames(m2_sovi)[colnames(m2_sovi) %in% colnames(m1_sovi)])

  sites_sovi <- m1_sovi %>%
    left_join(m2_sovi, by = "site_id")

  tidy_sites_sovi <- sites_sovi %>%
    select(site_id, name = name.x, county = county.x, state = state.x, 
           flood_rank = flood_rank.x, geometry = geometry.x, 
           matches("pop_wgtd_rpl*"), matches("sum_e_totpop*"),
           matches("max_rpl*")) %>%
    gather(method, value, -site_id, -name, -county, -state, -flood_rank, -geometry) %>%
    separate(method, c("metric", "method"), sep = "[.]") %>%
    spread(metric, value) %>%
    mutate(method = ifelse(method == "x", "uhc", "bi"),
           buffer = ifelse(method == "uhc", NA, buffer),
           pop_wgtd_rpl_themes = round(ifelse(pop_wgtd_rpl_themes < 0, NA, pop_wgtd_rpl_themes), 0),
           sum_e_totpop = round(sum_e_totpop, 0)) %>%
     arrange(name)
  
  return(tidy_sites_sovi)

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

# A helper function to generate the M3: Areal Apportionment data correctly

m3_dat <- function(sovi_data, site, buffer) {
  
  areal_appor <- sovi_data %>%
    mutate(area_m2 = st_area(.)) %>%
    st_intersection(st_buffer(site, buffer)) %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(area_m2 = as.numeric(gsub(" m^2", "", area_m2)),
           int_area_m2 = as.numeric(gsub(" m^2", "", st_area(.))),
           perc_itrsct = round(100 * int_area_m2 / area_m2, 4),
           wgtd_pop = round(e_totpop * perc_itrsct/100, 0))
  
  m3_sovi <- areal_appor
  
  m3_sgbp <- site %>%
    st_buffer(buffer) %>%
    st_intersects(m3_sovi)
  
  m3_summary <- m3_sgbp %>%
    lmap(summarize_data, blocks = m3_sovi, aa = TRUE) %>%
    extract2(1) %>%
    st_set_geometry(NULL)
  
  m3_sovi <- site %>%
    as.data.frame() %>%
    cbind(m3_summary) %>%
    mutate(method = "aa",
           buffer = buffer) %>%
    st_as_sf()
  
  return(m3_sovi)
  
}

# A helper function to loop over each site in the sites SF object. The function uses m3_dat inside a for loop.

generate_aa <- function(sovi_data, sites_data, buffer) {
  
  m3_sovi <- sites_data %>%
    filter(site_id == "NA")
  
  for(i in 1:nrow(sites_data)) {
    
    m3_sovi <- m3_sovi %>% 
      rbind(m3_dat(sovi_data, sites_data[i,], buffer)) 
    
  }
  
  m3_sovi <- m3_sovi %>%
    select(site_id, name, county, state, flood_rank, method, buffer, 
           geometry, sum_e_totpop, pop_wgtd_rpl_themes, pop_wgtd_rpl_t1, 
           pop_wgtd_rpl_t2, pop_wgtd_rpl_t3, pop_wgtd_rpl_t4, 
           matches("max_rpl*"))
  
  return(m3_sovi)
  
}
