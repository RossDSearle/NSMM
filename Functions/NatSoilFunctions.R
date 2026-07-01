

# checkResponse <- function(jsnList){
#   
#   if(is.null(jsnList)){
#     return(F)
#   }else if(length(jsnList)==0){
#     return(F)
#   }else{
#     return(T)
#   }
# }


parseMetaDataAgencies <- function(jsnList){
  
  jn<-jsnList
  
  cnt=1
  code<-vector('character', 1000)
  name<-vector('character', 1000)
  
  for (i in 1:length(jn)) {
    code[cnt] <- getVals(jn[[i]], 'agency_code')
    name[cnt] <- getVals(jn[[i]], 'agency_name')
    cnt=cnt+1
  }
  odf <- data.frame(code, name)[1:cnt,]
  return(odf)
}

parseMetaDataProjects <- function(jsnList){
  
  jn<-jsnList

  
  cnt=1
  a<-vector('character', 100000)
  p<-vector('character', 100000)
  pd<-vector('character', 100000)
  psd<-vector('character', 100000)
  pfd<-vector('character', 100000)

  for (i in 1:length(jn)) {
    ac <- getVals(jn[[i]], 'agency_code')
    
    for (j in 1:length(jn[[i]]$zProjects)) {
      a[cnt] <- ac
      p[cnt] <- getVals(jn[[i]]$zProjects[[j]], 'proj_code')
      pd[cnt] <- getVals(jn[[i]]$zProjects[[j]], 'proj_name')
      psd[cnt] <- getVals(jn[[i]]$zProjects[[j]], 'proj_start_date')
      pfd[cnt] <- getVals(jn[[i]]$zProjects[[j]], 'proj_finish_date')
      cnt=cnt+1
    }
    
  }
  odf <- data.frame(agencyCode=a, code=p, name=pd, start=psd, end=pfd)[1:cnt,]
  return(odf)
  
}

parseMetaDataSites <- function(jsnList){
  
  jn<-jsnList
 # jn<-jsnL
  
     cnt=1
     lat<-vector('character', 100000)
     lon<-vector('character', 100000)
     sid<-vector('character', 100000)
     agy<-vector('character', 100000)
     proj<-vector('character', 100000)
     
    for (i in 1:length(jn)) {
      a <- getVals(jn[[i]], 'agency_code')
      ad <- getVals(jn[[i]], 'agency_name')
      for (j in 1:length(jn[[i]]$zProjects)) {
        p <- getVals(jn[[i]]$zProjects[[j]], 'proj_code')
        pd <- getVals(jn[[i]]$zProjects[[j]], 'proj_name')
        
        for (k in 1:length(jn[[i]]$zProjects[[j]]$zSites)) {
          
          if(length(jn[[i]]$zProjects[[j]]$zSites[[k]]$locations)>0){
            lat[cnt] <- getVals(jn[[i]]$zProjects[[j]]$zSites[[k]]$locations[[1]], 'latitude')
            lon[cnt] <- getVals(jn[[i]]$zProjects[[j]]$zSites[[k]]$locations[[1]], 'longitude')
          }
          sid[cnt] <- getVals(jn[[i]]$zProjects[[j]]$zSites[[k]], 's_id')
          proj[cnt] <- p
          agy[cnt] <- a

           cnt=cnt+1
        }
        }
      }
odf <- data.frame(agency=agy, project=proj, sid=sid, longitude=lon, latitude=lat)[1:cnt,]
#write.csv(odf, 'c:/temp/sites.csv')
idxs <- which(odf$longitude=='')


if(length(idxs) > 0){
odf2 <- odf[-idxs,]
}else{
  odf2 <- odf
}

#write.csv(odf, 'c:/temp/sites.csv')
return(odf2)
}




initApp <- function(CSIROauth, develop, accessKey){
  

  if(develop == T){
    sinfo <<- read.csv('c:/projects/ANSIS/SiteSheet/sites.csv')
    codes <<- read.csv('c:/projects/ANSIS/SiteSheet/codes.csv')
    siteJSN <<- readLines('c:/projects/ANSIS/SiteSheet/site.jsn')
    labMethods <<- read.csv('c:/projects/ANSIS/SiteSheet/labMethods.csv')
    agenciesT <<- read.csv('c:/projects/ANSIS/SiteSheet/agencies.csv')
    projectsT <<- read.csv('c:/projects/ANSIS/SiteSheet/projects.csv')
  }else{
    
    #if(CSIROauth){
        resp <- GET('https://soils.csiro.au/data/natsoil-api/site-meta-data?All=false', add_headers("APIKey"=accessKey))
        print(paste0('Getting data from the api using - ', accessKey))
    # }else{
    #     resp <- GET('https://soils.csiro.au/data/natsoil-api/site-meta-data?All=false')
    # }

    jsn <- content(resp, 'text')
    jsnL <- fromJSON(jsn, simplifyVector = F)
  #  cat(jsn, file='c:/temp/sites.json')
  #  saveRDS(jsnL, 'c:/temp/sites.rds')
  }
  return(jsnL)
}

isCSIRO <- function(ip){

  bits <- str_split(ip, '[.]')
  ipRoot <- paste0(bits[[1]][1], '.', bits[[1]][2])
  ips <- read.csv(paste0('./Secrets/ip.csv'))
  isInternal <- ipRoot %in% ips$IP
  return(isInternal)
}

getSiteDescription <- function(jsnList, projects, agencies){
  
  j <- jsnList
  
  locT<-'Unknown'
  loc<-''
  xy <- getProfileLocation(j)
  xl=xy$lon
  yl=xy$lat
  
 
  if(!is.null(xl)){
         url <- paste0('https://nominatim.openstreetmap.org/reverse?format=xml&lat=', yl, '&lon=', xl, '&addressdetails=1')
        # resp <- GET(url, user_agent("Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0") )
         resp <- GET(url )
         xml <- content(resp, 'text')
         df <- as.list( read_xml(xml))
        loc <- xml_text(xml_find_all(df, xpath = "//reversegeocode/result"))
        locT <- paste0(xl, ' E.   ', yl, ' N.<BR> Near ',loc)
  }
  
  agencyC <- j[[1]]$agency_code
  projC <- j[[1]]$proj_code
  html <- paste0('<BR><table>
                 <colgroup>
                   <col span="1" style="width: 500px;">
                   <col span="1" style="width: 500px;">
                </colgroup><tbody>
                 <tr><td><b>Profile ID: </b>', agencyC, '_', projC , '_', j[[1]]$s_id, '</td>')
  html <- paste0(html, '<td><b>Location: </b>', locT, '</td></tr>')
  
  an <- agencies[agencies$code==agencyC,]$name
  pn <- projects[projects$agencyCode ==agencyC & projects$code==projC,]$name
  
  html <- paste0(html, '<tr><td><b>Agency: </b>', an, '</td>')
  html <- paste0(html, '<td><b>Project: </b>', pn, '</td></tr>')
  
   person <- getVals(j[[1]], 'desc_by', 'C_S_DESC_BY')
   html <- paste0(html, '<tr><td><b>Described by: </b>', person, '</td>')
   dt <- getVals(j[[1]], 's_date_desc')
   sdt <- ''
   if(nchar(dt) > 0){
     dy <- str_sub(dt, 1,2)
     mn <- str_sub(dt, 3,4)
    yr <-  str_sub(dt, 5,8)
    sdt <- paste0(dy, '/', mn, '/', yr)
   }
   html <- paste0(html, '<td><b>Date Described: </b>', sdt, '</td></tr>')
   
   aOrd <- getVals( j[[1]]$z_observations[[1]], 'o_asc_ord', 'C_O_ASC_ORD')
   aSubOrd <- getVals( j[[1]]$z_observations[[1]], 'o_asc_subord', 'C_O_ASC')
   agrp <- getVals( j[[1]]$z_observations[[1]], 'o_asc_gg', 'C_O_ASC')
   asubgrp <- getVals( j[[1]]$z_observations[[1]], 'o_asc_subg', 'C_O_ASC')
   html <- paste0(html, '<tr><td><b>Aust Soil Classification: </b>', asubgrp, ' ',agrp, ' ', aSubOrd, ' ', aOrd, '</td>')
   html <- paste0(html, '<td><b>Great Soil Group: </b>', getVals(j[[1]]$z_observations[[1]], 'o_gsg', 'C_O_GSG'), '</td></tr>')
   html <- paste0(html, '<tr><td><b>Principal Profile Form: </b>', getVals(j[[1]]$z_observations[[1]], 'o_ppf'), '</td>')
   html <- paste0(html, '<td><b>Disturbance: </b>', ifelse(length(j[[1]]$z_observations[[1]]$z_disturbances)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_disturbances[[1]], 'dist_type', 'C_DIST_TYPE')), '</td></tr>')
   
   html <- paste0(html, '<td><b>Landform Pattern: </b>', getVals(j[[1]], 's_patt_type', 'C_S_PATT_TYPE'), '</td>')
   html <- paste0(html, '<td><b>Landform Element: </b>', getVals(j[[1]], 's_elem_type', 'C_S_ELEM_TYPE'), '</td></tr>')
   
   html <- paste0(html, '<td><b>Morphological Type: </b>', getVals(j[[1]], 's_morph_type', 'C_S_MORPH_TYPE'), '</td>')
   html <- paste0(html, '<td><b>Relief Modal Slope: </b>', getVals(j[[1]], 's_rel_ms_class', 'C_S_REL_MS_CLASS'), '</td></tr>')
   
   html <- paste0(html, '<td><b>Slope Class: </b>', getVals(j[[1]], 's_slope_class', 'C_S_SLOPE_CLASS'), '</td>')
   html <- paste0(html, '<td><b>Slope: </b>', getVals(j[[1]], 's_slope'), ' % </td></tr>')
   
   veg=''
   if(!is.null(j[[1]]$z_observations[[1]]$z_veg_strata) & length(j[[1]]$z_observations[[1]]$z_veg_strata)>0){
     for (k in 1:length(j[[1]]$z_observations[[1]]$z_veg_strata)) {
       if(length(j[[1]]$z_observations[[1]]$z_veg_strata)>0){
         for(nn in 1:length(j[[1]]$z_observations[[1]]$z_veg_strata[[k]]$z_veg_species)){
           if(length(j[[1]]$z_observations[[1]]$z_veg_strata[[k]]$z_veg_species)>0){
           v <- j[[1]]$z_observations[[1]]$z_veg_strata[[k]]$z_veg_species[[nn]]$vsp_code
           if(!is.null(v)){
             if(v!= ''){
              veg <- paste0(veg, v,  ';')
             }
           }
           }
         }
     }
     }
   }
   
   mr_type <- ifelse(length(j[[1]]$z_observations[[1]]$z_micro_reliefs)==0, '', getVals(j[[1]]$z_observations[[1]]$z_micro_reliefs[[1]], 'mr_type', 'C_MR_TYPE'))
   mr_hint <- ifelse(length(j[[1]]$z_observations[[1]]$z_micro_reliefs)==0, '', getVals(j[[1]]$z_observations[[1]]$z_micro_reliefs[[1]], 'mr_horizontal_interval'))
   mr_vint <- ifelse(length(j[[1]]$z_observations[[1]]$z_micro_reliefs)==0, '', getVals(j[[1]]$z_observations[[1]]$z_micro_reliefs[[1]], 'mr_vertical_interval'))
   mr <- paste0(mr_type, ' ',mr_hint, ' ',mr_vint)
   if (!grepl("^\\s*$", mr)){ omr<- paste0(mr)  }else {omr<- ''}

   html <- paste0(html, '<td><b>Veg species: </b>', veg, '</td>')
   html <- paste0(html, '<td><b>Microrelief: </b>',  mr , '</td></tr>')
   
   scf_abun <-ifelse(length(j[[1]]$z_observations[[1]]$z_surface_coarse_frags)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_surface_coarse_frags[[1]], 'scf_abun', 'N_CF_ABUN'))
   scf_size <- ifelse(length(j[[1]]$z_observations[[1]]$z_surface_coarse_frags)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_surface_coarse_frags[[1]], 'scf_size', 'N_CF_SIZE'))
   scf_shape <- ifelse(length(j[[1]]$z_observations[[1]]$z_surface_coarse_frags)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_surface_coarse_frags[[1]], 'scf_shape', 'C_CF_SHAPE'))
   scf_lith <- ifelse(length(j[[1]]$z_observations[[1]]$z_surface_coarse_frags)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_surface_coarse_frags[[1]], 'scf_lith', 'C_LITHOLOGY'))
   scf <- paste0(scf_abun, ' ',scf_size, ' ',scf_shape, ' ', scf_lith)
   if (!grepl("^\\s*$", scf)){ oscf<- paste0(scf)  }else {oscf<- ''}
   
   scon <- ifelse(length(j[[1]]$z_observations[[1]]$z_surf_conditions)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_surf_conditions[[1]], 'scon_stat', 'C_SCON_STAT'))
   
   html <- paste0(html, '<td><b>Surface Coarse Fragments: </b>', oscf, '</td>')
   html <- paste0(html, '<td><b>Surface Condition: </b>',  scon , '</td></tr>')
   
   ro_abun <-ifelse(length(j[[1]]$z_observations[[1]]$z_rock_outcrops)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_rock_outcrops[[1]], 'ro_abun', 'N_RO_ABUN'))
   ro_lith <- ifelse(length(j[[1]]$z_observations[[1]]$z_rock_outcrops)==0, '',  getVals(j[[1]]$z_observations[[1]]$z_rock_outcrops[[1]], 'ro_lith', 'C_LITHOLOGY')) 
   ro <- paste0(ro_abun, ' ',ro_lith)
   if (!grepl("^\\s*$", ro)){ oro <- paste0(ro)  }else {oro <- ''}
   html <- paste0(html, '<td><b>Rock Outcrop: </b>', oro, '</td>')
   html <- paste0(html, '<td><b> </b>',  '</td></tr>')
   
   
   n1 <- j[[1]]$s_notes
   n2 <-  j[[1]]$z_observations[[1]]$o_notes
   html <- paste0(html, '<td colspan="2"><b>Notes: </b>',n1, ' - ', n2, '</td>')
  # html <- paste0(html, '<td><b>Microrelief: </b>',  getVals(j[[1]]$z_observations[[1]]$z_micro_releifs[[1]],  'mr_type'), '</td></tr>')
   
   
   
  html <- paste0(html, '</tbody></table>')
  hors <- j[[1]]$z_observations[[1]]$z_horizons
  
  
  txt <- vector(mode = 'character', length = length(hors))
  
  if(length(hors)>0){
  
  for (i in 1:length(hors)) {
    
    o <- hors[[i]]
    ph<-''
    bdy<-''
    hname <-''
    hstructure<-''
    tex<-''
    colcode=''
    
    hpref <- getVals(o, 'h_desig_num_pref')
    hname <- getVals(o, 'h_desig_master')
    hsubdiv <- getVals(o, 'h_desig_subdiv')
    hnsuf <- getVals(o, 'h_desig_suffix')
    hname <- paste0(hpref, hname, hsubdiv, hnsuf)
    if (!grepl("^\\s*$", hname)){hname=paste0('<B>', hname, ' : </B>')}
    
    str_grade <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_grade', domain = 'C_STR_PED_GRADE'))
    str_size <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_size', domain = 'N_STR_PED_SIZE'))
    str_type <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_type', domain = 'C_STR_PED_TYPE'))
    
    hstructure <- paste0(str_grade, ' ', str_size, ' ', str_type)
    if (!grepl("^\\s*$", hstructure)){hstructure=paste0(hstructure, ' structure; ')}
    
    
    b_dist <- getVals(o, 'h_bound_distinct', 'N_H_BOUND_DISTINCT')
    if(b_dist!=''){bdy <- paste0(b_dist, ' change to')}
    
    col <-  ifelse(length(o$z_colours)==0, '', getVals(o$z_colours[[1]], 'col_hue_val_chrom', 'C_MUNSELL_COLOUR'))
    colcode <-  ifelse(length(o$z_colours)==0, '', getVals(o$z_colours[[1]], 'col_hue_val_chrom_desc'))
    if(colcode!=''){colcode=paste0(' (', colcode, '); ')}
    
    tex <- getVals(o, 'h_texture', 'C_H_TEXTURE')
    if(tex!=''){tex=paste0(tex, '; ')}

    phVal <- ifelse(length(o$z_phs)==0, '', getVals(o$z_phs[[1]], 'ph_value'))
    if(phVal!=''){ph <- paste0('pH : ', phVal, '; ')}  
    
    ec <- getVals(o, 'h_ec')
    if(ec!=''){ph <- paste0('EC : ', ec, '; ')}  
    
    drain <- getVals(o, 'h_drainage', 'C_O_DRAINAGE')
    if(drain!=''){ph <- paste0(drain, '; ')} 

    perm <- getVals(o, 'h_permeability', 'C_PERMEABILITY')
    if(perm!=''){perm <- paste0(perm, '; ')} 
    
    # sw <- getVals(o, 'h_soil_water_stat')
    # sticky <- getVals(o, 'h_stickiness')
    # repel <- getVals(o, 'h_water_repellence')
   
    cf_abun <- ifelse(length(o$z_coarse_frags)==0, '', getVals(obj = o$z_coarse_frags[[1]], attribute = 'cf_abun', domain = 'N_CF_ABUN'))
    cf_dist<- ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_distribution', domain ='C_CF_DISTRIBUTION'))
    cf_shape <- ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_shape', domain ='C_CF_SHAPE'))
    cf_size <-  ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_size', domain ='N_CF_SIZE'))
    cf_lith <-  ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_lith', domain ='C_LITHOLOGY'))
    
    cf <- paste0(cf_abun, ' ',cf_dist, ' ',cf_shape, ' ',cf_size, ' ',cf_lith )
    if (!grepl("^\\s*$", cf)){ ocf <- paste0(cf, ' coarse fragments; ')  }else {ocf<- ''}
    
    seg_abun <- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_abun', domain = 'N_SEG_ABUN'))
    seg_form<- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_form', domain = 'C_SEG_FORM'))
    seg_nat <- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_nature', domain = 'C_SEG_NATURE'))
    seg_size <- ifelse(length(o$z_segregations)==0, '',  getVals(o$z_segregations[[1]], 'seg_size', domain = 'N_SEG_SIZE'))
    seg_strength <-  ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_strength', domain = 'C_SEG_STRENGTH'))
    
    seg <- paste0(seg_abun, ' ',seg_nat, ' ', seg_form, ' ', seg_size, ' ',seg_strength )
    if (!grepl("^\\s*$", seg)){ oseg <- paste0(seg, ' segregations; ')  }else {oseg<- ''}
    
    mott_cont <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]],  'mott_contrast', domain = 'C_CONTRAST'))
    mott_abun <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_abun', domain = 'N_MOTT_ABUN'))
    mott_size<- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_size', domain = 'N_MOTT_SIZE'))
    mott_col <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_hue_val_chrom', domain = 'C_MOTT_COLOUR'))
    
    mot <- paste0(mott_cont, ' ', mott_abun, ' ',mott_size, ' ', mott_col )
    if (!grepl("^\\s*$", mot)){ omot <- paste0(mot, ' mottles; ')  }else {omot<- ''}
    
    
    
    
       
    
    
    
    desc <- paste0(hname, ' ', col, colcode, tex, hstructure, ph, ec, ocf, oseg, omot, bdy)
    txt[[i]] <- paste0('<P>', desc, '</P>')
    
  }
}
  allhtml <- c(html,'<BR><BR>', txt)
  return(allhtml) 
}
  

getSiteData <- function(jsnList){
  
  j <- jsnList
  s<- j[[1]]
  agency <- getVals(s, 'agency_code')
  proj <- getVals(s, 'proj_code')
  s_id <- getVals(s, 's_id')
  desc_by <- getVals(s, 'desc_by')
  date <- getVals(s, 's_date_desc')
  e_type <- getVals(s, 's_elem_type')
  scale <- getVals(s, 's_map_scale')
  map_sheet <- getVals(s, 's_map_sheet_no')
  m_slope <- getVals(s, 's_modal_slope')
  m_type <- getVals(s, 's_morph_type')
  p_type <- getVals(s, 's_patt_type')
  rel_ms <- getVals(s, 's_rel_ms_class')
  relief <- getVals(s, 's_relief')
  r_class <- getVals(s, 's_relief_class')
  slope <- getVals(s, 's_slope')
  s_class <- getVals(s, 's_slope_class')
  s_eval <- getVals(s, 's_slope_eval')
  
 odf <- data.frame(agency, proj, s_id, desc_by, date, e_type, scale, map_sheet,m_slope,m_type,p_type,rel_ms,
                   relief,r_class, slope, s_class, s_eval )
 
 return(odf)
  
}

getObservationData <- function(jsnList){
  
  j <- jsnList
  o <- j[[1]]$z_observations[[1]]
  otype <- getVals(o, 'o_type')
  lat <- getVals(o, 'o_latitude_GDA94')
  lon <- getVals(o, 'o_longitude_GDA94')
  datum <- getVals(o, 'o_datum')
  A_ord <- getVals(o, 'o_asc_ord')
  A_sub <- getVals(o, 'o_asc_subord')
  A_gg <- getVals(o, 'o_asc_gg')
  A_subg <- getVals(o, 'o_asc_subg')
  A_fam1 <- getVals(o, 'o_asc_fam1')
  A_fam2 <- getVals(o, 'o_asc_fam2')
  A_fam3 <- getVals(o, 'o_asc_fam3')
  A_fam4 <- getVals(o, 'o_asc_fam4')
  A_fam5 <- getVals(o, 'o_asc_fam5')
  gsg <- getVals(o, 'o_gsg')
  ppf <- getVals(o, 'o_ppf')
  drain<- getVals(o, 'o_drainage')
  perm <- getVals(o, 'o_permeability')
  elev <- getVals(o, 'o_elevation')
  elevT <- getVals(o, 'o_elevation_eval')
  lith <- getVals(o, 'o_sb_lith')
  
  odf <- data.frame(otype, lat, lon, datum, A_ord, A_sub, A_gg, A_subg, A_fam1, A_fam2, A_fam3, A_fam4, A_fam5, gsg, ppf,
                    drain, perm, elev, elevT, lith)
  
  return(odf)
  
}

getNotes <- function(jsnList){
  
  j <- jsnList

  siteNote <- as.character(getVals(j[[1]], 's_notes'))
  ObsNote <-  as.character(getVals(j[[1]]$z_observations[[1]], 'o_notes'))
  LocNote <-  as.character(getVals(j[[1]]$z_observations[[1]], 'o_location_notes'))

  nlist <- c(siteNote, ObsNote, LocNote)
  idxs <- which(nlist!='')
  if(length(idxs)==0){
    return(NULL)
  }else{
    recs <- nlist[idxs]
    df<- data.frame(Notes=str_trim(recs))
    return(df)
  }
}
  
getHorizonData <- function(jsnList){

    j <- jsnList
    #json_tree_view(jsn)
    
    hors <- j[[1]]$z_observations[[1]]$z_horizons
    if(length(hors)>0){
    odf <- data.frame()
    
    for (i in 1:length(hors)) {
      
      o <- hors[[i]]
      h_no <-  getVals(o, 'h_no')
      ud <- getVals(o, 'h_depth_upper')
      ld <- getVals(o, 'h_depth_lower')
      hpref <- getVals(o, 'h_desig_num_pref')
      hname <- getVals(o, 'h_desig_master')
      hsubdiv <- getVals(o, 'h_desig_subdiv')
      hnsuf <- getVals(o, 'h_desig_suffix')
      dist <- getVals(o, 'h_bound_distinct')
      shape <- getVals(o, 'h_bound_shape')
      carb <- getVals(o, 'h_carbonate_eff')
      disp <- getVals(o, 'h_dispersion')
      texture <- getVals(o, 'h_texture')
      texq <- getVals(o, 'h_texture_qual')
      colour <- ifelse(length(o$z_colours)==0, '', getVals(o$z_colours[[1]], 'col_hue_val_chrom'))
      colm <- ifelse(length(o$z_colours)==0, '', getVals(o$z_colours[[1]], 'col_moisture_stat_code'))
      
      drain <- getVals(o, 'h_drainage')
      ec <- getVals(o, 'h_ec')
      pH <- ifelse(length(o$Z_phs)==0, '', getVals(o$Z_phs[[1]], 'ph_value'))
      perm <- getVals(o, 'h_permeability')
      sw <- getVals(o, 'h_soil_water_stat')
      sticky <- getVals(o, 'h_stickiness')
      repel <- getVals(o, 'h_water_repellence')
      
      sGrade <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_grade'))
      sSize <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_size'))
      sType <- ifelse(length(o$z_structures)==0, '', getVals(o$z_structures[[1]], 'str_ped_type'))
      
      cf_abun <- ifelse(length(o$z_coarse_frags)==0, '', getVals(obj = o$z_coarse_frags[[1]], attribute = 'cf_abun'))
      cf_dist<- ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_distribution'))
      cf_shape <- ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_shape'))
      cf_size <-  ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_size'))
      cf_lith <-  ifelse(length(o$z_coarse_frags)==0, '', getVals(o$z_coarse_frags[[1]], 'cf_lith'))
      
      seg_abun <- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_abun'))
      seg_form<- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_form'))
      seg_nat <- ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_nature'))
      seg_size <- ifelse(length(o$z_segregations)==0, '',  getVals(o$z_segregations[[1]], 'seg_size'))
      seg_strength <-  ifelse(length(o$z_segregations)==0, '', getVals(o$z_segregations[[1]], 'seg_strength'))
      
      mott_abun <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_abun', domain = ))
      mott_size<- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_size'))
      mott_hvc <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_hue_val_chrom'))
      mott_col <- ifelse(length(o$z_mottles)==0, '', getVals(o$z_mottles[[1]], 'mott_colour'))
     
      
      
      # tdesc <- getVals(o, 'h_texture', 'C_H_TEXTURE')
      rdf <- data.frame(h_no, ud, ld, hname=paste0(hpref, hname, hsubdiv, hnsuf), 
                        colour,colm, tex=paste0(texture, texq), sGrade, sSize, sType, drain, perm,
                        cf_abun, cf_dist, cf_shape, cf_size, cf_lith,
                        mott_abun, mott_size, mott_col,  mott_hvc,
                        pH,  ec, 
                        seg_abun, seg_form, seg_nat, seg_size, seg_strength,
                        dist, shape, carb, disp,  sticky, repel
                        )
      odf <- rbind(odf, rdf)
    }

  return(odf)
    }else{
      return(NULL)
    }

}



getLabData <- function(jsnList){
  jl <- jsnList
  
  hors <- jl[[1]]$z_observations[[1]]$z_horizons
  
  if(length(hors)>0){
  odf <- data.frame()

      for (i in 1:length(hors)) {
        o <- hors[[i]]
        s <- o$z_samples
        
        if(!is.null(s) & length(s)>0){
          
          for (j in 1:length(s)) {
            
            lr <- s[[j]]$z_lab_results
            
            if(!is.null(lr) & length(lr)>0){
              #cnt=cnt+1
              sn <- s[[j]]$samp_no
              ud <- s[[j]]$samp_upper_depth
              ld <- s[[j]]$samp_lower_depth
              
              for(k in 1:length(lr)){
                rec <- lr[[k]]
                meth <- getVals(rec, 'labm_code')
                v <- getVals(rec, 'labr_value')
                rdf <- data.frame(SampleNo=sn, UD=ud, LD=ld, LabMCode=meth, Value=v )
                odf <- rbind(odf, rdf)
               # m[cnt, meth] <- v
              }
            }
          }
        }
      }  
  
  if(nrow(odf)==0){
    m=NULL
  }else{
    m <- makeWideTable(odf)
  }
 
      return(m)
  }else{
    return(NULL)
  }
}


makeWideTable <- function(df){
  

  cols <- unique(df$LabMCode)
  
  baseCols <- unique(df[c('SampleNo','UD', 'LD')])
  baseCols <- baseCols[with(baseCols, order(UD, LD), SampleNo), ]
  
  for (i in 1:length(cols)) {
    c <- cols[i]
    baseCols[c] <- rep('', nrow(baseCols))
  }
  
  bt <- baseCols
  nt <- df
  for (i in 1:nrow(bt)) {
    rec <- bt[i, ]
    sn <- rec$SampleNo
    ud <- rec$UD
    ld <- rec$LD
    for (j in 1:length(cols)) {
      att <- cols[j]
      v <- nt[nt$SampleNo==sn & nt$UD==ud & nt$LD==ld & nt$LabMCode==att, ]$Value
      vc <- paste(v, sep = " ", collapse = '; ')
      bt[i, ][att] <- vc
    }
    
  }
  colnames(bt)[2:3] <- c('Upper', 'Lower')
  return(bt)
}


getLabDataOld <- function(jsnList){
  jl <- jsnList
  
  hors <- jl[[1]]$z_observations[[1]]$z_horizons
  #json_tree_view(hors)
  
  if(!is.null(hors[[1]]$z_samples) & length(hors[[1]]$z_samples>0)){
    
    
    if(!is.null(hors[[1]]$z_samples[[1]]$z_lab_results[[1]]$labm_code)){
  
        #meths <- suppressMessages(json_path(jl, '$..labm_code'))
        labMeths <- unique(suppressMessages(json_path(jl, '$..labm_code')))
        sampCnt <- length(suppressMessages(json_path(jl, '$..samp_no')))
        
        if(sampCnt==0){return(NULL)}
        m <- as.data.frame(matrix(0, ncol = length(labMeths)+3, nrow = sampCnt))
        colnames(m) <- c('SampNo', 'Upper', 'Lower', labMeths)
        m[,]<-''
        
        cnt=0
        for (i in 1:length(hors)) {
          o <- hors[[i]]
          s <- o$z_samples
          
          if(!is.null(s)){
          
            for (j in 1:length(s)) {
              
              lr <- s[[j]]$z_lab_results
            
              if(!is.null(lr)){
                  cnt=cnt+1
                  m[cnt, 'SampNo'] <- s[[j]]$samp_no
                  m[cnt, 'Upper'] <- s[[j]]$samp_upper_depth
                  m[cnt, 'Lower'] <- s[[j]]$samp_lower_depth
                  
                  for(k in 1:length(lr)){
                    rec <- lr[[k]]
                    meth <- getVals(rec, 'labm_code')
                    v <- getVals(rec, 'labr_value')
                    m[cnt, meth] <- v
                  }
              }
            }
          }
        }  
      ### Check if all values in rows are blank eg = ''
      ol <- list()
      for (rr in 1: nrow(m)) {
       if(length(unique(as.list(m[rr,]))) == 1){
         ol <- c(ol, rr)
       }
      }
      idxs <- unlist(ol)
      if(length(idxs > 0)){
        m <- m[-idxs, ]
      }
             
        
 return(m)
    }else{return(NULL)}
  }else{return(NULL)}
}



getVals<- function(obj, attribute, domain=NULL){
  
  idx <- which(names(obj)==attribute)
  v <- as.character(obj[idx])
  if(is.null(v) | length(v) ==0){
    return('')
  }else{
    if(is.null(domain)){
      if(v=='NULL'){
        return('')
      }else{
        return(v)
      }
     
    }else{
      
      if(v=='NULL'){
        return('')
      }
      
      cds <- codes[codes$code_domain==domain,]
      if(nrow(cds>0)){
        dec <- cds[cds$code_value==v,]
        if(nrow(dec)==0){
          return(v)
        }else{
          desc <- dec$code_desc
          return(desc)
        }
      }else{
        return(v)
      }
    }
  }
}



getHueValueChroma <- function(){
  
}

getSoilProfileDiagram <- function(sid, p){
  
  marg = 0.2
  
  if(!'colour' %in% colnames(p)){
    hue <- ''
    value <-''
    chroma <- ''
  }else{
    colnames(p)
    hue <- str_sub(p$colour, start = 1, end = nchar(p$colour) - 2)
    value <- str_sub(p$colour, start = nchar(hue)+1, end = nchar(hue)+1)
    chroma <- str_sub(p$colour, start = nchar(hue)+2, end = nchar(hue)+2)
  }
  

  
  if(!'H_DESIG_MASTER' %in% colnames(p)){
    hname=''
  }else{
    hname=p$hname
  }

  sp <-  data.frame(ud=as.numeric(p$ud)*100, ld=as.numeric(p$ld)*100, hname=hname, hue, value, chroma, p$hname, p$cf_abun, p$cf_size, p$seg_abun, p$seg_size, p$mott_hvc, p$mott_col, p$mott_size, p$mott_abun)
  
  depth = max(sp$ld)
  # par(oma=c(0,0,0,0)) # all sides have 3 lines of space
    par(mar=c(0.1,3,6,2))
  
  lableft = 1.5
  profleft = 2.5
  profWidth = 2
  
  plot.new(); plot.window(xlim=c(1,10),ylim=rev(c(0,depth)) )
  text(lableft, sp$ud[1], 'Depth (cm)')
  for (i in 1:nrow(sp)) {
    col='white'
    rec=sp[i,]
    if(rec$hue!=''){
      mnsl <- paste0(rec$hue, ' ', rec$value, '/', rec$chroma)
     try(col <- munsell::mnsl2hex(in_gamut(mnsl, fix = T)))
    }else{
      col='white'
    }
    rect(xleft = profleft, xright = profleft+profWidth, ybottom = rec$ld,   ytop = rec$ud, col=col)
    text(lableft, rec$ld, rec$ld, cex = 1, font=1)
   
    
    ## Segregations 
    if(rec$p.seg_abun !=''){
      if(rec$p.seg_size !=''){
        segsize <- as.numeric(rec$p.seg_size)
      }else{
        segsize <- 2
      }
       

      rx <- runif( n=as.numeric(rec$p.seg_abun)*2, min = profleft+marg, max= profleft+profWidth-marg)
      ry <- runif(n=as.numeric(rec$p.seg_abun)*2, min = rec$ud+marg, max= rec$ld-marg)
      points(rx,ry,  pch = 19, col='lightgray', cex= )
    }
    
    ## Coarse Frags 
    if(rec$p.cf_abun !=''){
      rx <- runif( n=as.numeric(rec$p.cf_abun)*2, min = profleft, max= profleft+profWidth)
      ry <- runif(n=as.numeric(rec$p.cf_abun)*2, min = rec$ud, max= rec$ld)
      points(rx,ry,  pch = 18, col='red' )
    }
    
    ## Mottles 
    if(rec$p.mott_abun !='' & rec$p.mott_abun!='0'){
      
      hgt = rec$ld-rec$ud 
      
      if(rec$p.mott_abun=='1'){
        ap <- ceiling(hgt * 0.05)
      }else if(rec$p.mott_abun=='2'){
        ap <- ceiling(hgt * 0.1)
      }else if(rec$p.mott_abun=='3'){
        ap <- ceiling(hgt * 0.2)
      }else if(rec$p.mott_abun=='4'){
        ap <- ceiling(hgt * 0.5)
      }else if(rec$p.mott_abun=='5'){
        ap <- ceiling(hgt * 0.7)
      }else{
        ap <- ceiling(hgt * 0.2)
      }
     
      mpercAbun = ap * 3
      print(mpercAbun)
      
      if(rec$p.mott_hvc !=''){
        mcol='black'
        hue <- str_sub(rec$p.mott_hvc, start = 1, end = nchar(rec$p.mott_hvc) - 2)
        value <- str_sub(rec$p.mott_hvc, start = nchar(hue)+1, end = nchar(hue)+1)
        chroma <- str_sub(rec$p.mott_hvc, start = nchar(hue)+2, end = nchar(hue)+2)
        try(mcol <- munsell::mnsl2hex(in_gamut(mnsl, fix = T)))

      }else if(rec$p.mott_col !=''){
        
        if(rec$p.mott_col=='R'){
          mcol='red'
        } else if(rec$p.mott_col=='B'){
          mcol='brown'
        }else if(rec$p.mott_col=='D'){
          mcol='#4d3319'
        }else if(rec$p.mott_col=='G'){
          mcol='azure4'
        }else if(rec$p.mott_col=='L'){
          mcol='darkcyan'
        }else if(rec$p.mott_col=='P'){
          mcol='snow'
        }else if(rec$p.mott_col=='Y'){
          mcol='yellow2'
        }else {
          mcol='black'
        }
      }
      
      rx <- runif( n=mpercAbun, min = profleft+marg, max= profleft+profWidth-marg)
      ry <- runif(n=mpercAbun, min = rec$ud+marg, max= rec$ld-marg)
      points(rx,ry,  pch = 15, col=mcol)
    }
    
    ## Horizon Name
    text(profleft + profWidth/2, rec$ud +  ((rec$ld-rec$ud)/2), rec$p.hname, cex = 0.8, font=2, col='black')
  }
} 


getProfileLocation <- function(jsnList){
  jl <- jsnList
  lon=NULL
  lat=NULL
  lon <- jl[[1]]$z_observations[[1]]$o_longitude_GDA94
  lat <- jl[[1]]$z_observations[[1]]$o_latitude_GDA94
  
  ## Check original location
  if(is.null(lon) & is.null(lat)){
    lon <- jl[[1]]$z_observations[[1]]$o_orig_longitude
    lat <- jl[[1]]$z_observations[[1]]$o_orig_latitude
  }
  
  if(is.null(lon) & is.null(lat)){
        return(NULL)
    }else{
        return(data.frame(lon=lon, lat=lat))
    }
}

getProfileID <- function(jsnList){
  jl <- jsnList
  sid <- paste0( jl[[1]]$agency_code, '_', jl[[1]]$proj_code, '_', jl[[1]]$s_id)
  return(sid)
}




plotLabData <- function(df, i){
  
  my_i=i

  vals <- as.numeric(df[,colnames(df)[3+my_i]])
  inDF <- data.frame(UD=df$Upper, LD=df$Lower, vals)
  idxs <- which(complete.cases(inDF))
  xdf <- inDF[idxs,]
  
  att <- colnames(df)[3+my_i]
  units <- labMethods[labMethods$LABM_CODE==att, ]$LABM_UNITS
  prop <- labMethods[labMethods$LABM_CODE==att, ]$LABM_SHORT_NAME
  
  xBound <-  max(xdf$vals)  + (max(xdf$vals) * 0.2)
  depth <- max(as.numeric(inDF$LD))
  f <- tempfile()
  
  png(filename = f, width = 250, height = 300, units = 'px')
  
  par(oma=c(0,0,0,0))
  par(mar=c(5,5,3,2))
  plot( 0, type="n",  col.main = 'blue', cex.main=1,
        xlab= paste0( inDF$Units[1]),
        ylab='Soil Depth (m)',
        yaxs = "i", xaxs = "i", xlim = c(0, xBound), ylim = rev(range(c(0,depth))),
        cex.lab = 1,
        main = paste0(prop, '\n', att, ' (', units, ')')
        
  )
  
  for (s in 1:nrow(xdf)) {
    x <- c(0.1, xdf$val[s], xdf$val[s], 0, 0)
    y =  c( xdf$LD[s],  xdf$LD[s],  xdf$UD[s],  xdf$UD[s],  xdf$LD[s])
    polygon(x,y, col=c("burlywood"), border=c("black"), lwd=1, lty=c("solid"))
  }
  
  dev.off()
  b64 = base64enc::base64encode(f)
  unlink(f)
  return(b64)
  
}

