notone = function(x){ # Makes the equivalent for ne in SAS code as R != returns NA if the value is NA but we want 2 == NA to be FALSE not NA 
  isit = (x !=1 | is.na(x))
  return(isit)
}

recode.mssic = function(dat, levels = F){
  new.dat = dat |> 
    mutate(male = case_when(
      gender ==1 ~ 1, 
      gender == 2 ~ 0,
      T~NA
    ),
    bmi_cat2 = ifelse(bmi >= 25 & bmi < 30, 1,0),
    bmi_cat3 = ifelse(bmi >= 30, 1,0),
    bmi_cat = case_when(
      bmi < 18.5 ~ "underweight",
      bmi >= 18.5 & bmi <= 24.9 ~ "normal",
      25 <= bmi & bmi < 3 ~ "overweight", 
      bmi >= 30 ~ "obese",
      T ~ NA
    ),
    bmi_cats = case_when(
      bmi_cat %in% c("underweight", "normal") ~ 1, 
      bmi_cat == "overweight" ~ 2, 
      bmi_cat == "obese" ~ 3
    ),
    priv_ins = ifelse(primaryinsurance %in% c(1,3,4,7,10), 1, 0),
    # TODO Why was this coded the other way? 
    scoliosis = case_when(
      scoliosis_degree_e == 1 ~ 1,
      scoliosis_degree_e == 0 ~ 0,
      T ~ NA
    ),
    diabetes = ifelse(e_diabetes_type == 1,0,1),
    flg_bc = ifelse(flg_bc == 2, 0, flg_bc),
    pmh_dvt = flg_bc, 
    flg_cad = ifelse(flg_cad == 2, 0, flg_cad),
    pmh_cad = flg_cad, 
    flg_depression = ifelse(flg_depression == 2,0,flg_depression),
    pmh_depression = flg_depression,
    flg_anxiety = ifelse(flg_anxiety == 2, 0, flg_anxiety),
    pmh_anxiety = flg_anxiety, 
    flg_osteo = ifelse(flg_osteo == 2, 0, flg_osteo),
    pmh_osteo = flg_osteo,
    flg_chf = ifelse(flg_chf == 2, 0, flg_chf),
    pmh_chf = flg_chf,
    flg_copd = ifelse(flg_copd == 2, 0, flg_copd),
    pmh_copd = flg_copd, 
    flg_hypertension = ifelse(flg_hypertension == 2, 0, flg_hypertension),
    pmh_htn = flg_hypertension, 
    flg_ami = ifelse(flg_ami == 2,0,flg_ami),
    pmh_ami = flg_ami, 
    flg_afib = ifelse(flg_afib == 2, 0,flg_afib),
    pmh_afib = flg_afib,
    flg_tia = ifelse(flg_tia == 2,0,flg_tia),
    pmh_tia = flg_tia, 
    flg_cva = ifelse(flg_cva == 2,0,flg_cva),
    pmh_cva = flg_cva, 
    flg_prescription_anticoag_use = ifelse(flg_prescription_anticoag_use == 2, 0,flg_prescription_anticoag_use),
    pmh_rx_anticoag_use = flg_prescription_anticoag_use,
    flg_fibromyalgia = ifelse(flg_fibromyalgia == 2, 0, flg_fibromyalgia),
    pmh_fibromyalgia = flg_fibromyalgia, 
    
    sxduration1 = ifelse(e_cmb_symptom_duration ==2, 1,0),
    sxduration2 = ifelse(e_cmb_symptom_duration == 3, 1,0),
    motor_deficit = ifelse(e_cmb_motor_deficit %in% c(1,2), 1,0),
    ind_amb_preop = ifelse(e_cmb_ambulation %in% c(1,4), 1,0),
    prior_physical_therapy = ifelse(grepl("1", e_nonsurgical_treatments), 1,0),
    prior_chiropractor = ifelse(grepl("2", e_nonsurgical_treatments), 1,0),
    prior_injection = ifelse(grepl("3", e_nonsurgical_treatments), 1,0),
    asa_gt2 = case_when(
      e_asa_grade %in% c(1,2) ~ 0, 
      e_asa_grade %in% C(3:5) ~ 1, 
      T ~ NA
    ),
    surgical_approach_cerv = case_when(
      grepl("1", surgicalapproach_cerv) & !grepl("2", surgicalapproach_cerv) ~ 1, # anterior only
      grepl("2", surgicalapproach_cerv) & !grepl("1", surgicalapproach_cerv) ~ 2, # posterior only 
      grepl("1", surgicalapproach_cerv) & grepl("2", surgicalapproach_cerv) ~ 3 # anterior and posterior 
    ),
    anterior = ifelse(surgical_approach_cerv == 1, 1,0) ,
    posterior = ifelse(surgical_approach_cerv == 2, 1,0),
    anterior_posterior = ifelse(surgical_approach_cerv == 3, 1,0), 
    prev_spinesurgery = case_when(
      e_prev_spinesurgery == 1 ~ 1,
      e_prev_spinesurgery %in% c(0,2) | e_prev_surgery %in% c(0,2) ~ 0,
      T ~ NA
      ),
    
    flg_cerv_path_disc_herniation = ifelse(grepl("1", e_cerv_dis_state), 1,0),
    flg_cerv_path_foraminal_steno = ifelse(grepl("2", e_cerv_dis_state), 1,0),
    flg_cerv_path_central_stenosis = ifelse(grepl("3", e_cerv_dis_state), 1,0),
    flg_cerv_path_instability = ifelse(grepl("4", e_cerv_dis_state), 1,0),
    flg_cerv_path_adjacent = ifelse(grepl("6", e_cerv_dis_state), 1,0),
    flg_cerv_path_revision = ifelse(grepl("5", e_cerv_dis_state), 1,0),
    flg_cerv_path_pseudoarthritis = ifelse(grepl("8", e_cerv_dis_state), 1,0),
    flg_cerv_path_synovial_cyst = ifelse(grepl("9", e_cerv_dis_state), 1,0),
    
    flg_cerv_clin_radicular = ifelse(grepl("2", e_cerv_clinpresentation), 1,0),
    flg_cerv_clin_balance_gait = ifelse(grepl("9", e_cerv_clinpresentation), 1,0),
    flg_cerv_clin_bowel_bladder = ifelse(grepl("6", e_cerv_clinpresentation), 1,0),
    
    phq2_depression_baseline = ifelse(bl_phq2score %in% c(0:2), 0, 1),
    
    flg_lum_path_spondylolisthesis = ifelse(grepl("2", e_lum_dis_state), 1,0),
    flg_lum_path_stenosis = ifelse(grepl("3", e_lum_dis_state), 1,0),
    flg_lum_path_disc_herniation = ifelse(grepl("1", e_lum_dis_state), 1,0),
    flg_lum_path_recurrent_disc = ifelse(grepl("4", e_lum_dis_state), 1,0),
    flg_lum_path_adjacent = ifelse(grepl("5", e_lum_dis_state), 1,0),
    flg_lum_path_revision = ifelse(grepl("6", e_lum_dis_state), 1,0),
    flg_lum_path_peudoarthrosis = ifelse(grepl("8", e_lum_dis_state), 1,0),
    flg_lum_path_synovial_cyst = ifelse(grepl("9", e_lum_dis_state), 1,0),
    
    flg_lum_clin_axial_pain = ifelse(grepl("1", e_lum_clinpresentation),1,0),
    flg_lum_clin_radicular_pain = ifelse(grepl("2", e_lum_clinpresentation),1,0),
    flg_lum_clin_bowel_dysfunction = ifelse(grepl("4", e_lum_clinpresentation),1,0),
    flg_lum_clin_muscle_weakness = ifelse(grepl("5", e_lum_clinpresentation),1,0),
    flg_lum_clin_footdrop = ifelse(grepl("6", e_lum_clinpresentation),1,0),
    flg_lum_clin_balance_gait = ifelse(grepl("9", e_lum_clinpresentation),1,0),
    
    
    race = bl_race, 
    race = case_when(
      race_e == 1 ~ 5,
      race_e == 2 ~ 3,
      race_e == 3 ~ 2,
      race_e == 4 ~ 4,
      race_e == 5 ~ 9,
      race_e == 6 ~ 8, 
      race_e == 7 ~ 1, 
      race_e == 8 ~ 10, 
      T ~ race 
    ),
    race1 = factor(case_when(
      race == 1 ~ "White", 
      race == 2 ~ "Black",
      race %in% c(3:5, 7:9) ~ "Other",
      T ~ NA
    ), levels = c("White", "Black", "Other")),
    race_miss = ifelse(bl_race %in% c(NA,10) & race_e %in% c(NA,8),1,0),
    black = ifelse((bl_race == 2 | (bl_race %in% c(NA,10) & race_e == 3)), 1,0),
    other_race = ifelse(bl_race %in% c(3:5,7:9) | (bl_race %in% c(NA,10) & race_e %in% c(1:6)),1,0),
    smoking_status = factor(case_when(
      bl_smoker %in% c(1,2) | (is.na(bl_smoker) & tobacco_use_e == 1) ~ 1, 
      bl_smoker ==3 | (is.na(bl_smoker) & tobacco_use_e == 2) ~ 2,
      bl_smoker ==4 | (is.na(bl_smoker) & tobacco_use_e == 3) ~ 3, 
      T ~ NA
    ), levels = c(1,2,3,NA), labels = c("Current", "Former", "Never")),
    smoke_miss = ifelse(is.na(bl_smoker) & tobacco_use_e %in% c(NA,4), 1,0),
    current_smoker = ifelse(bl_smoker %in% c(1,2) | (is.na(bl_smoker) & tobacco_use_e == 1), 1,0),
    # TODO: What is going on with education here? 
    educ_lths = ifelse(bl_educ == 1, 1,0), 
    educ_college2 = ifelse(bl_educ == 3, 1, 0),
    educ_degree = ifelse(bl_educ %in% c(4,5), 1,0),
    educ = case_when(
      bl_educ == 1 ~ "<HS",
      bl_educ == 2 ~ "HS",
      bl_educ == 3 ~ "2-yr",
      bl_educ == 4 ~ "College",
      bl_educ == 5 ~ "College+",
      T ~ NA
    ),
    
    preop_opiuse = ifelse(bl_op_painkiller == 1, 1,0),
    phq2_depression_baseline = ifelse(bl_phq2score %in% c(0,1,2),0,1),
    educ_miss = ifelse(is.na(educ_lths),1,0),
    opiod_miss = ifelse(is.na(preop_opiuse),1,0),
    phq2_miss = ifelse(is.na(phq2_depression_baseline),1,0),
    motor_miss = case_when(
      is.na(e_cmb_motor_deficit) ~ NA,
      is.na(motor_deficit) ~ 1, 
      T ~ 0
    ),
    #sx_duration1 = ifelse(duration_miss == 1, 0,sx_duration1), 
    #sx_duration2 = ifelse(duration_miss == 1, 0,sx_duration2),
    across(c("educ_lths", "educ_college2", "educ_degree", "preop_opiuse", "phq2_depression_baseline"), ~replace_na(.x,0)),
    outpatient = ifelse(encounter_type_e == 2, 1,0),
    proctype = case_when(
      flg_arthroplasty == 1 ~ 0, 
      (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) & (flg_instrumentationplaced_cerv == 1 | flg_instrumentationplaced_lumb ==1) ~ 1, 
      (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) & (flg_instrumentationplaced_cerv == 0 | flg_instrumentationplaced_lumb ==0) ~ 2,
      (notone(flg_arthrodesis_cerv) & notone(flg_arthrodesis_lumb)) & notone(flg_arthroplasty) & (fl_laminectomyetc ==1 | flg_discectomy == 1) & (notone(flg_instrumentationplaced_cerv) & notone(flg_instrumentationplaced_lumb)) ~ 3, 
      #flg_arthrodesis_cerv != 1 & flg_arthrodesis_lumb != 1) & flg_arthroplasty !=1 & (fl_laminectomyetc ==1 | flg_discectomy == 1) & (flg_instrumentationplaced_cerv !=1 & flg_instrumentationplaced_lumb !=1) ~ 3,
      T ~ 4
    ),
    fusion = ifelse(proctype %in% c(1,2), 1,0),
    discharge_not_home = case_when(
      e_discharge_place %in% c(1,2) ~ 0 , 
      e_discharge_place %in% c(4,5,8,9,10) ~1,
      T ~ NA
    ),
    surgery_length_hour = val_surgery_length/60,
    e_cmb_symptom_duration = ifelse(e_cmb_symptom_duration == 4, NA, e_cmb_symptom_duration),
    flg_readmit = ifelse(
      (e_readmit1==1 & notone(e_readmit1_multi_stage) & e_readmit1_complications != 29) | 
        (e_readmit2==1 & notone(e_readmit2_multi_stage) & e_readmit2_complications != 29)  | 
        (e_readmit3==1 & notone(e_readmit3_multi_stage) & e_readmit3_complications !=29),
      1,
      0 
    ),
    flg_readmit_reop = ifelse((returntoor1==1 & !(unplanned_spine_90d_e %in% c(3,4,5))) |  
                                (returntoor2==1 & !(unplanned_spine_90d_e %in% c(3,4,5))) |
                                (returntoor3==1 & !(unplanned_spine_90d_e %in% c(3,4,5))),
                              1,
                              0),
    # TODO employed_baseline = ifelse(e_employment == 1, 1,0) 
    #CANT FIND E_EMPLOYMENT 
    bl_promis_pf1 = as.numeric(bl_promis_pf),
    flg_urinary_retention = e_lt_utireqcatheter, 
    flg_ssi = e_lt_ssi,
    flg_any_mortality = e_lt_pt_died,
    flg_axial_pain = e_lt_axial_pain,
    flg_claudication = e_lt_claudication, 
    flg_dvt = e_lt_deepveinthrombosis, 
    flg_ed_visit = visit_obs_b, 
    flg_ileus = e_lt_ileus, 
    flg_mi = e_lt_myocardialinfarction,
    flg_myelopathy = e_lt_myelopathy, 
    flg_bowel_bladder = e_lt_bowel_bladder, 
    flg_newneurodeficit_stroke_mri = e_lt_newneurodeficit, 
    flg_pe = e_lt_pulmonaryembolism, 
    flg_radicular_findings = e_lt_radicular_findings, 
    flg_weakness = e_lt_weakness, 
    flg_wound_dehiscense = e_lt_wounddehiscense, 
    flg_unpln_reop = flg_unpln_ret1_or, 
    any_complication = ifelse((
      flg_any_mortality == 1 | 
      flg_dvt == 1 | flg_ileus == 1 | 
      flg_mi == 1 | 
      flg_newneurodeficit_stroke_mri == 1 | 
      flg_readmit_reop == 1 | 
      flg_unpln_reop == 1 | 
      flg_readmit == 1 | 
      flg_pe == 1 | 
      flg_ssi == 1 | 
      flg_myelopathy == 1 | 
      flg_dysphagia == 1), 
      1,
      0),
    
   
 flg_satisfy_90d = dplyr::case_when(
     D90_satisfaction %in% c(1,2) ~ 1, 
     D90_satisfaction %in% c(3,4) ~ 0, 
     T ~ NA
   ),
   flg_satisfy_1y = case_when(
     Y1_satisfaction %in% c(1,2) ~ 1, 
     Y1_satisfaction %in% c(3,4) ~ 0, 
     T ~ NA
   ),
   flg_satisfy_2y = case_when(
     Y2_satisfaction %in% c(1,2) ~ 1, 
     Y2_satisfaction %in% c(3,4) ~ 0, 
     T ~ NA
   ),
   phq_depression_90d = case_when(
     D90_phq2score %in% c(0,1,2) ~ 0, 
     D90_phq2score %in% c(3,4,5,6) ~ 1, 
     T ~ NA
   ),
   phq_depression_1y = case_when(
     Y1_phq2score %in% c(0,1,2) ~ 0, 
     Y1_phq2score %in% c(3,4,5,6) ~ 1, 
     T ~ NA
   ),
   phq_depression_2y = case_when(
     Y2_phq2score %in% c(0,1,2) ~ 0, 
     Y2_phq2score %in% c(3,4,5,6) ~ 1, 
     T ~ NA
   )
   )
    
  if(levels){
    new.dat = new.dat |> rowwise() |>  
      mutate(
      # TODO Should there be this many NAs? 
      num_levels = case_when(
        flg_arthroplasty == 1 ~ val_athrolevels, 
        flg_arthroplasty %in% c(0,NA) & (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) ~ max(e_arthrodesis_segments_cerv,e_arthrodesis_segments_lumb,na.rm = T),
        flg_arthroplasty %in% c(0, NA) & (notone(flg_arthrodesis_cerv) & notone(flg_arthrodesis_lumb)) & (fl_laminectomyetc == 1 | flg_discectomy == 1) & (notone(flg_instrumentationplaced_cerv) & notone(flg_instrumentationplaced_lumb)) ~ max(e_laminectomy_levels,discectomy_levels_e, na.rm = T),
        flg_arthroplasty %in% c(0, NA) & (notone(flg_arthrodesis_cerv) & notone(flg_arthrodesis_lumb))&(flg_instrumentationplaced_cerv==1 | flg_instrumentationplaced_lumb==1)  ~ max(e_laminectomy_levels, e_arthrodesis_segments_lumb,e_arthrodesis_segments_cerv,discectomy_levels_e, na.rm = T)
       )#,
      # num_levels_lum = case_when(
      #   flg_arthroplasty == 1 & val_athrolevels > 1 ~ val_athrolevels, 
      #   flg_arthroplasty %in% c(0,NA) & (flg_arthrodesis_lumb == 1) & e_arthrodesis_segments_lumb > 1 ~ e_arthrodesis_segments_lumb,
      #   flg_arthroplasty %in% c(0, NA) & (flg_arthrodesis_lumb %in% c(0,NA)) & (fl_laminectomyetc == 1 | flg_discectomy == 1) & flg_instrumentationplaced_lumb == 0 ~ max(e_laminectomy_levels,discectomy_levels_e,na.rm = T),
      #   flg_arthroplasty %in% c(0, NA) & (flg_arthrodesis_lumb %in% c(0,NA)) & flg_instrumentationplaced_lumb==1  ~ max(e_laminectomy_levels, e_arthrodesis_segments_lumb,discectomy_levels_e,na.rm = T)
      # )
      ) |> 
      mutate(num_levels = ifelse(is.infinite(num_levels), NA,num_levels)) #|> 
      #mutate(num_levels = factor(num_levels, levels = c("1","2","3", "4+")))
  }
  return(new.dat)
}



# Additional Functions ############### 












remove.died.in.hospital = function(dat){
  dat |> filter(e_discharge_place != 6)
}
# 
value.label.flag.missing = function(varname) factor(
  varname, levels = c(1,0,NA), labels = c("Yes","No","Missing"), exclude =""
)

value.label.flag = function(varname) factor(
  varname, levels = c(0,1), labels = c("No", "Yes")
)

add.missing.as.factor = function(dat,varname){
  u = unique(dat[[varname]])
  dat[[varname]] = factor(dat[[varname]], levels = c(u,NA), labels = c(as.character(u), "Missing"), exclude = "")
  dat
}


add.new.label = function(dat, old.label, new.label){
  label(dat[[old.label]]) = new.label
  dat
}
