#' Main ?package function to recode data
#'
#' Recodes a merged MSSIC dataset according to the standards set out by the MSSIC analytics team
#' All values not yet verified to match SAS and STATA.
#'
#' @param dat merged mssic dataset
#' @param levels logical value asking whether number of levels should be computed. Defaults to T as usually they are wanted, but sometimes takes a short while to run hence the option
#'
#' @export recode.mssic

recode.mssic = function(dat, levels = T, mcids = T){
  new.dat = dat |>
    mutate(
      calc_age = ifelse(Pat_Age < 18, NA, Pat_Age),
      male = case_when(
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
    priv_ins = case_when(
      primaryinsurance %in% c(1,3,4,7,10) ~ 1,
      primaryinsurance %in% c(2,5,6,8,9,11,12) ~ 0,
      T ~ NA),
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
    e_cmb_symptom_duration = ifelse(e_cmb_symptom_duration == 4, NA, e_cmb_symptom_duration),
    sx_duration1 = ifelse(e_cmb_symptom_duration == 2, 1,0),
    sx_duration2 = ifelse(e_cmb_symptom_duration == 3, 1,0),
    ind_amb_preop = case_when(
      e_cmb_ambulation %in% c(1,4) ~ 1,
      e_cmb_ambulation %in% c(2,3) ~ 0,
      T ~ NA),
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
    anterior_posterior = case_when(surgical_approach_cerv == 3 ~ 1,
                                   T ~ 0),
    anterior = case_when(surgical_approach_cerv == 1 ~ 1,
                         T ~ 0) ,
    posterior = case_when(surgical_approach_cerv == 2 ~  1,
                          T ~ 0),
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
    flg_cerv_path_pseudoarthrosis = ifelse(grepl("8", e_cerv_dis_state), 1,0),
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
    flg_lum_path_pseudoarthrosis = ifelse(grepl("8", e_lum_dis_state), 1,0),
    flg_lum_path_synovial_cyst = ifelse(grepl("9", e_lum_dis_state), 1,0),

    flg_lum_clin_axial_pain = ifelse(grepl("1", e_lum_clinpresentation),1,0),
    flg_lum_clin_radicular_pain = ifelse(grepl("2", e_lum_clinpresentation),1,0),
    flg_lum_clin_bowel_dysfunction = ifelse(grepl("4", e_lum_clinpresentation),1,0),
    flg_lum_clin_muscle_weakness = ifelse(grepl("5", e_lum_clinpresentation),1,0),
    flg_lum_clin_footdrop = ifelse(grepl("6", e_lum_clinpresentation),1,0),
    flg_lum_balance_gait = ifelse(grepl("9", e_lum_clinpresentation),1,0),


    race = bl_race,
    race = ifelse(race == 6 | race == 10 ,
                  case_when(
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
                  race),
    race1 = factor(case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race %in% c(3:5, 7:9) ~ "Other",
      T ~ NA
    ), levels = c("White", "Black", "Other")),
    race_miss = ifelse(bl_race %in% c(NA,10) & race_e %in% c(NA,8),1,0),
    black = case_when((bl_race == 2 | (bl_race %in% c(NA,10) & race_e == 3)) ~ 1,
                      T ~ 0),
    other_race = case_when(bl_race %in% c(3:5,7:9) | (bl_race %in% c(NA,10) & race_e %in% c(1,2,4:6)) ~ 1,
                           T ~ 0),
    smoking_status = factor(case_when(
      bl_smoker %in% c(1,2) | (is.na(bl_smoker) & tobacco_use_e == 1) ~ 1,
      bl_smoker ==3 | (is.na(bl_smoker) & tobacco_use_e == 2) ~ 2,
      bl_smoker ==4 | (is.na(bl_smoker) & tobacco_use_e == 3) ~ 3,
      T ~ NA
    ), levels = c(1,2,3,NA), labels = c("Current", "Former", "Never")),
    smoke_miss = ifelse(is.na(bl_smoker) & tobacco_use_e %in% c(NA,4), 1,0),
    current_smoker = case_when(
      smoking_status == "Current" ~ 1,
      smoking_status == "Former" | smoking_status == "Never" ~ 0,
      T ~ NA
    ),
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
    phq2_depression_baseline = case_when(bl_phq2score %in% c(3:6) ~ 1,
                                         bl_phq2score %in% c(0,1,2) ~ 0,
                                         T ~ NA),
    educ_miss = ifelse(is.na(educ_lths),1,0),
    opiod_miss = ifelse(is.na(preop_opiuse),1,0),
    phq2_miss = ifelse(is.na(phq2_depression_baseline),1,0),
    motor_deficit = case_when(
      e_cmb_motor_deficit %in% c(1,2) ~ 1,
      e_cmb_motor_deficit == 0 ~ 0,
      T ~ NA),
    motor_miss = case_when(
      is.na(e_cmb_motor_deficit) ~ NA,
      is.na(motor_deficit) ~ 1,
      T ~ 0
    ),
    #sx_duration1 = ifelse(duration_miss == 1, 0,sx_duration1),
    #sx_duration2 = ifelse(duration_miss == 1, 0,sx_duration2),
    #across(c("educ_lths", "educ_college2", "educ_degree", "preop_opiuse"), ~replace_na(.x,0)),
    outpatient = ifelse(encounter_type_e == 2, 1,0),
    proctype = case_when(
      flg_arthroplasty == 1 ~ 0,
      (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) & (flg_instrumentationplaced_cerv == 1 | flg_instrumentationplaced_lumb ==1) ~ 1,
      (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) & (flg_instrumentationplaced_cerv == 0 | flg_instrumentationplaced_lumb ==0) ~ 2,
      (is.not(flg_arthrodesis_cerv) & is.not(flg_arthrodesis_lumb)) & is.not(flg_arthroplasty) & (fl_laminectomyetc ==1 | flg_discectomy == 1) & (is.not(flg_instrumentationplaced_cerv) & is.not(flg_instrumentationplaced_lumb)) ~ 3,
      #flg_arthrodesis_cerv != 1 & flg_arthrodesis_lumb != 1) & flg_arthroplasty !=1 & (fl_laminectomyetc ==1 | flg_discectomy == 1) & (flg_instrumentationplaced_cerv !=1 & flg_instrumentationplaced_lumb !=1) ~ 3,
      T ~ 4
    ),
    fusion = ifelse(proctype %in% c(1,2), 1,0),
    discharge_home = case_when(
      e_discharge_place %in% c(1,2) ~ 1 ,
      e_discharge_place %in% c(4,5,8,9,10) ~0,
      T ~ NA
    ),
    surgery_length_hour = val_surgery_length/60,
    flg_readmit = ifelse(
      (!is.na(e_readmit1) & e_readmit1==1 & is.not(e_readmit1_multi_stage) & is.not(e_readmit1_complications, 29)) |
        (!is.na(e_readmit2) & e_readmit2==1 & is.not(e_readmit2_multi_stage) & is.not(e_readmit2_complications,29))  |
        (!is.na(e_readmit3) & e_readmit3==1 & is.not(e_readmit3_multi_stage) & is.not(e_readmit3_complications,29)),
      1,
      0
    ),
    flg_readmit_reop = ifelse((!is.na(returntoor1) & returntoor1==1 & !(unplanned_spine_90d_e %in% c(3,4,5))) |
                                (!is.na(returntoor2) & returntoor2==1 & !(unplanned_spine2_90d_e %in% c(3,4,5))) |
                                (!is.na(returntoor3) & returntoor3==1 & !(unplanned_spine3_90d_e %in% c(3,4,5))),
                              1,
                              0),
    employed_baseline = case_when(
              bl_employment %in% c(5,6) ~ 1,
              bl_employment %in% c(0,2,3,4) ~ 0,
              T ~ NA),
    #CANT FIND E_EMPLOYMENT
    bl_promis_pf1 = as.numeric(bl_promis_pf),
    flg_urinary_retention = e_lt_utireqcatheter,
    flg_ssi = e_lt_ssi,
    flg_any_mortality = e_lt_pt_died,
    flg_axial_pain = e_lt_axial_pain,
    flg_claudication = e_lt_claudication,
    flg_dvt = e_lt_deepveinthrombosis,
    flg_ed_visit_obs_stay = visit_obs_b,
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
      flg_urinary_retention == 1 |
      flg_wound_dehiscense == 1 |
      flg_csfleak == 1 |
      flg_myelopathy == 1 |
      flg_dysphagia == 1),
      1,
      0),
  any_complication = ifelse(is.na(any_complication),0, any_complication),



 rtw_90day = case_when(
   !is.na(D90_returntowork) & D90_returntowork %in% c(1,4) & bl_pln_return2work == 1 ~ 1,
   !is.na(D90_returntowork) & !(D90_returntowork %in% c(1,4)) & bl_pln_return2work == 1 ~ 0,
   T ~ NA
 ),

 rtw_1yr = case_when(
   !is.na(Y1_returntowork) & Y1_returntowork %in% c(1,4) & bl_pln_return2work == 1 ~ 1,
   !is.na(Y1_returntowork) & !(Y1_returntowork %in% c(1,4)) & bl_pln_return2work == 1 ~ 0,
   T ~ NA
 ),

 rtw_2yr = case_when(
   !is.na(Y2_returntowork) & Y2_returntowork %in% c(1,4) & bl_pln_return2work == 1 ~ 1,
   !is.na(Y2_returntowork) & !(Y2_returntowork %in% c(1,4)) & bl_pln_return2work == 1 ~ 0,
   T ~ NA
 ),

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
   phq2_depression_90d = case_when(
     D90_phq2score %in% c(0,1,2) ~ 0,
     D90_phq2score %in% c(3,4,5,6) ~ 1,
     T ~ NA
   ),
   phq2_depression_1y = case_when(
     Y1_phq2score %in% c(0,1,2) ~ 0,
     Y1_phq2score %in% c(3,4,5,6) ~ 1,
     T ~ NA
   ),
   phq2_depression_2y = case_when(
     Y2_phq2score %in% c(0,1,2) ~ 0,
     Y2_phq2score %in% c(3,4,5,6) ~ 1,
     T ~ NA
   )
   )

  new.dat = new.dat |> rowwise() |> mutate(
    n_fused = max(c(e_arthrodesis_segments_cerv, e_arthrodesis_segments_lumb), na.rm = T) + 1,
    n_fused = ifelse(is.infinite(n_fused), NA,n_fused),
    n_instrumented = case_when(
      (!is.na(flg_instrumentationplaced_cerv) &  flg_instrumentationplaced_cerv == 1) | (!is.na(flg_instrumentationplaced_lumb) &  flg_instrumentationplaced_lumb == 1)  ~ n_fused,
      T ~ 0
    ),
    surg_invasiveness = sum(c(n_fused,n_instrumented, e_laminectomy_levels, discectomy_levels_e), na.rm = T),
    surg_invasiveness = ifelse(all(sapply(c(n_fused,n_instrumented, e_laminectomy_levels, discectomy_levels_e), is.na)), NA, surg_invasiveness)
  ) |> ungroup()

  if(levels){
    new.dat = new.dat |> rowwise() |>
      mutate(
      # TODO Should there be this many NAs?
      num_levels = case_when(
        flg_arthroplasty == 1 ~ val_athrolevels,
        flg_arthroplasty %in% c(0,NA) & (flg_arthrodesis_cerv == 1 | flg_arthrodesis_lumb == 1) ~ max(e_arthrodesis_segments_cerv,e_arthrodesis_segments_lumb,na.rm = T),
        flg_arthroplasty %in% c(0, NA) & (is.not(flg_arthrodesis_cerv) & is.not(flg_arthrodesis_lumb)) & (fl_laminectomyetc == 1 | flg_discectomy == 1) & (is.not(flg_instrumentationplaced_cerv) & is.not(flg_instrumentationplaced_lumb)) ~ max(e_laminectomy_levels,discectomy_levels_e, na.rm = T),
        flg_arthroplasty %in% c(0, NA) & (is.not(flg_arthrodesis_cerv) & is.not(flg_arthrodesis_lumb))&(flg_instrumentationplaced_cerv==1 | flg_instrumentationplaced_lumb==1)  ~ max(e_laminectomy_levels, e_arthrodesis_segments_lumb,e_arthrodesis_segments_cerv,discectomy_levels_e, na.rm = T)
       )#,
      # num_levels_lum = case_when(
      #   flg_arthroplasty == 1 & val_athrolevels > 1 ~ val_athrolevels,
      #   flg_arthroplasty %in% c(0,NA) & (flg_arthrodesis_lumb == 1) & e_arthrodesis_segments_lumb > 1 ~ e_arthrodesis_segments_lumb,
      #   flg_arthroplasty %in% c(0, NA) & (flg_arthrodesis_lumb %in% c(0,NA)) & (fl_laminectomyetc == 1 | flg_discectomy == 1) & flg_instrumentationplaced_lumb == 0 ~ max(e_laminectomy_levels,discectomy_levels_e,na.rm = T),
      #   flg_arthroplasty %in% c(0, NA) & (flg_arthrodesis_lumb %in% c(0,NA)) & flg_instrumentationplaced_lumb==1  ~ max(e_laminectomy_levels, e_arthrodesis_segments_lumb,discectomy_levels_e,na.rm = T)
      # )
      ) |>
      mutate(num_levels = ifelse(is.infinite(num_levels), NA,num_levels))
    }#|>
    if(mcids){
      new.dat = new.dat |>
        mutate(
          mcid_back_pain_90 = case_when(
            bl_back_pain >= 2 & !is.na(D90_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - D90_back_pain) >= 2 ~ 1,
            bl_back_pain >= 2 & !is.na(D90_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - D90_back_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_back_pain_1y = case_when(
            bl_back_pain >= 2 & !is.na(Y1_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - Y1_back_pain) >= 2 ~ 1,
            bl_back_pain >= 2 & !is.na(Y1_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - Y1_back_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_back_pain_2y = case_when(
            bl_back_pain >= 2 & !is.na(Y2_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - Y2_back_pain) >= 2 ~ 1,
            bl_back_pain >= 2 & !is.na(Y2_back_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_back_pain - Y2_back_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_leg_pain_90 = case_when(
            bl_leg_pain >= 2 & !is.na(D90_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - D90_leg_pain) >= 2 ~ 1,
            bl_leg_pain >= 2 & !is.na(D90_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - D90_leg_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_leg_pain_1y = case_when(
            bl_leg_pain >= 2 & !is.na(Y1_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - Y1_leg_pain) >= 2 ~ 1,
            bl_leg_pain >= 2 & !is.na(Y1_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - Y1_leg_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_leg_pain_2y = case_when(
            bl_leg_pain >= 2 & !is.na(Y2_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - Y2_leg_pain) >= 2 ~ 1,
            bl_leg_pain >= 2 & !is.na(Y2_leg_pain) & Diagnosis_location_e.ab == "Lumbar" & (bl_leg_pain - Y2_leg_pain) < 2 ~ 0,
            T ~ NA
          ),
          mcid_neck_pain_90 = case_when(
            bl_neck_pain >= 3 & !is.na(D90_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - D90_neck_pain) >= 3 ~ 1,
            bl_neck_pain >= 3 & !is.na(D90_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - D90_neck_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_neck_pain_1y = case_when(
            bl_neck_pain >= 3 & !is.na(Y1_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - Y1_neck_pain) >= 3 ~ 1,
            bl_neck_pain >= 3 & !is.na(Y1_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - Y1_neck_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_neck_pain_2y = case_when(
            bl_neck_pain >= 3 & !is.na(Y2_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - Y2_neck_pain) >= 3 ~ 1,
            bl_neck_pain >= 3 & !is.na(Y2_neck_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_neck_pain - Y2_neck_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_arm_pain_90 = case_when(
            bl_arm_pain >= 3 & !is.na(D90_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - D90_arm_pain) >= 3 ~ 1,
            bl_arm_pain >= 3 & !is.na(D90_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - D90_arm_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_arm_pain_1y = case_when(
            bl_arm_pain >= 3 & !is.na(Y1_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - Y1_arm_pain) >= 3 ~ 1,
            bl_arm_pain >= 3 & !is.na(Y1_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - Y1_arm_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_arm_pain_2y = case_when(
            bl_arm_pain >= 3 & !is.na(Y2_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - Y2_arm_pain) >= 3 ~ 1,
            bl_arm_pain >= 3 & !is.na(Y2_arm_pain) & Diagnosis_location_e.ab == "Cervical" & (bl_arm_pain - Y2_arm_pain) < 3 ~ 0,
            T ~ NA
          ),
          mcid_promis_pf_90 = case_when(
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(D90_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (D90_promis_pf - bl_promis_pf) >= 4.5) | (Diagnosis_location_e.ab == "Cervical" & (D90_promis_pf - bl_promis_pf) >= 3))  ~ 1,
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(D90_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (D90_promis_pf - bl_promis_pf) < 4.5) | (Diagnosis_location_e.ab == "Cervical" & (D90_promis_pf - bl_promis_pf) < 3)) ~ 0 ,
            T ~ NA
          ),
          mcid_promis_pf_1y = case_when(
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(Y1_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (Y1_promis_pf - bl_promis_pf) >= 4.5) | (Diagnosis_location_e.ab == "Cervical" & (Y1_promis_pf - bl_promis_pf) >= 3))  ~ 1,
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(Y1_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (Y1_promis_pf - bl_promis_pf) < 4.5) | (Diagnosis_location_e.ab == "Cervical" & (Y1_promis_pf - bl_promis_pf) < 3)) ~ 0 ,
            T ~ NA
          ),
          mcid_promis_pf_2y = case_when(
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(Y2_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (Y2_promis_pf - bl_promis_pf) >= 4.5) | (Diagnosis_location_e.ab == "Cervical" & (Y2_promis_pf - bl_promis_pf) >= 3))  ~ 1,
            bl_promis_pf < 56.9 & !is.na(bl_promis_pf) & !is.na(Y2_promis_pf) & ((Diagnosis_location_e.ab == "Lumbar" & (Y2_promis_pf - bl_promis_pf) < 4.5) | (Diagnosis_location_e.ab == "Cervical" & (Y2_promis_pf - bl_promis_pf) < 3)) ~ 0 ,
            T ~ NA
          )
        )

    }
      #mutate(num_levels = factor(num_levels, levels = c("1","2","3", "4+")))
  return(new.dat)
}








