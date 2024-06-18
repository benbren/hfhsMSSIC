#' Label recoded MSSIC data for documentation and visualization
#' @param dat recoded MSSIC dataset
#' @param missing.as.factor defaults to T, whether to add missing as a factor or leave it as NA
#'
#' @export



add.labels.after.recode = function(dat, missing.as.factor = T){

  require("expss")
  if(!require("expss")){stop("Please install expss")}

  bincols = apply(dat,2, function(x){all(x %in% c(0,1))})
  dat[bincols] = lapply(dat[bincols], value.label.flag)

  if(missing.as.factor){
    bincols.m = apply(dat,2, function(x){all(x %in% c(0,1,NA)) & !all(x %in% c(0,1))})
    dat[bincols.m] = lapply(dat[bincols.m], value.label.flag.missing)
  }

  dat = expss::apply_labels(dat,
                            e_lt_ileus = "Ileus",
                            e_lt_utireqcatheter = "Urinary Retention",
                            discharge_not_home = "Non-home discharge",
                            race1 = "Race",
                            fusion = "Fusion",
                            diabetes = "Diabetes",
                            scoliosis = "Scoliosis",
                            smoking_status = "Smoking Status",
                            flg_chf = "CHF",
                            flg_copd = "COPD",
                            flg_hypertension = "Hypertension",
                            flg_osteo = "Osteoporosis",
                            ind_amb_preop = "Independendent Ambulation PreOp",
                            asa_gt2 = "ASA > 2",
                            outpatient = "Outpatient",
                            prior_physical_therapy = "Prior Physical Therapy",
                            prior_injection = "Prior Injection",
                            prior_chiropractor = "Prior Chiropractor",
                            priv_ins = "Private Insurance",
                            e_lt_deepveinthrombosis = "DVT",
                            male = "Male",
                            num_levels = "Number of Levels",
                            Pat_Age = "Age",
                            surgery_length_hour = "Surgery Length (Hours)",
                            bmi = "BMI"

  )
  dat

}
