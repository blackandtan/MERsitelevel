#  Purpose: Import site level MER data for all OUs
#  R citation
#  citation(package = "base", lib.loc = NULL, auto = NULL)
#
#  Author: B. Anderson, bha6@cdc.gov
#
#  Revisions
#  Initial draft ... 2019.10.09

### WORKSPACE -----------------------------------------------------------------
  
  library(grid)
  library(magrittr)
  
  library(lubridate)
  library(stringr)
  library(forcats)
  library(scales)
  library(tidyverse)
  library(purrr)

  dir.create("./output")


### IMPORT DATA -----------------------------------------------------------------

  
  ##### change directories and filenames in this block as needed !!!  ---------------------------------------
  
  # file version
  dsversion <- "MER_Structured_Datasets_SITE_IM_FY17-20_20191115_v1_1_" # FY19 Q4 pre-clean
  
  # download all site level zip files from Panorama into this location:
  datapath_originals <- "//cdc.gov/locker/CGH_EHSRB/MERdata/FY19Q4_pre-clean/originaldata/sitelevel/"  # FY19Q4 pre-cleaning
  datapathout <- "//cdc.gov/locker/CGH_EHSRB/MERdata/FY19Q4_pre-clean/"

  
  ##### ---------------------------------------------------------------------------------------
  
  
  
  # read directories
  thefileinfo <- data.frame(thefilepath = list.files(path = datapath_originals, pattern = ".zip",
                                                     full.names = T),stringsAsFactors = FALSE)
  filenest <- thefileinfo %>%
    mutate(thefilename = basename(file.path(thefilepath)),
           theOU = str_replace(basename(file.path(thefilepath)), dsversion, ""),
           theOU = str_replace(theOU, ".zip", ""),
           OU = theOU) %>% 
    nest(theOU, thefilepath)
  
  
  ### functions ------
  read_the_SLD <- function(filepathx){
    
    theSLD <- read_delim(file = filepathx,
                         delim = "\t",
                         col_names= T,
                         col_types = cols(
                           orgunituid = col_character(),
                           sitename = col_character(),
                           region = col_character(),
                           regionuid = col_character(),
                           operatingunit = col_character(),
                           operatingunituid = col_character(),
                           countryname = col_character(),
                           snu1 = col_character(),
                           snu1uid = col_character(),
                           psnu = col_character(),
                           psnuuid = col_character(),
                           snuprioritization = col_double(),
                           typemilitary = col_character(),
                           dreams = col_character(),
                           primepartner = col_character(),
                           fundingagency = col_character(),
                           mech_code = col_double(),
                           mech_name = col_character(),
                           pre_rgnlztn_hq_mech_code = col_double(),
                           prime_partner_duns = col_character(),
                           award_number = col_character(),
                           communityuid = col_character(),
                           community = col_character(),
                           communityprioritization = col_character(),
                           facilityuid = col_character(),
                           facility = col_character(),
                           facilityprioritization = col_character(),
                           sitetype = col_character(),
                           indicator = col_character(),
                           numeratordenom = col_character(),
                           indicatortype = col_character(),
                           disaggregate = col_character(),
                           standardizeddisaggregate = col_character(),
                           categoryoptioncomboname = col_character(),
                           ageasentered = col_character(),
                           trendsfine = col_character(),
                           trendssemifine = col_character(),
                           trendscoarse = col_character(),
                           sex = col_character(),
                           statushiv = col_character(),
                           statustb = col_logical(),
                           statuscx = col_logical(),
                           hiv_treatment_status = col_logical(),
                           population = col_logical(),
                           otherdisaggregate = col_character(),
                           coarsedisaggregate = col_character(),
                           modality = col_character(),
                           fiscal_year = col_double(),
                           targets = col_double(),
                           qtr1 = col_double(),
                           qtr2 = col_double(),
                           qtr3 = col_double(),
                           qtr4 = col_double(),
                           cumulative = col_double()
                         ),
                         trim_ws = T,
                         progress = show_progress())
    # spec(theSLD)
    # problems(theSLD)
    
    # for Q4, names are all lower case ... unlike Q3
    # map over to old names for code, then change to lower case at the end for use ....
    MERFY19Q4names <- colnames(theSLD)

    Q3names <-   c(
      "orgUnitUID" ,
      "SiteName" ,
      "Region" ,
      "RegionUID" ,
      "OperatingUnit" ,
      "OperatingUnitUID" ,
      "CountryName" ,
      "SNU1" ,
      "SNU1Uid" ,
      "PSNU" ,
      "PSNUuid" ,
      "SNUPrioritization" ,
      "typeMilitary" ,
      "DREAMS" ,
      "PrimePartner" ,
      "FundingAgency" ,
      "mech_code" ,
      "mech_name" ,
      "pre_rgnlztn_hq_mech_code",
      "prime_partner_duns" ,
      "award_number" ,
      "CommunityUID" ,
      "Community" ,
      "CommunityPrioritization" ,
      "FacilityUID" ,
      "Facility" ,
      "FacilityPrioritization" ,
      "SiteType" ,
      "indicator" ,
      "numeratorDenom" ,
      "indicatorType" ,
      "disaggregate" ,
      "standardizedDisaggregate" ,
      "categoryOptionComboName" ,
      "AgeAsEntered" ,
      "TrendsFine" ,
      "TrendsSemiFine" ,
      "TrendsCoarse" ,
      "Sex" ,
      "StatusHIV" ,
      "StatusTB" ,
      "StatusCX" ,
      "hiv_treatment_status" ,
      "population" ,
      "otherDisaggregate" ,
      "coarseDisaggregate" ,
      "modality" ,
      "Fiscal_Year" ,
      "TARGETS" ,
      "Qtr1" ,
      "Qtr2" ,
      "Qtr3" ,
      "Qtr4" ,
      "Cumulative"
    )   
    colnames(theSLD) <- Q3names
    
    return(theSLD)
  }
  
  
  calculate_TotalNum <- function(dfx){

    dfx_TotalNum <- dfx %>%
      filter(standardizedDisaggregate == "Total Numerator") %>%
      group_by(Region, RegionUID, OperatingUnit, OperatingUnitUID, CountryName,
               SNU1, SNU1Uid, PSNU, PSNUuid,
               FundingAgency, PrimePartner, prime_partner_duns, mech_code, mech_name,
               orgUnitUID, SiteName,
               CommunityUID, Community, FacilityUID, Facility, SiteType,
               Fiscal_Year, indicator, standardizedDisaggregate) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                # standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
                
                SNUPrioritization = as.character(paste(unique(  SNUPrioritization ), collapse=",")),    # not needed
                typeMilitary = as.character(paste(unique(  typeMilitary ), collapse=",")),    # not needed
                DREAMS = as.character(paste(unique(  DREAMS ), collapse=",")),    # not needed
                pre_rgnlztn_hq_mech_code = as.character(paste(unique(  pre_rgnlztn_hq_mech_code ), collapse=",")),    # not needed
                award_number = as.character(paste(unique(  award_number ), collapse=",")),    # not needed
                CommunityPrioritization = as.character(paste(unique(  CommunityPrioritization ), collapse=",")),    # not needed
                FacilityPrioritization = as.character(paste(unique(  FacilityPrioritization ), collapse=",")),    # not needed
                disaggregate = as.character(paste(unique(  disaggregate ), collapse=",")),    # not needed
                AgeAsEntered = as.character(paste(unique(  AgeAsEntered ), collapse=",")),    # not needed
                TrendsFine = as.character(paste(unique(  TrendsFine ), collapse=",")),    # not needed
                TrendsSemiFine = as.character(paste(unique(  TrendsSemiFine ), collapse=",")),    # not needed
                TrendsCoarse = as.character(paste(unique(  TrendsCoarse ), collapse=",")),    # not needed
                StatusHIV = as.character(paste(unique(  StatusHIV ), collapse=",")),    # not needed
                StatusTB = as.character(paste(unique(  StatusTB ), collapse=",")),    # not needed
                StatusCX = as.character(paste(unique(  StatusCX ), collapse=",")),    # not needed
                hiv_treatment_status = as.character(paste(unique(  hiv_treatment_status ), collapse=",")),    # not needed
                population = as.character(paste(unique(  population ), collapse=",")),    # not needed
                otherDisaggregate = as.character(paste(unique(  otherDisaggregate ), collapse=",")),    # not needed
                coarseDisaggregate = as.character(paste(unique(  coarseDisaggregate ), collapse=",")),    # not needed
                
                
                TARGETS = sum(TARGETS, na.rm = T),
                Qtr1 = sum(Qtr1, na.rm = T),
                Qtr2 = sum(Qtr2, na.rm = T),
                Qtr3 = sum(Qtr3, na.rm = T),
                Qtr4 = sum(Qtr4, na.rm = T),
                Cumulative = sum(Cumulative, na.rm = T)) %>%
      ungroup()

    return(dfx_TotalNum)
  }

  calculate_TotalDenom <- function(dfx){

    dfx_TotalDenom <- dfx %>%
      filter(indicator == "TX_PVLS", standardizedDisaggregate == "Total Denominator") %>%
      group_by(Region, RegionUID, OperatingUnit, OperatingUnitUID, CountryName,
               SNU1, SNU1Uid, PSNU, PSNUuid,
               FundingAgency, PrimePartner, prime_partner_duns, mech_code, mech_name,
               orgUnitUID, SiteName,
               CommunityUID, Community, FacilityUID, Facility, SiteType,
               Fiscal_Year, indicator, standardizedDisaggregate) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                # standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
                
                SNUPrioritization = as.character(paste(unique(  SNUPrioritization ), collapse=",")),    # not needed
                typeMilitary = as.character(paste(unique(  typeMilitary ), collapse=",")),    # not needed
                DREAMS = as.character(paste(unique(  DREAMS ), collapse=",")),    # not needed
                pre_rgnlztn_hq_mech_code = as.character(paste(unique(  pre_rgnlztn_hq_mech_code ), collapse=",")),    # not needed
                award_number = as.character(paste(unique(  award_number ), collapse=",")),    # not needed
                CommunityPrioritization = as.character(paste(unique(  CommunityPrioritization ), collapse=",")),    # not needed
                FacilityPrioritization = as.character(paste(unique(  FacilityPrioritization ), collapse=",")),    # not needed
                disaggregate = as.character(paste(unique(  disaggregate ), collapse=",")),    # not needed
                AgeAsEntered = as.character(paste(unique(  AgeAsEntered ), collapse=",")),    # not needed
                TrendsFine = as.character(paste(unique(  TrendsFine ), collapse=",")),    # not needed
                TrendsSemiFine = as.character(paste(unique(  TrendsSemiFine ), collapse=",")),    # not needed
                TrendsCoarse = as.character(paste(unique(  TrendsCoarse ), collapse=",")),    # not needed
                StatusHIV = as.character(paste(unique(  StatusHIV ), collapse=",")),    # not needed
                StatusTB = as.character(paste(unique(  StatusTB ), collapse=",")),    # not needed
                StatusCX = as.character(paste(unique(  StatusCX ), collapse=",")),    # not needed
                hiv_treatment_status = as.character(paste(unique(  hiv_treatment_status ), collapse=",")),    # not needed
                population = as.character(paste(unique(  population ), collapse=",")),    # not needed
                otherDisaggregate = as.character(paste(unique(  otherDisaggregate ), collapse=",")),    # not needed
                coarseDisaggregate = as.character(paste(unique(  coarseDisaggregate ), collapse=",")),    # not needed
                
                TARGETS = sum(TARGETS, na.rm = T),
                Qtr1 = sum(Qtr1, na.rm = T),
                Qtr2 = sum(Qtr2, na.rm = T),
                Qtr3 = sum(Qtr3, na.rm = T),
                Qtr4 = sum(Qtr4, na.rm = T),
                Cumulative = sum(Cumulative, na.rm = T)) %>%
      ungroup()

    return(dfx_TotalDenom)
  }


  calculate_HRHCURR_disaggs <- function(dfx){

    dfx_HRHCURR_disaggs <- dfx %>%
      filter(indicator == "HRH_CURR", standardizedDisaggregate != "Total Numerator") %>%
      group_by(Region, RegionUID, OperatingUnit, OperatingUnitUID, CountryName,
               SNU1, SNU1Uid, PSNU, PSNUuid,
               FundingAgency, PrimePartner, prime_partner_duns, mech_code, mech_name,
               orgUnitUID, SiteName,
               CommunityUID, Community, FacilityUID, Facility, SiteType,
               Fiscal_Year, indicator, standardizedDisaggregate, categoryOptionComboName) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                # standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                # categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
                
                SNUPrioritization = as.character(paste(unique(  SNUPrioritization ), collapse=",")),    # not needed
                typeMilitary = as.character(paste(unique(  typeMilitary ), collapse=",")),    # not needed
                DREAMS = as.character(paste(unique(  DREAMS ), collapse=",")),    # not needed
                pre_rgnlztn_hq_mech_code = as.character(paste(unique(  pre_rgnlztn_hq_mech_code ), collapse=",")),    # not needed
                award_number = as.character(paste(unique(  award_number ), collapse=",")),    # not needed
                CommunityPrioritization = as.character(paste(unique(  CommunityPrioritization ), collapse=",")),    # not needed
                FacilityPrioritization = as.character(paste(unique(  FacilityPrioritization ), collapse=",")),    # not needed
                disaggregate = as.character(paste(unique(  disaggregate ), collapse=",")),    # not needed
                AgeAsEntered = as.character(paste(unique(  AgeAsEntered ), collapse=",")),    # not needed
                TrendsFine = as.character(paste(unique(  TrendsFine ), collapse=",")),    # not needed
                TrendsSemiFine = as.character(paste(unique(  TrendsSemiFine ), collapse=",")),    # not needed
                TrendsCoarse = as.character(paste(unique(  TrendsCoarse ), collapse=",")),    # not needed
                StatusHIV = as.character(paste(unique(  StatusHIV ), collapse=",")),    # not needed
                StatusTB = as.character(paste(unique(  StatusTB ), collapse=",")),    # not needed
                StatusCX = as.character(paste(unique(  StatusCX ), collapse=",")),    # not needed
                hiv_treatment_status = as.character(paste(unique(  hiv_treatment_status ), collapse=",")),    # not needed
                population = as.character(paste(unique(  population ), collapse=",")),    # not needed
                otherDisaggregate = as.character(paste(unique(  otherDisaggregate ), collapse=",")),    # not needed
                coarseDisaggregate = as.character(paste(unique(  coarseDisaggregate ), collapse=",")),    # not needed
                
                TARGETS = sum(TARGETS, na.rm = T),
                Qtr1 = sum(Qtr1, na.rm = T),
                Qtr2 = sum(Qtr2, na.rm = T),
                Qtr3 = sum(Qtr3, na.rm = T),
                Qtr4 = sum(Qtr4, na.rm = T),
                Cumulative = sum(Cumulative, na.rm = T)) %>%
      ungroup()
    return(dfx_HRHCURR_disaggs)
  }

  calculate_HRHSTAFF_disaggs <- function(dfx){

    dfx_HRHCURR_disaggs <- dfx %>%
      filter(indicator %in% c("HRH_STAFF", "HRH_STAFF_NAT"), standardizedDisaggregate != "Total Numerator") %>%
      group_by(Region, RegionUID, OperatingUnit, OperatingUnitUID, CountryName,
               SNU1, SNU1Uid, PSNU, PSNUuid,
               FundingAgency, PrimePartner, prime_partner_duns, mech_code, mech_name,
               orgUnitUID, SiteName,
               CommunityUID, Community, FacilityUID, Facility, SiteType,
               Fiscal_Year, indicator, standardizedDisaggregate, categoryOptionComboName) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                # standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                # categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
                
                SNUPrioritization = as.character(paste(unique(  SNUPrioritization ), collapse=",")),    # not needed
                typeMilitary = as.character(paste(unique(  typeMilitary ), collapse=",")),    # not needed
                DREAMS = as.character(paste(unique(  DREAMS ), collapse=",")),    # not needed
                pre_rgnlztn_hq_mech_code = as.character(paste(unique(  pre_rgnlztn_hq_mech_code ), collapse=",")),    # not needed
                award_number = as.character(paste(unique(  award_number ), collapse=",")),    # not needed
                CommunityPrioritization = as.character(paste(unique(  CommunityPrioritization ), collapse=",")),    # not needed
                FacilityPrioritization = as.character(paste(unique(  FacilityPrioritization ), collapse=",")),    # not needed
                disaggregate = as.character(paste(unique(  disaggregate ), collapse=",")),    # not needed
                AgeAsEntered = as.character(paste(unique(  AgeAsEntered ), collapse=",")),    # not needed
                TrendsFine = as.character(paste(unique(  TrendsFine ), collapse=",")),    # not needed
                TrendsSemiFine = as.character(paste(unique(  TrendsSemiFine ), collapse=",")),    # not needed
                TrendsCoarse = as.character(paste(unique(  TrendsCoarse ), collapse=",")),    # not needed
                StatusHIV = as.character(paste(unique(  StatusHIV ), collapse=",")),    # not needed
                StatusTB = as.character(paste(unique(  StatusTB ), collapse=",")),    # not needed
                StatusCX = as.character(paste(unique(  StatusCX ), collapse=",")),    # not needed
                hiv_treatment_status = as.character(paste(unique(  hiv_treatment_status ), collapse=",")),    # not needed
                population = as.character(paste(unique(  population ), collapse=",")),    # not needed
                otherDisaggregate = as.character(paste(unique(  otherDisaggregate ), collapse=",")),    # not needed
                coarseDisaggregate = as.character(paste(unique(  coarseDisaggregate ), collapse=",")),    # not needed
                
                TARGETS = sum(TARGETS, na.rm = T),
                Qtr1 = sum(Qtr1, na.rm = T),
                Qtr2 = sum(Qtr2, na.rm = T),
                Qtr3 = sum(Qtr3, na.rm = T),
                Qtr4 = sum(Qtr4, na.rm = T),
                Cumulative = sum(Cumulative, na.rm = T)) %>%
      ungroup()
    return(dfx_HRHCURR_disaggs)
  }
  
  importSiteLevelData <- function(fileinfo){

    OUx <- fileinfo %>%  pull(theOU)
    print(paste0("Importing site data for ", OUx))
    filepathx <- file.path(fileinfo$thefilepath)

    sldx <- read_the_SLD(filepathx)

    # write unfiltered file to csv - only use when isolating to one OU for eda!
    # these are huge files that take time to process and write
    # write_excel_csv(sldx, path = paste0(datapathout,"site_level_data_JB/", OUx, "/", Sys.Date(),"_", OUx, "_FY19Q4_siteleveldata_UNFILTERED.csv"))
    
    
    targetindicators <- c(
      "HTS_TST", 
      "HTS_TST_POS",
      "HRH_CURR",
      "HRH_CURR_SiteLevel",
      "HRH_CURR_AboveSite",
      "HRH_STAFF",
      "HRH_STAFF_NAT",
      "HRH_CURR_ClinicalCadre",
      "HRH_CURR_ManagementCadre" ,
      "HRH_CURR_ClinicalSupportCadre",
      "HRH_CURR_OtherCadre",
      "HRH_CURR_LayCadre",
      "HRH_CURR_Social_ServiceCadre" ,
      "HRH_CURR_LaboratoryCadre" ,
      "HRH_CURR_PharmacyCadre" ,
      "HRH_PRE",
      "TX_CURR",
      "TX_CURR_NAT",
      "TX_CURR_SUBNAT",
      "TX_ML",
      "TX_NEW",
      "TX_NET_NEW",
      "TX_PVLS",
      "TX_RET"
    )

    targetyears <- c("2019")
    
    # we just need totalnum for most variables ...
    # only a handful where disaggregate info needed
    # using this info can help slim down the data set
    
    sldx <- sldx %>%
      ungroup() %>% 
      filter(indicator %in% targetindicators, 
             Fiscal_Year %in% targetyears)  
      
    sldx_TotalNum <- calculate_TotalNum(sldx)
    sldx_TotalDenom <- calculate_TotalDenom(sldx)
    sldx_HRHCURR_disaggs <- calculate_HRHCURR_disaggs(sldx)
    sldx_HRHSTAFF_disaggs <- calculate_HRHSTAFF_disaggs(sldx)

    # combine the SLDs
    sldcombined <- bind_rows(
      sldx_TotalNum,
      sldx_TotalDenom,
      sldx_HRHCURR_disaggs,
      sldx_HRHSTAFF_disaggs
    ) 
    
    return(sldcombined)
  }

 
  
  
  
  
  ### import FY19 data sets and save as .rds files ---------
  sldnest <- filenest %>%
    # filter(OU %in% c("Tanzania", "Cambodia", "Asia Region", "West Africa Region", "Western Hemisphere Region")) %>%  # test with a few OUs first before running the whole code
    mutate(data = purrr::map(data, importSiteLevelData))
  
  glimpse(sldnest)
  sld <- sldnest %>% unnest()
  
  # convert back to lower case variable names to match Q4 MER
  sldQ3names <- colnames(sld)
  sldQ4names <- tolower(sldQ3names)
  colnames(sld) <- sldQ4names
  
  # order variables to match Q4 MER
  sld <- sld %>% 
    ungroup() %>% 
    select(
      thefilename, ou,
      orgunituid ,
      sitename ,
      region ,
      regionuid, 
      operatingunit, 
      operatingunituid, 
      countryname ,
      snu1 ,
      snu1uid, 
      psnu ,
      psnuuid ,
      snuprioritization ,
      typemilitary ,
      dreams ,
      primepartner, 
      fundingagency, 
      mech_code ,
      mech_name ,
      pre_rgnlztn_hq_mech_code, 
      prime_partner_duns ,
      award_number ,
      communityuid ,
      community ,
      communityprioritization, 
      facilityuid ,
      facility ,
      facilityprioritization, 
      sitetype ,
      indicator ,
      numeratordenom, 
      indicatortype ,
      disaggregate ,
      standardizeddisaggregate, 
      categoryoptioncomboname ,
      ageasentered ,
      trendsfine ,
      trendssemifine, 
      trendscoarse ,
      sex ,
      statushiv, 
      statustb ,
      statuscx ,
      hiv_treatment_status, 
      population ,
      otherdisaggregate, 
      coarsedisaggregate, 
      modality ,
      fiscal_year, 
      targets ,
      qtr1 ,
      qtr2 ,
      qtr3 ,
      qtr4 ,
      cumulative 
    )
  
  sldnest <- sld %>% 
    group_by(thefilename, ou) %>% 
    nest()
  
  sldnest$data[1]
  
  
  
  # save R file
  saveRDS(sldnest, file = paste0(datapathout, "Rdata/", "MER_FY19Q4_sitedata.rds"))
  

  
  
  # write csv file data 
  export4hshrt <- function(dfx){
    
    OUx <- dfx %>% distinct(operatingunit) %>%  pull(operatingunit)
    print(paste0("Exporting site data for ", OUx))

    write_excel_csv(dfx, path = paste0(datapathout,"site_level_data_JB/", OUx, "_FY19Q4_siteleveldata_", Sys.Date(), ".csv"))
    
    
  }

  # purrr::map(sldnest$data, export4hshrt)
  
 
  
  