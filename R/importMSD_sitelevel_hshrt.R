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
  
  library(RColorBrewer)
  library(plotly)
  
  library(DT)  
  library(readxl)
  
  library(ggridges)
  library(hrbrthemes)

  library(lubridate)
  library(stringr)
  library(forcats)
  library(scales)
  library(tidyverse)
  library(purrr)
  library(purrrlyr)
  
  dir.create("./output")


### IMPORT DATA -----------------------------------------------------------------

  # Important note
  # before running this script, you need to do 3 things manually
  #  1. download all of the site level MER data zip files from Panorama and put them in the file folder indicated below
  #  2. change the directory and filename references in the code block below to match the quarterly data of interest
  #  3. activate the unzip code (lines 49-51) to unzip the files, then comment it out again - only need to run it once
  
  ##### change directories and filenames in this block as needed !!!  ---------------------------------------
  
  # FY2019 Q3 post-clean v2_1
  dsversion <- "MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_"
  
  # download all site level zip files from Panorama into this location:
  datapath_originals <- "//cdc.gov/locker/CGH_EHSRB/MERdata/FY19Q3_post-clean/originaldata/sitelevel/"  # FY19Q3 post-cleaning
  zip_path <- "\\\\cdc.gov\\locker\\CGH_EHSRB\\MERdata\\FY19Q3_post-clean\\originaldata\\sitelevel\\"
  
  # unzip files using this code -- only need to run once and then comment it out
      # thezipfiles <- data.frame(thefilepath = list.files(path = datapath1, pattern = ".zip", full.names = T),
      #                           stringsAsFactors = FALSE)
      # unzip(zip_path, exdir = zip_path)
  
  
  datapathout <- "//cdc.gov/locker/CGH_EHSRB/MERdata/FY19Q3_post-clean/"

  
  ##### ---------------------------------------------------------------------------------------
  
  
  
  # read directories
  thefileinfo <- data.frame(thefilepath = list.files(path = datapath_originals, pattern = ".txt",
                                                     full.names = T),stringsAsFactors = FALSE)
  filenest <- thefileinfo %>%
    mutate(thefilename = basename(file.path(thefilepath)),
           theOU = str_replace(basename(file.path(thefilepath)), dsversion, ""),
           theOU = str_replace(theOU, ".txt", ""),
           OU = theOU) %>% 
    nest(theOU, thefilepath)
  
  
  ### functions ------
  read_the_SLD <- function(filepathx){
    
    theSLD <- read_delim(file = filepathx,
               delim = "\t",
               col_names=T,
               col_types = cols(
                 orgUnitUID = col_character(),
                 SiteName = col_character(),
                 Region = col_character(),
                 RegionUID = col_character(),
                 OperatingUnit = col_character(),
                 OperatingUnitUID = col_character(),
                 CountryName = col_character(),
                 SNU1 = col_character(),
                 SNU1Uid = col_character(),
                 PSNU = col_character(),
                 PSNUuid = col_character(),
                 SNUPrioritization = col_character(),
                 typeMilitary = col_character(),
                 DREAMS = col_character(),
                 PrimePartner = col_character(),
                 FundingAgency = col_character(),
                 mech_code = col_character(),
                 mech_name = col_character(),
                 pre_rgnlztn_hq_mech_code = col_character(),
                 prime_partner_duns = col_character(),
                 award_number = col_character(),
                 CommunityUID = col_character(),
                 Community = col_character(),
                 CommunityPrioritization = col_character(),
                 FacilityUID = col_character(),
                 Facility = col_character(),
                 FacilityPrioritization = col_character(),
                 SiteType = col_character(),
                 indicator = col_character(),
                 numeratorDenom = col_character(),
                 indicatorType = col_character(),
                 disaggregate = col_character(),
                 standardizedDisaggregate = col_character(),
                 categoryOptionComboName = col_character(),
                 AgeAsEntered = col_character(),
                 TrendsFine = col_character(),
                 TrendsSemiFine = col_character(),
                 TrendsCoarse = col_character(),
                 Sex = col_character(),
                 StatusHIV = col_character(),
                 StatusTB = col_logical(),
                 StatusCX = col_character(),
                 hiv_treatment_status = col_logical(),
                 population = col_character(),
                 otherDisaggregate = col_character(),
                 coarseDisaggregate = col_character(),
                 modality = col_character(),
                 Fiscal_Year = col_double(),
                 TARGETS = col_double(),
                 Qtr1 = col_double(),
                 Qtr2 = col_double(),
                 Qtr3 = col_double(),
                 Qtr4 = col_double(),
                 Cumulative = col_double()
               ),
               trim_ws = T,
               progress = show_progress())
    # spec(theSLD)
    # problems(theSLD)
    return(theSLD)
  }
  
  inventory_SLD <- function(dfx){
    
    OUx <- dfx %>%  pull(theOU)
    print(paste0("Inventory site data for ", OUx))
    filepathx <- file.path(dfx$thefilepath)
    
    fullSLD <- read_the_SLD(filepathx)
    summSLD <- fullSLD %>% 
      summarize(nrecords = length(orgUnitUID),
                nSNU = length(unique(SNU1Uid)),
                nPSNU = length(unique(PSNUuid)),
                nFundingAgencies = length(unique(FundingAgency)),
                nIP = length(unique(PrimePartner)),
                nIM = length(unique(mech_code)),
                nSites = length(unique(orgUnitUID)),
                nCommunities = length(unique(CommunityUID)),
                nFacilities = length(unique(FacilityUID)),
                SNUlist = list(unique(SNU1Uid)),
                PSNUlist = list(unique(PSNUuid)),
                FundingAgencylist = list(unique(FundingAgency)),
                IPlist = list(unique(PrimePartner)),
                IMlist = list(unique(mech_code)),
                Sitelist = list(unique(orgUnitUID)),
                Commlist = list(unique(CommunityUID)),
                Faclist = list(unique(FacilityUID))) 
    return(summSLD)
    
  }
  
  calculate_TotalNum <- function(dfx){

    dfx_TotalNum <- dfx %>%
      filter(standardizedDisaggregate == "Total Numerator") %>%
      group_by(Region, RegionUID, OperatingUnit, OperatingUnitUID, CountryName,
               SNU1, SNU1Uid, PSNU, PSNUuid,
               FundingAgency, PrimePartner, prime_partner_duns, mech_code, mech_name,
               orgUnitUID, SiteName,
               CommunityUID, Community, FacilityUID, Facility, SiteType,
               Fiscal_Year, indicator) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
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
               Fiscal_Year, indicator) %>%
      summarize(numeratorDenom = as.character(paste(unique(numeratorDenom), collapse=",")),
                indicatorType = as.character(paste(unique(indicatorType), collapse=",")),
                standardizedDisaggregate = as.character(paste(unique(standardizedDisaggregate), collapse=",")),
                categoryOptionComboName = as.character(paste(unique(categoryOptionComboName ), collapse=",")),
                Sex = as.character(paste(unique(Sex), collapse=",")),
                modality = as.character(paste(unique(modality), collapse=",")),
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
      # filter(standardizedDisaggregate %in% c("CadreCategory/FinancialSupport", "CadreOU/FinancialSupport")) %>%
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

    # use this sparingly - huge files that time to process and write!!
    # write_excel_csv(sldx, path = paste0(datapathout,"site_level_data_JB/", OUx, "/", Sys.Date(),"_", OUx, "_FY19Q3_siteleveldata_UNFILTERED.csv"))
    
    
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

    targetyears <- c("2018", "2019")
    
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
    ) %>% 
    
    return(sldcombined)
  }

 
  
  
  
  
  ### import FY19 data sets and save as .rds files ---------
  sldnest <- filenest %>%
    # filter(OU %in% c("Haiti")) %>%  # test with a few OUs first before running the whole code
    mutate(data = purrr::map(data, importSiteLevelData))
  
  glimpse(sldnest)
  sld <- sldnest %>% unnest()
  
  # save R file
  saveRDS(sldnest, file = paste0(datapathout, "Rdata/", "MER_FY19Q3_sitedata.rds"))
  
  
  
  
  # write csv file data 
  export4hshrt <- function(dfx){
    
    OUx <- dfx %>% distinct(OperatingUnit) %>%  pull(OperatingUnit)
    print(paste0("Exporting site data for ", OUx))

    ifelse(is.na(OUx), 
           print("no data"),
           write_excel_csv(dfx, path = paste0(datapathout,"site_level_data_JB/", OUx, "_FY19Q3_siteleveldata_", Sys.Date(), ".csv"))
    )
    
  }

  sldnest %>% purrr::map(sldnest$data, export4hshrt)
  
 
  
  