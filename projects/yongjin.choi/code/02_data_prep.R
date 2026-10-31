# ==============================================================
# 01_data_prep.R
# ==============================================================

# ── 1. Load and recode ────────────────────────────────────────
df.imms <- read_sav("/Users/mac/Library/CloudStorage/Dropbox/01_Research_IMMS/01_Data/IMMS_Wave1_Data_ORB_140526.sav") |>
  droplevels() |> 
  mutate(
    year = 2025,
    set = "IMMS",

    # VCI: original scale (1–5; DK coded as 3)
    vcImp = car::recode(VCI1, "1=1; 2=2; 3=4; 4=5; 98=3; 99=NA"),
    vcChd = car::recode(VCI2, "1=1; 2=2; 3=4; 4=5; 98=3; 99=NA"),
    vcSaf = car::recode(VCI3, "1=1; 2=2; 3=4; 4=5; 98=3; 99=NA"),
    vcEff = car::recode(VCI4, "1=1; 2=2; 3=4; 4=5; 98=3; 99=NA"),
    vcRel = car::recode(VCI5, "1=1; 2=2; 3=4; 4=5; 98=3; 99=NA"),

    # VCI: centred scale (-2 to 2; DK = 0)
    vcImp_re = car::recode(VCI1, "1=2; 2=1; 3=-1; 4=-2; 98=0; 99=NA"),
    vcChd_re = car::recode(VCI2, "1=2; 2=1; 3=-1; 4=-2; 98=0; 99=NA"),
    vcSaf_re = car::recode(VCI3, "1=2; 2=1; 3=-1; 4=-2; 98=0; 99=NA"),
    vcEff_re = car::recode(VCI4, "1=2; 2=1; 3=-1; 4=-2; 98=0; 99=NA"),
    vcRel_re = car::recode(VCI5, "1=2; 2=1; 3=-1; 4=-2; 98=0; 99=NA"),

    # VCI: composite index
    vci = rowMeans(
      across(c(vcImp_re, vcChd_re, vcSaf_re, vcEff_re)),
      na.rm = TRUE
    ),

    # VCI: 4-point reversed scale
    vcImp4_re = car::recode(VCI1, "1=4; 2=3; 3=2; 4=1; 98:99=NA"),
    vcChd4_re = car::recode(VCI2, "1=4; 2=3; 3=2; 4=1; 98:99=NA"),
    vcSaf4_re = car::recode(VCI3, "1=4; 2=3; 3=2; 4=1; 98:99=NA"),
    vcEff4_re = car::recode(VCI4, "1=4; 2=3; 3=2; 4=1; 98:99=NA"),
    vcRel4_re = car::recode(VCI5, "1=4; 2=3; 3=2; 4=1; 98:99=NA"),

    # VCI: binary (agree vs. disagree/DK)
    vcImp2 = as.integer(car::recode(VCI1, "1:2=1; 3:98=0; 99=NA")),
    vcChd2 = as.integer(car::recode(VCI2, "1:2=1; 3:98=0; 99=NA")),
    vcSaf2 = as.integer(car::recode(VCI3, "1:2=1; 3:98=0; 99=NA")),
    vcEff2 = as.integer(car::recode(VCI4, "1:2=1; 3:98=0; 99=NA")),
    vcRel2 = as.integer(car::recode(VCI5, "1:2=1; 3:98=0; 99=NA")),

    # VCI: binary (strongly agree only)
    vcImp2_sa = car::recode(VCI1, "1=1; 2:98=0; 99=NA"),
    vcChd2_sa = car::recode(VCI2, "1=1; 2:98=0; 99=NA"),
    vcSaf2_sa = car::recode(VCI3, "1=1; 2:98=0; 99=NA"),
    vcEff2_sa = car::recode(VCI4, "1=1; 2:98=0; 99=NA"),
    vcRel2_sa = car::recode(VCI5, "1=1; 2:98=0; 99=NA"),

    # Vaccine acceptance
    va1 = car::recode(U1, "1:2=0; 3=1; 99=NA"),
    va1_3 = car::recode(U1, "1=1; 2=2; 3=3; 99=NA"),

    # Child characteristics
    nChd = C0,
    chdAgeGap = CHILD_AGE_2 - CHILD_AGE_1,
    youngestChd = car::recode(
      CHILD_AGE_1,
      "0=0; 1=1; 2=2; 3=3; 4=4; 5:17=5; 98:99=NA"
    ),
    youngestChd_m = car::recode(
      CHILD_AGE_1_MONTHS,
      "0:12=0; 13:24=1; 25:36=2; 37:48=3; 49:900=4; 999=NA"
    ),

    # Vaccination status
    # 1 = vaccinated, 0 = not vaccinated
    vxBCG = car::recode(V1_1, "1=1; 2=0; -1=NA; 98:99=NA"),
    vxPolio = car::recode(V2_1, "1=1; 2=0; -1=NA; 98:99=NA"),
    vxDTP = car::recode(V5_1, "1=1; 2=0; -1=NA; 98:99=NA"),
    vxMMR = car::recode(V7_1, "1=1; 2=0; -1=NA; 98:99=NA"),  
    vxHPV = car::recode(V9_1, "1=1; 2=0; -1=NA; 98:99=NA"),
    vxMMR2 = car::recode(V7_1, "1=0; 2=1; -1=NA; 98:99=NA"),

    # BeSD
    soHCW = car::recode(B3, "1=1; 2=0; 97:99=NA"),
    normFam = car::recode(B6, "1=0; 2=1; 97:99=NA"),
    normRel = car::recode(B7, "1=0; 2=1; 97:99=NA"),
    normCom = car::recode(B8, "1=0; 2=1; 97:99=NA"),
    expContact = car::recode(B9, "1=0; 2=1; 97:99=NA"),
    expSatis = car::recode(B17, "1:2=2; 3=1; 4=0; 97:99=NA"),

    bAccess = car::recode(B14, "1:2=0; 3=1; 4=2; 97:99=NA"),
    bAfford = car::recode(B15, "1:2=0; 3=1; 4=2; 97:99=NA"),

    # Political orientation
    T5_2 = as.numeric(as.character(T5_2)),
    T5_3 = as.numeric(as.character(T5_3)),
    # Progressive-conservative: 0 = progressive, 1 = conservative
    poRight = car::recode(T5_2, "1:3=0; 4:6=1; 97:99=NA"),
    poRight3 = car::recode(T5_2, "1:2=0; 3:4=1; 5:6=2; 97:99=NA"),
    poRight6 = car::recode(T5_2, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 97:99=NA"),

    # Trust
    trGovComm = car::recode(T_T1_5, "1:2=1; 3:4=0; 97:99=NA"),
    trHCP = car::recode(T_T2_8, "1:2=1; 3:4=0; 97:99=NA"),
    trInternet = car::recode(T_T1_6, "1=0; 2=1; 3=2; 4=3; 97:99=NA"),
    trPress = car::recode(T_T1_3, "1:2=0; 3:4=1; 97:99=NA"),
    trTV = car::recode(T_T1_1, "1:2=0; 3:4=1; 97:99=NA"),
    sns = as.numeric(if_any(
      c(T_T3_1, T_T3_2, T_T3_3, T_T3_4, T_T3_5, T_T3_6, T_T3_7, T_T3_8),
      ~ !.x %in% c(98, 99) & .x %in% c(1, 2)
    )),

    # Demographics
    # female: 0 = other, 1 = female
    female = case_when(
      ADM1 %in% c(1:5, 7:12, 19:20) ~
        ifelse(S2A == 2, 1, ifelse(S2A %in% c(1, 97:99), 0, NA_real_)),
      ADM1 %in% setdiff(1:23, c(1:5, 7:12, 19:20)) ~
        ifelse(S2B == 2, 1, ifelse(S2B %in% c(1, 3, 4, 97:99), 0, NA_real_)),
      TRUE ~ NA_real_
    ),
    age = car::recode(S3_RECODE, "1:2=0; 3=1; 4:5=2; 98:99=NA"), # 0=18–34, 1=35–44, 2=45+
    edu = car::recode(S4, "1:3=0; 4:6=1; 7:99=NA"), # 0=primary or below, 1=secondary+
    income = car::recode(S9, "1:2=0; 3:5=1; 6:99=NA"), # 0=basic needs or below, 1=basic goods+
    job = car::recode(S5, "1:2=0; 3:6=1; 99=NA"), # 0=employed, 1=other
    rel = car::recode(religion_RECODE, "8=0; 1=1; 2=2; 3:7=3; 9=3; 10=NA"),
    # 0=none, 1=Christian/Catholic, 2=Islam, 3=other
    havRel = car::recode(religion_RECODE, "1:7=1; 8=0; 9=1; 10=NA"),
    rural = car::recode(S13, "1=0; 2=1; 3=2; 99=NA") # 0=urban, 1=peri-urban, 2=rural
  )

ETH_MAP <- c(
  # White
  "1"   = "White",
  # Black
  "5"   = "Black",   # Black (Canada, UK)
  "26"  = "Black",   # African (UK)
  "27"  = "Black",   # Caribbean (UK)
  "28"  = "Black",   # Other Black/African/Caribbean (UK)
  "32"  = "Black",   # Black / African American (USA)
  # Asian
  "2"   = "Asian",   # South Asian (Canada)
  "4"   = "Asian",   # Chinese (Canada)
  "8"   = "Asian",   # West Asian (Canada)
  "9"   = "Asian",   # Korean (Canada)
  "10"  = "Asian",   # Japanese (Canada)
  "23"  = "Asian",   # South Asian (UK)
  "24"  = "Asian",   # East/South-East Asian (UK)
  "33"  = "Asian",   # Asian / Asian American (USA)
  # Other, including all mixed backgrounds
  "3"   = "Other",   # Indigenous (Canada)
  "6"   = "Other",   # Arab (all)
  "7"   = "Other",   # Latin American (Canada)
  "11"  = "Other",   # Multiracial (Canada)
  "29"  = "Other", "30" = "Other",                   # UK mixed
  "31"  = "Other",                                   # Other mixed (UK, USA)
  "34"  = "Other",   # Indigenous American / Alaskan (USA)
  "35"  = "Other",   # Middle Eastern / North African (USA)
  "36"  = "Other",   # Native Hawaiian / Pacific Islander (USA)
  "37"  = "Other", "38" = "Other", "39" = "Other",
  "40"  = "Other", "41" = "Other",                   # USA mixed
  "175" = "Other"    # Other (all)
  # 176 "Prefer not to say" as NA
)

df.imms <- df.imms |>
  mutate(
    ethnicity = factor(
      ETH_MAP[as.character(as.numeric(S7))],
      levels = c("White", "Black", "Asian", "Other")
    )
  )

REL_MAP <- c(
  "1"  = "Christian",
  "2"  = "Muslim",
  "8"  = "None",
  "3"  = "Other religion",   # Hindu
  "4"  = "Other religion",   # Sikh
  "5"  = "Other religion",   # Jewish
  "6"  = "Other religion",   # Buddhist
  "9"  = "Other religion"    # Other
)

df.imms <- df.imms |>
  mutate(
    religion = factor(
      REL_MAP[as.character(as.numeric(religion_RECODE))],
      levels = c("Christian", "None", "Muslim", "Other religion")
    )
  )


# ── 2. Misinformation (MIST-20) ───────────────────────────────
# Raw response codes: 1 = True, 2 = False, -1 / 98 / 99 = no answer
# Items M1_1–M1_10: false headlines (correct answer = False = 2),
# items M1_11–M1_20: true headlines (correct answer = True = 1).

mist_soln <- c(
  M1_1  = "False", M1_2  = "False", M1_3  = "False", M1_4  = "False",
  M1_5  = "False", M1_6  = "False", M1_7  = "False", M1_8  = "False",
  M1_9  = "False", M1_10 = "False",

  M1_11 = "True",  M1_12 = "True",  M1_13 = "True",  M1_14 = "True",
  M1_15 = "True",  M1_16 = "True",  M1_17 = "True",  M1_18 = "True",
  M1_19 = "True",  M1_20 = "True"
)

mist_text <- c(
  M1_1  = "Gov't officials manipulated stock prices to hide scandals",
  M1_2  = "Corporate media controlled by military-industrial complex",
  M1_3  = "Left-wingers more likely to get higher salary",
  M1_4  = "Gov't manipulating public perceptions around genetic engineering",
  M1_5  = "Left-wing extremism causes more damage than terrorism",
  M1_6  = "Certain vaccines loaded with dangerous chemicals and toxins",
  M1_7  = "Clear relationship between intelligence and eye colour",
  M1_8  = "Gov't knowingly spreading disease through airwaves and food supply",
  M1_9  = "Ebola virus caused by US nuclear weapons testing",
  M1_10 = "Gov't officials illegally manipulated weather to cause storms",

  M1_11 = "Attitudes towards EU largely positive, both within Europe and outside",
  M1_12 = "One-in-three worldwide lack confidence in NGOs",
  M1_13 = "109 US counties have become majority non-white since 2000",
  M1_14 = "America is less respected globally",
  M1_15 = "Hyatt will remove small bottles from hotel bathrooms by 2021",
  M1_16 = "Morocco's king appoints committee chief to fight poverty and inequality",
  M1_17 = "Republicans divided on Trump's conduct, Democrats broadly critical",
  M1_18 = "Democrats more supportive than Republicans of federal spending for Science",
  M1_19 = "Global warming age gap: younger Americans most worried",
  M1_20 = "US support for legal marijuana steady in past year"
)

# Correct response code per item
mist_key <- ifelse(mist_soln == "True", 1L, 2L)

MIST_ITEMS <- names(mist_soln)
MIST_FAKE  <- names(mist_soln)[mist_soln == "False"]
MIST_REAL  <- names(mist_soln)[mist_soln == "True"]
 
# Score each item: 1 = correctly classified, 0 = incorrectly, NA = no answer
for (v in MIST_ITEMS) {
  x <- as.numeric(df.imms[[v]])
  x[!x %in% c(1, 2)] <- NA
  df.imms[[paste0("mist_", v)]] <- as.integer(x == mist_key[[v]])
}

MIST_SCORED      <- paste0("mist_", MIST_ITEMS)
MIST_SCORED_FAKE <- paste0("mist_", MIST_FAKE)
MIST_SCORED_REAL <- paste0("mist_", MIST_REAL)

stopifnot(all(MIST_ITEMS %in% names(df.imms)))

df.imms <- df.imms |>
  mutate(
    across(
      all_of(MIST_ITEMS),
      ~ {
        x <- as.numeric(.x)
        x[!x %in% c(1, 2)] <- NA
        as.integer(x == mist_key[[cur_column()]])
      },
      .names = "mist_{.col}"
    ),

    mistFakeN = rowSums(!is.na(across(all_of(MIST_SCORED_FAKE)))),
    mistRealN = rowSums(!is.na(across(all_of(MIST_SCORED_REAL)))),

    mistFake = ifelse(
      mistFakeN == 0, NA_real_,
      rowSums(across(all_of(MIST_SCORED_FAKE)), na.rm = TRUE)
    ),
    mistReal = ifelse(
      mistRealN == 0, NA_real_,
      rowSums(across(all_of(MIST_SCORED_REAL)), na.rm = TRUE)
    ),

    mistVer = mistFake + mistReal,
    mistDis = pmax(mistFake - mistReal, 0),
    mistNai = pmax(mistReal - mistFake, 0)
  )

# ── 3. Country and region labels ──────────────────────────────

code_to_country <- c(
  "1" = "Chad",
  "2" = "DR Congo",
  "3" = "Egypt",
  "4" = "Ethiopia",
  "5" = "Ghana",
  "6" = "Indonesia",
  "7" = "Kenya",
  "8" = "Nigeria",
  "9" = "Philippines",
  "10" = "Senegal",
  "11" = "South Africa",
  "12" = "South Sudan",
  "13" = "UK",
  "14" = "Canada",
  "15" = "France",
  "16" = "Japan",
  "17" = "Netherlands",
  "18" = "Romania",
  "19" = "Republic of Korea",
  "20" = "Türkiye",
  "21" = "USA",
  "22" = "Brazil",
  "23" = "Mexico"
)

code_to_abbr <- c(
  "1" = "TCD",
  "2" = "COD",
  "3" = "EGY",
  "4" = "ETH",
  "5" = "GHA",
  "6" = "IDN",
  "7" = "KEN",
  "8" = "NGA",
  "9" = "PHL",
  "10" = "SEN",
  "11" = "ZAF",
  "12" = "SSD",
  "13" = "GBR",
  "14" = "CAN",
  "15" = "FRA",
  "16" = "JPN",
  "17" = "NLD",
  "18" = "ROU",
  "19" = "KOR",
  "20" = "TUR",
  "21" = "USA",
  "22" = "BRA",
  "23" = "MEX"
)

code_to_region <- c(
  "225" = "Ontario",
  "226" = "Quebec",
  "227" = "British Columbia",
  "228" = "Alberta",
  "229" = "Manitoba",
  "230" = "Saskatchewan",
  "231" = "Atlantic Provinces",
  "232" = "Northern Territories"
)

df.imms$ctryTxt <- ""
df.imms$ctryAbb <- ""
df.imms$regionTxt <- ""

for (code in names(code_to_country)) {
  df.imms$ctryTxt[df.imms$ADM1 == as.numeric(code)] <- code_to_country[[code]]
  df.imms$ctryAbb[df.imms$ADM1 == as.numeric(code)] <- code_to_abbr[[code]]
}
for (code in names(code_to_region)) {
  df.imms$regionTxt[df.imms$ADM2 == as.numeric(code)] <- code_to_region[[code]]
}

lab.adm2 <- attr(df.imms$ADM2, "labels")
lab.us   <- attr(df.imms$REGION_US_STATES, "labels")

df.imms$subregTxt <- ifelse(
  df.imms$ctryTxt == "USA",
  names(lab.us)[match(df.imms$REGION_US_STATES, lab.us)],
  names(lab.adm2)[match(df.imms$ADM2, lab.adm2)]
)

# ── 4. Design weights ─────────────────────────────────────────

tmp <- read_sav("/Users/mac/Library/CloudStorage/Dropbox/01_Research_IMMS/01_Data/IMMS_Wave1_27052026.sav") |> 
  dplyr::select(LINE_NO, WEIGHT) |> 
  rename(dWEIGHT = WEIGHT)

df.imms <- df.imms |> 
  left_join(tmp, by = "LINE_NO") |> 
  filter(ctryTxt %in% c("USA", "UK", "Canada"))

# ── 5. Vaccination schedule ───────────────────────────────────

vaxSchedule <- read.csv("data/vaxAgeReq_new.csv") |>
  mutate(
    ctry_region = ifelse(
      ctryTxt == "Canada",
      paste0(ctryAbb, "_", regionTxt),
      ctryAbb
    )
  ) |>
  dplyr::select(-ctryTxt, -ctryAbb, -regionTxt)

df.imms <- df.imms |>
  mutate(
    ctry_region = ifelse(
      ctryTxt == "Canada",
      paste0(ctryAbb, "_", regionTxt),
      ctryAbb
    )
  ) |>
  left_join(vaxSchedule, by = "ctry_region") |>
  mutate(
    # Eligibility: 1 = child has reached scheduled age, 0 = not yet,
    # NA = cannot determine
    # No missing values in CHILD_AGE_1_MONTHS
    req_BCG = case_when(
      is.na(ageBCG) | is.na(CHILD_AGE_1_MONTHS) ~ NA_real_,
      ageBCG == 999 ~ 0,
      CHILD_AGE_1_MONTHS >= ageBCG ~ 1,
      TRUE ~ 0
    ),
    req_Polio = case_when(
      is.na(agePolio) | is.na(CHILD_AGE_1_MONTHS) ~ NA_real_,
      agePolio == 999 ~ 0,
      CHILD_AGE_1_MONTHS >= agePolio ~ 1,
      TRUE ~ 0
    ),
    req_DTP = case_when(
      is.na(ageDTP) | is.na(CHILD_AGE_1_MONTHS) ~ NA_real_,
      ageDTP == 999 ~ 0,
      CHILD_AGE_1_MONTHS >= ageDTP ~ 1,
      TRUE ~ 0
    ),
    req_MMR = case_when(
      is.na(ageMMR) | is.na(CHILD_AGE_1_MONTHS) ~ NA_real_,
      ageMMR == 999 ~ 0,
      CHILD_AGE_1_MONTHS >= ageMMR ~ 1,
      TRUE ~ 0
    ),
    # HPV eligibility depends on child sex (C1_1: 0 = male, 1 = female)
    req_HPV = case_when(
      is.na(C1_1) | is.na(CHILD_AGE_1) ~ NA_real_,
      C1_1 == 0 & is.na(ageHPV_M) ~ NA_real_,
      C1_1 == 1 & is.na(ageHPV_F) ~ NA_real_,
      C1_1 == 0 & ageHPV_M == 999 ~ 0,
      C1_1 == 1 & ageHPV_F == 999 ~ 0,
      C1_1 == 0 & CHILD_AGE_1 >= ageHPV_M ~ 1,
      C1_1 == 1 & CHILD_AGE_1 >= ageHPV_F ~ 1,
      TRUE ~ 0
    ),
    # Vaccination status conditional on eligibility (NA if not eligible)
    vxBCG = case_when(
      req_BCG == 0 ~ NA_real_,
      req_BCG == 1 & V1_1 == 1 ~ 1,
      req_BCG == 1 & V1_1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    vxPolio = case_when(
      req_Polio == 0 ~ NA_real_,
      req_Polio == 1 & V2_1 == 1 ~ 1,
      req_Polio == 1 & V2_1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    vxDTP = case_when(
      req_DTP == 0 ~ NA_real_,
      req_DTP == 1 & V5_1 == 1 ~ 1,
      req_DTP == 1 & V5_1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    vxMMR = case_when(
      req_MMR == 0 ~ NA_real_,
      req_MMR == 1 & V7_1 == 1 ~ 1,
      req_MMR == 1 & V7_1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    vxHPV = case_when(
      req_HPV == 0 ~ NA_real_,
      req_HPV == 1 & V9_1 == 1 ~ 1,
      req_HPV == 1 & V9_1 == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Missing at least one of BCG, Polio, DTP, MMR
    # 1 = missed at least one eligible vaccine
    # 0 = received all eligible vaccines
    # NA = no eligible vaccines or all relevant values missing/unknown
    vxMissing = case_when(
      rowSums(across(c(vxBCG, vxPolio, vxDTP, vxMMR), ~ .x == 0), na.rm = TRUE) > 0 ~ 1,
      rowSums(!is.na(across(c(vxBCG, vxPolio, vxDTP, vxMMR)))) > 0 &
        rowSums(across(c(vxBCG, vxPolio, vxDTP, vxMMR), ~ .x == 0), na.rm = TRUE) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )