# fields_nat2024.py — fixed-width layout of the NCHS 2024 public-use natality file.
# Copied verbatim from the notebook (cell 4); positions follow the NCHS user guide.

fields = [
    ("FILLER1",   8),   #  1–8
    ("DOB_YY",    4),   #  9-12 Birth Year
    ("DOB_MM",    2),   # 13-14 Birth Month
    ("FILLER2",   4),   # 15–18
    ("DOB_TT",    4),   # Time of Birth
    ("DOB_WK",    1),   # Birth Day of Week
    ("FILLER3",   8),   # 24–31
    ("BFACIL",    1),   # Birth Place
    ("F_BFACIL",  1),   # Reporting Flag for Birth Place
    ("FILLER4",  16),   # 34–49
    ("BFACIL3",     1),   #  50: Birth Place Recode
    ("FILLER5",    22),   # 51–72: Filler
    ("MAGE_IMPFLG", 1),   #    73: Mother’s Age Imputed Flag
    ("MAGE_REPFLG", 1),   #    74: Reported Age of Mother Used Flag
    ("MAGER",       2),   # 75–76: Mother’s Single Years of Age
    ("MAGER14",    2),   # 77–78: Mother’s Age Recode 14
    ("MAGER9",     1),   #    79: Mother’s Age Recode 9
    ("FILLER6",    4),   # 80–83: Filler
    ("MBSTATE_REC",1),   #    84: Mother’s Nativity
    ("FILLER7",   19),   # 85–103: Filler

    ("RESTATUS",   1),   #    104: Residence Status
    ("MRACE31",   2),   #    105-106: Mother’s Race Recode 31
    ("MRACE6",     1),   # 107   Mother’s Race Recode 6
    ("MRACE15",    2),   # 108–109 Mother’s Race Recode 15
    ("FILLER9",    1),   # 110   Filler
    ("MRACEIMP",   1),   # 111   Mother’s Race Imputed Flag
    ("MHISPX",     1),   # 112   Mother’s Hispanic Origin
    ("FILLER10",   2),   # 113–114 Filler
    ("MHISP_R",    1),   # 115   Mother’s Hispanic Origin Recode
    ("F_MHISP",    1),   # 116: Reporting Flag for Mother’s Origin
    ("MRACEHISP",  1),   # 117: Mother’s Race/Hispanic Origin
    ("FILLER11",   1),   # 118: Filler
    ("MAR_P",      1),   # 119: Paternity Acknowledged
    ("DMAR",       1),   # 120: Marital Status
    ("MAR_IMP",    1),   # 121: Mother’s Marital Status Imputed
    ("FILLER12",   1),   # 122: Filler
    ("F_MAR_P",    1),   # 123: Reporting Flag for Paternity
    ("MEDUC",      1),   # 124: Mother’s Education
    ("FILLER13",      1),   # 125      Filler
    ("F_MEDUC",       1),   # 126      Reporting Flag for Education of Mother
    ("FILLER14",     15),   # 127–141  Filler

    ("FAGERPT_FLG",   1),   # 142      Father’s Reported Age Used Flag
    ("FILLER15",      4),   # 143–146  Filler
    ("FAGECOMB",      2),   # 147–148  Father’s Combined Age
    ("FAGEREC11",     2),   # 149–150  Father’s Age Recode 11
    ("FRACE31",       2),   # 151–152  Father’s Race Recode 31
    ("FRACE6",   1),   # 153   Father’s Race Recode 6
    ("FRACE15",  2),   # 154–155 Father’s Race Recode 15
    ("FILLER16",   3),   # 156–158: Filler
    ("FHISPX",     1),   #    159: Father’s Hispanic Origin
    ("FHISP_R",    1),   #    160: Father’s Hispanic Origin Recode
    ("F_FHISP",    1),   #    161: Reporting Flag for Father’s Origin
    ("FRACEHISP",  1),   #    162: Father’s Race/Hispanic Origin
    ("FEDUC",      1),   #    163: Father’s Education
    ("FILLER17",   1),   #    164: Filler
    ("f_FEDUC",    1),   # 165   Reporting Flag for Education of Father
    ("FILLER18",   5),   # 166–170 Filler

    ("PRIORLIVE",  2),   # 171–172 Prior Births Now Living
    ("PRIORDEAD",  2),   # 173–174 Prior Births Now Dead
    ("PRIORTERM",  2),   # 175–176 Prior Other Terminations
    ("FILLER19",   2),   # 177–178 Filler
    ("LBO_REC",    1),   # 179   Live Birth Order Recode
    ("FILLER20",   2),   # 180–181 Filler
    ("TBO_REC",    1),   # 182   Total Birth Order Recode
    ("FILLER21",  15),   # 183–197 Filler
    ("ILLB_R",     3),   # 198–200 Interval Since Last Live Birth Recode
    ("ILLB_R11",   2),   # 201–202 Interval Since Last Live Birth Recode 11
    ("FILLER22",   3),   # 203–205: Filler
    ("ILOP_R",     3),   # 206–208: Interval Since Last Other Pregnancy Recode
    ("ILOP_R11",   2),   # 209–210: Interval Since Last Other Pregnancy Recode 11
    ("FILLER23",   3),   # 211–213: Filler
    ("ILP_R",      3),   # 214–216: Interval Since Last Pregnancy Recode
    ("ILP_R11",    2),   # 217–218: Interval Since Last Pregnancy Recode 11
    ("FILLER24",   5),   # 219–223: Filler
    ("PRECARE",    2),   # 224–225: Month Prenatal Care Began
    ("F_MPCB",     1),   # 226: Reporting Flag for Month Prenatal Care Began
    ("PRECARE5",   1),   # 227: Month Prenatal Care Began Recode
    ("FILLER25",  10),   # 228–237: Filler
    ("PREVIS",     2),   # 238–239: Number of Prenatal Visits
    ("FILLER26",   2),   # 240–241: Filler
    ("PREVIS_REC", 2),   # 242–243: Number of Prenatal Visits Recode
    ("F_TPCV",     1),   # 244: Reporting Flag for Total Prenatal Care Visits
    ("FILLER27",   6),   # 245–250: Filler

    ("WIC",        1),   # 251: WIC
    ("F_WIC",      1),   # 252: Reporting Flag for WIC
    ("CIG_0",      2),   # 253–254: Cigarettes Before Pregnancy
    ("CIG_1",  2),   # 255–256: Cigarettes 1st Trimester
    ("CIG_2",  2),   # 257–258: Cigarettes 2nd Trimester
    ("CIG_3",  2),   # 259–260: Cigarettes 3rd Trimester
    ("CIG0_R", 1),   #    261: Cigarettes Before Pregnancy Recode
    ("CIG1_R", 1),   #    262: Cigarettes 1st Trimester Recode
    ("CIG2_R", 1),   #    263: Cigarettes 2nd Trimester Recode
    ("CIG3_R", 1),   #    264: Cigarettes 3rd Trimester Recode
    ("F_CIGS_0", 1),    # 265: Reporting Flag for Cigarettes before Pregnancy
    ("F_CIGS_1", 1),    # 266: Reporting Flag for Cigarettes 1st Trimester
    ("F_CIGS_2", 1),    # 267: Reporting Flag for Cigarettes 2nd Trimester
    ("F_CIGS_3", 1),    # 268: Reporting Flag for Cigarettes 3rd Trimester
    ("CIG_REC",  1),    # 269: Cigarette Recode
    ("F_TOBACO", 1),    # 270: Reporting Flag for Tobacco use
    ("FILLER28", 9),    # 271–279: Filler
    ("M_Ht_In",  2),    # 280–281: Mother’s Height in Total Inches
    ("F_M_HT",   1),    # 282: Reporting Flag for Mother’s Height
    ("BMI",      4),    # 283–286: Body Mass Index
    ("BMI_R",    1),    # 287: Body Mass Index Recode
    ("FILLER29", 4),    # 288–291: Filler
    ("PWgt_R",   3),    # 292–294: Pre-pregnancy Weight Recode
    ("F_PWGT",      1),   # 295: Reporting Flag for Pre‑pregnancy Weight
    ("FILLER30",    3),   # 296–298: Filler
    ("DWGT_R",      3),   # 299–301: Delivery Weight Recode
    ("FILLER31",    1),   #    302: Filler
    ("F_DWGT",      1),   # 303: Reporting Flag for Delivery Weight
    ("WTGAIN",      2),   # 304–305: Weight Gain
    ("WTGAIN_REC",  1),   # 306: Weight Gain Recode
    ("F_WTGAIN",    1),   # 307: Reporting Flag for Weight Gain
    ("FILLER32",    5),   # 308–312: Filler

    ("RF_PDIAB", 1),   # 313: Pre‑pregnancy Diabetes
    ("RF_GDIAB", 1),   # 314: Gestational Diabetes
    ("RF_PHYPE", 1),   # 315: Pre‑pregnancy Hypertension
    ("RF_GHYPE",   1),   # 316: Gestational Hypertension
    ("RF_EHYPE",   1),   # 317: Hypertension Eclampsia
    ("RF_PPTERM",  1),   # 318: Previous Preterm Birth
    ("F_RF_PDIAB", 1),   # 319: Reporting Flag for Pre‑pregnancy Diabetes
    ("F_RF_GDIAB", 1),   # 320: Reporting Flag for Gestational Diabetes
    ("F_RF_PHYPER",1),   # 321: Reporting Flag for Pre‑pregnancy Hypertension
    ("F_RF_GHYPER",1),   # 322: Reporting Flag for Gestational Hypertension
    ("F_RF_ECLAMP",1),   # 323: Reporting Flag for Hypertension Eclampsia
    ("F_RF_PPB",   1),   # 324: Reporting Flag for Previous Preterm Birth
    ("RF_INFTR",   1),   # 325: Infertility Treatment Used
    ("RF_FEDRG",   1),   # 326: Fertility Enhancing Drugs
    ("RF_ARTEC",   1),   # 327: Asst. Reproductive Technology
    ("F_RF_INFT",  1),   # 328: Reporting Flag for Infertility Treatment
    ("F_RF_INF_DRG",  1),   # 329: Reporting Flag for Fertility Enhancing Drugs
    ("F_RF_INF_ART",  1),   # 330: Reporting Flag for Reproductive Technology
    ("RF_CESAR",      1),   # 331: Previous Cesarean
    ("RF_CESARN",     2),   # 332–333: Number of Previous Cesareans
    ("FILLER33",      1),   # 334: Filler
    ("F_RF_CESAR",    1),   # 335: Reporting Flag for Previous Cesarean
    ("F_RF_NCESAR",   1),   # 336: Reporting Flag for Number of Previous Cesareans
    ("NO_RISKS",      1),   # 337: No Risk Factors Reported
    ("FILLER34",      5),   # 338–342: Filler

    ("IP_GON",   1),   # 343: Gonorrhea
    ("IP_SYPH",  1),   # 344: Syphilis
    ("IP_CHLAM", 1),   # 345: Chlamydia
    ("IP_HEPB",   1),   # 346: Hepatitis B
    ("IP_HEPC",   1),   # 347: Hepatitis C
    ("F_IP_GONOR",1),   # 348: Reporting Flag for Gonorrhea
    ("F_IP_SYPH", 1),   # 349: Reporting Flag for Syphilis
    ("F_IP_CHLAM",1),   # 350: Reporting Flag for Chlamydia
    ("F_IP_HEPATB",1),  # 351: Reporting Flag for Hepatitis B
    ("F_IP_HEPATC",1),  # 352: Reporting Flag for Hepatitis C
    ("NO_INFEC",  1),   # 353: No Infections Reported
    ("FILLER35",  5),   # 354–358: Filler   
    ("FILLER36", 1),   # 359: Filler

    ("OB_ECVS",   1),   # 360: Successful External Cephalic Version
    ("OB_ECVF",   1),   # 361: Failed External Cephalic Version
    ("FILLER37", 1),   # 362: Filler
    ("F_OB_SUCC", 1),   # 363: Reporting Flag for Successful ECV
    ("F_OB_FAIL", 1),   # 364: Reporting Flag for Failed ECV
    ("FILLER38", 18),   # 365–382: Filler
    ("LD_INDL",   1),   # 383: Induction of Labor
    ("LD_AUGM",   1),   # 384: Augmentation of Labor
    ("LD_STER",   1),   # 385: Steroids
    ("LD_ANTB",   1),   # 386: Antibiotics
    ("LD_CHOR",   1),   # 387: Chorioamnionitis
    ("LD_ANES",   1),   # 388: Anesthesia
    ("F_LD_INDL", 1),   # 389: Reporting Flag for Induction of Labor
    ("F_LD_AUGM", 1),   # 390: Reporting Flag for Augmentation of Labor
    ("F_LD_STER", 1),   # 391: Reporting Flag for Steroids
    ("F_LD_ANTB",1),   # 392: Reporting Flag for Antibiotics
    ("F_LD_CHOR",1),   # 393: Reporting Flag for Chorioamnionitis
    ("F_LD_ANES",1),   # 394: Reporting Flag for Anesthesia
    ("NO_LBRDLV", 1),   # 395: No Characteristics of Labor Reported
    ("FILLER39",  5),   # 396–400: Filler

    # Method of Delivery
    ("ME_PRES",   1),   # 401: Fetal Presentation at Delivery
    ("ME_ROUT",   1),   # 402: Final Route & Method of Delivery
    ("ME_TRIAL",  1),   # 403: Trial of Labor Attempted (if cesarean)
    ("F_ME_PRES", 1),   # 404: Reporting Flag for Fetal Presentation
    ("F_ME_ROUT", 1),   # 405: Reporting Flag for Final Route & Method
    ("F_ME_TRIAL",1),   # 406: Reporting Flag for Trial of Labor Attempted

    # Delivery method recode
    ("RDMETH_REC",1),   # 407: Delivery Method Recode
    ("DMETH_REC",   1),   # 408: Delivery Method Recode
    ("F_DMETH_REC", 1),   # 409: Reporting Flag for Method of Delivery Recode
    ("FILLER40",    5),   # 410–414: Filler (not on certificate)

    # Maternal Morbidity (415–432; 총 18개 필드)
    ("MM_MTR",      1),   # 415: Maternal Transfusion
    ("MM_PLAC",     1),   # 416: Perineal Laceration
    ("MM_RUPT",     1),   # 417: Ruptured Uterus
    ("MM_UHYST",    1),   # 418: Unplanned Hysterectomy
    ("MM_AICU",     1),   # 419: Admit to Intensive Care
    ("FILLER41",    1),   # 420: Filler (not on certificate)
    ("F_MM_MTR",    1),   # 421: Reporting Flag for Maternal Transfusion
    ("F_MM_PLAC",   1),   # 422: Reporting Flag for Perineal Laceration
    ("F_MM_RUPT",   1),   # 423: Reporting Flag for Ruptured Uterus
    ("F_MM_UHYST",  1),   # 424: Reporting Flag for Unplanned Hysterectomy
    ("F_MM_AICU",   1),   # 425: Reporting Flag for Admit to Intensive Care
    ("FILLER42", 1),   # 426: Filler (not on certificate)    
    ("NO_MMORB",  1),  # 427: No Maternal Morbidity Reported
    ("FILLER43",  5),  # 428–432: Filler (not on certificate)

    ("ATTEND",    1),  # 433: Attendant at Birth
    ("MTRAN",     1),  # 434: Mother Transferred
    ("PAY",       1),  # 435: Payment Source for Delivery
    ("PAY_REC",   1),  # 436: Payment Recode
    ("F_PAY",      1),   # 437: Reporting Flag for Source of Payment
    ("F_PAY_REC",  1),   # 438: Reporting Flag for Payment Recode
    ("FILLER44",   5),   # 439–443: Filler

    ("APGAR5",     2),   # 444–445: Five Minute APGAR Score
    ("APGAR5R",    1),   # 446: Five Minute APGAR Recode
    ("F_APGAR5",   1),   # 447: Reporting Flag for Five minute APGAR
    ("APGAR10",    2),   # 448–449: Ten Minute APGAR Score
    ("APGAR10R",   1),   # 450: Ten Minute APGAR Recode
    ("FILLER45",   3),   # 451–453: Filler

    ("DPLURAL",    1),   # 454: Plurality Recode
    ("FILLER46",   1),   # 455: Filler
    ("IMP_PLUR",   1),   # 456: Plurality Imputed
    ("FILLER47",   2),   # 457–458: Filler
    ("SETORDER_R", 1),   # 459: Set Order Recode
    ("FILLER48", 15),    # 460–474: Filler

    # Sex & Last Menses
    ("SEX",        1),   # 475: Sex of Infant
    ("IMP_SEX",    1),   # 476: Imputed Sex
    ("DLMP_MM",    2),   # 477–478: Last Normal Menses Month
    ("FILLER49",   2),   # 479–480: Filler
    ("DLMP_YY",    4),   # 481–484: Last Normal Menses Year
    ("FILLER50",   3),   # 485–487: Filler

    # Gestation flags & estimate
    ("COMPGST_IMP",1),   # 488: Combined Gestation Imputation Flag
    ("OBGEST_FLG",1),    # 489: Obstetric Estimate Used Flag
    ("COMBGEST",   2),   # 490–491: Combined Gestation – Detail in Weeks
    ("GESTREC10", 2),   # 492–493: Combined Gestation Recode 10
    ("GESTREC3",  1),   # 494    : Combined Gestation Recode 3
    ("FILLER51",  3),   # 495–497: Filler
    ("LMPUSED",   1),   # 498    : Combined Gestation Used Flag
    ("OEGest_Comb",2),  # 499–500: Obstetric Estimate Edited
    ("OEGest_R10",2),   # 501–502: Obstetric Estimate Recode10
    ("OEGest_R3", 1),   # 503    : Obstetric Estimate Recode 3
    ("DBWT",      4),   # 504–507: Birth Weight – Detail in Grams (Edited)
    ("FILLER52", 1),   # 508: Filler
    ("BWTR12",   2),   # 509–510: Birth Weight Recode 12
    ("BWTR4",    1),   # 511:   Birth Weight Recode 4
    ("FILLER53", 5),   # 512–516: Filler

    # 517–536: Abnormal Conditions of the Newborn (20 fields, each length=1)
    ("AB_AVEN1", 1),   # 517: Assisted Ventilation (immediately)
    ("AB_AVEN6", 1),   # 518: Assisted Ventilation > 6 hrs
    ("AB_NICU",  1),   # 519: Admission to NICU
    ("AB_SURF",  1),   # 520: Surfactant
    ("AB_ANTI",  1),   # 521: Antibiotics for Newborn
    ("AB_SEIZ",     1),   # 522: Seizures
    ("FILLER54",    1),   # 523: Filler
    ("F_AB_VENT",   1),   # 524: Reporting Flag for Assisted Ventilation (0 hrs)
    ("F_AB_VENT6",  1),   # 525: Reporting Flag for Assisted Ventilation (>6 hrs)
    ("F_AB_NICU",   1),   # 526: Reporting Flag for Admission to NICU
    ("F_AB_SURFAC", 1),   # 527: Reporting Flag for Surfactant
    ("F_AB_ANTIBIO",1),   # 528: Reporting Flag for Antibiotics
    ("F_AB_SEIZ",   1),   # 529: Reporting Flag for Seizures
    ("FILLER55",    1),   # 530: Filler
    ("NO_ABNORM",   1),   # 531: No Abnormal Conditions Checked
    ("FILLER56",    5),   # 532–536: Filler

    # Congenital Anomalies of the Newborn (positions 537–566; 30 fields of length=1)
    ("CA_ANEN",     1),   # 537: Anencephaly
    ("CA_MNSB",     1),   # 538: Meningomyelocele/Spina Bifida
    ("CA_CCHD",     1),   # 539: Cyanotic Congenital Heart Disease
    ("CA_CDH",     1),   # 540: Congenital Diaphragmatic Hernia
    ("CA_OMPH",    1),   # 541: Omphalocele
    ("CA_GAST",    1),   # 542: Gastroschisis
    ("F_CA_ANEN",   1),  # 543: Reporting Flag for Anencephaly
    ("F_CA_MENIN",  1),  # 544: Reporting Flag for Meningomyelocele/Spina Bifida
    ("F_CA_HEART",  1),  # 545: Reporting Flag for Cyanotic Congenital Heart Disease
    ("F_CA_HERNIA", 1),  # 546: Reporting Flag for Congenital Diaphragmatic Hernia
    ("F_CA_OMPHA",  1),  # 547: Reporting Flag for Omphalocele
    ("F_CA_GASTRO", 1),  # 548: Reporting Flag for Gastroschisis

    # More anomalies (549–552)
    ("CA_LIMB",    1),   # 549: Limb Reduction Defect
    ("CA_CLEFT",   1),   # 550: Cleft Lip w/ or w/o Cleft Palate
    ("CA_CLPAL",   1),   # 551: Cleft Palate alone
    ("CA_DOWN",    1),   # 552: Down Syndrome
    ("CA_DISOR",   1),   # 553: Suspected Chromosomal Disorder
    ("CA_HYPO",    1),   # 554: Hypospadias
    ("F_CA_LIMB",  1),   # 555: Reporting Flag for Limb Reduction Defect
    ("F_CA_CLEFTLP",1),  # 556: Reporting Flag for Cleft Lip w/ or w/o Cleft Palate
    ("F_CA_CLEFT", 1),   # 557: Reporting Flag for Cleft Palate Alone
    ("F_CA_DOWNS", 1),   # 558: Reporting Flag for Down Syndrome
    ("F_CA_CHROM", 1),   # 559: Reporting Flag for Suspected Chromosomal Disorder
    ("F_CA_HYPOS", 1),   # 560: Reporting Flag for Hypospadias
    ("NO_CONGEN",  1),   # 561: No Congenital Anomalies Checked
    ("FILLER57",   5),   # 562–566: Filler

    ("ITRAN",      1),   # 567: Infant Transferred
    ("ILIVE",      1),   # 568: Infant Living at Time of Report
    ("BFED",     1),    # 569: Infant Breastfed at Discharge
    ("F_BFED",   1),    # 570: Reporting Flag for Breastfed at Discharge
    ("FILLER58",760),   # 571–1330: Filler
]
