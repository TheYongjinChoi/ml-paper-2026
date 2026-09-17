# 03_split.py

df_s1 = df.dropna(subset=pred_vars + ["ptb"]).copy()
df_s2 = df_s1[df_s1["mager"] >= 40].reset_index(drop=True)
df_s3 = df_s1[df_s1["rf_ppterm"] == 1].reset_index(drop=True)

# Sample sizes ---------------------------------------------------------
PILOT_SIZE  = 50_000     # 파일럿: train/test 각각
SAMPLE_SIZE = 300_000    # 본 분석 샘플 1: train/test 각각

sample_sizes = pd.DataFrame({
    "sample": ["Initial",
               "0. Pilot (code check)",
               "1. All mothers (complete cases)",
               "2. Age 40+",
               "3. Previous PTB"],
    "n": [len(df), 2 * PILOT_SIZE, len(df_s1), len(df_s2), len(df_s3)],
})

# Predictors
numeric_cols = ["mager", "wtgain", "rf_cesarn", "cig_1", "cig_2", "cig_3", "m_ht_in", "bmi"]
cat_cols = ["wic",
            "mm_mtr", "mm_plac", "mm_rupt", "mm_uhyst", "mm_aicu",
            "ca_anen", "ca_mnsb", "ca_cchd", "ca_cdh", "ca_omph",
            "ca_gast", "ca_limb", "ca_cleft", "ca_clpal", "ca_down",
            "ca_disor", "ca_hypo",
            "rf_pdiab", "rf_gdiab", "rf_phype", "rf_ghype", "rf_ehype",
            "rf_ppterm", "rf_inftr", "rf_fedrg", "rf_artec",
            "dob_mm", "mbstate_rec", "restatus",
            "mrace31", "mhispx", "meduc",
            "lbo_rec", "tbo_rec", "priorlive", "priordead", "priorterm",
            "precare", "previs",
            "illb_r11", "ilop_r11", "ilp_r11"]

preprocessor = ColumnTransformer([
    ("num", "passthrough", numeric_cols),
    ("cat", OneHotEncoder(handle_unknown="ignore"), cat_cols),
])

# Splits ---------------------------------------------------------------
def _xy(train, test, outcome="ptb", dtype="float32"):
    return dict(
        X_train=train[pred_vars].astype(dtype), y_train=train[outcome].astype(int),
        X_test=test[pred_vars].astype(dtype),   y_test=test[outcome].astype(int),
    )

def _split(data, outcome="ptb", **kw):
    """층화 분할 후 X/y 딕셔너리로 반환."""
    tr, te = train_test_split(data, random_state=SEED, shuffle=True,
                              stratify=data[outcome], **kw)
    return _xy(tr.reset_index(drop=True), te.reset_index(drop=True), outcome=outcome)

np.random.seed(SEED)

s0 = _split(df_s1, train_size=PILOT_SIZE,  test_size=PILOT_SIZE)    # 파일럿
s1 = _split(df_s1, train_size=SAMPLE_SIZE, test_size=SAMPLE_SIZE)
s2 = _split(df_s2, test_size=0.2)
s3 = _split(df_s3, test_size=0.2)

samples = {0: s0, 1: s1, 2: s2, 3: s3}