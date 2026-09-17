# 02_recode.py

if "RECODE_SPEC" not in dir():
    raise RuntimeError("Run code/01_read_data.py first — it defines df and RECODE_SPEC.")

MISSING_CUTOFF = {"miss40": 40, "miss99": 99, "miss9999": 9999}
target_vars = []

for group, cols in RECODE_SPEC.items():
    for col in cols:
        new = col.lower()
        target_vars.append(new)
        if group == "binary":
            df[new] = df[col].map({"Y": 1, "N": 0, "X": 0, "U": pd.NA}).astype("Int64")
        else:
            s = pd.to_numeric(df[col], errors="coerce")
            if group == "miss9":
                s = s.mask(s == 9, pd.NA)          # exactly 9, not >= 9
            elif group in MISSING_CUTOFF:
                s = s.mask(s >= MISSING_CUTOFF[group], pd.NA)
            df[new] = s.astype("Float64")

# Outcomes
df["ptb"] = (df["combgest"] < 37).astype("Int64").where(df["combgest"].notna(), pd.NA)
df["lbw"] = (df["dbwt"] < 2500).astype("Int64").where(df["dbwt"].notna(), pd.NA)
target_vars += ["ptb", "lbw"]

exclude = ["ptb", "lbw", "combgest", "dbwt"]
pred_vars = [v for v in target_vars if v not in exclude]

# Keep only the recoded columns, drop the raw string ones, and cache
df = df[target_vars].copy()
df.to_parquet(DATA_CACHE, index=False)
print(f"Recoded {len(target_vars)} variables → cached to {DATA_CACHE.name}")

# Variable summary: missingness and positive rate
rows = []
for var in target_vars:
    ser = df[var]
    n_total, n_missing = len(ser), ser.isna().sum()
    n_nonmiss, n_pos = n_total - n_missing, ser.eq(1).sum()
    rows.append({"variable": var, "Total": n_total,
                 "pct_missing": n_missing / n_total * 100,
                 "n_positive": n_pos,
                 "pct_positive": n_pos / n_nonmiss * 100 if n_nonmiss else pd.NA,
                 "min": ser.min(), "max": ser.max()})
var_summary = pd.DataFrame(rows).set_index("variable").round(1)

