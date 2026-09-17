# 01_read_data.py

if "pd" not in dir():
    raise RuntimeError("Run code/00_setup.py first (%run -i code/00_setup.py).")
if not DATA_RAW.exists():
    raise FileNotFoundError(
        f"Raw file not found:\n  {DATA_RAW}\n"
        f"Current working directory is {Path.cwd()}.\n"
        "Put the .txt in data/, and run from the project root "
        "(_quarto.yml sets execute-dir: project)."
    )

import runpy

RECODE_SPEC = {
    # Y/N/X/U  →  1/0/0/NA
    "binary": ["WIC",
               # Maternal morbidity
               "MM_MTR", "MM_PLAC", "MM_RUPT", "MM_UHYST", "MM_AICU",
               # Birth defects
               "CA_ANEN", "CA_MNSB", "CA_CCHD", "CA_CDH", "CA_OMPH",
               "CA_GAST", "CA_LIMB", "CA_CLEFT", "CA_CLPAL", "CA_DOWN",
               "CA_DISOR", "CA_HYPO",
               # Maternal risk factors
               "RF_PDIAB", "RF_GDIAB", "RF_PHYPE", "RF_GHYPE", "RF_EHYPE",
               "RF_PPTERM", "RF_INFTR", "RF_FEDRG", "RF_ARTEC"],
    # numeric, no missing code
    "numeric": ["DOB_MM", "MAGER", "MBSTATE_REC", "RESTATUS", "MRACE31"],
    # numeric, missing coded as exactly 9
    "miss9": ["MHISPX", "MEDUC", "LBO_REC", "TBO_REC", "WTGAIN"],
    # numeric, missing coded as 40 or over
    "miss40": ["PRIORLIVE", "PRIORDEAD", "PRIORTERM", "PRECARE", "PREVIS", "RF_CESARN"],
    # numeric, missing coded as 99 or over
    "miss99": ["COMBGEST", "CIG_1", "CIG_2", "CIG_3", "M_Ht_In", "BMI",
               "ILLB_R11", "ILOP_R11", "ILP_R11"],
    # numeric, missing coded as 9999 or over
    "miss9999": ["DBWT"],
}
KEEP = [c for group in RECODE_SPEC.values() for c in group]

fields = runpy.run_path(str(ROOT / "code" / "fields_nat2024.py"))["fields"]
positions, pos = {}, 0
for nm, ln in fields:
    positions[nm] = (pos, pos + ln)
    pos += ln
record_len = pos

unknown = [c for c in KEEP if c not in positions]
if unknown:
    raise KeyError(f"Not in the field layout: {unknown}")

colspecs = [positions[c] for c in KEEP]

print(f"Reading {DATA_RAW.name} ({DATA_RAW.stat().st_size / 1e9:.1f} GB, "
      f"{len(KEEP)} of {len(positions)} columns)…")

chunks = []
for i, chunk in enumerate(pd.read_fwf(DATA_RAW, colspecs=colspecs, names=KEEP,
                                      dtype=str, chunksize=500_000)):
    chunks.append(chunk)
    print(f"  chunk {i + 1}: {len(chunk):,} records", flush=True)
df = pd.concat(chunks, ignore_index=True)
del chunks

print(f"Raw records: {len(df):,}  |  columns: {df.shape[1]}")
