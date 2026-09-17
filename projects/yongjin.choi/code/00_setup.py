# 00_setup.py

# ── Dependency check

import importlib.util
import subprocess
import sys
import plotly
import kaleido
import importlib.metadata

REQUIREMENTS = {                       # import name → pip name
    "numpy": "numpy", "pandas": "pandas", "pyarrow": "pyarrow",
    "plotly": "plotly", "sklearn": "scikit-learn",
    "imblearn": "imbalanced-learn", "xgboost": "xgboost", "lightgbm": "lightgbm",
}
missing = [pkg for mod, pkg in REQUIREMENTS.items()
           if importlib.util.find_spec(mod) is None]
if missing:
    print(f"Installing: {', '.join(missing)}")
    cmd = [sys.executable, "-m", "pip", "install", "--quiet", *missing]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise ImportError(
            "pip install failed:\n" + (proc.stderr or proc.stdout).strip()[-800:]
            + "\n\nRun it yourself in the environment this kernel uses:\n  "
            + " ".join(cmd)
        )
    importlib.invalidate_caches()
    print("Done.")

if any(importlib.util.find_spec(m) is None for m in ("tensorflow", "scikeras")):
    print("Note: tensorflow/scikeras unavailable — the DNN section will not run.")

# ── Imports ──────────────────────────────────────────────────
import os
import warnings
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd

import plotly.graph_objects as go
import plotly.io as pio
from plotly.subplots import make_subplots

from sklearn.compose import ColumnTransformer
from sklearn.metrics import (
    accuracy_score, balanced_accuracy_score, precision_score, recall_score,
    f1_score, roc_auc_score, confusion_matrix, make_scorer,
)
from sklearn.model_selection import train_test_split, RepeatedStratifiedKFold, GridSearchCV
from sklearn.preprocessing import OneHotEncoder
from sklearn.utils.class_weight import compute_class_weight

from imblearn.pipeline import Pipeline as ImbPipeline
from imblearn.over_sampling import SMOTE

from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from lightgbm import LGBMClassifier

warnings.filterwarnings("ignore", category=FutureWarning, module="sklearn.base")

pio.renderers.default = "plotly_mimetype+notebook_connected"

# ── Paths
ROOT = Path.cwd()
DATA_RAW = ROOT / "data" / "Nat2024PublicUS.c20250512.r20250708.txt"
DATA_CACHE = ROOT / "data" / "nat2024_recoded.parquet"
OUT = ROOT / "output"
OUT.mkdir(exist_ok=True)
TODAY = date.today().strftime("%Y%m%d")

# ── Performance metrics
def specificity_score(y_true, y_pred, **kwargs):
    tn, fp, fn, tp = confusion_matrix(y_true, y_pred).ravel()
    return tn / (tn + fp)

scoring = {
    "accuracy": "accuracy",
    "balanced_accuracy": "balanced_accuracy",
    "precision": "precision",
    "recall": "recall",          # sensitivity
    "f1": "f1",
    "roc_auc": "roc_auc",
    "specificity": make_scorer(specificity_score),
}

SEED = 123