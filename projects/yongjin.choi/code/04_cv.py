# 04_cv.py

def run_grid(name, model, param_grid, data, *, refit="roc_auc",
             n_splits=5, n_repeats=1, sampler=None, verbose=1):
    """
    name        : label written to the results table
    model       : unfitted estimator
    param_grid  : dict with 'model__' prefixed keys
    data        : dict with X_train, y_train, X_test, y_test  (samples[k])
    refit       : metric used to pick the best estimator
    Returns (results_df, best_estimator)
    """
    X_train, y_train = data["X_train"], data["y_train"]
    X_test, y_test = data["X_test"], data["y_test"]
    sampler = sampler if sampler is not None else SMOTE(random_state=SEED)

    pipe = ImbPipeline([
        ("preprocessor", preprocessor),
        ("sampler", sampler),
        ("model", model),
    ])
    cv = RepeatedStratifiedKFold(n_splits=n_splits, n_repeats=n_repeats, random_state=SEED)
    grid = GridSearchCV(pipe, param_grid=param_grid, scoring=scoring,
                        refit=refit, cv=cv, n_jobs=-1, verbose=verbose)
    grid.fit(X_train, y_train)

    cvr = grid.cv_results_
    rows = []
    for i, params in enumerate(cvr["params"]):
        rows.append({
            "stage": "CV", "model": name, **params,
            "accuracy":          cvr["mean_test_accuracy"][i],
            "balanced_accuracy": cvr["mean_test_balanced_accuracy"][i],
            "precision":         cvr["mean_test_precision"][i],
            "sensitivity":       cvr["mean_test_recall"][i],
            "specificity":       cvr["mean_test_specificity"][i],
            "f1":                cvr["mean_test_f1"][i],
            "roc_auc":           cvr["mean_test_roc_auc"][i],
        })

    # Hold-out test with the refit best estimator
    best = grid.best_estimator_
    y_pred = best.predict(X_test)
    tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
    rows.append({
        "stage": "Test", "model": name, **grid.best_params_,
        "accuracy":          accuracy_score(y_test, y_pred),
        "balanced_accuracy": balanced_accuracy_score(y_test, y_pred),
        "precision":         precision_score(y_test, y_pred),
        "sensitivity":       recall_score(y_test, y_pred),
        "specificity":       tn / (tn + fp),
        "f1":                f1_score(y_test, y_pred),
        "roc_auc":           roc_auc_score(y_test, best.predict_proba(X_test)[:, 1]),
    })
    res = pd.DataFrame(rows)
    t = res[res.stage == "Test"].iloc[0]
    print(f"▶ {name} hold-out — bAcc {t.balanced_accuracy:.3f}  Sen {t.sensitivity:.3f}  "
          f"Spe {t.specificity:.3f}  AUC {t.roc_auc:.3f}")
    return res, best


def save_results(res, sample, model_tag):
    """outputs/S{sample}_{model_tag}_{YYYYMMDD}.csv, mirroring '_S3_RF_20250801.csv'."""
    path = OUT / f"S{sample}_{model_tag}_{TODAY}.csv"
    res.to_csv(path, index=False)
    print(f"saved → {path.relative_to(ROOT)}")
    return path


def load_latest(sample, model_tag):
    """Most recent results CSV for this sample/model, or None."""
    files = sorted(OUT.glob(f"S{sample}_{model_tag}_*.csv"))
    return pd.read_csv(files[-1]) if files else None
