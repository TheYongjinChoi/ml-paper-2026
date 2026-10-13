# ==============================================================
# 00_initial.R — packages, theme, palettes, font styles
# ==============================================================

# --------------------------------------------------------------
# 1. Packages
# --------------------------------------------------------------

library(rjags)
library(coda)
library(dplyr)
library(doParallel)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)

packages <- unique(c(
  # Data manipulation
  "tidyverse",
  "data.table",
  "haven",
  "sjlabelled",
  "car",

  # Visualisation
  "plotly",
  "ggplot2",
  "GGally",
  "htmlwidgets",
  "webshot2",

  # Inference
  "fixest",
  "sandwich",
  "lmtest",
  "broom",
  "MASS",
  "nnet",
  "lme4",

  # Missing data
  "naniar",
  "mice",

  # Double / Debiased ML
  "DoubleML",
  "mlr3",
  "mlr3learners",
  "mlr3tuning",
  "paradox",
  "ranger",
  "xgboost",

  # Reporting
  "flextable",
  "officer",
  "epiDisplay",

  # Parallel processing
  "future",
  "future.apply",
  "furrr",
  "lgr"
))

load_packages <- function(pkgs) {
  installed <- rownames(installed.packages())
  missing   <- pkgs[!pkgs %in% installed]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    for (pkg in missing) {
      tryCatch(
        install.packages(pkg, quiet = TRUE, dependencies = TRUE),
        error = function(e) message("FAILED to install ", pkg, " — ", conditionMessage(e))
      )
    }
  }

  not_loaded <- Filter(function(pkg)
    !suppressPackageStartupMessages(
      requireNamespace(pkg, quietly = TRUE) &&
        require(pkg, character.only = TRUE, quietly = TRUE)
    ),
    pkgs)

  if (length(not_loaded) > 0) {
    warning("Could not load: ", paste(not_loaded, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

load_packages(packages)


# --------------------------------------------------------------
# 2. ggplot2 theme
# --------------------------------------------------------------

theme_set(theme_minimal(base_size = 12))


# --------------------------------------------------------------
# 3. Color palettes
# --------------------------------------------------------------

palettes <- list(
  BlueEyes  = "#488AC7",
  Slate     = "#757C88",

  Clr10 = c(
    "#176D90", "#841618", "#F18821", "#3D7337", "#A039B7",
    "#0D9BD8", "#E73F4B", "#FFB86F", "#6FC14F", "#9E6EAC"
  ),

  ClrGrey = c(
    "#000000", "#696969", "#808080", "#A9A9A9",
    "#C0C0C0", "#D3D3D3", "#DCDCDC"
  ),

  Clr3Dark   = c("#175B7F", "#6F0E10", "#F18821"),
  Clr3Normal = c("#4C88A1", "#98393D", "#F5AC4A"),

  ClrORB = c("#A01523", "#374251", "#9D8B8C", "#818495", "#C8CBD0"),

  ClrBlueRed  = c("#175B7F", "#4A708B", "#A9A9A9", "#872727", "#6F0E10"),

  ClrBlueRed2 = c(
    "#45a3d5ff", "#e4f5faff", "#fbd0a2ff", "#f28983ff", "#d8657eff"
  ),

  ClrBlueRed3 = c("#0B3C5D", "#5A95C9", "#E5E5E5", "#B03030", "#400000"),

  ClrBlueRed7 = c(
    "#175B7F", "#477A95", "#2591CC",
    "#A9A9A9",
    "#BD5153", "#883A3C", "#6F0E10"
  ),

  ClrBlue4vax = c("#6F0E10", "#84959e", "#3c6e8a", "#175B7F"),
  ClrBlue2    = c("#C8CBD0", "#175B7F")
)


# --------------------------------------------------------------
# 4. Plotly font styles
# --------------------------------------------------------------

define_font_styles <- function(family, sizes) {
  lapply(sizes, function(size) list(family = family, size = size))
}

font_styles <- list(
  NotoSans  = define_font_styles("Noto Sans",        c(11, 12, 13, 14, 15, 16, 26)),
  NotoSansL = define_font_styles("Noto Sans Light",  c(11, 12, 13, 14, 15, 16, 26)),
  NotoSansB = define_font_styles("Noto Sans SemBd",  c(11, 12, 13, 14, 15, 16, 26))
)

fntlist <- list(
  noto11  = list(family = "Noto Sans", size = 11),
  noto12  = list(family = "Noto Sans", size = 12),
  noto13  = list(family = "Noto Sans", size = 13),
  noto14  = list(family = "Noto Sans", size = 14),
  noto15  = list(family = "Noto Sans", size = 15),
  noto16  = list(family = "Noto Sans", size = 16),
  noto18  = list(family = "Noto Sans", size = 18),
  noto22  = list(family = "Noto Sans", size = 22),
  notoL14 = list(family = "Noto Sans Light", size = 14),
  notoL15 = list(family = "Noto Sans Light", size = 15),
  notoL16 = list(family = "Noto Sans Light", size = 16),
  notoL18 = list(family = "Noto Sans Light", size = 18),
  notoB14 = list(family = "Noto Sans SemBd", size = 14),
  notoB16 = list(family = "Noto Sans SemBd", size = 16),
  notoB18 = list(family = "Noto Sans SemBd", size = 18),
  notoB22 = list(family = "Noto Sans SemBd", size = 22)
)

mlstyle <- list(color = "black", width = 1)
