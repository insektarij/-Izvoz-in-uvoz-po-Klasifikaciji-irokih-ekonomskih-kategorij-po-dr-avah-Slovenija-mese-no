# app.R
rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(shiny)
  library(pxR)
  library(tidyverse)
  library(data.table)
  library(scales)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(leaflet)
  library(DT)
  library(htmltools)
  library(plotly)
  library(countrycode)  # slovenska imena držav (CLDR)
})

# ---------------------------
# Color palette (UNIFIED)
# ---------------------------
COL_EXPORT  <- "#BFE6BF"  # pastelno zelena (izvoz / pozitivno)
COL_IMPORT  <- "#F2B9B9"  # pastelno rdeča (uvoz / negativno)
COL_BALANCE <- "#BFD7FF"  # pastelno modra (bilanca samo pri črtnih grafih)

# temnejše barve samo za linije (bolj vidno)
COL_EXPORT_LINE <- "#2E7D32"
COL_IMPORT_LINE <- "#C62828"

# ---------------------------
# Helpers (DT safe + formatting)
# ---------------------------

tooltip_meur <- function(x) {
  label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ",")(x)
}

dt_safe_df <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  
  for (nm in names(df)) {
    v <- df[[nm]]
    
    if (is.list(v)) {
      v <- vapply(v, function(z) paste(z, collapse = " "), character(1))
    }
    if (inherits(v, "Date") || inherits(v, "POSIXt")) v <- as.character(v)
    if (is.factor(v)) v <- as.character(v)
    
    v <- as.vector(v)
    v <- unname(v)
    attributes(v) <- NULL
    df[[nm]] <- v
  }
  
  names(df) <- make.unique(names(df))
  rownames(df) <- NULL
  df
}

fmt_eur_m <- function(x) {
  out <- label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ",")(x)
  out[is.na(x) | !is.finite(x)] <- "NA"
  out
}

# ---------------------------
# Slovenska imena držav (CLDR)
# ---------------------------
country_name_vec_sl <- function(sf_dat) {
  iso2 <- sf_dat$iso_a2
  
  nm_fallback <- sf_dat$name_long
  if (is.null(nm_fallback)) nm_fallback <- sf_dat$name
  if (is.null(nm_fallback)) nm_fallback <- sf_dat$admin
  if (is.null(nm_fallback)) nm_fallback <- rep("?", nrow(sf_dat))
  
  nm_sl <- suppressWarnings(countrycode(
    sourcevar   = iso2,
    origin      = "iso2c",
    destination = "cldr.name.sl",
    warn        = FALSE
  ))
  
  nm_sl <- ifelse(!is.na(iso2) & iso2 == "XK", "Kosovo", nm_sl)
  
  out <- ifelse(is.na(nm_sl) | nm_sl == "", as.character(nm_fallback), as.character(nm_sl))
  out
}

# ---------------------------
# PX file resolver (repo)
# ---------------------------

PX_FILENAME <- "Izvoz in uvoz po Klasifikaciji širokih ekonomskih kategorij, po državah, Slovenija, mesečno.px"

resolve_px_path <- function(fname = PX_FILENAME) {
  p1 <- file.path("data", fname)
  p2 <- file.path(fname)
  
  if (file.exists(p1)) return(normalizePath(p1, winslash = "/", mustWork = TRUE))
  if (file.exists(p2)) return(normalizePath(p2, winslash = "/", mustWork = TRUE))
  
  return(p1)
}

# ---------------------------
# Data prep
# ---------------------------

read_px_to_dt <- function(px_path, encoding = "CP1250") {
  px_obj <- read.px(px_path, encoding = encoding)
  df <- as.data.frame(px_obj) |> as_tibble()
  dt <- as.data.table(df)
  
  setnames(
    dt,
    old = c("MESEC", "KLASIFIKACIJA.ŠIROKIH.EKONOMSKIH.KATEGORIJ", "DRŽAVA", "UVOZ.IZVOZ"),
    new = c("MESEC", "KSEK", "DRZAVA", "TOK")
  )
  
  dt[, LETO := as.integer(substr(as.character(MESEC), 1, 4))]
  dt
}

prep_annual <- function(dt) {
  dt_tot <- dt[
    ENOTA == "EUR" &
      grepl("^00\\s+Države\\s+-\\s+SKUPAJ", as.character(DRZAVA)) &
      grepl("^00\\s+Klasifikacija\\s+širokih\\s+ekonomskih\\s+kategorij\\s+-\\s+SKUPAJ", as.character(KSEK))
  ]
  
  letno_long <- dt_tot[, .(EUR = sum(value, na.rm = TRUE)), by = .(LETO, TOK)]
  setorder(letno_long, LETO, TOK)
  
  letno <- dcast(letno_long, LETO ~ TOK, value.var = "EUR", fill = 0)
  if (!("Uvoz" %in% names(letno)))  letno[, Uvoz := 0]
  if (!("Izvoz" %in% names(letno))) letno[, Izvoz := 0]
  setnames(letno, old = c("Uvoz", "Izvoz"), new = c("Uvoz_EUR", "Izvoz_EUR"))
  
  letno[, Bilanca_EUR := Izvoz_EUR - Uvoz_EUR]
  setcolorder(letno, c("LETO", "Izvoz_EUR", "Uvoz_EUR", "Bilanca_EUR"))
  
  letno_tbl <- as_tibble(letno)
  
  letno_tbl_meur <- letno_tbl |>
    mutate(
      `Izvoz (M€)`   = Izvoz_EUR / 1e6,
      `Uvoz (M€)`    = Uvoz_EUR / 1e6,
      `Bilanca (M€)` = Bilanca_EUR / 1e6
    ) |>
    select(LETO, `Izvoz (M€)`, `Uvoz (M€)`, `Bilanca (M€)`)
  
  list(dt_tot = dt_tot, letno_tbl = letno_tbl, letno_tbl_meur = letno_tbl_meur)
}

make_annual_plots <- function(letno_tbl) {
  
  letno_trade_long <- letno_tbl |>
    pivot_longer(cols = c(Izvoz_EUR, Uvoz_EUR), names_to = "Tok", values_to = "EUR") |>
    mutate(Tok = recode(Tok, Izvoz_EUR = "Izvoz", Uvoz_EUR = "Uvoz"))
  
  p_trade <- ggplot(letno_trade_long, aes(x = LETO, y = EUR, color = Tok)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    scale_color_manual(values = c("Izvoz" = COL_EXPORT, "Uvoz" = COL_IMPORT)) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(title = "Skupni uvoz in izvoz blaga (EUR) po letih", x = "Leto", y = "EUR", color = "") +
    theme_minimal(base_size = 12)
  
  p_balance <- ggplot(letno_tbl, aes(
    x = LETO, y = Bilanca_EUR, fill = Bilanca_EUR >= 0,
    text = paste0("Leto: ", LETO, "<br>Bilanca: ", tooltip_meur(Bilanca_EUR))
  )) +
    geom_col() +
    scale_fill_manual(values = c(`TRUE` = COL_EXPORT, `FALSE` = COL_IMPORT), guide = "none") +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(title = "Trgovinska bilanca (Izvoz – Uvoz) po letih", x = "Leto", y = "EUR") +
    theme_minimal(base_size = 12)
  
  letno_all_long <- letno_tbl |>
    pivot_longer(cols = c(Izvoz_EUR, Uvoz_EUR, Bilanca_EUR), names_to = "Serija", values_to = "EUR") |>
    mutate(Serija = recode(Serija, Izvoz_EUR = "Izvoz", Uvoz_EUR = "Uvoz", Bilanca_EUR = "Bilanca"))
  
  p_all <- ggplot(letno_all_long, aes(x = LETO, y = EUR, color = Serija)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("Izvoz" = COL_EXPORT, "Uvoz" = COL_IMPORT, "Bilanca" = COL_BALANCE)) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(title = "Izvoz, uvoz in bilanca po letih (EUR)", x = "Leto", y = "EUR", color = "") +
    theme_minimal(base_size = 12)
  
  list(p_trade = p_trade, p_balance = p_balance, p_all = p_all)
}

prep_world_bal <- function(dt) {
  dt_map <- dt[
    ENOTA == "EUR" &
      grepl("^00\\s+Klasifikacija\\s+širokih\\s+ekonomskih\\s+kategorij\\s+-\\s+SKUPAJ", as.character(KSEK)) &
      !grepl("^00\\s+Države\\s+-\\s+SKUPAJ", as.character(DRZAVA))
  ]
  
  dt_map[, DRZAVA_KODA := trimws(substr(as.character(DRZAVA), 1, 2))]
  
  country_year_long <- dt_map[
    , .(EUR = sum(value, na.rm = TRUE)),
    by = .(DRZAVA_KODA, LETO, TOK)
  ]
  
  country_year <- dcast(country_year_long, DRZAVA_KODA + LETO ~ TOK, value.var = "EUR", fill = 0)
  if (!("Uvoz" %in% names(country_year)))  country_year[, Uvoz := 0]
  if (!("Izvoz" %in% names(country_year))) country_year[, Izvoz := 0]
  setnames(country_year, old = c("Uvoz", "Izvoz"), new = c("Uvoz_EUR", "Izvoz_EUR"))
  country_year[, Bilanca_EUR := Izvoz_EUR - Uvoz_EUR]
  
  world <- ne_countries(scale = "medium", returnclass = "sf") |>
    mutate(DRZAVA_KODA = iso_a2)
  
  world_bal <- world |>
    left_join(as_tibble(country_year), by = "DRZAVA_KODA")
  
  years_available <- sort(unique(country_year$LETO))
  
  list(world_bal = world_bal, years_available = years_available)
}

prep_growth <- function(dt_tot) {
  # letno YoY
  letno_long <- dt_tot[, .(EUR = sum(value, na.rm = TRUE)), by = .(LETO, TOK)]
  letno <- dcast(letno_long, LETO ~ TOK, value.var = "EUR", fill = 0)
  if (!("Uvoz" %in% names(letno)))  letno[, Uvoz := 0]
  if (!("Izvoz" %in% names(letno))) letno[, Izvoz := 0]
  setnames(letno, old = c("Uvoz", "Izvoz"), new = c("Uvoz_EUR", "Izvoz_EUR"))
  setorder(letno, LETO)
  
  lag_I <- shift(letno$Izvoz_EUR, 1)
  lag_U <- shift(letno$Uvoz_EUR, 1)
  letno[, Izvoz_YoY_pct := fifelse(is.na(lag_I) | lag_I == 0, NA_real_, 100 * (Izvoz_EUR / lag_I - 1))]
  letno[, Uvoz_YoY_pct  := fifelse(is.na(lag_U) | lag_U == 0, NA_real_, 100 * (Uvoz_EUR  / lag_U - 1))]
  letno_tbl <- as_tibble(letno)
  
  # mesečno
  m_long <- dt_tot[, .(EUR = sum(value, na.rm = TRUE)), by = .(MESEC, TOK)]
  m_long[, LETO := as.integer(substr(as.character(MESEC), 1, 4))]
  m_long[, MES  := as.integer(substr(as.character(MESEC), 6, 7))]
  m_long[, DATE := as.Date(sprintf("%04d-%02d-01", LETO, MES))]
  
  m_ts <- dcast(m_long, DATE + LETO + MES ~ TOK, value.var = "EUR", fill = 0)
  setorder(m_ts, DATE)
  if (!("Uvoz" %in% names(m_ts)))  m_ts[, Uvoz := 0]
  if (!("Izvoz" %in% names(m_ts))) m_ts[, Izvoz := 0]
  setnames(m_ts, old = c("Uvoz", "Izvoz"), new = c("Uvoz_EUR", "Izvoz_EUR"))
  
  lagI1  <- shift(m_ts$Izvoz_EUR, 1)
  lagU1  <- shift(m_ts$Uvoz_EUR, 1)
  lagI12 <- shift(m_ts$Izvoz_EUR, 12)
  lagU12 <- shift(m_ts$Uvoz_EUR, 12)
  
  m_ts[, Izvoz_MoM_pct := fifelse(is.na(lagI1)  | lagI1  == 0, NA_real_, 100 * (Izvoz_EUR / lagI1  - 1))]
  m_ts[, Uvoz_MoM_pct  := fifelse(is.na(lagU1)  | lagU1  == 0, NA_real_, 100 * (Uvoz_EUR  / lagU1  - 1))]
  m_ts[, Izvoz_YoY_pct := fifelse(is.na(lagI12) | lagI12 == 0, NA_real_, 100 * (Izvoz_EUR / lagI12 - 1))]
  m_ts[, Uvoz_YoY_pct  := fifelse(is.na(lagU12) | lagU12 == 0, NA_real_, 100 * (Uvoz_EUR  / lagU12 - 1))]
  m_tbl <- as_tibble(m_ts)
  
  # četrtletno
  m_ts_dt <- as.data.table(m_ts)
  m_ts_dt[, MES := as.integer(format(DATE, "%m"))]
  m_ts_dt[, LETO := as.integer(format(DATE, "%Y"))]
  m_ts_dt[, QTR := (MES - 1) %/% 3 + 1]
  
  q_ts <- m_ts_dt[, .(
    Izvoz_EUR = sum(Izvoz_EUR, na.rm = TRUE),
    Uvoz_EUR  = sum(Uvoz_EUR,  na.rm = TRUE)
  ), by = .(LETO, QTR)]
  q_ts[, Q_DATE := as.Date(sprintf("%04d-%02d-01", LETO, (QTR - 1) * 3 + 1))]
  setorder(q_ts, Q_DATE)
  
  lagIQ1 <- shift(q_ts$Izvoz_EUR, 1)
  lagUQ1 <- shift(q_ts$Uvoz_EUR, 1)
  lagIQ4 <- shift(q_ts$Izvoz_EUR, 4)
  lagUQ4 <- shift(q_ts$Uvoz_EUR, 4)
  
  q_ts[, Izvoz_QoQ_pct   := fifelse(is.na(lagIQ1) | lagIQ1 == 0, NA_real_, 100 * (Izvoz_EUR / lagIQ1 - 1))]
  q_ts[, Uvoz_QoQ_pct    := fifelse(is.na(lagUQ1) | lagUQ1 == 0, NA_real_, 100 * (Uvoz_EUR  / lagUQ1 - 1))]
  q_ts[, Izvoz_YoY_q_pct := fifelse(is.na(lagIQ4) | lagIQ4 == 0, NA_real_, 100 * (Izvoz_EUR / lagIQ4 - 1))]
  q_ts[, Uvoz_YoY_q_pct  := fifelse(is.na(lagUQ4) | lagUQ4 == 0, NA_real_, 100 * (Uvoz_EUR  / lagUQ4 - 1))]
  q_tbl <- as_tibble(q_ts)
  
  list(
    letno_yoy_tbl = letno_tbl,
    monthly_tbl   = m_tbl,
    quarterly_tbl = q_tbl
  )
}

# ---------------------------
# Plotly: popravi barve v legendi + odstrani duplikate
# ---------------------------
style_growth_plotly <- function(p) {
  b <- plotly_build(p)
  
  seen <- character(0)
  for (i in seq_along(b$x$data)) {
    nm <- b$x$data[[i]]$name
    if (is.null(nm) || is.na(nm) || nm == "") next
    
    # določimo barvo po imenu serije
    col <- NULL
    if (grepl("^Izvoz", nm)) col <- COL_EXPORT_LINE
    if (grepl("^Uvoz",  nm)) col <- COL_IMPORT_LINE
    
    if (!is.null(col)) {
      # linija + marker
      if (!is.null(b$x$data[[i]]$line))   b$x$data[[i]]$line$color   <- col
      if (!is.null(b$x$data[[i]]$marker)) b$x$data[[i]]$marker$color <- col
      # včasih line/marker še ne obstajata
      if (is.null(b$x$data[[i]]$line))   b$x$data[[i]]$line   <- list(color = col, width = 3)
      if (is.null(b$x$data[[i]]$marker)) b$x$data[[i]]$marker <- list(color = col)
    }
    
    # odstrani podvojene legend entry-je (facet duplicira trace)
    if (nm %in% seen) {
      b$x$data[[i]]$showlegend <- FALSE
    } else {
      b$x$data[[i]]$showlegend <- TRUE
      seen <- c(seen, nm)
    }
  }
  
  b |> layout(hoverlabel = list(align = "left"))
}

# ---------------------------
# Growth plots (z obdobjem)
# ---------------------------

make_growth_plots <- function(growth, year_from, year_to) {
  
  # varnost
  if (is.null(year_from) || is.null(year_to)) {
    year_from <- min(growth$letno_yoy_tbl$LETO, na.rm = TRUE)
    year_to   <- max(growth$letno_yoy_tbl$LETO, na.rm = TRUE)
  }
  if (year_from > year_to) {
    tmp <- year_from; year_from <- year_to; year_to <- tmp
  }
  
  date_from <- as.Date(sprintf("%04d-01-01", year_from))
  date_to   <- as.Date(sprintf("%04d-12-31", year_to))
  
  # ---- Letno YoY (filtrirano po obdobju)
  a <- growth$letno_yoy_tbl |>
    filter(LETO >= year_from, LETO <= year_to) |>
    select(LETO, Izvoz_YoY_pct, Uvoz_YoY_pct) |>
    pivot_longer(cols = c(Izvoz_YoY_pct, Uvoz_YoY_pct), names_to = "Serija", values_to = "pct") |>
    mutate(
      Serija = recode(Serija, Izvoz_YoY_pct = "Izvoz (YoY)", Uvoz_YoY_pct = "Uvoz (YoY)"),
      Serija = factor(Serija, levels = c("Izvoz (YoY)", "Uvoz (YoY)"))
    )
  
  p_annual <- ggplot(a, aes(
    x = LETO, y = pct / 100, color = Serija,
    text = paste0("Leto: ", LETO, "<br>", Serija, ": ",
                  percent(pct / 100, accuracy = 0.1, decimal.mark = ","))
  )) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_line(linewidth = 1.6) +
    geom_point(size = 2.4) +
    scale_color_manual(values = c("Izvoz (YoY)" = COL_EXPORT_LINE, "Uvoz (YoY)" = COL_IMPORT_LINE), drop = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(unique(a$LETO), n = 10)) +
    labs(title = "Letna rast (YoY)", x = "Leto", y = "Sprememba", color = "") +
    theme_minimal(base_size = 12)
  
  # ---- Mesečno (filtrirano po obdobju)
  m <- growth$monthly_tbl |>
    filter(DATE >= date_from, DATE <= date_to) |>
    arrange(DATE) |>
    select(DATE, Izvoz_MoM_pct, Uvoz_MoM_pct, Izvoz_YoY_pct, Uvoz_YoY_pct) |>
    pivot_longer(cols = -DATE, names_to = "Serija", values_to = "pct") |>
    mutate(
      Serija = recode(
        Serija,
        Izvoz_MoM_pct = "Izvoz (MoM)",
        Uvoz_MoM_pct  = "Uvoz (MoM)",
        Izvoz_YoY_pct = "Izvoz (YoY)",
        Uvoz_YoY_pct  = "Uvoz (YoY)"
      ),
      Serija = factor(Serija, levels = c("Izvoz (MoM)","Uvoz (MoM)","Izvoz (YoY)","Uvoz (YoY)")),
      Tip = ifelse(grepl("\\(MoM\\)", Serija), "MoM", "YoY")
    )
  
  p_monthly <- ggplot(m, aes(
    x = DATE, y = pct / 100, color = Serija,
    text = paste0("Datum: ", format(DATE, "%Y-%m"), "<br>",
                  Serija, ": ", percent(pct / 100, accuracy = 0.1, decimal.mark = ","))
  )) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 1.9) +
    facet_wrap(~Tip, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c(
      "Izvoz (MoM)" = COL_EXPORT_LINE,
      "Uvoz (MoM)"  = COL_IMPORT_LINE,
      "Izvoz (YoY)" = COL_EXPORT_LINE,
      "Uvoz (YoY)"  = COL_IMPORT_LINE
    ), drop = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
    labs(title = "Mesečni indeksi rasti", x = "Mesec", y = "Sprememba", color = "") +
    theme_minimal(base_size = 12)
  
  # ---- Četrtletno (filtrirano po obdobju)
  q <- growth$quarterly_tbl |>
    filter(Q_DATE >= date_from, Q_DATE <= date_to) |>
    arrange(Q_DATE) |>
    mutate(Q_lab = paste0(LETO, " Q", QTR)) |>
    select(Q_DATE, Q_lab, Izvoz_QoQ_pct, Uvoz_QoQ_pct, Izvoz_YoY_q_pct, Uvoz_YoY_q_pct) |>
    pivot_longer(cols = c(Izvoz_QoQ_pct, Uvoz_QoQ_pct, Izvoz_YoY_q_pct, Uvoz_YoY_q_pct),
                 names_to = "Serija", values_to = "pct") |>
    mutate(
      Serija = recode(
        Serija,
        Izvoz_QoQ_pct   = "Izvoz (QoQ)",
        Uvoz_QoQ_pct    = "Uvoz (QoQ)",
        Izvoz_YoY_q_pct = "Izvoz (YoY)",
        Uvoz_YoY_q_pct  = "Uvoz (YoY)"
      ),
      Serija = factor(Serija, levels = c("Izvoz (QoQ)","Uvoz (QoQ)","Izvoz (YoY)","Uvoz (YoY)")),
      Tip = ifelse(grepl("\\(QoQ\\)", Serija), "QoQ", "YoY")
    )
  
  p_quarter <- ggplot(q, aes(
    x = Q_DATE, y = pct / 100, color = Serija,
    text = paste0("Četrtletje: ", Q_lab, "<br>",
                  Serija, ": ", percent(pct / 100, accuracy = 0.1, decimal.mark = ","))
  )) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 1.9) +
    facet_wrap(~Tip, ncol = 1, scales = "free_y") +
    scale_color_manual(values = c(
      "Izvoz (QoQ)" = COL_EXPORT_LINE,
      "Uvoz (QoQ)"  = COL_IMPORT_LINE,
      "Izvoz (YoY)" = COL_EXPORT_LINE,
      "Uvoz (YoY)"  = COL_IMPORT_LINE
    ), drop = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
    labs(title = "Četrtletni indeksi rasti", x = "Čas", y = "Sprememba", color = "") +
    theme_minimal(base_size = 12)
  
  list(p_annual = p_annual, p_monthly = p_monthly, p_quarter = p_quarter)
}

# ---------------------------
# Barplot helpers for map tab
# ---------------------------

topN_plot_fill <- function(dat_year_sf, value_col, title_txt, fill_color) {
  nm <- country_name_vec_sl(dat_year_sf)
  
  tbl <- dat_year_sf |>
    st_drop_geometry() |>
    mutate(Drzava = nm) |>
    filter(!is.na(.data[[value_col]]), is.finite(.data[[value_col]])) |>
    arrange(desc(.data[[value_col]])) |>
    slice_head(n = 10)
  
  ggplot(tbl, aes(
    x = reorder(Drzava, .data[[value_col]]), y = .data[[value_col]],
    text = paste0("Država: ", Drzava, "<br>", title_txt, ": ", tooltip_meur(.data[[value_col]]))
  )) +
    geom_col(fill = fill_color) +
    coord_flip() +
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ",")) +
    labs(title = title_txt, x = NULL, y = NULL) +
    theme_minimal(base_size = 12)
}

top5_surplus_deficit_plots <- function(dat_year_sf, year_choice) {
  nm <- country_name_vec_sl(dat_year_sf)
  
  base <- dat_year_sf |>
    st_drop_geometry() |>
    mutate(Drzava = nm) |>
    filter(!is.na(Bilanca_EUR), is.finite(Bilanca_EUR))
  
  top_surplus <- base |>
    filter(Bilanca_EUR > 0) |>
    arrange(desc(Bilanca_EUR)) |>
    slice_head(n = 5) |>
    transmute(Drzava, Value = Bilanca_EUR) |>
    arrange(desc(Value)) |>
    mutate(Drzava = factor(Drzava, levels = Drzava))
  
  top_deficit <- base |>
    filter(Bilanca_EUR < 0) |>
    arrange(Bilanca_EUR) |>
    slice_head(n = 5) |>
    transmute(Drzava, Value = -Bilanca_EUR) |>
    arrange(desc(Value)) |>
    mutate(Drzava = factor(Drzava, levels = Drzava))
  
  p_surplus <- ggplot(top_surplus, aes(
    x = Drzava, y = Value,
    text = paste0("Država: ", Drzava, "<br>Presežek: ", tooltip_meur(Value))
  )) +
    geom_col(fill = COL_EXPORT) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ","),
      limits = c(0, NA)
    ) +
    labs(title = paste0("5 držav z največjim presežkom (Izvoz > Uvoz), ", year_choice), x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  p_deficit <- ggplot(top_deficit, aes(
    x = Drzava, y = Value,
    text = paste0("Država: ", Drzava, "<br>Deficit: ", tooltip_meur(Value))
  )) +
    geom_col(fill = COL_IMPORT) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ","),
      limits = c(0, NA)
    ) +
    labs(title = paste0("5 držav z največjim deficitom (Uvoz > Izvoz), ", year_choice), x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  list(surplus = p_surplus, deficit = p_deficit)
}

# ---------------------------
# UI
# ---------------------------

ui <- fluidPage(
  titlePanel("Pregled bilanc uvoza in izvoza - Slovenija 2000 - 2025"),
  sidebarLayout(
    sidebarPanel(
      helpText(paste0("PX datoteka iz repozitorija: ", PX_FILENAME)),
      actionButton("load_btn", "Izračunaj", class = "btn-primary"),
      hr(),
      uiOutput("year_ui"),
      uiOutput("growth_range_ui"),
      hr(),
      selectInput(
        "map_var",
        "Spremenljivka prikaza zemljevida",
        choices = c("Bilanca" = "Bilanca_EUR", "Uvoz" = "Uvoz_EUR", "Izvoz" = "Izvoz_EUR"),
        selected = "Bilanca_EUR"
      ),
      checkboxInput("map_winsor", "Manj ekstremnih barv na zemljevidih", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Letne spremembe",
          DTOutput("tbl_annual"),
          br(),
          plotOutput("plot_trade", height = 320),
          plotlyOutput("plot_balance", height = 320),
          plotOutput("plot_all", height = 320)
        ),
        tabPanel(
          "Zemljevid",
          leafletOutput("leaf_map", height = 620),
          br(),
          plotlyOutput("top_exporters", height = 320),
          plotlyOutput("top_importers", height = 320),
          br(),
          plotlyOutput("top_surplus", height = 360),
          plotlyOutput("top_deficit", height = 360)
        ),
        tabPanel(
          "Indeksi rasti",
          h4("Letno (YoY)"),
          DTOutput("tbl_growth_annual"),
          plotlyOutput("growth_plot_annual", height = 320),
          hr(),
          h4("Mesečno (najnovejše zgoraj)"),
          DTOutput("tbl_growth_monthly_tail"),
          plotlyOutput("growth_plot_monthly", height = 420),
          hr(),
          h4("Četrtletno (najnovejše zgoraj)"),
          DTOutput("tbl_growth_quarter_tail"),
          plotlyOutput("growth_plot_quarter", height = 420)
        )
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    dt = NULL,
    dt_tot = NULL,
    letno_tbl = NULL,
    letno_tbl_meur = NULL,
    annual_plots = NULL,
    world_bal = NULL,
    years_available = NULL,
    growth = NULL,
    growth_plots = NULL
  )
  
  observeEvent(input$load_btn, {
    px_path <- resolve_px_path(PX_FILENAME)
    
    if (!file.exists(px_path)) {
      showNotification(
        paste0("Ne najdem .px v repozitoriju. Pričakovana pot: ", px_path,
               " | Namig: daj datoteko v mapo data/ ali v root repo."),
        type = "error",
        duration = NULL
      )
      return()
    }
    
    withProgress(message = "Uvoz in izračun ...", value = 0, {
      dt <- read_px_to_dt(px_path, encoding = "CP1250")
      rv$dt <- dt
      
      a <- prep_annual(dt)
      rv$dt_tot <- a$dt_tot
      rv$letno_tbl <- a$letno_tbl
      rv$letno_tbl_meur <- a$letno_tbl_meur
      rv$annual_plots <- make_annual_plots(rv$letno_tbl)
      
      w <- prep_world_bal(dt)
      rv$world_bal <- w$world_bal
      rv$years_available <- w$years_available
      
      rv$growth <- prep_growth(rv$dt_tot)
      
      showNotification("Izračunano. Izberi leto zemljevida in obdobje indeksov rasti.", type = "message")
    })
  }, ignoreInit = TRUE)
  
  output$year_ui <- renderUI({
    req(rv$years_available)
    selectInput("year_choice", "Leto Zemljevida", choices = rv$years_available, selected = max(rv$years_available))
  })
  
  output$growth_range_ui <- renderUI({
    req(rv$years_available)
    ymin <- min(rv$years_available, na.rm = TRUE)
    ymax <- max(rv$years_available, na.rm = TRUE)
    
    sliderInput(
      "growth_year_range",
      "Obdobje za indekse rasti (grafi)",
      min = ymin, max = ymax,
      value = c(max(ymin, ymax - 10), ymax),
      sep = "",
      step = 1
    )
  })
  
  growth_year_from <- reactive({
    req(input$growth_year_range)
    as.integer(input$growth_year_range[1])
  })
  growth_year_to <- reactive({
    req(input$growth_year_range)
    as.integer(input$growth_year_range[2])
  })
  
  # ---- Letno: tabela v M€ + barvanje vrstic po bilanci (rdeča/zelena)
  output$tbl_annual <- renderDT({
    req(rv$letno_tbl_meur)
    
    df <- rv$letno_tbl_meur |> as.data.frame()
    datatable(
      dt_safe_df(df),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)", "Bilanca (M€)"), digits = 1) |>
      formatStyle(
        "Bilanca (M€)",
        target = "row",
        backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))
      )
  })
  
  # ---- Letno: grafi
  output$plot_trade <- renderPlot({
    req(rv$annual_plots)
    rv$annual_plots$p_trade
  })
  
  output$plot_all <- renderPlot({
    req(rv$annual_plots)
    rv$annual_plots$p_all
  })
  
  output$plot_balance <- renderPlotly({
    req(rv$annual_plots)
    ggplotly(rv$annual_plots$p_balance, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  # ---- Indeksi rasti: grafi (z izbranim obdobjem)
  growth_plots_now <- reactive({
    req(rv$growth, input$growth_year_range)
    make_growth_plots(rv$growth, growth_year_from(), growth_year_to())
  })
  
  output$growth_plot_annual <- renderPlotly({
    req(growth_plots_now())
    p <- ggplotly(growth_plots_now()$p_annual, tooltip = "text")
    style_growth_plotly(p)
  })
  
  output$growth_plot_monthly <- renderPlotly({
    req(growth_plots_now())
    p <- ggplotly(growth_plots_now()$p_monthly, tooltip = "text")
    style_growth_plotly(p)
  })
  
  output$growth_plot_quarter <- renderPlotly({
    req(growth_plots_now())
    p <- ggplotly(growth_plots_now()$p_quarter, tooltip = "text")
    style_growth_plotly(p)
  })
  
  # ---- Zemljevid: podatki za leto
  dat_year <- reactive({
    req(rv$world_bal, input$year_choice)
    rv$world_bal |> dplyr::filter(LETO == as.integer(input$year_choice))
  })
  
  output$leaf_map <- renderLeaflet({
    req(dat_year(), input$map_var)
    dat <- dat_year()
    nm <- country_name_vec_sl(dat)
    
    val_raw <- dat[[input$map_var]]
    finite_vals <- val_raw[is.finite(val_raw)]
    is_balance <- identical(input$map_var, "Bilanca_EUR")
    
    if (length(finite_vals) == 0) {
      return(leaflet(dat) |> addProviderTiles(providers$CartoDB.Positron))
    }
    
    if (is_balance) {
      q <- if (isTRUE(input$map_winsor)) quantile(finite_vals, probs = c(0.02, 0.98), na.rm = TRUE) else range(finite_vals, na.rm = TRUE)
      M <- max(abs(q))
      domain <- c(-M, M)
      val_plot <- pmin(pmax(val_raw, domain[1]), domain[2])
      
      pal <- colorNumeric(colorRampPalette(c("red", "white", "green"))(256), domain = domain, na.color = "#d9d9d9")
      labels <- sprintf("<strong>%s</strong><br/>%s", htmlEscape(nm), htmlEscape(fmt_eur_m(val_raw))) |> lapply(HTML)
      
      leaflet(dat) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = pal(val_plot), weight = 0.25, opacity = 1, color = "#444444", fillOpacity = 0.88,
                    label = labels, highlightOptions = highlightOptions(weight = 1.2, color = "#000000", bringToFront = TRUE)) |>
        addLegend(position = "bottomright", pal = pal, values = val_plot,
                  title = paste0("Bilanca (M€), ", input$year_choice),
                  labFormat = labelFormat(transform = function(x) x / 1e6, suffix = " M€"))
    } else {
      val_pos <- pmax(val_raw, 0)
      finite_pos <- val_pos[is.finite(val_pos)]
      upper <- if (isTRUE(input$map_winsor)) as.numeric(quantile(finite_pos, probs = 0.98, na.rm = TRUE)) else max(finite_pos, na.rm = TRUE)
      if (!is.finite(upper) || upper <= 0) upper <- 1
      domain <- c(0, upper)
      val_plot <- pmin(pmax(val_pos, domain[1]), domain[2])
      
      pal <- switch(
        input$map_var,
        "Uvoz_EUR"  = colorNumeric(colorRampPalette(c("white", "steelblue"))(256), domain = domain, na.color = "#d9d9d9"),
        "Izvoz_EUR" = colorNumeric(colorRampPalette(c("white", "darkgreen"))(256), domain = domain, na.color = "#d9d9d9")
      )
      
      labels <- sprintf("<strong>%s</strong><br/>%s", htmlEscape(nm), htmlEscape(fmt_eur_m(val_pos))) |> lapply(HTML)
      
      leaflet(dat) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = pal(val_plot), weight = 0.25, opacity = 1, color = "#444444", fillOpacity = 0.88,
                    label = labels, highlightOptions = highlightOptions(weight = 1.2, color = "#000000", bringToFront = TRUE)) |>
        addLegend(position = "bottomright", pal = pal, values = val_plot,
                  title = if (identical(input$map_var, "Uvoz_EUR")) paste0("Uvoz (M€), ", input$year_choice) else paste0("Izvoz (M€), ", input$year_choice),
                  labFormat = labelFormat(transform = function(x) x / 1e6, suffix = " M€"))
    }
  })
  
  # ---- Zemljevid tab: stolpci INTERAKTIVNO
  output$top_exporters <- renderPlotly({
    req(dat_year(), input$year_choice)
    p <- topN_plot_fill(dat_year(), "Izvoz_EUR", paste0("Top 10 največjih izvoznikov, ", input$year_choice), COL_EXPORT)
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_importers <- renderPlotly({
    req(dat_year(), input$year_choice)
    p <- topN_plot_fill(dat_year(), "Uvoz_EUR", paste0("Top 10 največjih uvoznikov, ", input$year_choice), COL_IMPORT)
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_surplus <- renderPlotly({
    req(dat_year(), input$year_choice)
    pp <- top5_surplus_deficit_plots(dat_year(), input$year_choice)
    ggplotly(pp$surplus, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_deficit <- renderPlotly({
    req(dat_year(), input$year_choice)
    pp <- top5_surplus_deficit_plots(dat_year(), input$year_choice)
    ggplotly(pp$deficit, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  # ---------------------------
  # INDEKSI RASTI: tabele v M€ + barvanje celic % (+ zeleno, - rdeče)
  # (tabele ostanejo "najnovejše zgoraj", slider je za grafe)
  # ---------------------------
  
  output$tbl_growth_annual <- renderDT({
    req(rv$growth)
    
    df <- rv$growth$letno_yoy_tbl |>
      transmute(
        LETO,
        `Izvoz (M€)` = Izvoz_EUR / 1e6,
        `Uvoz (M€)`  = Uvoz_EUR  / 1e6,
        Izvoz_YoY_pct = Izvoz_YoY_pct / 100,
        Uvoz_YoY_pct  = Uvoz_YoY_pct  / 100
      ) |>
      arrange(desc(LETO))
    
    datatable(dt_safe_df(df), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_YoY_pct","Uvoz_YoY_pct"), digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_YoY_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$tbl_growth_monthly_tail <- renderDT({
    req(rv$growth)
    
    df <- rv$growth$monthly_tbl |>
      transmute(
        DATE,
        `Izvoz (M€)` = Izvoz_EUR / 1e6,
        `Uvoz (M€)`  = Uvoz_EUR  / 1e6,
        Izvoz_MoM_pct = Izvoz_MoM_pct / 100,
        Uvoz_MoM_pct  = Uvoz_MoM_pct  / 100,
        Izvoz_YoY_pct = Izvoz_YoY_pct / 100,
        Uvoz_YoY_pct  = Uvoz_YoY_pct  / 100
      ) |>
      arrange(desc(DATE)) |>
      slice_head(n = 36)
    
    datatable(dt_safe_df(df), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_MoM_pct","Uvoz_MoM_pct","Izvoz_YoY_pct","Uvoz_YoY_pct"),
                       digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_MoM_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_MoM_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Izvoz_YoY_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$tbl_growth_quarter_tail <- renderDT({
    req(rv$growth)
    
    df <- rv$growth$quarterly_tbl |>
      transmute(
        Q_DATE,
        LETO,
        QTR,
        `Izvoz (M€)` = Izvoz_EUR / 1e6,
        `Uvoz (M€)`  = Uvoz_EUR  / 1e6,
        Izvoz_QoQ_pct   = Izvoz_QoQ_pct   / 100,
        Uvoz_QoQ_pct    = Uvoz_QoQ_pct    / 100,
        Izvoz_YoY_q_pct = Izvoz_YoY_q_pct / 100,
        Uvoz_YoY_q_pct  = Uvoz_YoY_q_pct  / 100
      ) |>
      arrange(desc(Q_DATE)) |>
      slice_head(n = 24)
    
    datatable(dt_safe_df(df), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_QoQ_pct","Uvoz_QoQ_pct","Izvoz_YoY_q_pct","Uvoz_YoY_q_pct"),
                       digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_QoQ_pct",   backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_QoQ_pct",    backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Izvoz_YoY_q_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_q_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
}

shinyApp(ui, server)
