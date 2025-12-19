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
  library(countrycode)
})

# ---------------------------
# Color palette (UNIFIED)
# ---------------------------
COL_EXPORT       <- "#BFE6BF"  # izvoz / pozitivno (stolpci)
COL_IMPORT       <- "#F2B9B9"  # uvoz / negativno (stolpci)
COL_BALANCE      <- "#BFD7FF"  # bilanca (črtni graf)
COL_EXPORT_LINE  <- "#BFE6BF"
COL_IMPORT_LINE  <- "#F2B9B9"

# ---------------------------
# Helpers
# ---------------------------
tooltip_meur <- function(x) {
  label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ",")(x)
}

fmt_eur_m <- function(x) {
  out <- label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ",")(x)
  out[is.na(x) | !is.finite(x)] <- "NA"
  out
}

dt_safe_df <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    v <- df[[nm]]
    if (is.list(v)) v <- vapply(v, function(z) paste(z, collapse = " "), character(1))
    if (inherits(v, "Date") || inherits(v, "POSIXt")) v <- as.character(v)
    if (is.factor(v)) v <- as.character(v)
    v <- as.vector(v); v <- unname(v); attributes(v) <- NULL
    df[[nm]] <- v
  }
  names(df) <- make.unique(names(df))
  rownames(df) <- NULL
  df
}

dt_opts_left <- function(page_len = 12, scrollX = TRUE) {
  list(
    pageLength = page_len,
    scrollX = scrollX,
    columnDefs = list(list(className = "dt-left", targets = "_all"))
  )
}

plotly_blank <- function(msg = NULL) {
  p <- plot_ly(
    type = "scatter", mode = "markers",
    x = 0, y = 0,
    marker = list(opacity = 0),
    hoverinfo = "skip",
    showlegend = FALSE
  ) |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      margin = list(l = 0, r = 0, t = 20, b = 0)
    )
  if (!is.null(msg)) {
    p <- p |>
      layout(
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = msg, showarrow = FALSE,
          font = list(size = 16, color = "black")
        ))
      )
  }
  p
}

# ---------------------------
# KSEK + DRZAVA helpers
# ---------------------------
ksek_code <- function(x) {
  out <- sub("^\\s*([0-9]{1,3}).*$", "\\1", as.character(x))
  out[!grepl("^\\d+$", out)] <- NA_character_
  out
}

ksek_sanitize_light <- function(selected, all_choices) {
  all_choices <- as.character(all_choices)
  
  selected <- unique(as.character(selected))
  selected <- selected[!is.na(selected) & nzchar(selected)]
  selected <- selected[selected %in% all_choices]
  
  # če ni nič izbrano -> privzeto 00 SKUPAJ (če obstaja)
  if (length(selected) == 0) {
    def <- all_choices[grepl("^00\\s+", all_choices)]
    if (length(def) > 0) return(def[1])
    return(all_choices[1])
  }
  
  sel_codes <- ksek_code(selected)
  is00 <- !is.na(sel_codes) & sel_codes == "00"
  
  # če je 00 skupaj z drugimi -> odstrani 00
  if (any(is00) && length(selected) > 1) {
    selected <- selected[!is00]
  }
  
  # če bi s tem ostalo prazno, vrni 00 ali prvi element
  if (length(selected) == 0) {
    def <- all_choices[grepl("^00\\s+", all_choices)]
    if (length(def) > 0) return(def[1])
    return(all_choices[1])
  }
  
  selected
}

ksek_adjust_last_click <- function(current, previous, all_choices) {
  all_choices <- as.character(all_choices)
  current  <- unique(as.character(current))
  previous <- unique(as.character(previous))
  
  if (length(previous) == 0) {
    return(ksek_sanitize_light(current, all_choices))
  }
  
  added <- setdiff(current, previous)
  out <- current
  
  if (length(added) == 0) {
    return(ksek_sanitize_light(out, all_choices))
  }
  
  for (a in added) {
    ca <- ksek_code(a)
    if (is.na(ca)) next
    
    # če je kliknjen 00 -> pobriši vse ostalo
    if (identical(ca, "00")) {
      out <- a
      break
    }
    
    # če klikneš karkoli drugega -> odstrani 00 iz izbire
    out_codes <- ksek_code(out)
    out <- out[is.na(out_codes) | out_codes != "00"]
    
    # če je dodan otrok: odstrani vse starše (prefix), ki so trenutno izbrani
    for (z in out) {
      cz <- ksek_code(z)
      if (is.na(cz) || identical(cz, "00")) next
      if (nchar(cz) < nchar(ca) && startsWith(ca, cz)) {
        out <- setdiff(out, z)
      }
    }
    
    # če je dodan starš: odstrani vse otroke
    children <- out[sapply(out, function(z) {
      cz <- ksek_code(z)
      !is.na(cz) && !identical(cz, "00") && nchar(cz) > nchar(ca) && startsWith(cz, ca)
    })]
    if (length(children) > 0) out <- setdiff(out, children)
  }
  
  ksek_sanitize_light(out, all_choices)
}

drzava_label_sl_only <- function(x) {
  x <- as.character(x)
  sub("^..\\s+", "", x)
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
  ifelse(is.na(nm_sl) | nm_sl == "", as.character(nm_fallback), as.character(nm_sl))
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

prep_annual <- function(dt, drzava_filter_vec = NULL, ksek_filter_vec = NULL) {
  
  dt0 <- dt[ENOTA == "EUR"]
  
  if (is.null(drzava_filter_vec)) {
    dt0 <- dt0[grepl("^00\\s+Države\\s+-\\s+SKUPAJ", as.character(DRZAVA))]
  } else {
    dt0 <- dt0[as.character(DRZAVA) %in% as.character(drzava_filter_vec)]
  }
  
  if (is.null(ksek_filter_vec)) {
    dt0 <- dt0[grepl("^00\\s+Klasifikacija\\s+širokih\\s+ekonomskih\\s+kategorij\\s+-\\s+SKUPAJ", as.character(KSEK))]
  } else {
    dt0 <- dt0[as.character(KSEK) %in% as.character(ksek_filter_vec)]
  }
  
  dt_tot <- dt0
  
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

# ---------------------------
# Letne spremembe: grafi v M€
# ---------------------------
make_annual_plots <- function(letno_tbl, title_suffix = "") {
  
  letno_tbl_m <- letno_tbl |>
    mutate(
      Izvoz_M = Izvoz_EUR / 1e6,
      Uvoz_M  = Uvoz_EUR  / 1e6,
      Bil_M   = Bilanca_EUR / 1e6
    )
  
  letno_trade_long <- letno_tbl_m |>
    select(LETO, Izvoz_M, Uvoz_M) |>
    pivot_longer(cols = c(Izvoz_M, Uvoz_M), names_to = "Tok", values_to = "M") |>
    mutate(Tok = recode(Tok, Izvoz_M = "Izvoz", Uvoz_M = "Uvoz"))
  
  p_trade <- ggplot(letno_trade_long, aes(x = LETO, y = M, color = Tok)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.8) +
    scale_color_manual(values = c("Izvoz" = COL_EXPORT_LINE, "Uvoz" = COL_IMPORT_LINE)) +
    scale_y_continuous(labels = label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(
      title = paste0("Skupni uvoz in izvoz blaga po letih", title_suffix),
      x = "Leto", y = "M€", color = ""
    ) +
    theme_minimal(base_size = 12)
  
  letno_tbl_m <- letno_tbl_m |>
    mutate(
      Status = factor(
        ifelse(Bilanca_EUR >= 0, "Presežek izvoza", "Presežek uvoza"),
        levels = c("Presežek izvoza", "Presežek uvoza")
      )
    )
  
  p_balance <- ggplot(
    letno_tbl_m,
    aes(
      x = LETO,
      y = Bil_M,
      fill = Status,
      text = paste0(
        "Leto: ", LETO,
        "<br>Bilanca: ",
        label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")(Bil_M)
      )
    )
  ) +
    geom_col() +
    scale_fill_manual(
      values = c("Presežek izvoza" = COL_EXPORT, "Presežek uvoza" = COL_IMPORT),
      breaks = c("Presežek izvoza", "Presežek uvoza"),
      name = ""
    ) +
    scale_y_continuous(labels = label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(
      title = paste0("Trgovinska bilanca (Izvoz – Uvoz) po letih", title_suffix),
      x = "Leto", y = "M€"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  letno_all_long <- letno_tbl_m |>
    select(LETO, Izvoz_M, Uvoz_M, Bil_M) |>
    pivot_longer(cols = c(Izvoz_M, Uvoz_M, Bil_M), names_to = "Serija", values_to = "M") |>
    mutate(Serija = recode(Serija, Izvoz_M = "Izvoz", Uvoz_M = "Uvoz", Bil_M = "Bilanca"))
  
  p_all <- ggplot(letno_all_long, aes(x = LETO, y = M, color = Serija)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = c("Izvoz" = COL_EXPORT_LINE, "Uvoz" = COL_IMPORT_LINE, "Bilanca" = COL_BALANCE)) +
    scale_y_continuous(labels = label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(letno_tbl$LETO, n = 12)) +
    labs(
      title = paste0("Izvoz, uvoz in bilanca po letih", title_suffix),
      x = "Leto", y = "M€", color = ""
    ) +
    theme_minimal(base_size = 12)
  
  list(p_trade = p_trade, p_balance = p_balance, p_all = p_all)
}

prep_world_bal <- function(dt, ksek_filter_vec = NULL) {
  
  dt_map <- dt[ENOTA == "EUR"]
  
  if (is.null(ksek_filter_vec)) {
    dt_map <- dt_map[
      grepl("^00\\s+Klasifikacija\\s+širokih\\s+ekonomskih\\s+kategorij\\s+-\\s+SKUPAJ", as.character(KSEK))
    ]
  } else {
    dt_map <- dt_map[as.character(KSEK) %in% as.character(ksek_filter_vec)]
  }
  
  dt_map <- dt_map[
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
  
  list(letno_yoy_tbl = letno_tbl, monthly_tbl = m_tbl, quarterly_tbl = q_tbl)
}

# ---------------------------
# Indeksi rasti: grafi
# ---------------------------
make_growth_plots <- function(growth, year_from, year_to) {
  if (year_from > year_to) { tmp <- year_from; year_from <- year_to; year_to <- tmp }
  date_from <- as.Date(sprintf("%04d-01-01", year_from))
  date_to   <- as.Date(sprintf("%04d-12-31", year_to))
  
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
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.0) +
    scale_color_manual(values = c("Izvoz (YoY)" = COL_EXPORT_LINE, "Uvoz (YoY)" = COL_IMPORT_LINE), drop = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1, decimal.mark = ",")) +
    scale_x_continuous(breaks = pretty(unique(a$LETO), n = 10)) +
    labs(title = "Letna rast (YoY)", x = "Leto", y = "Sprememba", color = "") +
    theme_minimal(base_size = 12)
  
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
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.6) +
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
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.6) +
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

style_growth_plotly <- function(p) {
  b <- plotly_build(p)
  
  seen <- character(0)
  for (i in seq_along(b$x$data)) {
    tr <- b$x$data[[i]]
    nm <- tr$name
    
    if (is.null(tr$type)) b$x$data[[i]]$type <- "scatter"
    
    has_line <- !is.null(tr$line)
    has_marker <- !is.null(tr$marker)
    
    if (identical(b$x$data[[i]]$type, "scatter")) {
      if (is.null(tr$mode)) {
        if (has_line && has_marker) b$x$data[[i]]$mode <- "lines+markers"
        else if (has_line) b$x$data[[i]]$mode <- "lines"
        else if (has_marker) b$x$data[[i]]$mode <- "markers"
      } else {
        md <- tr$mode
        if (has_marker && !grepl("markers", md)) md <- paste0(md, "+markers")
        if (has_line && !grepl("lines", md)) md <- paste0(md, "+lines")
        b$x$data[[i]]$mode <- md
      }
    }
    
    if (!is.null(nm) && !is.na(nm) && nzchar(nm)) {
      col <- NULL
      if (grepl("^Izvoz", nm)) col <- COL_EXPORT_LINE
      if (grepl("^Uvoz",  nm)) col <- COL_IMPORT_LINE
      
      if (!is.null(col)) {
        if (is.null(b$x$data[[i]]$line))   b$x$data[[i]]$line   <- list()
        if (is.null(b$x$data[[i]]$marker)) b$x$data[[i]]$marker <- list()
        b$x$data[[i]]$line$color   <- col
        b$x$data[[i]]$line$width   <- 3
        b$x$data[[i]]$marker$color <- col
        b$x$data[[i]]$marker$size  <- 6
      }
      
      if (nm %in% seen) b$x$data[[i]]$showlegend <- FALSE
      else { b$x$data[[i]]$showlegend <- TRUE; seen <- c(seen, nm) }
    }
  }
  
  b |> layout(hoverlabel = list(align = "left"))
}

# ---------------------------
# TOP grafi
# ---------------------------
topN_plot_fill <- function(dat_sf, value_col, title_txt, fill_color) {
  nm <- country_name_vec_sl(dat_sf)
  
  tbl <- dat_sf |>
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

top5_surplus_deficit_plots <- function(dat_sf, period_label) {
  nm <- country_name_vec_sl(dat_sf)
  
  base <- dat_sf |>
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
    text = paste0("Država: ", Drzava, "<br>Presežek izvoza: ", tooltip_meur(Value))
  )) +
    geom_col(fill = COL_EXPORT) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ","),
      limits = c(0, NA)
    ) +
    labs(title = paste0("5 držav z največjim presežkom izvoza (Izvoz > Uvoz), ", period_label),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  p_deficit <- ggplot(top_deficit, aes(
    x = Drzava, y = Value,
    text = paste0("Država: ", Drzava, "<br>Presežek uvoza: ", tooltip_meur(Value))
  )) +
    geom_col(fill = COL_IMPORT) +
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = " M€", big.mark = " ", decimal.mark = ","),
      limits = c(0, NA)
    ) +
    labs(title = paste0("5 držav z največjim presežkom uvoza (Uvoz > Izvoz), ", period_label),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  list(surplus = p_surplus, deficit = p_deficit)
}

# ---------------------------
# Plotly animacija zemljevida (ISO-3)
# ---------------------------
make_map_animation_plotly_iso3 <- function(world_bal_sf, years, map_var, winsor = TRUE) {
  
  years <- sort(unique(as.integer(years)))
  if (length(years) < 2) return(plotly_blank("Izberi vsaj 2 leti."))
  
  df <- world_bal_sf |>
    st_drop_geometry() |>
    filter(LETO %in% years) |>
    transmute(
      iso2 = DRZAVA_KODA,
      LETO,
      Uvoz_EUR, Izvoz_EUR, Bilanca_EUR
    ) |>
    mutate(
      iso3 = suppressWarnings(countrycode(iso2, "iso2c", "iso3c", warn = FALSE)),
      iso3 = ifelse(iso2 == "XK", NA_character_, iso3)
    ) |>
    filter(!is.na(iso3))
  
  if (nrow(df) == 0) return(plotly_blank("Ni podatkov za animacijo."))
  
  df <- df |> mutate(val_raw = .data[[map_var]])
  is_balance  <- identical(map_var, "Bilanca_EUR")
  finite_vals <- df$val_raw[is.finite(df$val_raw)]
  if (length(finite_vals) == 0) return(plotly_blank("Ni podatkov za animacijo."))
  
  if (is_balance) {
    q <- if (isTRUE(winsor)) quantile(finite_vals, probs = c(0.02, 0.98), na.rm = TRUE) else range(finite_vals, na.rm = TRUE)
    M <- max(abs(q))
    domain <- c(-M, M)
    df$val_plot <- pmin(pmax(df$val_raw, domain[1]), domain[2])
    colorscale  <- list(list(0.0, "red"), list(0.5, "white"), list(1.0, "green"))
    cbar_title  <- "Bilanca (M€)"
    df$hover    <- paste0("ISO3: ", df$iso3, "<br>Bilanca: ", fmt_eur_m(df$val_raw), "<br>Leto: ", df$LETO)
  } else {
    val_pos <- pmax(df$val_raw, 0)
    finite_pos <- val_pos[is.finite(val_pos)]
    upper <- if (isTRUE(winsor)) as.numeric(quantile(finite_pos, probs = 0.98, na.rm = TRUE)) else max(finite_pos, na.rm = TRUE)
    if (!is.finite(upper) || upper <= 0) upper <- 1
    domain <- c(0, upper)
    df$val_plot <- pmin(pmax(val_pos, domain[1]), domain[2])
    
    if (identical(map_var, "Uvoz_EUR")) {
      colorscale <- list(list(0.0, "white"), list(1.0, "red"))
      cbar_title <- "Uvoz (M€)"
      df$hover   <- paste0("ISO3: ", df$iso3, "<br>Uvoz: ", fmt_eur_m(df$val_raw), "<br>Leto: ", df$LETO)
    } else {
      colorscale <- list(list(0.0, "white"), list(1.0, "darkgreen"))
      cbar_title <- "Izvoz (M€)"
      df$hover   <- paste0("ISO3: ", df$iso3, "<br>Izvoz: ", fmt_eur_m(df$val_raw), "<br>Leto: ", df$LETO)
    }
  }
  
  df$FRAME <- as.character(df$LETO)
  
  year_df <- tibble::tibble(
    FRAME = as.character(years),
    lon = -165,
    lat = 78,
    label = as.character(years)
  )
  
  plot_ly() |>
    add_trace(
      data = df,
      type = "choropleth",
      locations = ~iso3,
      z = ~val_plot,
      frame = ~FRAME,
      locationmode = "ISO-3",
      text = ~hover,
      hoverinfo = "text",
      zmin = domain[1],
      zmax = domain[2],
      colorscale = colorscale,
      colorbar = list(title = cbar_title, ticksuffix = " M€")
    ) |>
    add_trace(
      data = year_df,
      type = "scattergeo",
      mode = "text",
      lon = ~lon,
      lat = ~lat,
      text = ~label,
      frame = ~FRAME,
      hoverinfo = "skip",
      showlegend = FALSE,
      textfont = list(size = 42, color = "black")
    ) |>
    layout(
      annotations = list(),
      geo = list(
        projection = list(type = "natural earth"),
        showframe = FALSE,
        showcoastlines = FALSE,
        bgcolor = "white"
      ),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      margin = list(l = 0, r = 0, t = 10, b = 70)
    ) |>
    animation_opts(frame = 1000, transition = 0, redraw = TRUE) |>
    animation_slider(
      x = 0.05, len = 0.90, y = -0.12,
      currentvalue = list(visible = FALSE)
    ) |>
    animation_button(
      x = 0.05, y = -0.18,
      xanchor = "left", yanchor = "top",
      direction = "left"
    )
}

# ---------------------------
# NOVO: Kategorije izdelkov (široke kategorije 1–7)
# ---------------------------
make_product_category_plots <- function(dt, drzava_vec, mode, year_choice, year_from, year_to,
                                        broad_digits = as.character(1:7),
                                        broad_label_map = NULL) {
  
  if (length(drzava_vec) == 0 || is.null(drzava_vec) || any(is.na(drzava_vec))) {
    return(list(
      tbl = tibble(),
      p_trade = plotly_blank("Ni izbrane države."),
      p_balance = plotly_blank("Ni izbrane države.")
    ))
  }
  
  dt0 <- dt[ENOTA == "EUR" & as.character(DRZAVA) %in% as.character(drzava_vec)]
  if (nrow(dt0) == 0) {
    return(list(
      tbl = tibble(),
      p_trade = plotly_blank("Ni podatkov za izbrano državo."),
      p_balance = plotly_blank("Ni podatkov za izbrano državo.")
    ))
  }
  
  dt0[, KCODE := ksek_code(KSEK)]
  dt0 <- dt0[!is.na(KCODE) & nchar(KCODE) == 1]
  
  dt0 <- dt0[KCODE %in% broad_digits]
  
  if (identical(mode, "year")) {
    dt0 <- dt0[LETO == as.integer(year_choice)]
  } else {
    dt0 <- dt0[LETO >= as.integer(year_from) & LETO <= as.integer(year_to)]
  }
  
  if (nrow(dt0) == 0) {
    return(list(
      tbl = tibble(),
      p_trade = plotly_blank("Ni podatkov za izbrano obdobje."),
      p_balance = plotly_blank("Ni podatkov za izbrano obdobje.")
    ))
  }
  
  agg <- dt0[, .(EUR = sum(value, na.rm = TRUE)), by = .(KCODE, KSEK, TOK)]
  wide <- dcast(agg, KCODE + KSEK ~ TOK, value.var = "EUR", fill = 0)
  if (!("Uvoz" %in% names(wide)))  wide[, Uvoz := 0]
  if (!("Izvoz" %in% names(wide))) wide[, Izvoz := 0]
  setnames(wide, old = c("Uvoz", "Izvoz"), new = c("Uvoz_EUR", "Izvoz_EUR"))
  wide[, Bilanca_EUR := Izvoz_EUR - Uvoz_EUR]
  
  if (!is.null(broad_label_map)) {
    wide[, KLABEL := ifelse(KCODE %in% names(broad_label_map), broad_label_map[KCODE], as.character(KSEK))]
  } else {
    wide[, KLABEL := as.character(KSEK)]
  }
  
  wide[, KCODE_INT := as.integer(KCODE)]
  setorder(wide, KCODE_INT)
  wide[, KCODE_INT := NULL]
  
  tbl <- as_tibble(wide) |>
    transmute(
      Kategorija = KLABEL,
      `Izvoz (M€)`   = Izvoz_EUR / 1e6,
      `Uvoz (M€)`    = Uvoz_EUR / 1e6,
      `Bilanca (M€)` = Bilanca_EUR / 1e6
    )
  
  long_trade <- tbl |>
    pivot_longer(cols = c(`Izvoz (M€)`, `Uvoz (M€)`), names_to = "Tok", values_to = "M") |>
    mutate(Tok = recode(Tok, `Izvoz (M€)` = "Izvoz", `Uvoz (M€)` = "Uvoz"))
  
  p_trade <- ggplot(long_trade, aes(
    x = reorder(Kategorija, M),
    y = M,
    fill = Tok,
    text = paste0("Kategorija: ", Kategorija, "<br>", Tok, ": ",
                  label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")(M))
  )) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    scale_fill_manual(values = c("Izvoz" = COL_EXPORT, "Uvoz" = COL_IMPORT), name = "") +
    labs(title = "Uvoz in izvoz po kategorijah", x = NULL, y = "M€") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  tbl2 <- tbl |>
    mutate(Tok = ifelse(`Bilanca (M€)` >= 0, "Izvoz", "Uvoz"))
  
  p_balance <- ggplot(tbl2, aes(
    x = reorder(Kategorija, `Bilanca (M€)`),
    y = `Bilanca (M€)`,
    fill = Tok,
    text = paste0(
      "Kategorija: ", Kategorija,
      "<br>Bilanca: ",
      label_number(suffix = " M€", big.mark = " ", decimal.mark = ",")(`Bilanca (M€)`)
    )
  )) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Izvoz" = COL_EXPORT, "Uvoz" = COL_IMPORT), name = "") +
    labs(title = "Bilanca po kategorijah (Izvoz – Uvoz)", x = NULL, y = "M€") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
  
  list(tbl = tbl, p_trade = p_trade, p_balance = p_balance)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Pregled bilanc uvoza in izvoza - Slovenija 2000 - 2025"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        paste0(
          "PX datoteka iz repozitorija: ", PX_FILENAME, "\n\n",
          "Podatki pridobljeni iz podatkovne baze SiStat (SURS) - ",
          "Izvoz in uvoz po Standardni mednarodni trgovinski klasifikaciji, po državah, Slovenija, mesečno  ",
          "[ID tabele:  2490301S]"
        )
      ),
      hr(),
      
      uiOutput("ksek_ui"),
      hr(),
      checkboxInput("country_all", "Vse države (SKUPAJ) – samo za grafe", value = TRUE),
      uiOutput("country_ui"),
      hr(),
      
      uiOutput("year_ui"),
      uiOutput("growth_range_ui"),
      radioButtons(
        "map_mode",
        "Vir podatkov v zavihku Zemljevid",
        choices = c("Leto" = "year", "Obdobje (slider)" = "range"),
        selected = "year",
        inline = FALSE
      ),
      radioButtons(
        "annual_mode",
        "Letne spremembe: izbrano obdobje",
        choices = c("Obdobje iz sliderja" = "range", "Celotno obdobje (2000–2025)" = "all"),
        selected = "range",
        inline = FALSE
      ),
      hr(),
      selectInput(
        "map_var",
        "Spremenljivka prikaza zemljevida",
        choices = c("Bilanca" = "Bilanca_EUR", "Uvoz" = "Uvoz_EUR", "Izvoz" = "Izvoz_EUR"),
        selected = "Bilanca_EUR"
      ),
      checkboxInput("map_winsor", "Bolj intenzivne barve manjših vrednosti, manjše razlike med ekstremi (na zemljevidih)", value = TRUE)
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
          "Kategorije izdelkov",
          DTOutput("tbl_categories"),
          br(),
          plotlyOutput("cat_trade", height = 420),
          plotlyOutput("cat_balance", height = 420)
        ),
        tabPanel(
          "Zemljevid",
          leafletOutput("leaf_map", height = 620),
          br(),
          plotlyOutput("map_anim", height = 520),
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
          h4("Mesečno"),
          DTOutput("tbl_growth_monthly"),
          plotlyOutput("growth_plot_monthly", height = 420),
          hr(),
          h4("Četrtletno"),
          DTOutput("tbl_growth_quarter"),
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
    ksek_all = NULL,
    drzava_all = NULL
  )
  
  observeEvent(TRUE, {
    px_path <- resolve_px_path(PX_FILENAME)
    
    if (!file.exists(px_path)) {
      showNotification(
        paste0("Ne najdem .px v repozitoriju. Pričakovana pot: ", px_path,
               " | Namig: datoteko daj v data/ ali v root."),
        type = "error",
        duration = NULL
      )
      return()
    }
    
    withProgress(message = "Uvoz in izračun ...", value = 0, {
      dt <- read_px_to_dt(px_path, encoding = "CP1250")
      rv$dt <- dt
      
      rv$ksek_all <- sort(unique(as.character(dt$KSEK)))
      rv$drzava_all <- sort(unique(as.character(dt$DRZAVA)))
      
      showNotification("Izračunano.", type = "message")
    })
  }, once = TRUE)
  
  # ---------------------------
  # UI: KSEK + DRZAVA
  # ---------------------------
  output$ksek_ui <- renderUI({
    req(rv$ksek_all)
    
    choices_vals <- rv$ksek_all
    codes <- ksek_code(choices_vals)
    
    indent_px <- ifelse(
      !is.na(codes) & codes != "00" & nchar(codes) == 2, 14,
      ifelse(!is.na(codes) & codes != "00" & nchar(codes) >= 3, 28, 0)
    )
    
    choice_labels_html <- sprintf(
      '<span style="display:inline-block; padding-left:%dpx;">%s</span>',
      indent_px,
      htmlEscape(choices_vals)
    )
    
    def <- choices_vals[grepl("^00\\s+", choices_vals)]
    def_sel <- if (length(def) > 0) def[1] else choices_vals[1]
    
    checkboxGroupInput(
      inputId = "ksek_choice",
      label   = "Ekonomska kategorija (velja za vse grafe, tudi Zemljevid)",
      choiceNames  = lapply(choice_labels_html, HTML),
      choiceValues = choices_vals,
      selected     = def_sel
    )
  })
  
  ksek_prev <- reactiveVal(character(0))
  
  observeEvent(input$ksek_choice, {
    req(rv$ksek_all)
    curr <- unique(as.character(input$ksek_choice))
    prev <- isolate(ksek_prev())
    
    fixed <- ksek_adjust_last_click(curr, prev, rv$ksek_all)
    
    if (!identical(sort(fixed), sort(curr))) {
      updateCheckboxGroupInput(session, "ksek_choice", selected = fixed)
      ksek_prev(fixed)
    } else {
      ksek_prev(curr)
    }
  }, ignoreInit = TRUE)
  
  output$country_ui <- renderUI({
    req(rv$drzava_all)
    if (isTRUE(input$country_all)) return(NULL)
    
    drz <- rv$drzava_all
    drz <- drz[!grepl("^00\\s+Države\\s+-\\s+SKUPAJ", drz)]
    
    lab <- drzava_label_sl_only(drz)
    choices_named <- setNames(drz, lab)
    
    selectInput(
      "country_choice",
      "Država (samo za grafe)",
      choices = choices_named,
      selected = if ("DE Nemčija" %in% drz) "DE Nemčija" else drz[1]
    )
  })
  
  ksek_selected <- reactive({
    req(rv$ksek_all)
    ksek_sanitize_light(input$ksek_choice, rv$ksek_all)
  })
  
  drzava_selected <- reactive({
    req(rv$drzava_all)
    if (isTRUE(input$country_all) || is.null(input$country_choice) || !nzchar(input$country_choice)) {
      d0 <- rv$drzava_all[grepl("^00\\s+Države\\s+-\\s+SKUPAJ", rv$drzava_all)]
      if (length(d0) > 0) return(d0[1])
      return("00 Države - SKUPAJ")
    }
    input$country_choice
  })
  
  # ---------------------------
  # Zemljevid: samo KSEK (država filter ne vpliva)
  # ---------------------------
  world_bal_now <- reactive({
    req(rv$dt, ksek_selected())
    prep_world_bal(rv$dt, ksek_filter_vec = ksek_selected())
  })
  
  output$year_ui <- renderUI({
    req(world_bal_now()$years_available)
    ya <- world_bal_now()$years_available
    selectInput("year_choice", "Leto zemljevida", choices = ya, selected = max(ya))
  })
  
  output$growth_range_ui <- renderUI({
    req(world_bal_now()$years_available)
    ya <- world_bal_now()$years_available
    ymin <- min(ya, na.rm = TRUE)
    ymax <- max(ya, na.rm = TRUE)
    sliderInput(
      "growth_year_range",
      "Obdobje (slider) za indekse rasti",
      min = ymin, max = ymax,
      value = c(max(ymin, ymax - 10), ymax),
      sep = "",
      step = 1
    )
  })
  
  growth_year_from <- reactive({ req(input$growth_year_range); as.integer(input$growth_year_range[1]) })
  growth_year_to   <- reactive({ req(input$growth_year_range); as.integer(input$growth_year_range[2]) })
  
  growth_date_from <- reactive(as.Date(sprintf("%04d-01-01", growth_year_from())))
  growth_date_to   <- reactive(as.Date(sprintf("%04d-12-31", growth_year_to())))
  
  period_label_range <- reactive({
    req(growth_year_from(), growth_year_to())
    paste0(growth_year_from(), "–", growth_year_to())
  })
  
  # ---------------------------
  # Letne spremembe + rast: podatki (KSEK + država)
  # ---------------------------
  annual_now <- reactive({
    req(rv$dt, ksek_selected(), drzava_selected())
    prep_annual(
      rv$dt,
      drzava_filter_vec = drzava_selected(),
      ksek_filter_vec   = ksek_selected()
    )
  })
  
  growth_now <- reactive({
    req(annual_now())
    prep_growth(as.data.table(annual_now()$dt_tot))
  })
  
  # ---------------------------
  # Letne spremembe
  # ---------------------------
  letno_tbl_for_annual <- reactive({
    req(annual_now()$letno_tbl)
    if (identical(input$annual_mode, "all")) annual_now()$letno_tbl
    else annual_now()$letno_tbl |> filter(LETO >= growth_year_from(), LETO <= growth_year_to())
  })
  
  annual_title_suffix <- reactive({
    if (identical(input$annual_mode, "all")) "" else paste0(" (", period_label_range(), ")")
  })
  
  annual_plots_now <- reactive({
    req(letno_tbl_for_annual())
    make_annual_plots(letno_tbl_for_annual(), title_suffix = annual_title_suffix())
  })
  
  output$tbl_annual <- renderDT({
    req(annual_now()$letno_tbl_meur)
    df <- annual_now()$letno_tbl_meur |>
      arrange(desc(LETO)) |>
      as.data.frame()
    
    datatable(dt_safe_df(df), options = dt_opts_left(page_len = 20, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)", "Bilanca (M€)"), digits = 1) |>
      formatStyle("Bilanca (M€)", target = "cell",
                  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$plot_trade <- renderPlot({
    req(annual_plots_now())
    annual_plots_now()$p_trade
  })
  
  output$plot_all <- renderPlot({
    req(annual_plots_now())
    annual_plots_now()$p_all
  })
  
  output$plot_balance <- renderPlotly({
    req(annual_plots_now())
    ggplotly(annual_plots_now()$p_balance, tooltip = "text") |>
      layout(hoverlabel = list(align = "left"))
  })
  
  # ---------------------------
  # Indeksi rasti
  # ---------------------------
  growth_plots_now <- reactive({
    req(growth_now(), input$growth_year_range)
    make_growth_plots(growth_now(), growth_year_from(), growth_year_to())
  })
  
  output$growth_plot_annual <- renderPlotly({
    req(growth_plots_now())
    style_growth_plotly(ggplotly(growth_plots_now()$p_annual, tooltip = "text"))
  })
  
  output$growth_plot_monthly <- renderPlotly({
    req(growth_plots_now())
    style_growth_plotly(ggplotly(growth_plots_now()$p_monthly, tooltip = "text"))
  })
  
  output$growth_plot_quarter <- renderPlotly({
    req(growth_plots_now())
    style_growth_plotly(ggplotly(growth_plots_now()$p_quarter, tooltip = "text"))
  })
  
  output$tbl_growth_annual <- renderDT({
    req(growth_now(), input$growth_year_range)
    df <- growth_now()$letno_yoy_tbl |>
      filter(LETO >= growth_year_from(), LETO <= growth_year_to()) |>
      transmute(
        LETO,
        `Izvoz (M€)` = Izvoz_EUR / 1e6,
        `Uvoz (M€)`  = Uvoz_EUR  / 1e6,
        Izvoz_YoY_pct = Izvoz_YoY_pct / 100,
        Uvoz_YoY_pct  = Uvoz_YoY_pct  / 100
      ) |>
      arrange(desc(LETO))
    
    datatable(dt_safe_df(df), options = dt_opts_left(page_len = 15, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_YoY_pct","Uvoz_YoY_pct"), digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_YoY_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$tbl_growth_monthly <- renderDT({
    req(growth_now(), input$growth_year_range)
    df <- growth_now()$monthly_tbl |>
      filter(DATE >= growth_date_from(), DATE <= growth_date_to()) |>
      transmute(
        DATE,
        `Izvoz (M€)` = Izvoz_EUR / 1e6,
        `Uvoz (M€)`  = Uvoz_EUR  / 1e6,
        Izvoz_MoM_pct = Izvoz_MoM_pct / 100,
        Uvoz_MoM_pct  = Uvoz_MoM_pct  / 100,
        Izvoz_YoY_pct = Izvoz_YoY_pct / 100,
        Uvoz_YoY_pct  = Uvoz_YoY_pct  / 100
      ) |>
      arrange(desc(DATE))
    
    datatable(dt_safe_df(df), options = dt_opts_left(page_len = 12, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_MoM_pct","Uvoz_MoM_pct","Izvoz_YoY_pct","Uvoz_YoY_pct"),
                       digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_MoM_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_MoM_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Izvoz_YoY_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$tbl_growth_quarter <- renderDT({
    req(growth_now(), input$growth_year_range)
    df <- growth_now()$quarterly_tbl |>
      filter(Q_DATE >= growth_date_from(), Q_DATE <= growth_date_to()) |>
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
      arrange(desc(Q_DATE))
    
    datatable(dt_safe_df(df), options = dt_opts_left(page_len = 12, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)"), digits = 1) |>
      formatPercentage(columns = c("Izvoz_QoQ_pct","Uvoz_QoQ_pct","Izvoz_YoY_q_pct","Uvoz_YoY_q_pct"),
                       digits = 1, dec.mark = ",") |>
      formatStyle("Izvoz_QoQ_pct",   backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_QoQ_pct",    backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Izvoz_YoY_q_pct", backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT))) |>
      formatStyle("Uvoz_YoY_q_pct",  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  # ---------------------------
  # Kategorije izdelkov (novo)
  # ---------------------------
  broad_label_map <- reactive({
    req(rv$ksek_all)
    out <- setNames(rep(NA_character_, 7), as.character(1:7))
    for (d in 1:7) {
      hit <- rv$ksek_all[grepl(paste0("^", d, "\\s+"), rv$ksek_all)]
      if (length(hit) > 0) out[as.character(d)] <- hit[1]
    }
    out[!is.na(out)]
  })
  
  # vedno prikazuj vse nadkategorije 1..7 (neodvisno od KSEK izbire)
  broad_digits_for_cat_tab <- reactive({
    as.character(1:7)
  })
  
  categories_now <- reactive({
    req(rv$dt, drzava_selected(), input$map_mode, input$year_choice, input$growth_year_range)
    make_product_category_plots(
      dt = rv$dt,
      drzava_vec = drzava_selected(),
      mode = input$map_mode,
      year_choice = input$year_choice,
      year_from = growth_year_from(),
      year_to   = growth_year_to(),
      broad_digits = broad_digits_for_cat_tab(),
      broad_label_map = broad_label_map()
    )
  })
  
  output$tbl_categories <- renderDT({
    req(categories_now())
    df <- categories_now()$tbl |> arrange(Kategorija) |> as.data.frame()
    datatable(dt_safe_df(df), options = dt_opts_left(page_len = 12, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = c("Izvoz (M€)", "Uvoz (M€)", "Bilanca (M€)"), digits = 1) |>
      formatStyle("Bilanca (M€)", target = "cell",
                  backgroundColor = styleInterval(0, c(COL_IMPORT, COL_EXPORT)))
  })
  
  output$cat_trade <- renderPlotly({
    req(categories_now())
    ggplotly(categories_now()$p_trade, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$cat_balance <- renderPlotly({
    req(categories_now())
    ggplotly(categories_now()$p_balance, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  # ---------------------------
  # Zemljevid: podatki leto / obdobje
  # ---------------------------
  dat_year <- reactive({
    req(world_bal_now()$world_bal, input$year_choice)
    world_bal_now()$world_bal |> dplyr::filter(LETO == as.integer(input$year_choice))
  })
  
  dat_year_range <- reactive({
    req(world_bal_now()$world_bal, input$growth_year_range)
    
    df <- world_bal_now()$world_bal |>
      st_drop_geometry() |>
      filter(!is.na(DRZAVA_KODA),
             LETO >= growth_year_from(),
             LETO <= growth_year_to()) |>
      group_by(DRZAVA_KODA) |>
      summarise(
        Uvoz_EUR    = sum(Uvoz_EUR, na.rm = TRUE),
        Izvoz_EUR   = sum(Izvoz_EUR, na.rm = TRUE),
        Bilanca_EUR = sum(Bilanca_EUR, na.rm = TRUE),
        .groups = "drop"
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf") |>
      mutate(DRZAVA_KODA = iso_a2)
    
    world |> left_join(df, by = "DRZAVA_KODA")
  })
  
  dat_map <- reactive({
    req(world_bal_now()$world_bal)
    if (identical(input$map_mode, "range")) dat_year_range() else dat_year()
  })
  
  map_label <- reactive({
    if (identical(input$map_mode, "range")) period_label_range() else as.character(input$year_choice)
  })
  
  # ---------------------------
  # Leaflet map
  # ---------------------------
  output$leaf_map <- renderLeaflet({
    req(dat_map(), input$map_var)
    
    dat <- dat_map()
    nm  <- country_name_vec_sl(dat)
    
    val_raw     <- dat[[input$map_var]]
    finite_vals <- val_raw[is.finite(val_raw)]
    is_balance  <- identical(input$map_var, "Bilanca_EUR")
    
    if (length(finite_vals) == 0) {
      return(leaflet(dat) |> addProviderTiles(providers$CartoDB.Positron))
    }
    
    if (is_balance) {
      q <- if (isTRUE(input$map_winsor)) {
        quantile(finite_vals, probs = c(0.02, 0.98), na.rm = TRUE)
      } else {
        range(finite_vals, na.rm = TRUE)
      }
      
      M      <- max(abs(q))
      domain <- c(-M, M)
      
      val_plot <- pmin(pmax(val_raw, domain[1]), domain[2])
      
      pal <- colorNumeric(
        colorRampPalette(c("red", "white", "green"))(256),
        domain = domain,
        na.color = "#d9d9d9"
      )
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        htmlEscape(nm),
        htmlEscape(fmt_eur_m(val_raw))
      ) |> lapply(HTML)
      
      M_meur <- M / 1e6
      
      legend_html <- sprintf('
<div style="
  background: rgba(255,255,255,0.95);
  padding: 10px 12px;
  border-radius: 10px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.15);
  font-size: 12px;
  line-height: 1.2;
  min-width: 190px;
">
  <div style="font-weight:600; margin-bottom:8px;">Bilanca (M€), %s</div>

  <div style="display:flex; gap:10px; align-items:stretch;">
    <div style="
      width: 18px;
      height: 150px;
      border-radius: 8px;
      border: 1px solid rgba(0,0,0,0.25);
      background: linear-gradient(to bottom, green 0%%, white 50%%, red 100%%);
      position: relative;
    ">
      <div style="
        position:absolute; left:-2px; right:-2px; top:50%%;
        height:1px; background: rgba(0,0,0,0.45);
      "></div>
    </div>

    <div style="display:flex; flex-direction:column; justify-content:space-between;">
      <div>
        <div style="font-weight:600;">Presežek izvoza</div>
        <div style="opacity:0.85;">≈ +%s</div>
      </div>

      <div style="opacity:0.9;">0</div>

      <div>
        <div style="font-weight:600;">Presežek uvoza</div>
        <div style="opacity:0.85;">≈ −%s</div>
      </div>
    </div>
  </div>
</div>
',
                             map_label(),
                             label_number(accuracy = 0.1, big.mark = " ", decimal.mark = ",")(M_meur),
                             label_number(accuracy = 0.1, big.mark = " ", decimal.mark = ",")(M_meur)
      )
      
      leaflet(dat) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor = pal(val_plot),
          weight = 0.25, opacity = 1, color = "#444444", fillOpacity = 0.88,
          label = labels,
          highlightOptions = highlightOptions(weight = 1.2, color = "#000000", bringToFront = TRUE)
        ) |>
        addControl(html = HTML(legend_html), position = "bottomright")
      
    } else {
      val_pos     <- pmax(val_raw, 0)
      finite_pos  <- val_pos[is.finite(val_pos)]
      upper <- if (isTRUE(input$map_winsor)) {
        as.numeric(quantile(finite_pos, probs = 0.98, na.rm = TRUE))
      } else {
        max(finite_pos, na.rm = TRUE)
      }
      if (!is.finite(upper) || upper <= 0) upper <- 1
      
      domain   <- c(0, upper)
      val_plot <- pmin(pmax(val_pos, domain[1]), domain[2])
      
      pal <- switch(
        input$map_var,
        "Uvoz_EUR"  = colorNumeric(colorRampPalette(c("white", "red"))(256), domain = domain, na.color = "#d9d9d9"),
        "Izvoz_EUR" = colorNumeric(colorRampPalette(c("white", "darkgreen"))(256), domain = domain, na.color = "#d9d9d9")
      )
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        htmlEscape(nm),
        htmlEscape(fmt_eur_m(val_raw))
      ) |> lapply(HTML)
      
      leaflet(dat) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor = pal(val_plot),
          weight = 0.25, opacity = 1, color = "#444444", fillOpacity = 0.88,
          label = labels,
          highlightOptions = highlightOptions(weight = 1.2, color = "#000000", bringToFront = TRUE)
        ) |>
        addLegend(
          position = "bottomright", pal = pal, values = val_plot,
          title = if (identical(input$map_var, "Uvoz_EUR")) paste0("Uvoz (M€), ", map_label()) else paste0("Izvoz (M€), ", map_label()),
          labFormat = labelFormat(transform = function(x) x / 1e6, suffix = " M€")
        )
    }
  })
  
  # ---------------------------
  # Plotly animacija (map_anim)
  # ---------------------------
  output$map_anim <- renderPlotly({
    req(world_bal_now()$world_bal, input$growth_year_range, input$map_var)
    
    if (!identical(input$map_mode, "range")) {
      return(plotly_blank("Animacija se pokaže, ko izbereš: Vir podatkov → Obdobje (slider)."))
    }
    
    yrs <- seq.int(growth_year_from(), growth_year_to(), by = 1)
    
    make_map_animation_plotly_iso3(
      world_bal_sf = world_bal_now()$world_bal,
      years = yrs,
      map_var = input$map_var,
      winsor = isTRUE(input$map_winsor)
    )
  })
  
  # ---------------------------
  # TOP grafi (neodvisno od izbire države)
  # ---------------------------
  output$top_exporters <- renderPlotly({
    req(dat_map())
    title_txt <- if (identical(input$map_mode, "range")) {
      paste0("10 najpomembnejših izvoznikov v obdobju ", map_label(), ".")
    } else {
      paste0("10 najpomembnejših izvoznikov v letu ", map_label(), ".")
    }
    p <- topN_plot_fill(dat_map(), "Izvoz_EUR", title_txt, COL_EXPORT)
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_importers <- renderPlotly({
    req(dat_map())
    title_txt <- if (identical(input$map_mode, "range")) {
      paste0("10 najpomembnejših uvoznikov v obdobju ", map_label(), ".")
    } else {
      paste0("10 najpomembnejših uvoznikov v letu ", map_label(), ".")
    }
    p <- topN_plot_fill(dat_map(), "Uvoz_EUR", title_txt, COL_IMPORT)
    ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_surplus <- renderPlotly({
    req(dat_map())
    pp <- top5_surplus_deficit_plots(dat_map(), map_label())
    ggplotly(pp$surplus, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
  
  output$top_deficit <- renderPlotly({
    req(dat_map())
    pp <- top5_surplus_deficit_plots(dat_map(), map_label())
    ggplotly(pp$deficit, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
  })
}

shinyApp(ui, server)

