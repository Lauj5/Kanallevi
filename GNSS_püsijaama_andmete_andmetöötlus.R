# Käesoleva R-keele skripti koostamisel on kasutatud tehisintellekti abi
# (Claude, Anthropic). Autor on koodi üle vaadanud, kohandanud ja
# kontrollinud selle vastavust töö eesmärkidele.

rm(list = ls()); graphics.off()

# ============================================================
# 0. PACKAGES & SETUP
# ============================================================
reticulate::use_python("C:/Users/lauri/AppData/Local/Programs/Python/Python310/python.exe", required = TRUE)
Sys.setlocale("LC_ALL",  ".UTF-8")          # UTF-8 (Windows) HTML-väljundi jaoks
Sys.setlocale("LC_TIME", "Estonian_Estonia") # Eesti k. kuupäevad (peab tulema viimasena)

library(plotly); library(htmlwidgets); library(htmltools)
library(svDialogs); library(dplyr); library(lubridate)
library(tidyr)

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  sp <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                 error = function(e) "")
  if (nzchar(sp)) setwd(dirname(sp))
}

# ============================================================
# 1. KONFIGURATSIOON
# ============================================================
# --- Bändid ja signaalid ---
BANDS <- list(
  L1 = c("BDSS1P", "GALS1C", "GLOS1C", "GPSS1C", "BDSS2I"),
  L2 = c("GPSS2S", "GLOS2C", "GLOS2P", "BDSS6I", "GALS6C"),
  L5 = c("BDSS5P", "GALS5Q", "GPSS5Q", "BDSS7D", "BDSS7I", "GALS7Q")
)
bands_enabled <- c(L1 = TRUE, L2 = TRUE, L5 = TRUE)

band_colors  <- c(L1 = "royalblue", L2 = "darkgreen", L5 = "purple")
band_symbols <- c(L1 = "circle",    L2 = "square",    L5 = "diamond")

# --- Režiimide määramine ---
# regime_mode: "manual" = kasuta regime_periods_manual listi
#              "off"    = kogu periood ühe võrdlustasemega
regime_mode <- "manual"

# Kasutusel ainult kui regime_mode = "manual"
regime_periods_manual <- list(
  list(name = "Periood enne seadme vahetust",
       start = NA_character_, end = "2024-05-29 10:29:59"),
  list(name = "Periood peale seadme vahetust",
       start = "2024-05-29 10:30:00", end = NA_character_)
    # Siia võib lisada juurde perioode eristuse ajahetki
  # list(name = "Täiendav perioodi eristus",
  #      start = "2024-05-29 10:30:00", end = NA_character_)
)

# --- Detektsiooni parameetrid ---
ref_quantile        <- 0.60   # Baseline'i kvantiil signaali kohta
k_mad               <- 3.5    # Hajuvuse kordaja
min_drop_db         <- 2.5    # Minimaalne absoluutne langus (dB)
min_signals_for_jam <- 3      # Mitu signaali peab bändis langema

# ============================================================
# 2. ANDMETE LUGEMINE
# ============================================================
file_path <- dlgOpen(title = "Vali GNSS CSV fail",
                     filters = "CSV Files (*.csv)|*.csv")$res
if (!nzchar(file_path)) stop("Faili ei valitud.")

# Jaama nimi (kasutusel hooaja-occupancy salvestuses)
station_name <- dlgInput(message = "Sisesta jaama nimi (nt 'Hiiumaa')",
                         default = tools::file_path_sans_ext(
                           basename(file_path)))$res
if (!length(station_name) || !nzchar(station_name)) station_name <- "unknown"
station_name <- gsub("[^A-Za-z0-9_-]", "_", station_name)  # sanitize

id <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
if (!"timestamp" %in% names(id)) stop("Column 'timestamp' is missing.")

id$timestamp       <- as.POSIXct(id$timestamp, tz = "UTC")
id                 <- id[!is.na(id$timestamp), ][order(id$timestamp), ]
id$timestamp_local <- with_tz(id$timestamp, tzone = "Europe/Tallinn")

clean_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "N/A", "NA", "NULL")] <- NA
  as.numeric(gsub(",", ".", x, fixed = FALSE))
}

# Ainult lubatud bändide signaalid
active_band_names <- names(bands_enabled)[bands_enabled]
signal_cols_requested <- unlist(BANDS[active_band_names], use.names = FALSE)
signal_cols <- intersect(signal_cols_requested, names(id))

missing_signals <- setdiff(signal_cols_requested, names(id))
if (length(missing_signals) > 0)
  warning("Missing signal columns skipped: ",
          paste(missing_signals, collapse = ", "))
if (length(signal_cols) == 0)
  stop("No enabled signal columns were found in the data file.")

id[signal_cols] <- lapply(id[signal_cols], clean_num)

active_signals_by_band <- lapply(BANDS, function(b) intersect(b, signal_cols))

cat("\nFail:", basename(file_path), "| read:", nrow(id),
    "| periood:", as.character(min(id$timestamp_local)),
    "...", as.character(max(id$timestamp_local)), "\n")

# ============================================================
# 3. ABIFUNKTSIOONID
# ============================================================
get_signal_band <- function(sig) {
  names(BANDS)[sapply(BANDS, function(b) sig %in% b)][1]
}

parse_dt <- function(x, tz = "UTC") {
  if (is.null(x) || !length(x) || is.na(x) || !nzchar(x))
    return(as.POSIXct(NA, tz = tz))
  as.POSIXct(x, tz = tz)
}

rolling_apply <- function(x, window, fun, min_n = 5) {
  n <- length(x); hw <- floor(window / 2)
  sapply(seq_len(n), function(i) {
    v <- x[max(1, i - hw):min(n, i + hw)]
    v <- v[!is.na(v)]
    if (length(v) >= min_n) fun(v) else NA_real_
  })
}

# Tunnise telje helperid (kasutuses graafikutel 2 ja 3)
HOUR_ORIGIN <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
to_tod      <- function(h) HOUR_ORIGIN + h * 3600
hour_xaxis  <- list(
  title     = "Kellaaeg",
  tickformat = "%H:%M",
  range     = c(to_tod(-0.5), to_tod(24.5)),
  dtick     = 3600000,
  tickangle = -45
)

# ============================================================
# 4. REŽIIMIDE MÄÄRAMINE
# ============================================================
assign_regimes <- function(df, regime_periods, tz = "UTC") {
  if (length(regime_periods) == 0) {
    df$regime <- factor("R_all", levels = "R_all")
    return(df)
  }
  
  regime_vec <- rep(NA_character_, nrow(df))
  regime_names <- character(0)
  
  for (rp in regime_periods) {
    start_dt <- parse_dt(rp$start, tz = tz)
    end_dt   <- parse_dt(rp$end,   tz = tz)
    idx <- rep(TRUE, nrow(df))
    if (!is.na(start_dt)) idx <- idx & (df$timestamp_local >= start_dt)
    if (!is.na(end_dt))   idx <- idx & (df$timestamp_local <= end_dt)
    regime_vec[idx] <- rp$name
    regime_names <- c(regime_names, rp$name)
  }
  
  if (any(is.na(regime_vec)))
    stop(sprintf("%d rida ei kuulu ühtegi režiimi. Kontrolli regime_periods.",
                 sum(is.na(regime_vec))))
  
  df$regime <- factor(regime_vec, levels = unique(regime_names))
  df
}

# Vali regime_periods vastavalt mode'ile
regime_periods <- switch(regime_mode,
  "manual" = regime_periods_manual,
   "off"    = list(),
   stop("Vale regime_mode: peab olema 'manual' või 'off'")
)

id <- assign_regimes(id, regime_periods, tz = "UTC")
cat("\nRežiimid kasutuses:\n"); print(table(id$regime, useNA = "ifany"))

# ============================================================
# 5. SEGAMISE TUVASTAMINE (per band)
# ============================================================
# Režiimi sees: per-signal drop & flag, siis per-band count, median, flag
detect_jamming_band <- function(df, sigs, band_name) {
  count_col  <- paste0("drop_count_",   band_name)
  median_col <- paste0("median_drop_",  band_name)
  flag_col   <- paste0("jamming_flag_", band_name)
  
  df[[count_col]]  <- 0L
  df[[median_col]] <- NA_real_
  df[[flag_col]]   <- FALSE
  
  if (length(sigs) == 0) return(df)
  
  # Per-signaali drop_db ja drop_flag veerud
  for (sig in sigs) {
    df[[paste0(sig, "_drop_db")]]   <- NA_real_
    df[[paste0(sig, "_drop_flag")]] <- FALSE
  }
  
  min_sig_eff <- max(1, min(min_signals_for_jam, length(sigs)))
  
  for (rg in levels(df$regime)) {
    idx <- which(df$regime == rg)
    if (!length(idx)) next
    
    # Per-signaali drop & flag
    for (sig in sigs) {
      x <- df[[sig]][idx]
      if (sum(!is.na(x)) < 10) next
      
      ref_val    <- quantile(x, ref_quantile, na.rm = TRUE, type = 8)
      spread_val <- max(0.25, mad(x, center = median(x, na.rm = TRUE),
                                   constant = 1.4826, na.rm = TRUE))
      threshold  <- max(min_drop_db, k_mad * spread_val)
      
      # Salvesta võrdlustaseme info hilisemaks tabeliks
      ref_table_collector[[length(ref_table_collector) + 1L]] <<- data.frame(
        regime    = rg,
        band      = band_name,
        signal    = sig,
        n_obs     = sum(!is.na(x)),
        ref_val   = round(ref_val, 2),
        mad_val   = round(spread_val, 3),
        threshold = round(threshold, 2),
        stringsAsFactors = FALSE
      )
      
      drop_vals <- ref_val - x
      df[[paste0(sig, "_drop_db")]][idx]   <- drop_vals
      df[[paste0(sig, "_drop_flag")]][idx] <- !is.na(drop_vals) &
        (drop_vals >= threshold)
    }
    
    # Bändi tasemel agregatsioon
    flag_cols <- paste0(sigs, "_drop_flag")
    drop_cols <- paste0(sigs, "_drop_db")
    
    df[[count_col]][idx] <- rowSums(df[idx, flag_cols, drop = FALSE],
                                     na.rm = TRUE)
    df[[median_col]][idx] <- apply(df[idx, drop_cols, drop = FALSE], 1,
                                    function(v) {
                                      if (all(is.na(v))) NA_real_
                                      else median(v, na.rm = TRUE)
                                    })
    df[[flag_col]][idx] <- !is.na(df[[median_col]][idx]) &
      df[[count_col]][idx] >= min_sig_eff
  }
  df
}

ref_table_collector <- list()

for (bn in names(bands_enabled)) {
  if (!bands_enabled[[bn]]) next
  id <- detect_jamming_band(id, active_signals_by_band[[bn]], bn)
}

# Võrdlustaseme tabel — iga signaali ja iga režiimi kohta
ref_table <- if (length(ref_table_collector)) {
  do.call(rbind, ref_table_collector)
} else {
  data.frame()
}

if (nrow(ref_table)) {
  # Sorteeri loetavalt
  ref_table <- ref_table[order(ref_table$regime, ref_table$band,
                                ref_table$signal), ]
  rownames(ref_table) <- NULL
  
  cat("\n=== Võrdlustaseme tabel (per signaal, per režiim) ===\n")
  print(ref_table, row.names = FALSE)
  
  # Salvesta CSV-na sama kausta kuhu CSV
  ref_csv <- file.path(dirname(file_path),
                       paste0(station_name, "_vordlustaseme_tabel.csv"))
  write.csv2(ref_table, ref_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nVõrdlustaseme tabel salvestatud:", ref_csv, "\n")
}

# ============================================================
# 6. SÜNDMUSTE ETTEVALMISTAMINE VISUALISEERIMISEKS
# ============================================================
all_events <- bind_rows(lapply(names(BANDS), function(bn) {
  if (!bands_enabled[[bn]]) return(NULL)
  flag_col <- paste0("jamming_flag_",  bn)
  drop_col <- paste0("median_drop_",   bn)
  if (!(flag_col %in% names(id))) return(NULL)
  
  data.frame(
    timestamp_local = id$timestamp_local,
    flag = id[[flag_col]],
    drop = id[[drop_col]],
    band = bn
  ) %>%
    filter(!is.na(timestamp_local), flag == TRUE, !is.na(drop)) %>%
    mutate(year = year(timestamp_local))
})) %>%
  mutate(band = factor(band, levels = names(BANDS)))

# Aastate vahemik kogu andmestiku põhjal (mitte ainult sündmuste)
data_years_range <- range(year(id$timestamp_local), na.rm = TRUE)
years_to_plot <- seq(data_years_range[1], data_years_range[2])

first_year_by_band <- {
  default_year <- data_years_range[1]
  fb <- setNames(rep(default_year, length(BANDS)), names(BANDS))
  if (nrow(all_events)) {
    seen <- all_events %>% group_by(band) %>%
      summarise(first_year = min(year), .groups = "drop")
    seen_v <- setNames(seen$first_year, as.character(seen$band))
    # kirjuta üle ainult need bändid, kus tegelikult sündmusi oli
    fb[names(seen_v)] <- seen_v
  }
  fb
}
legend_rank_map <- c(L1 = 1, L2 = 2, L5 = 3)

# ============================================================
# 7. GRAAFIK 1 — AASTANE AJATELG
# ============================================================
y_max_timeline <- if (nrow(all_events))
  max(all_events$drop, na.rm = TRUE) * 1.05 else 1
if (!is.finite(y_max_timeline) || y_max_timeline <= 0) y_max_timeline <- 1

make_year_timeline_plot <- function(events_df, year_value, y_max) {
  x_start <- as.POSIXct(sprintf("%d-01-01 00:00:00", year_value), tz = "UTC")
  x_end   <- as.POSIXct(sprintf("%d-12-31 23:59:59", year_value), tz = "UTC")
  
  p <- plot_ly() %>%
    # "Skelett" trace x-telje kuupäevaliseks fikseerimiseks
    add_markers(x = c(x_start, x_end), y = c(NA_real_, NA_real_),
                showlegend = FALSE, hoverinfo = "skip",
                marker = list(opacity = 0))
  
  for (bn in names(BANDS)) {
    if (!bands_enabled[[bn]]) next
    sub <- events_df %>% filter(year == year_value, band == bn)
    show_legend <- identical(year_value, first_year_by_band[[bn]])
    
    if (nrow(sub) == 0) {
      if (isTRUE(show_legend)) {
        p <- p %>% add_markers(
          x = x_start, y = NA_real_, name = bn, legendgroup = bn,
          legendrank = legend_rank_map[[bn]], showlegend = TRUE,
          marker = list(color = band_colors[[bn]], size = 4),
          hoverinfo = "skip")
      }
    } else {
      p <- p %>% add_markers(
        data = sub, x = ~timestamp_local, y = ~drop,
        name = bn, legendgroup = bn, legendrank = legend_rank_map[[bn]],
        showlegend = isTRUE(show_legend),
        marker = list(color = band_colors[[bn]], size = 4),
        hovertemplate = paste0("Band: ", bn,
          "<br>Aeg: %{x|%Y-%m-%d %H:%M}",
          "<br>Mediaanlangus: %{y:.2f} dB<extra></extra>"))
    }
  }
  
  p %>% layout(
    title = paste("Segamise juhtumid", year_value),
    xaxis = list(title = "Aeg", type = "date",
                 range = c(x_start, x_end), tickformat = "%b %Y"),
    yaxis = list(title = "Mediaan SNR langus (dB)",
                 range = c(0, y_max)))
}

p_timeline <- if (length(years_to_plot)) {
  subplot(lapply(years_to_plot, make_year_timeline_plot,
                 events_df = all_events, y_max = y_max_timeline),
          nrows = length(years_to_plot), shareX = FALSE, shareY = FALSE,
          titleX = TRUE, titleY = TRUE, margin = 0.03) %>%
    layout(title = list(
             text = sprintf("%d - %d GNSS segamise juhtumite ajatelg",
                            min(years_to_plot), max(years_to_plot)),
             y = 0.97, yanchor = "top"),
           margin = list(t = 70),
           showlegend = TRUE) %>% config(locale = "et")
} else {
  plot_ly() %>% layout(title = "Andmeid ei leitud") %>% config(locale = "et")
}

p_timeline

# ============================================================
# 8. GRAAFIK 2 — 24H OCCUPANCY (tunnised tulbad, bändid stacked)
# ============================================================
occ_data <- bind_rows(lapply(rev(names(BANDS)), function(bn) {
  if (!bands_enabled[[bn]]) return(NULL)
  flag_col <- paste0("jamming_flag_", bn)
  
  full <- data.frame(hour_local = 0:23)
  occ <- id %>%
    transmute(hour_local = hour(timestamp_local),
              flag = .data[[flag_col]]) %>%
    filter(flag == TRUE) %>%
    group_by(hour_local) %>%
    summarise(event_count = n(), .groups = "drop")
  
  full %>% left_join(occ, by = "hour_local") %>%
    mutate(event_count = ifelse(is.na(event_count), 0L, event_count),
           band = bn,
           tod_time = to_tod(hour_local))
})) %>% arrange(hour_local, band)

p_occ <- plot_ly(
  data = occ_data, x = ~tod_time, y = ~event_count,
  color = ~band, colors = band_colors, type = "bar",
  hovertemplate = paste0("Band: %{fullData.name}",
    "<br>Kellaaeg: %{x|%H:%M}",
    "<br>Segamise juhtumeid: %{y}<extra></extra>")) %>%
  layout(
    barmode = "stack",
    title = list(text = "Ööpäeva GNSS segamise juhtumite arv",
                 y = 0.97, yanchor = "top"),
    margin = list(t = 70),
    xaxis = hour_xaxis,
    yaxis = list(title = "Segamise juhtumite arv",
                 rangemode = "tozero")) %>%
  config(locale = "et")

p_occ

# ============================================================
# 9. GRAAFIK 3 — 24H INTENSITY (mediaan + min/max vahemik)
# ============================================================
min_events_intensity <- 2

prepare_intensity <- function(df, bn, min_events = 2) {
  flag_col <- paste0("jamming_flag_", bn)
  drop_col <- paste0("median_drop_",  bn)
  
  ev <- df %>%
    transmute(timestamp_local = timestamp_local,
              flag = .data[[flag_col]],
              drop = .data[[drop_col]]) %>%
    filter(flag == TRUE, !is.na(drop)) %>%
    mutate(hour_local = hour(timestamp_local))
  
  data.frame(hour_local = 0:23) %>%
    left_join(
      ev %>% group_by(hour_local) %>%
        summarise(n_events = n(),
                  min_drop    = min(drop,    na.rm = TRUE),
                  median_drop = median(drop, na.rm = TRUE),
                  max_drop    = max(drop,    na.rm = TRUE),
                  .groups = "drop"),
      by = "hour_local") %>%
    mutate(
      n_events    = ifelse(is.na(n_events), 0L, n_events),
      min_drop    = ifelse(n_events >= min_events, min_drop,    NA_real_),
      median_drop = ifelse(n_events >= min_events, median_drop, NA_real_),
      max_drop    = ifelse(n_events >= min_events, max_drop,    NA_real_),
      err_plus  = ifelse(!is.na(max_drop) & !is.na(median_drop),
                         max_drop - median_drop, NA_real_),
      err_minus = ifelse(!is.na(min_drop) & !is.na(median_drop),
                         median_drop - min_drop, NA_real_),
      tod_time = to_tod(hour_local))
}

make_intensity_plot <- function(df, bn, color, y_max) {
  plot_ly(data = df) %>%
    add_markers(
      x = ~tod_time, y = ~median_drop,
      name = paste0(bn, " mediaan"),
      marker = list(color = color, size = 7,
                    line = list(color = "black", width = 0.4)),
      error_y = list(type = "data", symmetric = FALSE,
                     array = ~err_plus, arrayminus = ~err_minus,
                     color = color, thickness = 1.2, width = 3),
      customdata = ~cbind(n_events, min_drop, max_drop),
      hovertemplate = paste0("Band: ", bn,
        "<br>Kellaaeg: %{x|%H:%M}",
        "<br>Mediaan drop: %{y:.2f} dB",
        "<br>Min drop: %{customdata[1]:.2f} dB",
        "<br>Max drop: %{customdata[2]:.2f} dB",
        "<br>Sündmusi tunnis: %{customdata[0]}<extra></extra>")) %>%
    layout(
      title = paste0(bn, " 24h intensity profile"),
      xaxis = hour_xaxis,
      yaxis = list(title = "SNR langus (dB)",
                   range = if (is.finite(y_max) && y_max > 0) c(0, y_max)
                           else NULL,
                   rangemode = "tozero"))
}

int_data <- setNames(
  lapply(names(BANDS), function(bn) {
    if (!bands_enabled[[bn]]) return(NULL)
    prepare_intensity(id, bn, min_events = min_events_intensity)
  }), names(BANDS))

y_max_int <- max(unlist(lapply(int_data, `[[`, "max_drop")),
                 na.rm = TRUE)
if (!is.finite(y_max_int) || y_max_int <= 0) y_max_int <- 1
y_max_int <- y_max_int * 1.10

int_panels <- lapply(names(BANDS), function(bn) {
  if (is.null(int_data[[bn]])) return(NULL)
  make_intensity_plot(int_data[[bn]], bn, band_colors[[bn]], y_max_int)
})
int_panels <- int_panels[!sapply(int_panels, is.null)]

p_int <- subplot(int_panels, nrows = length(int_panels), shareX = FALSE,
                 shareY = TRUE, titleX = TRUE, titleY = TRUE,
                 margin = 0.04) %>%
  layout(title = list(text = "Ööpäeva GNSS segamise tugevus varieeruvus",
                      y = 0.97, yanchor = "top"),
         margin = list(t = 70)) %>%
  config(locale = "et")

p_int

# ============================================================
# 10. GRAAFIK 4 — ÜLILEVI KESTUSTE JAOTUS
# ============================================================
# Iga sündmus = järjestikused jamming_flag=TRUE epohhid.
# Lubatud "auk" sees: max_gap_epochs * 15 min (vaikimisi 1 = 15min auk OK).
# Andmestikus olevaid pikemaid auke (instrument maas) ei loeta sündmuse sisse.
EPOCH_MIN       <- 15      # andmete sammu pikkus minutites
max_gap_epochs  <- 1       # 0 = range, 1 = lubab 15-min augu sees, 2 = 30-min jne

# Bin-piirid minutites + sildid
dur_breaks <- c(0, 15, 30, 60, 120, 240, 480, 1440, Inf)
dur_labels <- c("≤15min", "15-30min", "30-60min", "1-2h", "2-4h",
                "4-8h", "8-24h", ">24h")

# Leia sündmuste algused/lõpud ühe lipu-vektori jaoks (timestamp + flag)
find_events <- function(ts, flag, gap_epochs = max_gap_epochs,
                        epoch_min = EPOCH_MIN) {
  if (!any(flag, na.rm = TRUE)) return(data.frame(
    start = as.POSIXct(character(0), tz = "Europe/Tallinn"),
    end   = as.POSIXct(character(0), tz = "Europe/Tallinn"),
    duration_min = numeric(0)))
  
  ord  <- order(ts)
  ts   <- ts[ord]
  flag <- flag[ord]
  flag[is.na(flag)] <- FALSE
  
  on_idx <- which(flag)
  if (!length(on_idx)) return(data.frame(
    start = as.POSIXct(character(0), tz = "Europe/Tallinn"),
    end   = as.POSIXct(character(0), tz = "Europe/Tallinn"),
    duration_min = numeric(0)))
  
  # Reaalne ajavahe (minutites) järjestikuste flag=TRUE epohhide vahel
  gaps_min <- c(Inf, diff(as.numeric(ts[on_idx])) / 60)
  # Uus sündmus algab, kui vahe > (1 + lubatud_auk) * sammu_pikkus
  new_evt  <- gaps_min > (1 + gap_epochs) * epoch_min
  evt_id   <- cumsum(new_evt)
  
  data.frame(
    start = tapply(ts[on_idx], evt_id, min),
    end   = tapply(ts[on_idx], evt_id, max)
  ) %>% mutate(
    start = as.POSIXct(start, origin = "1970-01-01", tz = "Europe/Tallinn"),
    end   = as.POSIXct(end,   origin = "1970-01-01", tz = "Europe/Tallinn"),
    # Sündmuse pikkus = (lõpp - algus) + üks epohh (sest algusepohh ka kestab)
    duration_min = as.numeric(difftime(end, start, units = "mins")) + epoch_min
  )
}

# Kogu kõikide bändide sündmused
events_by_band <- lapply(names(BANDS), function(bn) {
  if (!bands_enabled[[bn]]) return(NULL)
  flag_col <- paste0("jamming_flag_", bn)
  if (!(flag_col %in% names(id))) return(NULL)
  ev <- find_events(id$timestamp_local, id[[flag_col]])
  if (!nrow(ev)) return(NULL)
  ev$band <- bn
  ev
})
events_by_band <- setNames(events_by_band, names(BANDS))
events_all <- bind_rows(events_by_band[!sapply(events_by_band, is.null)])

if (nrow(events_all)) {
  events_all$band       <- factor(events_all$band, levels = names(BANDS))
  events_all$duration_h <- events_all$duration_min / 60
  events_all$bin        <- cut(events_all$duration_min,
                               breaks = dur_breaks, labels = dur_labels,
                               right = TRUE, include.lowest = TRUE)
  
  # Konsoolis kokkuvõte
  cat("\n=== ÜLILEVI SÜNDMUSTE PIKKUSED ===\n")
  cat(sprintf("(lubatud auk sündmuse sees: %d epohhi = %d min)\n",
              max_gap_epochs, max_gap_epochs * EPOCH_MIN))
  for (bn in levels(events_all$band)) {
    sub <- events_all[events_all$band == bn, ]
    if (!nrow(sub)) next
    cat(sprintf("\n  %s: %d sündmust, kogukestus %.1f h\n",
                bn, nrow(sub), sum(sub$duration_h)))
    cat(sprintf("    pikkus: med=%.0fmin, mean=%.0fmin, max=%.0fmin (%.1fh)\n",
                median(sub$duration_min), mean(sub$duration_min),
                max(sub$duration_min),    max(sub$duration_h)))
  }
  
  # Tulpdiagramm: x = bin, y = arv, värv = bänd (grupeeritud)
  bar_data <- events_all %>%
    count(band, bin, .drop = FALSE) %>%
    arrange(band, bin)
  
  p_dur <- plot_ly(
    data = bar_data, x = ~bin, y = ~n,
    color = ~band, colors = band_colors, type = "bar",
    hovertemplate = paste0("Band: %{fullData.name}",
      "<br>Pikkus: %{x}",
      "<br>Sündmusi: %{y}<extra></extra>")) %>%
    layout(
      barmode = "group",
      title = list(text = "Kanallevi sündmuste pikkuste jaotus",
                   y = 0.97, yanchor = "top"),
      margin = list(t = 70),
      xaxis = list(title = "Sündmuse kestus", categoryorder = "array",
                   categoryarray = dur_labels),
      yaxis = list(title = "Sündmuste arv", rangemode = "tozero")) %>%
    config(locale = "et")
} else {
  cat("\n=== ÜLILEVI SÜNDMUSTE PIKKUSED ===\n  Ühtegi sündmust ei leitud.\n")
  p_dur <- plot_ly() %>% layout(title = "Sündmusi ei leitud") %>%
    config(locale = "et")
}

p_dur

# ============================================================
# 11. HOOAJA-OCCUPANCY (DJV/MAM/JJA/SON) — KONTROLLGRAAFIK
# ============================================================
# Iga (hooaeg, tund, bänd) lahter saab oma nimetaja:
#   pct = (jamming_flag=TRUE epohhe selles aknas) / (kõigi epohhide arv selles aknas)
# Nii ei moonuta tulemust see, kui mõnes hooajas oli andmeid vähem.

month_to_season <- function(m) {
  factor(c("DJV","DJV","MAM","MAM","MAM","JJA",
           "JJA","JJA","SON","SON","SON","DJV")[m],
         levels = c("DJV","MAM","JJA","SON"))
}

id$hour_local <- hour(id$timestamp_local)
id$season     <- month_to_season(month(id$timestamp_local))

# Nimetaja: epohhide arv iga (hooaeg, tund) kohta — sama kõikide bändide jaoks
denom <- id %>%
  count(season, hour_local, name = "n_total") %>%
  tidyr::complete(season, hour_local = 0:23, fill = list(n_total = 0L))

# Lugeja: jam-flagid bändide kaupa, sama (hooaeg, tund) küljed
seasonal_occ <- bind_rows(lapply(names(BANDS), function(bn) {
  if (!bands_enabled[[bn]]) return(NULL)
  flag_col <- paste0("jamming_flag_", bn)
  if (!(flag_col %in% names(id))) return(NULL)
  
  id %>%
    transmute(season, hour_local, flag = .data[[flag_col]]) %>%
    group_by(season, hour_local) %>%
    summarise(n_jam = sum(flag, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(season, hour_local = 0:23, fill = list(n_jam = 0L)) %>%
    left_join(denom, by = c("season", "hour_local")) %>%
    mutate(band = bn,
           pct  = ifelse(n_total > 0, 100 * n_jam / n_total, NA_real_),
           station = station_name)
})) %>%
  select(station, season, hour = hour_local, band, n_jam, n_total, pct) %>%
  arrange(season, band, hour)

# Salvestust eraldi failina ei tehta — hooaja-occupancy saab hiljem
# arvutada toorest "jamming_raw_<jaam>.rds" andmestikust (peatükk 13).
# See plokk arvutab agregeerimise ainult joonise jaoks.

# Kontrollgraafik samale jaamale: 1x4 paneeli (DJV/MAM/JJA/SON)
make_season_panel <- function(season_lbl, df, show_legend = FALSE) {
  sub <- df[df$season == season_lbl, ]
  
  p <- plot_ly()
  for (bn in names(BANDS)) {
    if (!bands_enabled[[bn]]) next
    s <- sub[sub$band == bn, ]
    if (!nrow(s)) next
    p <- p %>% add_lines(
      x = s$hour, y = s$pct, name = bn, legendgroup = bn,
      showlegend = show_legend,
      line = list(color = band_colors[[bn]], width = 2),
      hovertemplate = paste0(season_lbl, " — Band: ", bn,
        "<br>Tund: %{x}:00",
        "<br>Esinemissagedus: %{y:.2f} %<extra></extra>"))
  }
  p %>% layout(
    annotations = list(list(x = 0.5, y = 1.02, xref = "paper", yref = "paper",
                            text = season_lbl, showarrow = FALSE,
                            yanchor = "bottom",
                            font = list(size = 13))),
    xaxis = list(title = "Tund", dtick = 3, range = c(-0.5, 23.5)),
    yaxis = list(range = c(0, 20), rangemode = "tozero"))
}

p_season <- subplot(
  lapply(seq_along(c("DJV","MAM","JJA","SON")), function(i) {
    make_season_panel(c("DJV","MAM","JJA","SON")[i], seasonal_occ,
                      show_legend = (i == 1))
  }),
  nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.02) %>%
  layout(title = list(
           text = "Kanallevi ööpäevane esinemine neljal eri aastaajal",
           y = 0.98, yanchor = "top"),
         margin = list(t = 90),   # ruumi pealkirjale ja hooaja-tähistele
         yaxis = list(title = "Esinemissagedus (%)")) %>%
  config(locale = "et")

p_season

# ============================================================
# 12. TOORTE FLAGIDE + DROP'IDE SALVESTUS (per-jaama analüüsiks)
# ============================================================
# Wide format — iga rida = üks 15-min epohh, veerud bändide kaupa.
# Hiljem 20 jaama .rds-i bind_rows-iga -> kogu Eesti võrgustik samas tabelis,
# millest saab arvutada korrelatsioone, ühiseid sündmusi jms.
flags_wide <- data.frame(
  station         = station_name,
  timestamp_local = id$timestamp_local,
  stringsAsFactors = FALSE
)

for (bn in names(BANDS)) {
  if (!bands_enabled[[bn]]) next
  flag_col <- paste0("jamming_flag_", bn)
  drop_col <- paste0("median_drop_",  bn)
  if (flag_col %in% names(id))
    flags_wide[[paste0("flag_", bn)]] <- id[[flag_col]]
  if (drop_col %in% names(id))
    flags_wide[[paste0("drop_", bn)]] <- id[[drop_col]]
}

flags_path <- file.path(dirname(file_path),
                        paste0("jamming_raw_", station_name, ".rds"))
saveRDS(flags_wide, flags_path, compress = "xz")

n_jam_total <- sum(rowSums(flags_wide[, grep("^flag_", names(flags_wide)),
                                      drop = FALSE], na.rm = TRUE) > 0)
cat(sprintf("\nToored flagid + drop'id salvestatud: %s\n", flags_path))
cat(sprintf("  %d epohhi kokku, %d (%.2f%%) vähemalt ühe bändi flag=TRUE\n",
            nrow(flags_wide), n_jam_total,
            100 * n_jam_total / nrow(flags_wide)))

# ============================================================
# 13. GRAAFIK 5 — SNR + TUVASTUSED
# ============================================================
time_col        <- if ("timestamp_local" %in% names(id)) "timestamp_local" else "timestamp"
time_axis_title <- if (time_col == "timestamp_local") "Aeg" else "Aeg (UTC)"

snr_mat  <- as.matrix(id[signal_cols])
y_top    <- max(snr_mat, na.rm = TRUE)
y_bottom <- min(snr_mat, na.rm = TRUE)

band_ypos <- c(L1 = y_top + 1.5, L2 = y_top + 3.0, L5 = y_top + 4.5)

p_overview <- plot_ly()
trace_band_map <- character(0)

# (a) SNR jooned
for (sig in signal_cols) {
  bn <- get_signal_band(sig)
  p_overview <- p_overview %>% add_lines(
    x = id[[time_col]], y = id[[sig]],
    name = sig, legendgroup = bn,
    line = list(color = band_colors[[bn]], width = 1), opacity = 0.45,
    hovertemplate = paste0("Signal: ", sig, "<br>Band: ", bn,
      "<br>Aeg: %{x|%Y-%m-%d %H:%M}",
      "<br>SNR: %{y:.2f} dB-Hz<extra></extra>"))
  trace_band_map <- c(trace_band_map, bn)
}

# (b) Tuvastuse markerid iga bändi kohta (üks marker, mitte kolm tugevuse klassi)
for (bn in names(BANDS)) {
  if (!bands_enabled[[bn]]) next
  flag_col <- paste0("jamming_flag_", bn)
  if (!(flag_col %in% names(id))) next

  idx <- which(id[[flag_col]])
  if (!length(idx)) next

  p_overview <- p_overview %>% add_markers(
    x = id[[time_col]][idx],
    y = rep(band_ypos[[bn]], length(idx)),
    name = paste(bn, "tuvastus"),
    legendgroup = paste0("DET_", bn),
    marker = list(color = band_colors[[bn]], size = 8,
                  symbol = band_symbols[[bn]],
                  line = list(color = "black", width = 0.5)),
    hovertemplate = paste0("Band: ", bn,
      "<br>Aeg: %{x|%Y-%m-%d %H:%M}<extra></extra>"))
  trace_band_map <- c(trace_band_map, bn)
}

# (c) Filtri nupud
vis_all <- rep(TRUE, length(trace_band_map))
buttons <- list(
  list(method = "update", args = list(list(visible = vis_all)), label = "Kõik"),
  list(method = "update", args = list(list(visible = trace_band_map == "L1")),
       label = "Ainult L1"),
  list(method = "update", args = list(list(visible = trace_band_map == "L2")),
       label = "Ainult L2"),
  list(method = "update", args = list(list(visible = trace_band_map == "L5")),
       label = "Ainult L5"))

p_overview <- p_overview %>% layout(
  title = "Iga signaali SNR + segamise tuvastused",
  xaxis = list(title = time_axis_title),
  yaxis = list(title = "SNR (dB-Hz)", range = c(y_bottom, y_top + 6)),
  updatemenus = list(list(type = "buttons", direction = "right",
                          x = 0.01, y = 1.12, xanchor = "left", yanchor = "top",
                          showactive = TRUE, buttons = buttons)),
  showlegend = TRUE) %>% config(locale = "et")

p_overview

# ============================================================
# 14. HTML SALVESTUS — ainult overview, sama kausta CSV-ga
# ============================================================
# Teised graafikud (timeline, occupancy, intensity, durations, hooaja)
# eksporditakse käsitsi RStudio Vieweri kaudu (Export → Save as Web Page).
out_dir <- dirname(file_path)

overview_path <- file.path(out_dir,
  paste0(station_name, "_iga_signaali_snr_ja_tuvastused.html"))
saveWidget(as_widget(p_overview), file = overview_path,
           selfcontained = TRUE)
cat("\nOverview HTML salvestatud:", overview_path, "\n")


