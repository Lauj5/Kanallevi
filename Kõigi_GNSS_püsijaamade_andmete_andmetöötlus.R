# Käesoleva R-keele skripti koostamisel on kasutatud tehisintellekti abi
# (Claude, Anthropic). Autor on koodi üle vaadanud, kohandanud ja
# kontrollinud selle vastavust töö eesmärkidele.

rm(list = ls()); graphics.off()

# ============================================================
# 0. PACKAGES & SETUP
# ============================================================
Sys.setlocale("LC_ALL",  ".UTF-8")
Sys.setlocale("LC_TIME", "Estonian_Estonia")

library(plotly); library(htmlwidgets); library(htmltools)
library(svDialogs); library(dplyr); library(lubridate); library(tidyr)

# ============================================================
# 1. KONFIGURATSIOON
# ============================================================

# Jaamad, mille .rds failid välistatakse koondist (näiteks erandlikud
# jaamad, mis moonutaksid pilti). Tühi vektor = võta kõik.
EXCLUDE_STATIONS <- c("VERG")   # näiteks c("KIVI") või c("KIVI", "TARTU")

# Bändide värvid
BANDS         <- c("L1", "L2", "L5")
band_colors   <- list(L1 = "royalblue", L2 = "darkgreen", L5 = "purple")
band_symbols  <- list(L1 = "circle",  L2 = "square",  L5 = "diamond")

# Sündmuste agregeerimise parameetrid
EPOCH_MIN      <- 15  # andmete sammu pikkus minutites (ära muuda)

# Maksimaalne lubatud auk sündmuse sees minutites.
# Kahe järjestikuse flag-itud epohhi vahel olev auk peab olema
# <= max_gap_min, et ühendada nad samaks sündmuseks.
# Soovituslikud väärtused:
#   0   = range (iga 15-min auk lõpetab sündmuse)
#   15  = lubab 1 puuduva epohhi (praegune vaikeväärtus)
#   30  = lubab 2 puuduvat epohhi
#   60  = lubab 1 tunnise augu
#   120 = lubab 2 tunnise augu
max_gap_min    <- 15

max_gap_epochs <- max_gap_min %/% EPOCH_MIN  # arvutatakse automaatselt

# Kestuste binnid
# Kuna sammu pikkus on 15 min, on lühimad sündmused 15 min (1 lipp),
# 30 min (2 lippu) jne. Eraldi binnid 15 ja 30 min, edasi 45-60 (3-4 lippu),
# siis tunnipõhised binnid.
dur_breaks <- c(0, 15, 30, 60, 120, 240, 480, 1440, Inf) # minutites
dur_labels <- c("15 min", "30 min", "45-60 min", "1-2 h",
                "2-4 h", "4-8 h", "8-24 h", ">24 h")

# ============================================================
# 2. ANDMETE LUGEMINE
# ============================================================

# Vali kaust, kus on jamming_raw_*.rds failid
rds_dir <- dlgDir(title = "Vali kaust .rds failidega")$res
if (!nzchar(rds_dir)) stop("Kausta ei valitud.")

setwd(rds_dir)

files <- list.files(rds_dir, pattern = "^jamming_raw_.*\\.rds$",
                    full.names = TRUE)
if (!length(files)) stop("Kaustas ei leidu jamming_raw_*.rds faile.")

cat("Leitud", length(files), ".rds faili.\n")

# Loe kõik kokku
all_data_list <- lapply(files, function(f) {
  d <- readRDS(f)
  cat("  ", basename(f), "->", nrow(d), "rida\n")
  d
})
all_data <- do.call(rbind, all_data_list)
rm(all_data_list); gc(verbose = FALSE)

# Veendu, et kohustuslikud veerud on olemas
required_cols <- c("station", "timestamp_local",
                   "flag_L1", "flag_L2", "flag_L5",
                   "drop_L1", "drop_L2", "drop_L5")
missing_cols <- setdiff(required_cols, names(all_data))
if (length(missing_cols))
  stop("Puuduvad veerud: ", paste(missing_cols, collapse = ", "))

# Filter välistatud jaamad
if (length(EXCLUDE_STATIONS)) {
  before <- nrow(all_data)
  all_data <- all_data[!all_data$station %in% EXCLUDE_STATIONS, ]
  cat("Välistatud jaamad:", paste(EXCLUDE_STATIONS, collapse = ", "),
      "— eemaldati", before - nrow(all_data), "rida\n")
}

stations_used <- sort(unique(all_data$station))
cat("\nKasutuses jaamad (", length(stations_used), "):\n",
    paste(stations_used, collapse = ", "), "\n", sep = "")

# Andmete ajavahemik
ts_range <- range(all_data$timestamp_local, na.rm = TRUE)
years_range <- c(year(ts_range[1]), year(ts_range[2]))
cat("\nAndmete ajavahemik:", format(ts_range[1]), "kuni",
    format(ts_range[2]), "\n")

# ============================================================
# 3. ABIFUNKTSIOONID
# ============================================================

# find_events: koondab järjestikused flag=TRUE epohhid sündmusteks
find_events <- function(ts, flag, gap_epochs = max_gap_epochs,
                        epoch_min = EPOCH_MIN) {
  if (!length(ts) || !any(flag, na.rm = TRUE))
    return(data.frame(start = as.POSIXct(character(0)),
                      end   = as.POSIXct(character(0)),
                      duration_min = numeric(0)))
  
  flag[is.na(flag)] <- FALSE
  ord <- order(ts); ts <- ts[ord]; flag <- flag[ord]
  
  ts_flag <- ts[flag]
  if (!length(ts_flag))
    return(data.frame(start = as.POSIXct(character(0)),
                      end   = as.POSIXct(character(0)),
                      duration_min = numeric(0)))
  
  gaps_min <- as.numeric(diff(ts_flag), units = "mins")
  new_event <- c(TRUE, gaps_min > (gap_epochs + 1) * epoch_min)
  event_id  <- cumsum(new_event)
  
  data.frame(
    start = as.POSIXct(tapply(ts_flag, event_id, min),
                       origin = "1970-01-01", tz = tz(ts_flag)),
    end   = as.POSIXct(tapply(ts_flag, event_id, max),
                       origin = "1970-01-01", tz = tz(ts_flag)),
    duration_min = NA_real_,
    stringsAsFactors = FALSE
  ) %>%
    mutate(duration_min = as.numeric(difftime(end, start, units = "mins")) +
                          epoch_min)
}

# Tunnine telg helperid
to_tod <- function(h) as.POSIXct("1970-01-01", tz = "UTC") +
                       as.difftime(h, units = "hours")
hour_xaxis <- list(
  title      = "Kellaaeg",
  tickformat = "%H:%M",
  range      = c(to_tod(-0.5), to_tod(24.5)),
  dtick      = 3600000,
  tickangle  = -45
)

# ============================================================
# 4. SÜNDMUSTE ARVUTAMINE — IGA JAAM, IGA BÄND
# ============================================================

cat("\nArvutan sündmused...\n")

events_all <- do.call(rbind, lapply(stations_used, function(st) {
  sub <- all_data[all_data$station == st, ]
  do.call(rbind, lapply(BANDS, function(bn) {
    flag_col <- paste0("flag_", bn)
    drop_col <- paste0("drop_", bn)
    ev <- find_events(sub$timestamp_local, sub[[flag_col]])
    if (!nrow(ev)) return(NULL)
    
    # Lisa keskmine drop iga sündmuse jaoks
    ev$mean_drop <- vapply(seq_len(nrow(ev)), function(i) {
      idx <- sub$timestamp_local >= ev$start[i] &
             sub$timestamp_local <= ev$end[i] &
             sub[[flag_col]]
      mean(sub[[drop_col]][idx], na.rm = TRUE)
    }, numeric(1))
    
    ev$station <- st
    ev$band    <- bn
    ev
  }))
}))

cat("Kokku sündmusi:", nrow(events_all), "\n")
cat("Sündmused bändide kaupa:\n"); print(table(events_all$band))

# Flag-baasil andmestik: iga rida on üks flag-itud 15-min epohh
# Kasutatakse graafikutel 1, 3, 5
flags_long <- do.call(rbind, lapply(BANDS, function(bn) {
  flag_col <- paste0("flag_", bn)
  drop_col <- paste0("drop_", bn)
  idx <- !is.na(all_data[[flag_col]]) & all_data[[flag_col]]
  if (!any(idx)) return(NULL)
  data.frame(
    station         = all_data$station[idx],
    timestamp_local = all_data$timestamp_local[idx],
    band            = bn,
    drop_db         = all_data[[drop_col]][idx],
    stringsAsFactors = FALSE
  )
}))

cat("Kokku flag-itud epohhe:", nrow(flags_long), "\n")
cat("Flag-id bändide kaupa:\n"); print(table(flags_long$band))

# ============================================================
# 5. GRAAFIK 1 — AJATELG (kõik jaamad, sündmused markeritena)
# ============================================================

cat("\nKoostan graafikut 1 (ajatelg)...\n")

flags_long$year <- year(flags_long$timestamp_local)
years_to_plot   <- seq(years_range[1], years_range[2])

y_max_timeline <- max(flags_long$drop_db, na.rm = TRUE)
if (!is.finite(y_max_timeline) || y_max_timeline <= 0) y_max_timeline <- 1

# Jälgime iga trace'i bändi (andmete jaoks) — vajalik nuppude jaoks.
# Legendi-trace'ide juures kasutame "LEGEND" silti, et nupud neid ei lülitaks.
trace_band_map <- character(0)

make_year_plot <- function(yr, add_legend_traces = FALSE) {
  x_start <- as.POSIXct(sprintf("%d-01-01 00:00:00", yr), tz = "UTC")
  x_end   <- as.POSIXct(sprintf("%d-12-31 23:59:59", yr), tz = "UTC")
  
  p <- plot_ly()
  
  # Legendi-ainult trace'id (lisatakse ainult esimesse aasta paneeli):
  # üks per jaam, hallide ringidena. Kasutame eraldi legendgrouppi
  # ("LEG_<jaam>"), et legendi värv ei pärineks andmete trace'ide järjest.
  if (add_legend_traces) {
    legend_x <- as.POSIXct(sprintf("%d-01-01", yr), tz = "UTC")
    for (st in stations_used) {
      p <- p %>% add_trace(
        x = legend_x, y = -1000,
        name = st,
        legendgroup = paste0("LEG_", st),
        type = "scatter", mode = "markers",
        showlegend = TRUE,
        marker = list(color = "gray50", size = 9, symbol = "circle"),
        hoverinfo = "skip"
      )
      trace_band_map <<- c(trace_band_map, "LEGEND")
    }
  }
  
  # Tegelikud andmepunktid: ei ilmu legendis (showlegend = FALSE),
  # aga `legendgroup = station` ühendab need legendi-trace'iga
  for (st in stations_used) {
    for (bn in BANDS) {
      sub <- flags_long[flags_long$year == yr &
                        flags_long$band == bn &
                        flags_long$station == st, ]
      if (!nrow(sub)) next
      
      p <- p %>% add_markers(
        x = sub$timestamp_local, y = sub$drop_db,
        name = st, legendgroup = paste0("LEG_", st),
        showlegend = FALSE,
        marker = list(color = band_colors[[bn]], size = 4,
                      symbol = band_symbols[[bn]], opacity = 0.7),
        hovertemplate = paste0("Jaam: ", st,
                               "<br>Bänd: ", bn,
                               "<br>Aeg: %{x|%Y-%m-%d %H:%M}",
                               "<br>SNR langus: %{y:.1f} dB<extra></extra>")
      )
      trace_band_map <<- c(trace_band_map, bn)
    }
  }
  
  p %>% layout(
    xaxis = list(title = "Aeg", type = "date",
                 range = c(x_start, x_end)),
    yaxis = list(title = "SNR langus (dB)",
                 range = c(0, y_max_timeline * 1.05))
  )
}

p_timeline <- subplot(
  lapply(seq_along(years_to_plot), function(i) {
    make_year_plot(years_to_plot[i], add_legend_traces = (i == 1))
  }),
  nrows = length(years_to_plot), shareX = FALSE, shareY = FALSE,
  titleX = TRUE, titleY = TRUE, margin = 0.03
)

# L1/L2/L5 filtri nupud:
# - Legendi-trace'id ("LEGEND") jäävad alati nähtavaks
# - Andmete trace'id (L1/L2/L5) filtreeritakse bändi järgi
make_visibility <- function(active_bands) {
  ifelse(trace_band_map == "LEGEND", TRUE, trace_band_map %in% active_bands)
}

filter_buttons <- list(
  list(method = "update",
       args = list(list(visible = make_visibility(c("L1", "L2", "L5")))),
       label = "L1, L2, L5"),
  list(method = "update",
       args = list(list(visible = make_visibility("L1"))),
       label = "Ainult L1"),
  list(method = "update",
       args = list(list(visible = make_visibility("L2"))),
       label = "Ainult L2"),
  list(method = "update",
       args = list(list(visible = make_visibility("L5"))),
       label = "Ainult L5")
)

p_timeline <- p_timeline %>%
  layout(title = list(text = sprintf(
           "%d-%d Kõikide jaamade GNSS segamise juhtumid",
           years_range[1], years_range[2]),
           y = 0.97, yanchor = "top"),
         margin = list(t = 100),
         showlegend = TRUE,
         updatemenus = list(list(
           type = "buttons", direction = "right",
           x = 0.01, y = 1.10, xanchor = "left", yanchor = "top",
           showactive = TRUE, buttons = filter_buttons))) %>%
  config(locale = "et")

# ============================================================
# 6. GRAAFIK 2 — SÜNDMUSTE PIKKUSTE JAOTUS
# ============================================================

cat("Koostan graafikut 2 (kestuste jaotus)...\n")

events_all$dur_bin <- cut(events_all$duration_min,
                          breaks = dur_breaks, labels = dur_labels,
                          include.lowest = TRUE, right = TRUE)

dur_counts <- events_all %>%
  group_by(band, dur_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::complete(band = BANDS, dur_bin = dur_labels,
                  fill = list(n = 0L))

p_dur <- plot_ly()
for (bn in BANDS) {
  sub <- dur_counts[dur_counts$band == bn, ]
  p_dur <- p_dur %>% add_bars(
    x = sub$dur_bin, y = sub$n, name = bn,
    marker = list(color = band_colors[[bn]]),
    hovertemplate = paste0("Bänd: ", bn,
                           "<br>Kestus: %{x}",
                           "<br>Sündmusi: %{y}<extra></extra>")
  )
}
p_dur <- p_dur %>% layout(
  barmode = "group",
  title = list(text = "Kanallevi sündmuste pikkuste jaotus",
               y = 0.97, yanchor = "top"),
  margin = list(t = 70),
  xaxis = list(title = "Sündmuse kestus", categoryorder = "array",
               categoryarray = dur_labels),
  yaxis = list(title = "Sündmuste arv", rangemode = "tozero")
) %>% config(locale = "et")

# ============================================================
# 7. GRAAFIK 3 — ÖÖPÄEVANE JUHTUMITE ARV (24h occupancy, kõik jaamad summa)
# ============================================================

cat("Koostan graafikut 3 (ööpäevane juhtumite arv)...\n")

# Iga flag-itud epohh loetakse selle tunni järgi
flags_long$hour_local <- hour(flags_long$timestamp_local)

occ <- flags_long %>%
  group_by(band, hour_local) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::complete(band = BANDS, hour_local = 0:23,
                  fill = list(n = 0L)) %>%
  mutate(tod_time = to_tod(hour_local))

p_occ <- plot_ly()
for (bn in BANDS) {
  sub <- occ[occ$band == bn, ]
  p_occ <- p_occ %>% add_bars(
    x = sub$tod_time, y = sub$n, name = bn,
    marker = list(color = band_colors[[bn]]),
    hovertemplate = paste0("Bänd: ", bn,
                           "<br>Kellaaeg: %{x|%H:%M}",
                           "<br>Juhtumeid: %{y}<extra></extra>")
  )
}
p_occ <- p_occ %>% layout(
  barmode = "stack",
  title = list(text = "Ööpäeva GNSS segamise juhtumite arv",
               y = 0.97, yanchor = "top"),
  margin = list(t = 70),
  xaxis = hour_xaxis,
  yaxis = list(title = "Segamise juhtumite arv", rangemode = "tozero")
) %>% config(locale = "et")

# ============================================================
# 8. GRAAFIK 4 — ÖÖPÄEVANE TUGEVUSE VARIEERUVUS
# ============================================================

cat("Koostan graafikut 4 (ööpäevane tugevus)...\n")

# Kasutame kõikide jaamade flag-itud epohhide drop-väärtusi
all_data$hour_local <- hour(all_data$timestamp_local)

int_panels <- lapply(BANDS, function(bn) {
  flag_col <- paste0("flag_", bn)
  drop_col <- paste0("drop_", bn)
  
  sub <- all_data[!is.na(all_data[[flag_col]]) & all_data[[flag_col]], ]
  if (!nrow(sub))
    return(plot_ly() %>% layout(annotations = list(
      list(text = paste(bn, "- andmeid pole"), showarrow = FALSE,
           x = 0.5, y = 0.5, xref = "paper", yref = "paper"))))
  
  agg <- sub %>%
    group_by(hour_local) %>%
    summarise(med  = median(.data[[drop_col]], na.rm = TRUE),
              minv = min(.data[[drop_col]],    na.rm = TRUE),
              maxv = max(.data[[drop_col]],    na.rm = TRUE),
              .groups = "drop") %>%
    tidyr::complete(hour_local = 0:23) %>%
    mutate(tod_time = to_tod(hour_local))
  
  plot_ly() %>%
    add_trace(x = agg$tod_time, y = agg$med,
              type = "scatter", mode = "markers",
              error_y = list(type = "data",
                             array     = agg$maxv - agg$med,
                             arrayminus = agg$med - agg$minv,
                             color = band_colors[[bn]]),
              marker = list(color = band_colors[[bn]], size = 8),
              name = paste(bn, "mediaan"), legendgroup = bn,
              hovertemplate = paste0("Bänd: ", bn,
                                     "<br>Kellaaeg: %{x|%H:%M}",
                                     "<br>Mediaan: %{y:.1f} dB<extra></extra>")) %>%
    layout(yaxis = list(title = "SNR langus (dB)", rangemode = "tozero"),
           xaxis = hour_xaxis)
})

p_int <- subplot(int_panels, nrows = length(BANDS),
                 shareX = TRUE, titleY = TRUE, margin = 0.04) %>%
  layout(title = list(text = "Ööpäeva GNSS segamise tugevus",
                      y = 0.97, yanchor = "top"),
         margin = list(t = 70)) %>%
  config(locale = "et")

# ============================================================
# 9. GRAAFIK 5 — UUS: PÄEVA KOGUSUMMA AJATELG (PROGRESSEERUMINE)
# ============================================================

cat("Koostan graafikut 5 (päeva kogusumma)...\n")

# Iga flag-itud epohh saab selle päeva kuupäeva
flags_long$day_start <- floor_date(flags_long$timestamp_local, unit = "day")

daily <- flags_long %>%
  group_by(day_start, band) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::complete(day_start = seq(min(flags_long$day_start),
                                   max(flags_long$day_start),
                                   by = "day"),
                  band = BANDS,
                  fill = list(n = 0L)) %>%
  arrange(band, day_start)

p_weekly <- plot_ly()
for (bn in BANDS) {
  sub <- daily[daily$band == bn, ]
  p_weekly <- p_weekly %>% add_lines(
    x = sub$day_start, y = sub$n, name = bn,
    line = list(color = band_colors[[bn]], width = 1.5),
    hovertemplate = paste0("Bänd: ", bn,
                           "<br>Päev: %{x|%Y-%m-%d}",
                           "<br>Juhtumeid: %{y}<extra></extra>")
  )
}
p_weekly <- p_weekly %>% layout(
  title = list(text = sprintf(
                 "%d-%d Päevane segamise juhtumite arv",
                 years_range[1], years_range[2]),
               y = 0.97, yanchor = "top"),
  margin = list(t = 70),
  xaxis = list(title = "Päev", type = "date"),
  yaxis = list(title = "Juhtumite arv päevas", rangemode = "tozero")
) %>% config(locale = "et")

# ============================================================
# 9.5 GRAAFIK 6 — ÖÖPÄEVANE JUHTUMITE ARV NELJAL EI AASTAAJAL
# ============================================================

cat("Koostan graafikut 6 (ööpäevane neljal aastaajal)...\n")

# Aastaaja määramine kuu järgi
month_to_season <- function(m) {
  factor(ifelse(m %in% c(12, 1, 2),  "DJV",
         ifelse(m %in% c(3,  4, 5),  "MAM",
         ifelse(m %in% c(6,  7, 8),  "JJA",
                                      "SON"))),
         levels = c("DJV", "MAM", "JJA", "SON"))
}

flags_long$season <- month_to_season(month(flags_long$timestamp_local))

season_occ <- flags_long %>%
  group_by(season, band, hour_local) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::complete(season = c("DJV", "MAM", "JJA", "SON"),
                  band = BANDS, hour_local = 0:23,
                  fill = list(n = 0L))

# Ühine y-telje skaala kõigi paneelide vahel, et oleks visuaalselt võrreldavad
y_max_season <- max(
  season_occ %>%
    group_by(season, hour_local) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    pull(total),
  na.rm = TRUE
)

season_labels <- c(DJV = "Talv (DJV)", MAM = "Kevad (MAM)",
                   JJA = "Suvi (JJA)", SON = "Sügis (SON)")

make_season_panel <- function(seas, show_legend) {
  p <- plot_ly()
  for (bn in BANDS) {
    sub <- season_occ[season_occ$season == seas & season_occ$band == bn, ]
    p <- p %>% add_bars(
      x = sub$hour_local, y = sub$n, name = bn,
      legendgroup = bn, showlegend = show_legend,
      marker = list(color = band_colors[[bn]]),
      hovertemplate = paste0("Aastaaeg: ", season_labels[seas],
                             "<br>Bänd: ", bn,
                             "<br>Tund: %{x}:00",
                             "<br>Juhtumeid: %{y}<extra></extra>")
    )
  }
  p %>% layout(
    barmode = "stack",
    annotations = list(list(text = season_labels[seas],
                            x = 0.5, y = 1.05, xref = "paper", yref = "paper",
                            xanchor = "center", showarrow = FALSE,
                            font = list(size = 13))),
    xaxis = list(title = "Tund", dtick = 3, range = c(-0.5, 23.5)),
    yaxis = list(title = "Juhtumite arv",
                 range = c(0, y_max_season * 1.05))
  )
}

p_season_count <- subplot(
  list(make_season_panel("DJV", TRUE),
       make_season_panel("MAM", FALSE),
       make_season_panel("JJA", FALSE),
       make_season_panel("SON", FALSE)),
  nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.025
) %>%
  layout(title = list(text = "Kanallevi ööpäevane juhtumite arv neljal eri aastaajal",
                      y = 0.98, yanchor = "top"),
         margin = list(t = 90)) %>%
  config(locale = "et")

# ============================================================
# 10. GRAAFIKUTE KUVAMINE JA HTML SALVESTUS
# ============================================================

# Kuva graafikud RStudio Viewer-is
p_timeline
p_dur
p_occ
p_int
p_weekly
p_season_count

# Salvesta iga graafik eraldi HTML-failina
out_prefix <- file.path(rds_dir,
                        paste0("multi_station_",
                               paste(years_range, collapse = "_")))

cat("\n=== HTML salvestus ===\n")

save_one <- function(plot_obj, suffix) {
  path <- paste0(out_prefix, "_", suffix, ".html")
  saveWidget(as_widget(plot_obj), file = path, selfcontained = TRUE)
  cat("Salvestatud:", path, "\n")
}

save_one(p_timeline,     "ajatelg")
save_one(p_dur,          "kestuste_jaotus")
save_one(p_occ,          "ooopaeva_juhtumite_arv")
save_one(p_int,          "ooopaeva_tugevus")
save_one(p_weekly,       "paeva_kogusumma")
save_one(p_season_count, "ooopaeva_aastaaegade_kaupa")

cat("\n=== Valmis ===\n")
cat("Jaamad:", length(stations_used), "\n")
cat("Sündmusi kokku:", nrow(events_all), "\n")
cat("Flag-itud epohhe kokku:", nrow(flags_long), "\n")
if (length(EXCLUDE_STATIONS))
  cat("Välistatud:", paste(EXCLUDE_STATIONS, collapse = ", "), "\n")
