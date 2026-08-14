## ---------------------------------------------------------------------------
## 02_build_red_plus.R
##
## Builds UPDs/output/red_plus.Rds: the released RED with the unambiguous
## defects corrected, the judgement-laden ones flagged rather than deleted, and
## the derived columns the website computes internally but never shipped.
##
## Does NOT modify data/red.Rds. Base R only. Use a UTF-8 locale.
## ---------------------------------------------------------------------------

REL <- "data/red.Rds"
OUT <- "UPDs/output"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
log_lines <- character(0)
say <- function(...) { m <- paste0(...); message(m); log_lines <<- c(log_lines, m) }

stopifnot(file.exists(REL))
red <- readRDS(REL)
say("read ", REL, ": ", nrow(red), " rows x ", ncol(red), " columns")
orig_cols <- names(red)

## -- 1. string encoding ------------------------------------------------------
## 8 of the 14 character columns carry both UTF-8 and "unknown" encoding flags.
## This is COSMETIC, not a defect: every string containing a non-ASCII byte is
## already flagged UTF-8, and the "unknown" ones are pure ASCII, where that is
## R's normal marking. Normalised here so downstream code need not think about
## it. See KNOWN_ISSUES.md section 6 - do not report this as data corruption.
chr <- names(red)[vapply(red, is.character, TRUE)]
mixed <- vapply(red[chr], function(x) length(unique(Encoding(x))) > 1L, TRUE)
say("encoding: ", length(chr), " character columns, ", sum(mixed), " internally mixed")
for (v in chr) red[[v]] <- enc2utf8(red[[v]])
say("encoding: normalised (cosmetic - no non-ASCII string was mislabelled)")

## -- 2. online rows ----------------------------------------------------------
red$is_online <- !is.na(red$tik) & red$tik == "online_voting"
say("online aggregate rows flagged: ", sum(red$is_online))

## -- 3. longitude convention -------------------------------------------------
## Chukotka crosses the antimeridian. 62 rows use standard negative longitudes;
## 5 rows from 2012 use an unwrapped convention (180.7-189.0). Four stations
## carry both, which proves the 5 are the anomaly rather than a second standard.
## Normalise the release to WGS84 [-180, 180] and let any renderer unwrap.
unw <- which(!is.na(red$lon) & red$lon > 180)
if (length(unw)) {
  say("longitude: wrapping ", length(unw), " rows with lon > 180 into [-180, 180]")
  for (i in unw) say("   year ", red$year[i], " uik ", red$uik[i],
                     "  lon ", round(red$lon[i], 4), " -> ", round(red$lon[i] - 360, 4))
  red$lon[unw] <- red$lon[unw] - 360
}

## -- 4. impossible latitudes -------------------------------------------------
## Two rows sit north of Russia's northernmost land, both with lat copied from
## (or near) lon. Do not guess a replacement; drop to NA and record them.
badlat <- which(!is.na(red$lat) & red$lat > 82)
if (length(badlat)) {
  say("coordinates: nulling ", length(badlat), " impossible rows (lat > 82)")
  for (i in badlat) say("   year ", red$year[i], " uik ", red$uik[i],
                        "  lat ", round(red$lat[i], 4), " lon ", round(red$lon[i], 4))
  red$lat[badlat] <- NA_real_; red$lon[badlat] <- NA_real_
}

## -- 5. coordinate inheritance ----------------------------------------------
## Only 2012 and 2018-2024 carry observed coordinates; 2000-2011 and 2016 have
## none. A harmonized unit is the same polling station across elections, so a
## coordinate observed in one election locates it in the others. This is the
## logic the website has used since build_explorer_points_inherited.R; it is
## moved here so downloaders get it too, with the provenance kept explicit.
red$coord_source        <- ifelse(!is.na(red$lat), "observed", NA_character_)
red$coord_inherited_from <- NA_integer_

unit <- paste(red$region, red$uik, sep = "\r")
yr   <- as.integer(as.character(red$year))
obs  <- !is.na(red$lat) & !red$is_online

## donors: one coordinate per (unit, year) that has one
don <- data.frame(unit = unit[obs], yr = yr[obs],
                  lat = red$lat[obs], lon = red$lon[obs],
                  stringsAsFactors = FALSE)
don_by_unit <- split(seq_len(nrow(don)), don$unit)

need <- which(is.na(red$lat) & !red$is_online & unit %in% names(don_by_unit))
say("inheritance: ", length(need), " rows are candidates for an inherited coordinate")

if (length(need)) {
  nu <- unit[need]; ny <- yr[need]
  la <- rep(NA_real_, length(need)); lo <- la; fy <- rep(NA_integer_, length(need))
  best <- rep(Inf, length(need))
  ## Only four elections carry observed coordinates, so instead of searching per
  ## row, sweep the donor years in ascending order and keep the nearest. Using
  ## <= means a later election wins a tie, which is the safer choice: the 2012
  ## geocode is the one that misplaces units (see KNOWN_ISSUES.md section 2).
  for (dy in sort(unique(don$yr))) {
    sub <- don[don$yr == dy, , drop = FALSE]
    sub <- sub[!duplicated(sub$unit), , drop = FALSE]
    m   <- match(nu, sub$unit)
    d   <- abs(ny - dy)
    upd <- !is.na(m) & d <= best
    if (any(upd)) {
      best[upd] <- d[upd]
      la[upd] <- sub$lat[m[upd]]; lo[upd] <- sub$lon[m[upd]]
      fy[upd] <- dy
    }
  }
  red$lat[need] <- la; red$lon[need] <- lo
  red$coord_source[need]         <- ifelse(is.na(la), NA_character_, "inherited")
  red$coord_inherited_from[need] <- fy
  say("inheritance: placed ", sum(!is.na(la)), " coordinates")
  say("inheritance: donor election used — ",
      paste(names(table(fy)), table(fy), sep = ": ", collapse = " | "))
}
say("coord_source: observed ", sum(red$coord_source == "observed", na.rm = TRUE),
    " | inherited ",          sum(red$coord_source == "inherited", na.rm = TRUE),
    " | none ",               sum(is.na(red$coord_source)))

## -- 6. coordinate stability -------------------------------------------------
## A polling station does not move. Where a unit's OBSERVED coordinates disagree
## across elections, at least one is wrong; measure it rather than silently
## averaging or picking one.
red$coord_drift_km <- NA_real_
o <- which(obs)
if (length(o)) {
  ou <- unit[o]; sp <- split(o, ou); sp <- sp[lengths(sp) > 1L]
  drift <- vapply(sp, function(ix) {
    la <- red$lat[ix]; lo <- red$lon[ix]
    ok <- !is.na(la) & !is.na(lo)          # a few rows carry lat without lon
    if (sum(ok) < 2L) return(NA_real_)
    la <- la[ok]; lo <- lo[ok]
    mla <- median(la); mlo <- median(lo)
    max(sqrt(((la - mla) * 111)^2 + ((lo - mlo) * 111 * cos(la * pi / 180))^2))
  }, 1)
  red$coord_drift_km <- unname(drift[match(unit, names(drift))])
  say("coord drift: ", sum(!is.na(drift)), " units geocoded in >1 election; ",
      sum(drift > 25, na.rm = TRUE), " drift >25 km; ",
      sum(drift > 100, na.rm = TRUE), " drift >100 km")
}
red$flag_coord_unstable <- !is.na(red$coord_drift_km) & red$coord_drift_km > 25

## -- 7. 2000 unattributed votes ---------------------------------------------
## The 2000 ballot had eleven candidates; the release carries eight columns.
## Summing the eight and dividing by that sum gives systematically wrong shares.
## Expose the residual instead of pretending the ballot is complete.
red$other_votes <- NA_real_; red$other_share <- NA_real_
y0 <- which(yr == 2000 & !red$is_online)
c2000 <- intersect(c("Putin","Zyuganov","Zhirinovsky","Yavlinskiy","Tuleev",
                     "Titov_K","Pamfilova","against_all"), names(red))
if (length(y0) && length(c2000)) {
  resid <- red$valid[y0] - rowSums(red[y0, c2000], na.rm = TRUE)
  red$other_votes[y0] <- resid
  red$other_share[y0] <- ifelse(red$valid[y0] > 0, resid / red$valid[y0], NA_real_)
  say("2000 residual: ", format(round(sum(resid, na.rm = TRUE), 1), big.mark = ","),
      " votes (", round(100*sum(resid,na.rm=TRUE)/sum(red$valid[y0],na.rm=TRUE), 4),
      "% of valid), negative on ", sum(resid < -1e-9, na.rm = TRUE), " rows")
}

## -- 8. ballot gap -----------------------------------------------------------
## turnout_rate uses turnout/electorate; the website's maps use
## (valid+invalid)/electorate. The difference is this column. Clerical at the
## median - it is NOT a fraud measure at an individual precinct.
red$ballot_gap <- red$turnout - (red$valid + red$invalid)
say("ballot gap: non-zero on ",
    round(100 * mean(abs(red$ballot_gap) > 1e-9, na.rm = TRUE), 2), "% of rows")

## -- 9. provenance and arithmetic flags -------------------------------------
red$flag_source_handbook <- !is.na(red$source_primary) &
  !grepl("izbirkom", red$source_primary, fixed = TRUE)
red$flag_arith_impossible <-
  (!is.na(red$turnout) & !is.na(red$electorate) & red$turnout > red$electorate) |
  (!is.na(red$valid)   & !is.na(red$turnout)    & red$valid   > red$turnout)   |
  (!is.na(red$valid)   & abs(red$valid - round(red$valid)) > 1e-9)
red$flag_zero_electorate <- !is.na(red$electorate) & red$electorate == 0
say("handbook rows: ", sum(red$flag_source_handbook),
    " | arithmetically impossible: ", sum(red$flag_arith_impossible, na.rm = TRUE),
    " | zero electorate: ", sum(red$flag_zero_electorate))

## -- 10. duplicates ----------------------------------------------------------
## Flagged, never dropped: which row of a duplicate pair is authoritative is a
## judgement the release cannot make for the user.
rowkey <- do.call(paste, c(lapply(red[orig_cols], as.character), sep = "\r"))
red$flag_exact_duplicate <- duplicated(rowkey)
dkey <- paste(red$year, red$region, red$tik, red$uik, sep = "\r")
gsz  <- tabulate(match(dkey, unique(dkey)))[match(dkey, unique(dkey))]
red$flag_dup_key <- gsz > 1L

RES <- intersect(c("electorate","turnout","valid","invalid","turnout_rate",
                   "turnout_onsite","turnout_offsite","turnout_early"), names(red))
reskey <- do.call(paste, c(lapply(red[RES], as.character), sep = "\r"))
inkey  <- which(red$flag_dup_key)
differs <- tapply(reskey[inkey], dkey[inkey], function(z) length(unique(z)) > 1L)
red$flag_dup_key_results_differ <- red$flag_dup_key & dkey %in% names(differs)[differs]
say("duplicates: ", sum(red$flag_exact_duplicate), " exact | ",
    length(unique(dkey[red$flag_dup_key])), " duplicate-key groups | ",
    sum(differs), " of those disagree on a reported result")

## -- write -------------------------------------------------------------------
## gzip, matching the release itself. xz produced an unreadable file on a frame
## this size under R 4.3 (lzma decoding result 10) — do not "optimise" this back.
saveRDS(red, file.path(OUT, "red_plus.Rds"), compress = "gzip")
say("wrote ", file.path(OUT, "red_plus.Rds"), ": ", nrow(red), " x ", ncol(red),
    " (", ncol(red) - length(orig_cols), " columns added)")
writeLines(log_lines, file.path(OUT, "red_plus_build_log.txt"))
message("done")
