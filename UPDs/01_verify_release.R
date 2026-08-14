## ---------------------------------------------------------------------------
## 01_verify_release.R
##
## Recomputes every figure quoted in UPDs/KNOWN_ISSUES.md and on
## georgytarasenko.com/red/coverage.html directly from the released red.Rds, so
## that no published quality figure is asserted rather than measured.
##
## Read-only. Base R only. Use a UTF-8 locale.
## Writes UPDs/output/red_qc_report.txt
## ---------------------------------------------------------------------------

REL <- "data/red.Rds"
OUT <- "UPDs/output"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
con <- file(file.path(OUT, "red_qc_report.txt"), open = "wt", encoding = "UTF-8")
say <- function(...) { m <- paste0(...); message(m); writeLines(m, con) }

red <- readRDS(REL)
for (v in names(red)) if (is.character(red[[v]])) red[[v]] <- enc2utf8(red[[v]])
onl <- !is.na(red$tik) & red$tik == "online_voting"
P   <- red[!onl, ]
yr  <- as.integer(as.character(P$year))
say("RED release QC — ", REL)
say("rows ", nrow(red), " | precinct ", nrow(P), " | online ", sum(onl),
    " | columns ", ncol(red))

## fast hashed grouping (table() on 1M strings is pathologically slow in base R)
gid  <- function(x) match(x, unique(x))
gsz  <- function(x) { g <- gid(x); tabulate(g)[g] }
ndis <- function(by, val) vapply(split(val, gid(by)), function(z) length(unique(z)), 1L)

say("\n== 1. KEYS ==")
say("regions ", length(unique(P$region)),
    " | distinct tik ", length(unique(P$tik)),
    " | units (region,uik) ", length(unique(paste(P$region, P$uik, sep = "\r"))))
say("(year,uik) groups with >1 row: ",
    { k <- paste(P$year, P$uik, sep = "\r"); sum(gsz(k) > 1 & !duplicated(k)) })
say("max regions sharing one uik in a year: ",
    max(ndis(paste(P$year, P$uik, sep = "\r"), P$region)))
say("(region,uik) mapping to >1 tik: ",
    sum(ndis(paste(P$region, P$uik, sep = "\r"), P$tik) > 1), "   [expected 0]")
say("tik names appearing in >1 region: ", sum(ndis(P$tik, P$region) > 1))

say("\n== 2. DUPLICATES ==")
rowkey <- do.call(paste, c(lapply(red, as.character), sep = "\r"))
say("exact duplicate rows (all ", ncol(red), " columns): ", sum(duplicated(rowkey)))
print(table(red$year[duplicated(rowkey)]))
dkey <- paste(red$year, red$region, red$tik, red$uik, sep = "\r")
ink  <- gsz(dkey) > 1
nd   <- ndis(dkey[ink], rowkey[ink])
say("duplicate-key groups: ", length(nd),
    " | pure exact duplicates: ", sum(nd == 1),
    " | differing in >=1 column: ", sum(nd > 1))
RES <- intersect(c("electorate","turnout","valid","invalid","turnout_rate",
                   "turnout_onsite","turnout_offsite","turnout_early"), names(red))
rk  <- do.call(paste, c(lapply(red[RES], as.character), sep = "\r"))
say("  of which disagree on a reported result: ", sum(ndis(dkey[ink], rk[ink]) > 1))

say("\n== 3. COORDINATE COVERAGE BY YEAR (%) ==")
say(paste(sprintf("%d:%.1f", sort(unique(yr)),
      tapply(!is.na(P$lat), yr, mean) * 100), collapse = "  "))
say("\n== 4. REGIONS WITH NO COORDINATES IN ANY GEOCODED ELECTION ==")
GY <- c(2012, 2018, 2021, 2024)
cv <- t(sapply(sort(unique(P$region)), function(r)
  sapply(GY, function(y) round(100 * mean(!is.na(P$lat[P$region == r & yr == y])), 1))))
colnames(cv) <- GY
dead <- rownames(cv)[rowSums(cv) == 0]
say(if (length(dead)) paste(dead, collapse = " ; ") else "(none)")
say("\nregion-year collapses (>=80% in one election, <40% in another):")
coll <- rownames(cv)[apply(cv, 1, max) >= 80 & apply(cv, 1, min) < 40]
for (r in coll) say("  ", r, "  ", paste(GY, cv[r, ], sep = ":", collapse = "  "))

say("\n== 5. COORDINATE INTEGRITY ==")
say("lat > 82 (impossible): ", sum(red$lat > 82, na.rm = TRUE))
say("lon > 180 (unwrapped): ", sum(red$lon > 180, na.rm = TRUE))
say("lat == lon exactly: ", sum(red$lat == red$lon, na.rm = TRUE))
g   <- P[!is.na(P$lat) & !is.na(P$lon) & P$lat <= 82, ]
g$lon <- ifelse(g$lon > 180, g$lon - 360, g$lon)
sp  <- split(seq_len(nrow(g)), paste(g$region, g$uik, sep = "\r"))
sp  <- sp[lengths(sp) > 1]
mv  <- vapply(sp, function(ix) { la <- g$lat[ix]; lo <- g$lon[ix]
  max(sqrt(((la - median(la)) * 111)^2 +
           ((lo - median(lo)) * 111 * cos(la * pi / 180))^2)) }, 1)
say("units geocoded in >1 election: ", length(mv))
for (t in c(1, 5, 25, 100, 500))
  say(sprintf("  drift >%4d km: %6d (%.2f%%)", t, sum(mv > t), 100 * mean(mv > t)))

say("\n== 6. ENCODING ==")
chr <- names(red)[vapply(red, is.character, TRUE)]
say("character columns: ", length(chr))
say("internally mixed:  ", sum(vapply(readRDS(REL)[chr],
      function(x) length(unique(Encoding(x))) > 1L, TRUE)))

say("\n== 7. tik_2018 / tik_2021 ==")
for (v in c("tik_2018", "tik_2021")) if (v %in% names(red)) {
  x <- red[[v]]; nz <- !is.na(x) & x != ""
  say(v, ": populated ", sum(nz), " | distinct ", length(unique(x[nz])),
      " | regions ", length(unique(red$region[nz])),
      " | years ", length(unique(red$year[nz])))
}

say("\n== 8. HANDBOOK ROWS ==")
h <- red[!is.na(red$source_primary) &
         !grepl("izbirkom", red$source_primary, fixed = TRUE), ]
say("rows ", nrow(h),
    " | turnout>electorate ", sum(h$turnout > h$electorate, na.rm = TRUE),
    " | max turnout_rate ", round(max(h$turnout_rate, na.rm = TRUE), 2),
    " | valid>turnout ", sum(h$valid > h$turnout, na.rm = TRUE),
    " | non-integer valid ", sum(abs(h$valid - round(h$valid)) > 1e-9, na.rm = TRUE))

say("\n== 9. 2000 RESIDUAL ==")
y0 <- P[yr == 2000, ]
cd <- intersect(c("Putin","Zyuganov","Zhirinovsky","Yavlinskiy","Tuleev",
                  "Titov_K","Pamfilova","against_all"), names(y0))
rs <- y0$valid - rowSums(y0[, cd], na.rm = TRUE)
say("residual ", format(round(sum(rs, na.rm = TRUE), 1), big.mark = ","),
    " votes = ", round(100 * sum(rs, na.rm = TRUE) / sum(y0$valid, na.rm = TRUE), 4),
    "% of valid | non-zero on ", round(100 * mean(abs(rs) > 1e-9, na.rm = TRUE), 2),
    "% of rows | negative on ", sum(rs < -1e-9, na.rm = TRUE))

say("\n== 10. MISC ==")
say("ballot gap non-zero by year (%): ",
    paste(sprintf("%d:%.2f", sort(unique(yr)),
      tapply(abs(P$turnout - (P$valid + P$invalid)) > 1e-9, yr,
             function(z) 100 * mean(z, na.rm = TRUE))), collapse = "  "))
say("zero-electorate rows by year: ",
    paste(sprintf("%d:%d", sort(unique(yr)),
      tapply(P$electorate == 0, yr, sum, na.rm = TRUE)), collapse = "  "))
say("absentee total by year: ",
    paste(sprintf("%d:%d", sort(unique(yr)),
      tapply(P$turnout_absentee, yr, function(z) sum(z, na.rm = TRUE))), collapse = "  "))
say("channel identity violations (onsite+offsite+early != turnout): ",
    sum(abs(P$turnout - (P$turnout_onsite + P$turnout_offsite + P$turnout_early)) > 1e-9,
        na.rm = TRUE), "   [expected 0]")

close(con)
message("\nwrote ", file.path(OUT, "red_qc_report.txt"))
