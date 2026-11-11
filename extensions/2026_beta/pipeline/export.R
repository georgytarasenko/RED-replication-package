library(data.table)
library(arrow)

release <- readRDS(Sys.getenv("RED_BASELINE_RDS"))
stopifnot(nrow(release) == 1044599)
anchor <- as.data.table(release)[year == 2024 & tik != "online_voting",
  .(region, tik, uik, uik_old, electorate, address, lat, lon)]
write_parquet(anchor, "data/red_2024_anchor.parquet")
prior <- as.data.table(release)[year == 2021 & tik != "online_voting",
  .(region, tik, uik, electorate, turnout_rate, valid, UR, KPRF, LDPR,
    SR, NovyeLyudi, Zelenye, Rodina, KPKR, PartiyaPensionerov)]
write_parquet(prior, "data/red_2021_anchor.parquet")
offline <- as.data.table(readRDS("data/01_elec_offline.rds"))[year == 2026]
online <- as.data.table(readRDS("data/01_elec_online.rds"))[year == 2026]
aux <- as.data.table(readRDS("data/01b_2026_precinct_aux.rds"))
write_parquet(offline, "data/red_2026_clean.parquet")
write_parquet(online, "data/red_2026_deg.parquet")
write_parquet(aux, "data/red_2026_aux.parquet")
