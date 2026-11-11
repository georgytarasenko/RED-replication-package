"""Build the separate 2026 party-list BETA release from the RED cleaning stage.

Inputs are the three parquet exports made by the R cleaning scripts, plus the
published 2024 RED anchor. This script never rewrites data/red.Rds.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import pandas as pd

PARTIES = ["UR", "KPRF", "LDPR", "SR", "NovyeLyudi", "Zelenye",
           "Rodina", "KPKR", "PartiyaPensionerov", "PPD"]
KEY = ["region", "tik", "uik"]


def build(inputs: Path, out: Path) -> None:
    out.mkdir(parents=True, exist_ok=True)
    x = pd.read_parquet(inputs / "red_2026_clean.parquet")
    deg = pd.read_parquet(inputs / "red_2026_deg.parquet")
    aux = pd.read_parquet(inputs / "red_2026_aux.parquet")
    old = pd.read_parquet(inputs / "red_2024_anchor.parquet")
    assert len(x) == 87459 and len(deg) == 94
    assert not x.duplicated(["region", "uik"]).any()
    assert not aux.duplicated(["region", "uik"]).any()
    old = old.loc[~old.duplicated(KEY, keep=False)].copy()
    assert not old.duplicated(KEY).any()

    fields = ["region", "uik", "uik_id", "tik_code", "address_origin",
              "coordinate_source", "latitude", "longitude", "protocol_status",
              "repeated", "has_protocol", "geography_match_method"]
    x = x.merge(aux[fields], on=["region", "uik"], how="left", validate="one_to_one")
    assert x.uik_id.notna().all()
    old = old[KEY + ["electorate"]].rename(columns={
        "uik": "uik_2024", "electorate": "electorate_2024"})
    m = x.merge(old, left_on=KEY, right_on=["region", "tik", "uik_2024"],
                how="left", validate="one_to_one")
    ratio = m.electorate / m.electorate_2024.replace(0, float("nan"))
    candidate = m.uik_2024.notna()
    plausible = candidate & ratio.between(0.5, 2, inclusive="both")
    m["match_status"] = "unmatched"
    m.loc[candidate & ~plausible, "match_status"] = "review_electorate_change"
    m.loc[plausible, "match_status"] = "provisional_number_match"
    m["match_method"] = ""
    m.loc[candidate, "match_method"] = "same_region_tik_number"
    # A large electorate change may be a genuine reconfiguration or a recycled
    # number. Do not assign a longitudinal panel ID without further evidence.
    m.loc[~plausible, "uik_2024"] = pd.NA
    m["electorate_ratio_2026_to_2024"] = ratio.round(4)
    m["panel_key_2024"] = m.apply(
        lambda r: f"{r.region}|{r.tik}|{int(r.uik_2024)}"
        if pd.notna(r.uik_2024) else "", axis=1)
    m["snapshot_uik_id"] = m.uik_id
    m["turnout_rate"] = (100 * m.turnout / m.electorate.replace(0, float("nan"))).round(4)
    m["ur_share_valid"] = (100 * m.UR / m.valid.replace(0, float("nan"))).round(4)
    m["party_vote_sum"] = m[PARTIES].sum(axis=1)
    assert (m.party_vote_sum == m.valid).all()
    assert (m.turnout == m.turnout_onsite + m.turnout_offsite + m.turnout_early).all()
    assert not m.duplicated("snapshot_uik_id").any()
    assert m.region.nunique() == 83
    assert not m.region.str.contains("Крым|Севастопол|Донецк|Луганск|Запорож|Херсон", case=False).any()

    cols = ["year", "region", "tik", "uik", "snapshot_uik_id", "electorate",
            "turnout", "turnout_rate", "turnout_onsite", "turnout_offsite",
            "turnout_early", "valid", "invalid", *PARTIES,
            "ur_share_valid", "latitude", "longitude", "coordinate_source",
            "address_origin", "geography_match_method", "protocol_status", "repeated",
            "match_status", "match_method", "uik_2024", "panel_key_2024",
            "electorate_2024", "electorate_ratio_2026_to_2024",
            "source_primary", "source_secondary"]
    m[cols].to_csv(out / "precincts_2026_beta.csv.gz", index=False, compression="gzip")
    deg_cols = ["year", "region", "oik_old", "electorate", "turnout_online", "valid", "invalid", *PARTIES,
                "source_primary", "source_secondary"]
    deg[deg_cols].to_csv(out / "deg_2026_beta.csv.gz", index=False, compression="gzip")

    placed = m.longitude.between(19, 191) & m.latitude.between(41, 82)
    # Compact columnar payload for the website map. Every row remains available
    # in the downloadable table, including all unplaced and unmatched rows.
    for p in PARTIES:
        m["share_" + p] = (100 * m[p] / m.valid.replace(0, float("nan"))).round(2)
    keep = ["region", "tik", "uik", "electorate", "turnout_rate", "ur_share_valid",
            "latitude", "longitude", "match_status", "coordinate_source",
            *["share_" + p for p in PARTIES]]
    web = m[keep].copy()
    web["latitude"] = web.latitude.round(5)
    web["longitude"] = web.longitude.round(5)
    web["turnout_rate"] = web.turnout_rate.round(2)
    web["ur_share_valid"] = web.ur_share_valid.round(2)
    web["uik"] = web.uik.astype(int)
    payload = {"columns": keep, "rows": web.astype(object).where(pd.notna(web), None).values.tolist()}
    (out / "map_2026_beta.json").write_text(json.dumps(payload, ensure_ascii=False,
                                                         separators=(",", ":")), encoding="utf-8")
    qc = {
        "snapshot_date": "2026-09-30", "status": "BETA; research snapshot, not certified final results",
        "scope": "party-list ordinary precincts in the 83 regions covered by published RED; excludes Crimea, Sevastopol, four annexed regions, abroad/Baikonur",
        "ordinary_precincts": len(m), "regions": m.region.nunique(), "deg_district_rows": len(deg),
        "placed_map_points": int(placed.sum()), "unplaced_rows": int((~placed).sum()),
        "match_status": m.match_status.value_counts().to_dict(),
        "address_origin": m.address_origin.fillna("missing").value_counts().to_dict(),
        "protocol_status": m.protocol_status.fillna("missing").value_counts().to_dict(),
        "party_votes_equal_valid_on_all_rows": True,
        "turnout_components_equal_turnout_on_all_rows": True,
        "warning": "Numeric matching is provisional. Inherited addresses/coordinates cannot independently verify historical precinct identity. DEG completeness is unverified.",
    }
    (out / "quality_2026_beta.json").write_text(json.dumps(qc, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps(qc, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--inputs", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    build(args.inputs, args.out)
