"""Create a provisional 2021–2026 precinct change map for the website."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import pandas as pd

PARTIES = ["UR", "KPRF", "LDPR", "SR", "NovyeLyudi", "Zelenye",
           "Rodina", "KPKR", "PartiyaPensionerov"]
KEY = ["region", "tik", "uik"]


def build(inputs: Path, out: Path) -> None:
    old = pd.read_parquet(inputs / "red_2021_anchor.parquet")
    old = old.loc[~old.duplicated(KEY, keep=False)].copy()
    new = pd.read_csv(out / "precincts_2026_beta.csv.gz", low_memory=False)
    new = new[new.match_status == "provisional_number_match"].copy()
    merged = new.merge(old, left_on=["region", "tik", "uik_2024"],
                       right_on=KEY, how="inner", suffixes=("_26", "_21"),
                       validate="one_to_one")
    for p in PARTIES:
        merged[p + "_delta"] = (
            100 * merged[p + "_26"] / merged.valid_26.replace(0, float("nan"))
            - 100 * merged[p + "_21"] / merged.valid_21.replace(0, float("nan")))
    merged["turnout_delta"] = merged.turnout_rate_26 - merged.turnout_rate_21
    placed = merged.longitude.between(19, 191) & merged.latitude.between(41, 82)
    cols = ["region", "tik", "uik_26", "longitude", "latitude", "coordinate_source",
            "turnout_delta", *[p + "_delta" for p in PARTIES]]
    web = merged.loc[placed, cols].copy()
    for c in ["longitude", "latitude", "turnout_delta", *[p + "_delta" for p in PARTIES]]:
        web[c] = web[c].round(2 if c.endswith("_delta") else 5)
    payload = {"columns": cols, "rows": web.astype(object).where(pd.notna(web), None).values.tolist(),
               "metadata": {"basis": "provisional numeric match through RED 2024 anchor",
                            "matched_2021_2026": len(merged), "mapped": len(web),
                            "excluded_2026_rows": 87459 - len(merged),
                            "caution": "Arrows show change in percentage points at an approximate 2026 map position; they do not show geographic movement or a verified physical precinct match."}}
    (out / "swing_2021_2026_beta.json").write_text(
        json.dumps(payload, ensure_ascii=False, separators=(",", ":")), encoding="utf-8")
    print(json.dumps(payload["metadata"], ensure_ascii=False))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--inputs", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    build(args.inputs, args.out)
