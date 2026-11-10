"""Build the frozen local-model name lookup used by the Bihar career analysis."""

import hashlib
import json
import os
import re
import unicodedata
from importlib.metadata import version
from pathlib import Path

import indicate
import pandas as pd
import torch
import yaml
from indicate.resources import resolve_data

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "data/bihar_recontesting"
INPUTS = (
    "2016/winners.parquet",
    "2016/candidates.parquet",
    "2021/candidates.parquet",
)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def pinned_sources():
    manifest = yaml.safe_load((ROOT / "data/manifest.yaml").read_text())
    spec = manifest["upstream"]["local_elections_bihar"]
    cache = Path(os.environ.get("INDIA_DATA_HOME", manifest["cache_dir"])).expanduser()
    resolved = {}
    for name in INPUTS:
        relative = "data/release/" + name
        paths = (
            cache / "local_elections_bihar" / spec["ref"] / relative,
            ROOT / spec["sibling"] / relative,
        )
        found = next(
            (p for p in paths if p.is_file() and sha256(p) == spec["files"][relative]),
            None,
        )
        if found is None:
            raise FileNotFoundError(f"No checksum-matching upstream input: {relative}")
        resolved[name] = found
    return resolved


def main():
    torch.set_num_threads(1)
    sources = pinned_sources()
    keys = ["district_code", "block_code", "unit_code", "row"]
    winners = pd.read_parquet(
        sources[INPUTS[0]],
        filters=[("office", "==", "mukhiya")],
        columns=keys + ["candidate_name"],
    )
    candidates = pd.read_parquet(
        sources[INPUTS[1]],
        filters=[("office", "==", "mukhiya")],
        columns=keys + ["father_husband_name"],
    )
    winners = winners.merge(candidates, on=keys, validate="one_to_one")
    later = pd.read_parquet(
        sources[INPUTS[2]],
        filters=[("post_id", "==", 3)],
        columns=["candidate_name", "guardian_name"],
    )
    names = sorted(
        set(
            pd.concat(
                [
                    winners.candidate_name,
                    winners.father_husband_name,
                    later.candidate_name,
                    later.guardian_name,
                ]
            ).dropna()
        )
    )
    hindi = [n for n in names if re.search(r"[\u0900-\u097f]", n)]
    OUTPUT.mkdir(exist_ok=True)
    target = OUTPUT / "name_transliterations.csv"
    metadata_path = OUTPUT / "transliteration.json"
    assets = {
        asset: sha256(Path(resolve_data("hindi_to_english", asset)))
        for asset in (
            "hindi_tokens.json",
            "english_tokens.json",
            "saved_weights/encoder.safetensors",
            "saved_weights/decoder.safetensors",
        )
    }
    signature = {
        "indicate": version("indicate"),
        "torch": version("torch"),
        "engine": "model",
        "source": "hindi",
        "beam": 5,
        "assets": assets,
    }
    cached = {}
    if target.exists():
        previous = json.loads(metadata_path.read_text())
        if (
            previous["model"] != signature
            or sha256(target) != previous["output_sha256"]
        ):
            raise ValueError(
                "Existing transliteration cache has different model or bytes"
            )
        cached = dict(
            pd.read_csv(target, keep_default_na=False)[["name", "latin"]].values
        )
    missing = [n for n in hindi if n not in cached]
    # The model resolves words independently; decode each distinct word once.
    words = sorted(
        {
            word
            for name in missing
            for word in name.split()
            if re.search(r"[\u0900-\u097f]", word)
        },
        key=lambda x: (len(x), x),
    )
    tokens = {}
    for start in range(0, len(words), 128):
        batch = words[start : start + 128]
        translated = indicate.transliterate_batch(
            batch, source="hindi", engine=["model"], beam=5
        )
        tokens.update(zip(batch, translated, strict=True))
        print(
            f"Transliterated {min(start + 128, len(words))}/{len(words)} distinct words",
            flush=True,
        )
    for name in missing:
        cached[name] = " ".join(tokens.get(word, word) for word in name.split())
    records = []
    for name in names:
        latin = cached.get(name, name)
        latin = (
            unicodedata.normalize("NFKD", latin)
            .encode("ascii", "ignore")
            .decode()
            .lower()
        )
        suspect = bool(re.search(r"[0-9]", latin)) or not latin.strip()
        records.append(
            {
                "name": name,
                "latin": latin,
                "suspect": suspect,
                "method": (
                    "indicate model"
                    if re.search(r"[\u0900-\u097f]", name)
                    else "source Latin"
                ),
            }
        )
    pd.DataFrame(records).to_csv(target, index=False)
    metadata_path.write_text(
        json.dumps(
            {
                "model": signature,
                "inputs": {n: sha256(p) for n, p in sources.items()},
                "names": len(names),
                "model_names": len(hindi),
                "output_sha256": sha256(target),
                "interpretation": (
                    "Transliterations, not verified person identities. No sex inference."
                ),
            },
            indent=2,
        )
        + "\n"
    )
    print(
        f"Saved {len(names)} distinct names; {sum(r['suspect'] for r in records)} flagged"
    )


if __name__ == "__main__":
    main()
