#!/usr/bin/env python3
"""Helper to inspect pattern index and type vocab."""
import csv
from pathlib import Path

INDEX = Path('resources/sigils/patterns-index.tsv')
VOCAB = Path('resources/type_vocab.txt')

if INDEX.exists():
    with INDEX.open() as f:
        reader = csv.reader(f, delimiter='\t')
        for row in reader:
            print(row)
else:
    print("Index missing")
