import csv
from pathlib import Path

index_path = Path('resources/sigils/patterns-index.tsv')
with index_path.open() as f:
    reader = csv.reader(f, delimiter='\t')
    for row in reader:
        print(row)
