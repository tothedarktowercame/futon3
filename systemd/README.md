# Phase-5 Satisficing Zapper Timer

Install the weekly phase-5 satisficing-signature refresh as a user timer:

```bash
mkdir -p ~/.config/systemd/user
cp systemd/user-phase-5-signatures-weekly.service ~/.config/systemd/user/phase-5-signatures-weekly.service
cp systemd/user-phase-5-signatures-weekly.timer ~/.config/systemd/user/phase-5-signatures-weekly.timer

systemctl --user daemon-reload
systemctl --user enable --now phase-5-signatures-weekly.timer
```

The timer fires once per week on Sunday at 03:15 local time. The service runs
`scripts/run_phase_5_signatures_all_labels.sh`, which executes:

```bash
bb scripts/phase_5_signatures.clj --all-labels
```

and appends the stdout/stderr transcript to:

```bash
~/code/storage/futon3/phase-5-signatures/weekly.log
```

Useful checks:

```bash
systemctl --user status phase-5-signatures-weekly.timer --no-pager
journalctl --user -u phase-5-signatures-weekly.service -n 80 --no-pager
tail -n 80 ~/code/storage/futon3/phase-5-signatures/weekly.log
```
