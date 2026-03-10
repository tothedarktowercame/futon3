# Futon3 Windows Test Scripts

This directory now distinguishes two related but different lanes:

- workflow mode
  - preserve the current GitHub workflow locally
  - mirror `.github/workflows/test.yml`
  - keep receipt-oriented evidence where that already exists
- local testing lane
  - run broader repo-native test surfaces from Windows
  - may be wider than the current workflow while we are still onboarding and
    proving the Windows path

Current scripts:

- `futon3-test-windows.bat`
  - workflow mode
  - receipt-oriented local reproduction of `.github/workflows/test.yml`
- `test-windows.bat`
  - workflow mode
  - narrower Clojure runner used by the receipt wrapper
- `test-elisp-windows.bat`
  - local testing lane
  - current Windows wrapper for the Elisp phase of the repo-native suite
  - currently depends on:
    - sibling `futon0/contrib`
    - shared tools/state from sibling `futon3c/.tools`
- `full-test-windows.bat`
  - local testing lane
  - best current Windows approximation of repo-native `make test`
  - sequences:
    - `test-windows.bat`
    - `test-elisp-windows.bat`

Operator note:

- preserve the workflow lane even when the local testing lane grows broader
- reuse the workflow scripts where possible rather than forking the Clojure
  path
- treat the local testing lane as the place to encode additional Windows
  knowledge needed to approach the repo-native suite
