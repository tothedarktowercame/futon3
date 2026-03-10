@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "SCRIPTS_DIR=%%~fI"
for %%I in ("%SCRIPTS_DIR%\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon0") do set "FUTON0_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon3c") do set "FUTON3C_ROOT=%%~fI"
set "RUN_LOG_DIR=%SCRIPT_DIR%\raw"
set "RUN_LOG=%RUN_LOG_DIR%\latest-elisp-test.log"

set "TEST_FILE_1=%REPO_ROOT%\test\elisp\aob-chatgpt-test.el"
set "TEST_FILE_2=%REPO_ROOT%\test\elisp\flexiarg-test.el"
set "FUTON0_CONTRIB=%FUTON0_ROOT%\contrib"
set "FUTON3_TEST_ELISP=%REPO_ROOT%\test\elisp"
set "FUTON3_CONTRIB=%REPO_ROOT%\contrib"

if not exist "%TEST_FILE_1%" (
  1>&2 echo [test-elisp-windows] ERROR: missing %TEST_FILE_1%
  exit /b 2
)
if not exist "%TEST_FILE_2%" (
  1>&2 echo [test-elisp-windows] ERROR: missing %TEST_FILE_2%
  exit /b 2
)
if not exist "%FUTON0_CONTRIB%" (
  1>&2 echo [test-elisp-windows] ERROR: missing %FUTON0_CONTRIB%
  1>&2 echo The current Futon3 Elisp test lane still depends on futon0/contrib.
  exit /b 3
)

if not exist "%RUN_LOG_DIR%" mkdir "%RUN_LOG_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [test-elisp-windows] ERROR: unable to create %RUN_LOG_DIR%
  exit /b 4
)

call :find_emacs EMACS_EXE
if not defined EMACS_EXE (
  1>&2 echo [test-elisp-windows] ERROR: unable to locate emacs.exe
  1>&2 echo Set EMACS_EXE or EMACS to a usable Windows Emacs binary.
  exit /b 5
)

set "PATH=%FUTON3C_ROOT%\.tools\clojure\bin;%FUTON3C_ROOT%\.tools\bin;%PATH%"

echo [test-elisp-windows] mode=local-elisp-lane
echo [test-elisp-windows] scope=broader local test lane, not mirrored by the current GitHub workflow
echo [test-elisp-windows] repo=%REPO_ROOT%
echo [test-elisp-windows] emacs=%EMACS_EXE%
echo [test-elisp-windows] futon0_contrib=%FUTON0_CONTRIB%
echo [test-elisp-windows] log=%RUN_LOG%

pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [test-elisp-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 6
)

call "%EMACS_EXE%" -Q --batch ^
  -L "%FUTON3_TEST_ELISP%" ^
  -L "%FUTON3_CONTRIB%" ^
  -L "%FUTON0_CONTRIB%" ^
  -l "%TEST_FILE_1%" ^
  -l "%TEST_FILE_2%" ^
  --eval "(ert-run-tests-batch-and-exit (quote t))" > "%RUN_LOG%" 2>&1
set "RC=%ERRORLEVEL%"
type "%RUN_LOG%"

popd >nul 2>nul
exit /b %RC%

:find_emacs
set "EMACS_OUTVAR=%~1"
set "EMACS_FOUND="

if defined EMACS_EXE if exist "%EMACS_EXE%" set "EMACS_FOUND=%EMACS_EXE%"
if not defined EMACS_FOUND if defined EMACS if exist "%EMACS%" set "EMACS_FOUND=%EMACS%"

if not defined EMACS_FOUND (
  for /f "delims=" %%P in ('where emacs.exe 2^>nul') do (
    if not defined EMACS_FOUND set "EMACS_FOUND=%%P"
  )
)

if not defined EMACS_FOUND (
  for %%P in (
    "C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe"
    "C:\Program Files\Emacs\bin\emacs.exe"
    "%ProgramFiles%\Emacs\bin\emacs.exe"
  ) do (
    if exist "%%~fP" if not defined EMACS_FOUND set "EMACS_FOUND=%%~fP"
  )
)

set "%EMACS_OUTVAR%=%EMACS_FOUND%"
exit /b 0
