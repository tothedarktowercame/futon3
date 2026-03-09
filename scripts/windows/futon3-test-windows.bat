@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
set "RAW_DIR=%SCRIPT_DIR%\raw"
set "RECEIPT_PATH=%RAW_DIR%\latest-futon3-test-receipt.json"
if defined FUTON3_TEST_RECEIPT set "RECEIPT_PATH=%FUTON3_TEST_RECEIPT%"
set "RUNNER_PATH=%SCRIPT_DIR%\test-windows.bat"

set "WORKFLOW_REL=.github/workflows/test.yml"
set "WORKFLOW_PATH=%REPO_ROOT%\%WORKFLOW_REL%"
set "WF_JOB_TEST=Golden Transcript + Unit Tests"
set "WF_DIR_TEST=."
set "WF_CMD_TEST=clojure -M:test -m cognitect.test-runner"

set "RECEIPT_ERROR=0"
set "FAILURES=0"
set "WORKFLOW_SHA256=missing"
set "WORKFLOW_EXISTS=false"
if exist "%WORKFLOW_PATH%" set "WORKFLOW_EXISTS=true"
call :file_sha256 "%WORKFLOW_PATH%" WORKFLOW_SHA256
if not defined WORKFLOW_SHA256 set "WORKFLOW_SHA256=missing"

call :utc_now_iso RUN_START_ISO
call :epoch_now RUN_START_SECONDS

set "CMD_TEST=%RUNNER_PATH% %*"

echo [futon3-test-windows] mode=workflow-receipt
echo [futon3-test-windows] scope=local reproduction of .github/workflows/test.yml

echo [futon3-test-windows] START %WF_JOB_TEST%
call :utc_now_iso START_TEST_ISO
call :epoch_now START_TEST_SECONDS
call "%RUNNER_PATH%" %*
set "RC_TEST=%ERRORLEVEL%"
call :epoch_now END_TEST_SECONDS
call :utc_now_iso END_TEST_ISO
call :duration_seconds "%START_TEST_SECONDS%" "%END_TEST_SECONDS%" DUR_TEST_SECONDS
if not "%RC_TEST%"=="0" set /a FAILURES+=1

call :utc_now_iso RUN_END_ISO
call :epoch_now RUN_END_SECONDS
call :duration_seconds "%RUN_START_SECONDS%" "%RUN_END_SECONDS%" RUN_DURATION_SECONDS

if "%DUR_TEST_SECONDS%"=="" set "DUR_TEST_SECONDS=0"
if "%RUN_DURATION_SECONDS%"=="" set "RUN_DURATION_SECONDS=0"

if %FAILURES% GTR 0 (
  set "TEST_EXIT_CODE=1"
  set "AGGREGATE_VERDICT=fail"
) else (
  set "TEST_EXIT_CODE=0"
  set "AGGREGATE_VERDICT=pass"
)

call :write_receipt
if errorlevel 1 (
  set "RECEIPT_ERROR=1"
  1>&2 echo [futon3-test-windows] ERROR unable to write receipt: %RECEIPT_PATH%
)

echo [futon3-test-windows] SUMMARY test=%RC_TEST%
echo [futon3-test-windows] RECEIPT %RECEIPT_PATH%
if "%TEST_EXIT_CODE%"=="1" (
  1>&2 echo [futon3-test-windows] FAIL failures=%FAILURES%
  exit /b 1
)
if "%RECEIPT_ERROR%"=="1" (
  1>&2 echo [futon3-test-windows] FAIL receipt-write
  exit /b 2
)

echo [futon3-test-windows] PASS
exit /b 0

:write_receipt
if not exist "%RAW_DIR%" mkdir "%RAW_DIR%" >nul 2>nul
if errorlevel 1 exit /b 1

call :json_escape REPO_ROOT ESC_REPO_ROOT
call :json_escape RECEIPT_PATH ESC_RECEIPT_PATH
if not defined ESC_RECEIPT_PATH (
  set "ESC_RECEIPT_PATH=%RECEIPT_PATH%"
  set "ESC_RECEIPT_PATH=!ESC_RECEIPT_PATH:\=\\!"
  set "ESC_RECEIPT_PATH=!ESC_RECEIPT_PATH:"=\"!"
)
call :json_escape CMD_TEST ESC_CMD_TEST
set "ESC_WORKFLOW_REL=%WORKFLOW_REL%"
call :json_escape WORKFLOW_SHA256 ESC_WORKFLOW_SHA256
call :json_escape WF_JOB_TEST ESC_WF_JOB_TEST
call :json_escape WF_DIR_TEST ESC_WF_DIR_TEST
call :json_escape WF_CMD_TEST ESC_WF_CMD_TEST

> "%RECEIPT_PATH%" (
  echo {
  echo   "schema": "futon3-test-receipt/v1",
  echo   "generated_at_utc": "%RUN_END_ISO%",
  echo   "runner": "scripts/windows/futon3-test-windows.bat",
  echo   "repo_root": "%ESC_REPO_ROOT%",
  echo   "receipt_path": "%ESC_RECEIPT_PATH%",
  echo   "workflow_source": "%ESC_WORKFLOW_REL%",
  echo   "workflow_source_exists": %WORKFLOW_EXISTS%,
  echo   "workflow_source_sha256": "%ESC_WORKFLOW_SHA256%",
  echo   "jobs": [
  echo     {
  echo       "name": "test",
  echo       "workflow_job_name": "%ESC_WF_JOB_TEST%",
  echo       "workflow_workdir": "%ESC_WF_DIR_TEST%",
  echo       "workflow_command": "%ESC_WF_CMD_TEST%",
  echo       "command": "%ESC_CMD_TEST%",
  echo       "start_utc": "%START_TEST_ISO%",
  echo       "end_utc": "%END_TEST_ISO%",
  echo       "duration_seconds": %DUR_TEST_SECONDS%,
  echo       "exit_code": %RC_TEST%
  echo     }
  echo   ],
  echo   "summary": {
  echo     "start_utc": "%RUN_START_ISO%",
  echo     "end_utc": "%RUN_END_ISO%",
  echo     "duration_seconds": %RUN_DURATION_SECONDS%,
  echo     "failures": %FAILURES%,
  echo     "aggregate_exit_code": %TEST_EXIT_CODE%,
  echo     "aggregate_verdict": "%AGGREGATE_VERDICT%"
  echo   }
  echo }
)
if errorlevel 1 exit /b 1
exit /b 0

:json_escape
set "ESC_VALUE=!%~1!"
set "ESC_VALUE=!ESC_VALUE:\=\\!"
set "ESC_VALUE=!ESC_VALUE:"=\"!"
set "%~2=!ESC_VALUE!"
exit /b 0

:utc_now_iso
set "%~1="
for /f "usebackq delims=" %%I in (`powershell -NoProfile -Command "Get-Date -Format o"`) do set "%~1=%%I"
if not defined %~1 set "%~1=unknown"
exit /b 0

:epoch_now
set "%~1="
for /f "usebackq delims=." %%I in (`powershell -NoProfile -Command "Get-Date -UFormat %%s"`) do set "%~1=%%I"
if not defined %~1 set "%~1=0"
exit /b 0

:duration_seconds
set "%~3=0"
if "%~1"=="" exit /b 0
if "%~2"=="" exit /b 0
set /a "%~3=%~2-%~1" >nul 2>nul
if errorlevel 1 set "%~3=0"
if !%~3! LSS 0 set "%~3=0"
exit /b 0

:file_sha256
set "%~2="
if "%~1"=="" exit /b 0
if not exist "%~1" exit /b 0
set "HASH_VALUE="
for /f "usebackq tokens=1" %%I in (`certutil -hashfile "%~1" SHA256 ^| findstr /R /I "^[0-9a-f][0-9a-f]*$"`) do (
  set "HASH_VALUE=%%I"
)
if defined HASH_VALUE set "%~2=%HASH_VALUE%"
set "HASH_VALUE="
exit /b 0
