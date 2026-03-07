@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "SCRIPTS_DIR=%%~fI"
for %%I in ("%SCRIPTS_DIR%\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon3c") do set "FUTON3C_ROOT=%%~fI"
set "CLOJURE_BAT=%FUTON3C_ROOT%\.tools\clojure\bin\clojure.bat"
for %%I in ("%CLOJURE_BAT%") do set "CLOJURE_BAT=%%~fI"
set "RUN_LOG_DIR=%REPO_ROOT%\scripts\windows\raw"

if "%~1"=="" goto usage
set "WORK_DIR_REL=%~1"
shift
if "%~1"=="" goto usage
set "TEST_MODE=%~1"
shift

set "EXTRA_ARGS="
:collect_extra
if "%~1"=="" goto after_collect_extra
set "EXTRA_ARGS=%EXTRA_ARGS% %~1"
shift
goto collect_extra

:after_collect_extra
set "WORK_DIR=%REPO_ROOT%\%WORK_DIR_REL%"
if /I "%WORK_DIR_REL%"=="." set "WORK_DIR=%REPO_ROOT%"
if not exist "%WORK_DIR%" (
  1>&2 echo [run-clojure-test-windows] ERROR: work directory not found: %WORK_DIR%
  exit /b 2
)
if not exist "%RUN_LOG_DIR%" mkdir "%RUN_LOG_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to create %RUN_LOG_DIR%
  exit /b 7
)
set "WORK_DIR_SLUG=%WORK_DIR_REL:/=-%"
set "WORK_DIR_SLUG=%WORK_DIR_SLUG:\=-%"
set "RUN_LOG=%RUN_LOG_DIR%\latest-%WORK_DIR_SLUG%-test.log"

pushd "%WORK_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to enter %WORK_DIR%
  exit /b 3
)

:run_test
echo [run-clojure-test-windows] workdir=%WORK_DIR_REL%
if not exist "%CLOJURE_BAT%" (
  1>&2 echo [run-clojure-test-windows] ERROR: missing %CLOJURE_BAT%
  1>&2 echo Ensure sibling futon3c tools are bootstrapped.
  set "RC=4"
  goto done
)
call :configure_futon3c_clojure_state
if errorlevel 1 (
  set "RC=6"
  goto done
)

if /I "%TEST_MODE%"=="default" goto run_default
if /I "%TEST_MODE%"=="cognitect" goto run_cognitect

1>&2 echo [run-clojure-test-windows] ERROR: unknown TEST_MODE=%TEST_MODE%
set "RC=5"
goto done

:run_default
echo [run-clojure-test-windows] command="%CLOJURE_BAT%" -M:test%EXTRA_ARGS%
call "%CLOJURE_BAT%" -M:test%EXTRA_ARGS% > "%RUN_LOG%" 2>&1
type "%RUN_LOG%"
goto capture_rc

:run_cognitect
echo [run-clojure-test-windows] command="%CLOJURE_BAT%" -M:test -m cognitect.test-runner%EXTRA_ARGS%
call "%CLOJURE_BAT%" -M:test -m cognitect.test-runner%EXTRA_ARGS% > "%RUN_LOG%" 2>&1
type "%RUN_LOG%"
goto capture_rc

:capture_rc
set "RC=%ERRORLEVEL%"
call :analyze_test_log "%RUN_LOG%"
set "LOG_RC=%ERRORLEVEL%"
if not "%LOG_RC%"=="0" (
  if "%RC%"=="0" set "RC=%LOG_RC%"
)

:done
popd >nul 2>nul
exit /b %RC%

:configure_futon3c_clojure_state
set "CLJ_CONFIG=%FUTON3C_ROOT%\.clj-config"
set "LOCAL_M2=%FUTON3C_ROOT%\.m2\repository"
set "LOCAL_GITLIBS=%FUTON3C_ROOT%\.gitlibs"
set "LOCAL_TMP_DIR=%FUTON3C_ROOT%\.tmp\java"

if not exist "%FUTON3C_ROOT%" (
  1>&2 echo [run-clojure-test-windows] ERROR: missing FUTON3C_ROOT=%FUTON3C_ROOT%
  exit /b 1
)
if not exist "%CLJ_CONFIG%" mkdir "%CLJ_CONFIG%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to create %CLJ_CONFIG%
  exit /b 1
)
if not exist "%LOCAL_M2%" mkdir "%LOCAL_M2%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to create %LOCAL_M2%
  exit /b 1
)
if not exist "%LOCAL_GITLIBS%" mkdir "%LOCAL_GITLIBS%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to create %LOCAL_GITLIBS%
  exit /b 1
)
if not exist "%LOCAL_TMP_DIR%" mkdir "%LOCAL_TMP_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to create %LOCAL_TMP_DIR%
  exit /b 1
)

set "LOCAL_TMP=%LOCAL_TMP_DIR:\=/%"
set "TMP=%LOCAL_TMP%"
set "TEMP=%LOCAL_TMP%"
set "CLJ_CONFIG=%CLJ_CONFIG%"
set "GITLIBS=%LOCAL_GITLIBS%"
set "HTTP_PROXY="
set "HTTPS_PROXY="
set "ALL_PROXY="
set "GIT_HTTP_PROXY="
set "GIT_HTTPS_PROXY="
if defined JAVA_OPTS (
  set "JAVA_OPTS=-Djava.io.tmpdir=%LOCAL_TMP% %JAVA_OPTS%"
) else (
  set "JAVA_OPTS=-Djava.io.tmpdir=%LOCAL_TMP%"
)
if defined CLJ_JVM_OPTS (
  set "CLJ_JVM_OPTS=-Djava.io.tmpdir=%LOCAL_TMP% %CLJ_JVM_OPTS%"
) else (
  set "CLJ_JVM_OPTS=-Djava.io.tmpdir=%LOCAL_TMP%"
)

set "LOCAL_M2_EDN=%LOCAL_M2:\=/%"
> "%CLJ_CONFIG%\deps.edn" (
  echo {
  echo   :mvn/local-repo "%LOCAL_M2_EDN%"
  echo   :aliases {}
  echo }
)
if errorlevel 1 (
  1>&2 echo [run-clojure-test-windows] ERROR: unable to write %CLJ_CONFIG%\deps.edn
  exit /b 1
)

exit /b 0

:analyze_test_log
set "LOG_PATH=%~1"
if not exist "%LOG_PATH%" exit /b 11

findstr /I /C:"ERROR in (" "%LOG_PATH%" >nul && exit /b 12
findstr /I /C:"Execution error (" "%LOG_PATH%" >nul && exit /b 12
findstr /I /C:"Uncaught exception, not in assertion." "%LOG_PATH%" >nul && exit /b 12
findstr /I /C:"Error building classpath." "%LOG_PATH%" >nul && exit /b 17
findstr /I /C:"Invalid library spec:" "%LOG_PATH%" >nul && exit /b 17

findstr /R /C:"Ran [0-9][0-9]* tests containing" "%LOG_PATH%" >nul
if not errorlevel 1 (
  findstr /I /C:"0 failures, 0 errors." "%LOG_PATH%" >nul
  if errorlevel 1 exit /b 16
)

exit /b 0

:usage
1>&2 echo Usage: run-clojure-test-windows.bat WORK_DIR TEST_MODE [ARGS...]
1>&2 echo TEST_MODE: default ^| cognitect
1>&2 echo Example: run-clojure-test-windows.bat "." cognitect
exit /b 64
