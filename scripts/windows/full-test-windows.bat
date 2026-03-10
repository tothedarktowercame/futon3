@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

echo [full-test-windows] mode=local-full-suite
echo [full-test-windows] scope=best current Windows approximation of repo-native make test

echo [full-test-windows] START clojure
call "%SCRIPT_DIR%\test-windows.bat" %*
set "RC_CLOJURE=%ERRORLEVEL%"
if not "%RC_CLOJURE%"=="0" (
  1>&2 echo [full-test-windows] FAIL clojure=%RC_CLOJURE%
  exit /b %RC_CLOJURE%
)

echo [full-test-windows] START elisp
call "%SCRIPT_DIR%\test-elisp-windows.bat"
set "RC_ELISP=%ERRORLEVEL%"
if not "%RC_ELISP%"=="0" (
  1>&2 echo [full-test-windows] FAIL elisp=%RC_ELISP%
  exit /b %RC_ELISP%
)

echo [full-test-windows] PASS
exit /b 0
