@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

echo [test-windows] mode=workflow-clojure
echo [test-windows] scope=local reproduction of the current GitHub workflow test step

call "%SCRIPT_DIR%\run-clojure-test-windows.bat" "." cognitect %*
exit /b %ERRORLEVEL%
