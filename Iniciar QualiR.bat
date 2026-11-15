@echo off
chcp 65001 >nul
title QualiR
cd /d "%~dp0"

call :acharR
if not defined RSCRIPT goto semR

echo.
echo  ==============================================
echo    QualiR - iniciando...
echo    O navegador vai abrir em alguns segundos.
echo    NAO feche esta janela enquanto usar o QualiR.
echo    Para encerrar, feche esta janela.
echo  ==============================================
echo.
"%RSCRIPT%" -e "shiny::runApp('.', port = 3838, launch.browser = TRUE)"
echo.
echo  O QualiR foi encerrado. Se apareceu algum erro acima, copie a mensagem.
pause
exit /b

:acharR
set "RSCRIPT="
for %%P in ("%ProgramFiles%\R" "%LOCALAPPDATA%\Programs\R" "%ProgramFiles(x86)%\R") do (
  if exist "%%~P" (
    for /f "delims=" %%d in ('dir /b /ad /o-d "%%~P\R-*" 2^>nul') do (
      if not defined RSCRIPT if exist "%%~P\%%d\bin\Rscript.exe" set "RSCRIPT=%%~P\%%d\bin\Rscript.exe"
    )
  )
)
if not defined RSCRIPT for /f "delims=" %%r in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%r"
exit /b

:semR
echo.
echo  Nao encontrei o R instalado neste computador.
echo  Instale o R em https://cran.r-project.org e tente novamente.
pause
exit /b
