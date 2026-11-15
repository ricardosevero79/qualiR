@echo off
chcp 65001 >nul
title QualiR - modo equipe
cd /d "%~dp0"

call :acharR
if not defined RSCRIPT goto semR

echo.
echo  ==============================================================
echo    QualiR - MODO EQUIPE
echo    Os colegas (na mesma rede) devem abrir no navegador:
echo.
for /f "tokens=2 delims=:" %%i in ('ipconfig ^| findstr /c:"IPv4"') do (
  for /f "tokens=* delims= " %%j in ("%%i") do echo        http://%%j:3838
)
echo.
echo    Se o Windows perguntar sobre o Firewall, marque "Redes
echo    privadas" e clique em "Permitir acesso".
echo    NAO feche esta janela nem deixe o computador suspender.
echo  ==============================================================
echo.
"%RSCRIPT%" -e "shiny::runApp('.', host = '0.0.0.0', port = 3838, launch.browser = TRUE)"
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
