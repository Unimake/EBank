SET source=%1%
SET destDir=%2%
@ECHO Definindo o arquivo %source% para uso
@ECHO Definindo o diretório %destDir% para uso
@cls
ECHO Assinando arquivo %source%
:: SET signtool="C:\Program Files (x86)\Microsoft SDKs\ClickOnce\SignTool\signtool.exe"
:: SET signtoolParams=sign /n "UNIMAKE"
:: CALL %signtool% %signtoolParams% %source%
:: ECHO Sign ERRORLEVEL = %errorlevel%

C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %source% /codebase /tlb
ECHO Register tlb ERRORLEVEL = %errorlevel%

C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %destDir%EBank.Solutions.Primitives.dll /codebase /tlb
ECHO Register tlb ERRORLEVEL = %errorlevel%

C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe %destDir%Unimake.AuthServer.dll /codebase /tlb
ECHO Register tlb ERRORLEVEL = %errorlevel%

:exit
EXIT 0 /B