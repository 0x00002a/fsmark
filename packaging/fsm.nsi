
OutFile "fsm-installer.exe"

InstallDir $PROGRAMFILES\fsm

DirText "Custom install directory"

Section

SetOutPath $INSTDIR

File "fsm.exe"
WriteUninstaller $INSTDIR\uninstall.exe


SectionEnd

Section "Uninstall"

Delete $INSTDIR\uninstall.exe
Delete $INSTDIR\fsm.exe
RMDir $INSTDIR

SectionEnd

