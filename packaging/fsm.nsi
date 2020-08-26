!include LogicLib.nsh

OutFile "fsm-installer.exe"

InstallDir $PROGRAMFILES\fsm

DirText "Custom install directory"

Section

SetOutPath $INSTDIR

File "fsm.exe"
WriteUninstaller $INSTDIR\uninstall.exe

EnVar::AddValue "PATH" $INSTDIR

SectionEnd

Section "Uninstall"

Delete $INSTDIR\uninstall.exe
Delete $INSTDIR\fsm.exe
RMDir $INSTDIR
EnVar::Delete "PATH" $INSTDIR

SectionEnd

Section "" SecUninstallPrevious

Call UninstallPrevious

SectionEnd

Function UninstallPrevious
    ReadRegStr $R0 HKLM "${HKLM_REG_KEY}" "InstallDir"

    ${If} $R0 != ""
        DetailPrint "Removing previous install"
        ExecWait '"$R0\uninstall.exe" /S'
    ${EndIf}

FunctionEnd