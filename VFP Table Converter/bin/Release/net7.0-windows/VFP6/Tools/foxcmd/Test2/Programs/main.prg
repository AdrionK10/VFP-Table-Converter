** Set up screen:
_SCREEN.Caption = "FoxPro Command Emulator"

*_SCREEN.BackColor = RGB(128,128,128)
_SCREEN.BackColor = RGB(192,192,192)


_SCREEN.Left = 20
_SCREEN.Width = SYSMETRIC(1)-(_SCREEN.Left * 2)
_SCREEN.Top = 20
_SCREEN.Height = SYSMETRIC(2)-(_SCREEN.Top * 2) - 100

DO FORM command

READ EVENTS

CLOSE ALL
CLEAR ALL
RELEASE ALL EXTENDED