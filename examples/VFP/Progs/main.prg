* Seta Ambiente
SET SYSFORMATS ON 
SET DEFAULT TO FULLPATH(CURDIR())
SET PATH TO PROGS;FORMS;FULLPATH(CURDIR())
        
* Configura��oes UI
ACTIVATE WINDOW SCREEN
_SCREEN.WINDOWSTATE = 2
DO FORM formprincipal.scx
ON SHUTDOWN QUIT

READ EVENTS
RELEASE ALL