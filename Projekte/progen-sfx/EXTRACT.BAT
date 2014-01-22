@ECHO OFF
CLS
ECHO ----------------------------------
ECHO Directories are now created at C:\
ECHO ----------------------------------
c:\
md SMFF
md SMCP
md MMFF
ECHO --------------------------------------------------
ECHO SMFF-files are extraced into the directory C:\SMFF
ECHO --------------------------------------------------
copy a:\lha\lha.exe c:\smff
copy a:\instance\smff.lzh c:\smff
c:\
cd smff
lha e smff *.*
erase smff.lzh
erase lha.exe
ECHO --------------------------------------------------
ECHO SMCP-files are extraced into the directory C:\SMCP
ECHO --------------------------------------------------
copy a:\lha\lha.exe c:\smcp
copy a:\instance\smcp.lzh c:\smcp
c:\
cd\
cd smcp
lha e smcp *.*
erase smcp.lzh
erase lha.exe
ECHO --------------------------------------------------
ECHO MMFF-files are extraced into the directory C:\MMFF
ECHO --------------------------------------------------
copy a:\lha\lha.exe c:\mmff
copy a:\instance\mmff.lzh c:\mmff
c:\
cd\
cd mmff
lha e mmff *.*
erase mmff.lzh
erase lha.exe
cd\
a:\
