@echo off
setlocal
set script=%~f0
escript.exe "%script:.cmd=%" %*
