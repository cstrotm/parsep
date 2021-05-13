PROGRAM GetInTest;

USES CRT, PSCCRT;

VAR
  t : STRING;

BEGIN
  ClrScr;
  t := GetIn(10,10,12,5,'IP:###.###.###.###');
  WriteLn(t);
  t := GetIn(10,12,12,5,'TokenRing:############');
  WriteLn(t);
END.

