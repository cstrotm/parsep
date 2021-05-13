(**********************************************************************)
(*                                                                    *)
(*                                                                    *)
(* PhoeniX SoftCrew         ####   ####  ####                         *)
(* c/o Carsten Strotmann    #   # ##    ##                            *)
(*                          ####   ###  ##     Software Development   *)
(*                          #        ## ##                            *)
(*                          #     ####   ####                         *)
(*                                                                    *)
(**********************************************************************)

(*

  Programname    : Terminal Window Functions
  Filename       : WINDOW.PAS
  Author         : Carsten Strotmann
  License        : GPLv3

*)

UNIT Window;

INTERFACE

CONST
  winlo  : CHAR = 'É';
  winro  : CHAR = '»';
  winlu  : CHAR = 'È';
  winru  : CHAR = '¼';
  winwo  : CHAR = 'Í';
  winwu  : CHAR = 'Í';
  winsl  : CHAR = 'º';
  winsr  : CHAR = 'º';
  wintl  : CHAR = '[';
  wintr  : CHAR = ']';

TYPE
  wintype = RECORD
              num : BYTE;
              xpos,
              ypos,
              xlen,
              ylen,
              curx,
              cury,
              size  : WORD;
              titel : STRING[76];
              mem,
              next : POINTER;
            END;

  winptr = ^wintype;

VAR
  winshadow : BOOLEAN;
  wincut    : BOOLEAN;
  winuse    : ARRAY [0..$FF] OF BOOLEAN;
  winstart  : winptr;
  winact    : winptr;

FUNCTION  OpenWin (x1, y1, x2, y2 : BYTE; str : STRING) : BYTE;
FUNCTION  CloseWin (num : BYTE): BOOLEAN;
PROCEDURE WriteWinPos ( num, x, y : BYTE; str : STRING);
PROCEDURE WriteWinCenter (num, y : BYTE; str : STRING);
FUNCTION  WinGetIn (num,x,y,len,mode : BYTE; str : STRING):STRING;
PROCEDURE ErrorWin (str : STRING);
PROCEDURE WinTextAttr (num,x,y,len,attr : BYTE);
PROCEDURE WinClear (num : BYTE);
PROCEDURE WinFillText (num, x1, y1, x2, y2 : BYTE; c :CHAR);
FUNCTION  ListBox (x, y, xlen, ylen: BYTE; titel, list : STRING): STRING;

IMPLEMENTATION

USES CRT, PSCCRT, PSCDOS, MOUSE;

VAR
  u : BYTE;
  exitsave : POINTER;

FUNCTION SaveWinScreen (x1, y1, x2, y2 : WORD) : POINTER;
VAR
  x, y,
  off,
  xlen,
  ylen,
  memneed : WORD;
  memptr,
  ptr     : ^BYTE;

BEGIN
  IF winshadow THEN
  BEGIN
    Inc (x2);
    Inc (y2);
    xlen := x2-x1+1;
    ylen := y2-y1+1;
  END
  ELSE
  BEGIN
    xlen := x2-x1+1;
    ylen := y2-y1+1;
  END;
  memneed := xlen*ylen*2;

  IF memneed >= MaxAvail THEN
    SaveWinScreen := NIL
  ELSE
  BEGIN
    GetMem (ptr, memneed);
    memptr := ptr;
    FOR y := y1-1 TO y2-1 DO
      FOR x := x1-1 TO x2-1 DO
      BEGIN
        off := y*160+x*2;
        memptr^ := MEM[screenmem:off];
        Inc(memptr);
        memptr^ := MEM[screenmem:off+1];
        Inc(memptr);
      END;
    SaveWinScreen := ptr;
  END;
END;

Procedure RestoreWinScreen (x1, y1, xlen, ylen : WORD; ptr : POINTER);
VAR
  x, y,
  off,
  x2,
  y2,
  memneed : WORD;
  memptr  : ^BYTE;

BEGIN
  HideMPointer;
  IF winshadow THEN
  BEGIN
    Inc (xlen);
    Inc (ylen);
    x2 := x1 + xlen;
    y2 := y1 + ylen;
  END
  ELSE
  BEGIN
    x2 := x1 + xlen;
    y2 := y1 + ylen;
  END;
  memneed := (xlen+1)*(ylen+1)*2;
  memptr := ptr;
  FOR y := y1-1 TO y2-1 DO
    FOR x := x1-1 TO x2-1 DO
    BEGIN
      off := y*160+x*2;
      MEM[screenmem:off] := memptr^;
      Inc(memptr);
      MEM[screenmem:off+1] := memptr^;
      Inc(memptr);
    END;
  FreeMem(ptr, memneed);
  ShowMPointer;
END;

PROCEDURE WinFrame (x1,y1,x2,y2 :BYTE;  Name : STRING);

VAR
  i, u, x, y,
  xlen, ylen  : BYTE;
  laenge      : INTEGER;
  hoch, breit : BYTE;
  titel       : BOOLEAN;

BEGIN
  HideMPointer;
  FillTextBox (x1, y1, x2, y2,' ');
  breit := x2-x1;
  hoch  := y2-y1;
  titel := TRUE;
  laenge := Length (name);
  IF laenge > breit THEN
    titel := FALSE;
  IF titel THEN
  BEGIN
    GotoXY (x1,y1);
    Write (winlo);
    FOR i := 2 TO breit DO
       Write (winwo);
    Write (winro);
    GotoXY (x2-Round(breit/2)-Round(laenge/2),y1);
    Write (wintl+name+wintr);
  END
  ELSE
  BEGIN
    GotoXY (x1,y1);
    Write (winlo);
    FOR i:= 2 TO Breit DO
      Write (winwo);
    Write (winro);
  END;
  FOR i:=1 TO hoch-1 DO
  BEGIN
    GotoXY (x1,y1+i);
    Write (winsl);
    GotoXY (x2,y1+i);
    Write (winsr);
  END;
  GotoXY (x1,y2);
  Write (winlu);
  FOR i:= 2 TO breit DO
    Write (winwu);
  Write (winru);
  IF winshadow THEN
  BEGIN
    y := y2;
    FOR u := x1+1 TO x2 DO
      Mem[screenmem:(y*160)+(u*2)-1]:=Mem[screenmem:(y*160)+(u*2)-1] AND $0F;
    x := x2+1;
    FOR u := y1 TO y2 DO
      Mem[screenmem:(u*160)+(x*2)-1]:=Mem[screenmem:(u*160)+(x*2)-1] AND $0F;
  END;
  ShowMPointer;
END;

FUNCTION OpenWin (x1, y1, x2, y2 : BYTE; str : STRING) : BYTE;
VAR
  winnum : BYTE;
  winp : WINPTR;
BEGIN
  HideMPointer;
  u := 0;
  winact := winstart;
  WHILE winuse[u] = TRUE DO
  BEGIN
    Inc (u);
    IF u > 1 THEN
      winact := winact^.next;
  END;

  IF MaxAvail > SizeOf(wintype) THEN
  BEGIN
    New (winp);
    IF winstart = NIL THEN
    BEGIN
      winstart := winp;
      winact   := winp;
      winact^.next := NIL;
    END
    ELSE
    BEGIN
      winp^.next := winact^.next;
      winact^.next := winp;
      winact := winp;
    END;
    winact^.num := u;
    winact^.xpos := x1;
    winact^.ypos := y1;
    winact^.xlen := x2-x1;
    winact^.ylen := y2-y1;
    winact^.curx := 1;
    winact^.cury := 1;
    winact^.size := winact^.xlen * winact^.ylen * 2;
    winact^.titel:= str;
    winact^.mem  := SaveWinScreen (x1,y1,x2,y2);
    WinFrame (x1,y1,x2,y2,str);
    OpenWin := u;
    winuse[u] := TRUE;
  END
  ELSE
    OpenWin := $FF;
  ShowMPointer;
END;

FUNCTION CloseWin (num : BYTE): BOOLEAN;
VAR
  win, lwin : WINPTR;

BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
  BEGIN
    lwin := winact;
    winact := winact^.next;
  END;

  IF winact = NIL THEN
    CloseWin := FALSE
  ELSE
  BEGIN
    IF winact = winstart THEN
       winstart := winact^.next
    ELSE
       lwin^.next := winact^.next;

    RestoreWinScreen (winact^.xpos, winact^.ypos, winact^.xlen, winact^.ylen, winact^.mem);
    Dispose (winact);
    winuse[num] := FALSE;
  END;
  ShowMPointer;
END;

PROCEDURE WriteWinPos ( num, x, y : BYTE; str : STRING);
BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;

  IF wincut THEN
    str := Copy (str,1,winact^.xlen-x);

  IF winact^.num = num THEN
    WritePos (winact^.xpos+x, winact^.ypos+y, str);
  ShowMPointer;
END;

PROCEDURE WinClear (num :BYTE);
BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;

  IF winact^.num = num THEN
    FillTextBox (winact^.xpos+1, winact^.ypos+1, winact^.xpos+winact^.xlen-1, winact^.ypos+winact^.ylen-1,' ');
  ShowMPointer;
END;

PROCEDURE WinFillText (num, x1, y1, x2, y2 : BYTE; c :CHAR);
BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;

  IF winact^.num = num THEN
    FillTextBox (winact^.xpos+x1, winact^.ypos+y1, winact^.xpos+x2, winact^.ypos+y2,c);
  ShowMPointer;
END;

PROCEDURE WriteWinCenter (num, y : BYTE; str : STRING);
VAR
  len,
  x : BYTE;

BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;

  len := Length(str);
  IF len > winact^.xlen THEN
    len := winact^.xlen;

  x := (winact^.xlen-len) DIV 2 + 1;

  IF winact^.num = num THEN
    WritePos (winact^.xpos+x, winact^.ypos+y, Copy(str,1,winact^.xlen-x));
  ShowMPointer;
END;

FUNCTION WinGetIn (num,x,y,len,mode : BYTE; str : STRING):STRING;
BEGIN
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;

  IF len > (winact^.xlen-x) THEN
    len := winact^.xlen-x;

  x := x+winact^.xpos;
  y := y+winact^.ypos;

  WinGetIn := GetIn (x,y,len,mode,str);
END;

PROCEDURE ErrorWin (str : STRING);
VAR
  x1, x2, len, win, textatr : BYTE;
BEGIN
  HideMPointer;
  len := Length(str)+2;
  IF len > 78 THEN len := 78;
  x1 := 40 - len DIV 2;
  x2 := x1 + len+1;
  textatr := crt.textattr;
  TextBackGround (RED);
  TextColor (YELLOW);
  win := OpenWin (x1,14,x2,16,'ERROR');
  WriteWinPos (win,2,1,str);
  Beep;
  Key;
  CloseWin (win);
  TextBackGround (textatr SHR 4);
  TextColor (textatr AND $F);
  ShowMPointer;
END;

PROCEDURE WinTextAttr (num,x,y,len,attr : BYTE);
VAR
  u  : INTEGER;
  xl : BYTE;
BEGIN
  HideMPointer;
  winact := winstart;
  WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
    winact := winact^.next;
  x := x+winact^.xpos;
  y := y+winact^.ypos;
  xl := x+len;
  IF xl > (winact^.xlen + winact^.xpos - 1) THEN
    xl := winact^.xlen + winact^.xpos - 1;
  Dec (y);
  FOR u := x TO xl DO
    Mem[screenmem:(y*160)+(u*2)-1]:=attr;
  ShowMPointer;
END;

{$F+}
PROCEDURE ExitWin;
VAR
  x1, x2, len, win, textatr : BYTE;
  strn : STRING;
BEGIN
  HideMPointer;
  ExitProc := ExitSave;

  IF ErrorAddr <> NIL THEN
  BEGIN
    ErrorAddr := NIL;
    Str(ExitCode, strn);
    strn := 'Laufzeitfehler ' + strn;

    len := Length(strn)+2;
    IF len > 78 THEN len := 78;
    x1 := 40 - len DIV 2;
    x2 := x1 + len+1;
    textatr := crt.textattr;
    TextBackGround (RED);
    TextColor (YELLOW);
    win := OpenWin (x1,14,x2,18,'ERROR');
    WriteWinCenter (win,2,strn);
    WriteWinCenter (win,3,'< T A S T E >');
    Beep;
    Key;
    CloseWin (win);
    TextBackGround (textatr SHR 4);
    TextColor (textatr AND $F);
  END;
  ShowMPointer;
END;
{$F-}

FUNCTION ListBox (x, y, xlen, ylen: BYTE; titel, list : STRING): STRING;
VAR
  listelem : BYTE;
  lofs,
  lpos,
  lnum,
  llen,
  lmax,
  p,
  win      : BYTE;
  listarr  : ARRAY[0..30] OF STRING[80];
  rc       : STRING;

  FUNCTION GetText (x1, y1 : BYTE; text: STRING): STRING;
  VAR
    pos,l,u, len, x, y, z,
    crsr,f,attr       : BYTE;
    k                 : WORD;
    kk, lin           : CHAR;
    ins, gr           : BOOLEAN;
    value             : SET OF CHAR;
  BEGIN
    x := x1 + 1;
    y := y1 + 1;

    lin := ' ';
    len := llen;
    l   := Length(text);

    FOR u := l+1 TO len DO
      text[u] := lin;

    pos := 0;
    ins := TRUE;
    crsr := 72;

    C_Off;
    REPEAT
{      text[0]:=Chr(len);}

      WritePos (x, y, text);

      IF lnum < lmax - 1 THEN
        z := lnum
      ELSE
        z := lmax - 1;

      FOR u := 0 TO z DO
      BEGIN
        IF lpos >= lmax - 1 THEN
          lofs := lpos - lmax + 1
        ELSE
          lofs := 0;
        FOR z := 1 TO xlen-1 DO
          WriteWinPos (win, z, 3+u, ' ');
        WriteWinPos (win, 1, 3+u, listarr[u+lofs]);
      END;
      IF lnum > lmax - 1 THEN
        IF lofs > 0 THEN
          WritePos (x1+xlen-1, y1+3, #30)
        ELSE
          WritePos (x1+xlen-1, y1+ylen-1, #31);

      WinTextAttr (win, 1, 3+lpos-lofs, llen, BLUE*$10+WHITE);

      value := [' '..'z'];

      k := Key;

      IF k < $100 THEN
        kk := Chr(k)
      ELSE
        kk := Chr(0);

      IF (k = 336) AND (lpos < lnum) THEN
      BEGIN
        FOR u := 1 TO llen DO
          text[u] := ' ';
        WritePos (x, y, text);
        Inc(lpos);
        text := listarr[lpos];
      END;
      IF (k = 328) AND (lpos > 0) THEN
      BEGIN
        FOR u := 1 TO llen DO
          text[u] := ' ';
        WritePos (x, y, text);
        Dec(lpos);
        text := listarr[lpos];
      END;

{       IF (l = 1) THEN
        BEGIN
          u := $FF;
          REPEAT
            Inc(u);
          UNTIL (UpString(listarr[u]) >= UpString(text)) OR (u > lnum);
          IF u < lnum THEN
            lpos := u;
          text := listarr[lpos];
        END;}
        pos := 0;

    UNTIL k = 13;

    WritePos (x, y, text);

 {   text[0]:=Chr(Length(listarr[pos])); }
    C_On;
    GETTEXT := text;
  END;

BEGIN
  lnum := 0;
  llen := 0;
  IF ylen < 3 THEN
    ylen := 3;

  WHILE BYTE(list[0]) > 1 DO
  BEGIN
    p := Pos(';', list);
    IF p = 0 THEN
      p := Length(list);
    listarr[lnum] := Copy(list, 1, p - 1);
    list := Copy(list, p + 1, Length(list));
    IF Length(listarr[lnum]) > llen THEN
      llen := Length(listarr[lnum]);
    Inc (lnum);
  END;
  Dec(lnum);
  IF llen < Length(titel) + 2 THEN
    llen := Length(titel) + 2;

  REPEAT
    p := 0;
    FOR u := 0 TO lnum-1 DO
      IF listarr[u] > listarr[u+1] THEN
      BEGIN
        rc := listarr[u+1];
        listarr[u+1] := listarr[u];
        listarr[u] := rc;
        p := 1;
      END
  UNTIL p = 0;


  IF xlen = 0 THEN
    xlen := llen + 2;

  lmax := ylen - 3;
  lofs := 0;
  lpos := 0;

  win := OpenWin(x, y, x + xlen, y + ylen, titel);
  FOR p := 1 TO xlen DO
    WriteWinPos (win,p,2,'Ä');

  rc := GetText (x, y, listarr[lpos]);

  CloseWin(win);
  ListBox := rc;
END;

BEGIN
  FOR u := 0 TO $FF DO
    winuse[u] := FALSE;
  winstart := NIL;
  winact := NIL;
{  exitsave := ExitProc;
  ExitProc := @ExitWin;
}  winshadow := TRUE;
  wincut    := TRUE;
END.
