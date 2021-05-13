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

  Programname    : PSCCRT UNIT
  Filename       : PSCCRT.PAS
  Author         : Carsten Strotmann
  License        : GPLv3

*)

UNIT PSCCRT;

INTERFACE

USES DOS, CRT, GRAPH;

VAR
  savetxtattr   : BYTE;
  getin_lastkey : WORD;

PROCEDURE TextAttr (x,y,len,attr : BYTE);
{ Ver„ndert Textattribut }

PROCEDURE SaveTextAttr;
{ Sichert Textfarben }

PROCEDURE RestoreTextAttr;
{ Restauriert Textfarben }

FUNCTION GetAttr (x,y : WORD): BYTE;
{Laed Textattribut}

PROCEDURE WritePos (x,y : BYTE; text :STRING);
{ Textausgabe an Stelle x/y }

PROCEDURE WriteCenter (y : BYTE; text : STRING);
{ Textausgabe zentriert in Zeile Y }

PROCEDURE TextFrame (x1,y1,x2,y2 : BYTE; name : STRING);
{ Zeichnet Rahmen }

PROCEDURE C_Off;
{ Schaltet TextCursor aus }

PROCEDURE C_On;
{ Schaltet TextCursor an}

PROCEDURE Beep;
{ Gibt Warnbeep aus }

FUNCTION  UpString (str : STRING) : STRING;
{ Wandelt String in Grossbuchstaben }

FUNCTION  LowString (str : STRING) : STRING;
{ Wandelt String in Kleinbuchstaben }

FUNCTION NextChar (s : STRING; p : BYTE): BYTE;
{ Findet  n„chstes Zeichen in s ab Stelle p }

FUNCTION GetIn (x,y,len,mode : BYTE; txt : STRING): STRING;
{ Liest einen String ein
  MODE = 0 numerisch
         1 alphanumerisch
         2
         3 Verdeckt
         4 Datum }

FUNCTION  WildCard (str, cmp : STRING) : BOOLEAN;
{ Vergleicht STR mit cmp mit Wildcards }

FUNCTION  Key : INTEGER;
{ Wartet auf Tastendruck und gibt Code zurck }

FUNCTION  Stick (number : BYTE) : BYTE;
{ Joystickabfrage nach ATARI }

FUNCTION  Strig (number : BYTE) : BYTE;
{ Triggerabfrage nach ATARI }

PROCEDURE Invert (x1, y1, x2, y2 : WORD);
{ Invertiert Bildschirmausschnitt }

PROCEDURE ClearBox (x1, y1, x2, y2 : WORD);
PROCEDURE ClearText (x1, y1, x2, y2 : WORD);
{ L”scht Bildschirmausschnitt }

PROCEDURE FillTextBox (x1, y1, x2, y2 : WORD; chr : CHAR);
{ Fuellt Bildschirmauschnitt }

PROCEDURE Set43Lines;
{ Setzt 43 Zeilen Modus }

IMPLEMENTATION

  USES PSCDOS;

  PROCEDURE Beep;
  BEGIN
    Sound (277);
    Wait (2);
    NoSound;
  END;

  PROCEDURE C_Off;

  VAR
    r : REGISTERS;

  BEGIN
    r.ah := 1;
    r.ch := 32;
    r.cl := 7;
    Intr ($10,r);
  END;

  Procedure C_On;

  VAR
    r : REGISTERS;

  BEGIN
    r.ah := 1;
    IF screenmem = $B800 THEN
    BEGIN
      r.ch := 6;
      r.cl := 7;
    END
    ELSE
    BEGIN
      r.ch := 12;
      r.cl := 13;
    END;
    Intr($10,r);
  END;

  FUNCTION Key:INTEGER;

  VAR kk : INTEGER;

  BEGIN
    kk:=Ord(ReadKey);
    IF kk=0 THEN
      kk:=Ord(ReadKey)+$100;
    Key:=kk;
  END;

  PROCEDURE TextAttr (x,y,len,attr : BYTE);
  VAR
    u : INTEGER;

  BEGIN
    Dec (y);
    FOR u := x TO x+len-1 DO
      Mem[screenmem:(y*160)+(u*2)-1]:=attr;
  END;

  PROCEDURE SaveTextAttr;
  BEGIN
    savetxtattr := crt.textattr;
  END;

  PROCEDURE RestoreTextAttr;
  BEGIN
    TextBackGround (savetxtattr SHR 4);
    TextColor (savetxtattr AND $F);
  END;

  PROCEDURE WritePos (x,y : BYTE; text :STRING);
  BEGIN
    GotoXY (x,y);
    Write (text);
  END;

  PROCEDURE WriteCenter (y : BYTE; text : STRING);
  CONST
    maxx = 80;
  VAR
    x : BYTE;

  BEGIN
    x := (maxx-Length(text)+1) DIV 2;
    GotoXY (x,y);
    Write (text);
  END;

  PROCEDURE TextFrame (x1,y1,x2,y2 :BYTE;  Name : STRING);

  CONST
    lo                   = 'É';
    ro                   = '»';
    lu                   = 'È';
    ru                   = '¼';
    wa                   = 'Í';
    se                   = 'º';

  VAR
    i           : BYTE;
    laenge      : INTEGER;
    hoch, breit : BYTE;
    titel       : BOOLEAN;

  BEGIN
    breit := x2-x1;
    hoch  := y2-y1;
    titel := TRUE;
    laenge := Length (name);
    IF laenge > breit THEN
      titel := FALSE;
    IF titel THEN
    BEGIN
      GotoXY (x1,y1);
      Write (lo);
      FOR i := 2 TO breit DO
         Write (wa);
      Write (ro);
      GotoXY (x2-Round(breit/2)-Round(laenge/2),y1);
      Write (' '+name+' ');
    END
    ELSE
    BEGIN
      GotoXY (x1,y1);
      Write (lo);
      FOR i:= 2 TO Breit DO
        Write (wa);
      Write (ro);
    END;
    FOR i:=1 TO hoch-1 DO
    BEGIN
      GotoXY (x1,y1+i);
      Write (se);
      GotoXY (x2,y1+i);
      Write (se);
    END;
    GotoXY (x1,y2);
    Write (lu);
    FOR i:= 2 TO breit DO
      Write (wa);
    Write (ru);
  END;

  FUNCTION UpString (str : STRING) : STRING;

  VAR
    u : BYTE;

  BEGIN
    FOR u := 1 TO Length (str) DO
      str[u]:=UpCase(str[u]);

    UpString := str;
  END;

  FUNCTION LowString (str : STRING) : STRING;

  VAR
    u,
    s : BYTE;

  BEGIN
    FOR u := 1 TO Length (str) DO
    BEGIN
      s := Ord(str[u]);
      IF (s > 64) AND (s < 91) THEN
        str[u]:=Chr(s+$20);
    END;
    LowString := str;
  END;

  PROCEDURE Cursor ( x, y :WORD; xh, yh : BYTE);
  VAR
    u, s : WORD;

  BEGIN
    SetFillStyle (SOLIDFILL, $F);
    Bar (x,y,x+xh,y+yh);
  END;

  FUNCTION GetAttr (x,y : WORD): BYTE;
  BEGIN
    GetAttr := Mem[screenmem:((y-1)*160)+((x-1)*2)-1];
  END;

  FUNCTION GetIn (x,y,len,mode : BYTE; txt : STRING): STRING;

  VAR
    pos,l,u,i,j,z,
    crsr,f,attr       : BYTE;
    k                 : WORD;
    kk, lin           : CHAR;
    ins, gr           : BOOLEAN;
    value             : SET OF CHAR;
    text, text2,
    mask              : STRING[80];

  BEGIN
    text := txt;
    lin := ' ';
    l := Length (text);

    IF mode = 5 THEN
    BEGIN
      mask := text;
      text2:= '';
      text := '';
      FOR u := 1 TO Length(mask) DO
        text := text + ' ';
      text := '';
      l := Length (text);

    END;

    FOR u := l+1 TO len DO
      text[u] := lin;

    IF (GetGraphMode > 0) AND (directvideo = TRUE) THEN
    BEGIN
      gr := TRUE;
      f := TextWidth ('A');
    END
    ELSE
      gr := FALSE;

    IF NOT gr AND directvideo THEN
      C_Off;

    pos := 0;
    ins := TRUE;
    crsr := 72;

    CASE mode OF
      0 : value := ['0'..'9','.',','];
      1 : value := [' '..'z'];
      3 : value := [' '..'z'];
      4 : value := ['0'..'9'];
      5 : value := [' '..'z'];
    END;

    REPEAT
      text[0]:=Chr(len);

      IF NOT gr THEN
        CASE mode OF
          3 : BEGIN
                FillTextBox (x,y,x+l,y,'*');
                FillTextBox (x+l,y,x+len,y,' ');
              END;
          1 : WritePos (x, y, text);
          4 : WritePos (x, y, text);
          2 : WritePos (x, y, text);
          5 : BEGIN
                j := 1;
                i := Length(mask);
                text2 := mask;
                FOR u := 1 TO i DO
                  IF mask[u] = '#' THEN
                  BEGIN
                    text2[u] := text[j];
                    Inc(j);
                  END;
                WritePos (x, y, text2);
              END;
      END
      ELSE
      BEGIN
        ClearBox (x * f, y * f, (x + len) * f, y * f + f);
        OutTextXY (x * f, y * f, text);
        Cursor ((x+pos)*f,y*f,TextHeight('Û'), TextWidth('Û'));
      END;

      IF (pos<len) AND NOT gr THEN
        IF mode = 5 THEN
        BEGIN
          j := 0;
          i := Length(mask);
          z := i;
          FOR u := 1 TO i DO
            IF mask[u] = '#' THEN
            BEGIN
              IF j = pos THEN
                z := u-1;
              Inc(j);
            END;
          TextAttr (x+z,y,1,(crt.textattr XOR $FF)+$80);
        END
        ELSE
          TextAttr (x+pos,y,1,(crt.textattr XOR $FF)+$80);

      IF NOT directvideo AND NOT gr THEN
        REPEAT
          WritePos (x+pos,y,'Û');
          IF NOT Keypressed THEN Delay (100);
          IF mode = 3 THEN
            WritePos (x+pos,y,'*')
          ELSE
            WritePos (x+pos,y,text[pos+1]);
          IF NOT Keypressed THEN Delay (100);
        UNTIL Keypressed;

      k := Key;

      IF k < $100 THEN
        kk := Chr(k)
      ELSE
        kk := Chr(0);

      getin_lastkey := k;

      IF (kk IN value) AND (l < len)  THEN
      BEGIN
        Inc(pos);
        IF ins = TRUE THEN
        BEGIN
          Insert(lin,text,pos);
          Inc(l);
        END;
        IF pos>l THEN Inc(l);
          text[pos]:=Chr(k);
      END;
      IF (k = 8) AND (pos > 0) THEN
      BEGIN
        FOR u := pos TO len DO
          text[u]:=text[u+1];
        text[len]:=lin;
        Dec(pos);
        Dec(l);
      END;
      IF (k = 339) AND (l > pos) THEN
      BEGIN
        FOR u := pos+1 TO len DO
          text[u]:=text[u+1];
        text[len]:=lin;
        Dec(l);
      END;
      IF k = 9 THEN
      BEGIN
        pos := 0;
        text := '';
        l := 0;
        FOR u := l+1 TO len DO
          text[u] := lin;
      END;
      IF (k = 331) AND (pos > 0) THEN Dec(pos);
      IF (k = 333) AND (pos < l) THEN Inc(pos);
      IF k = 338 THEN ins := NOT ins;

    UNTIL k = 13;

    IF mode = 5 THEN
      text := text2;
    IF NOT gr THEN
      IF mode <> 3 THEN WritePos (x, y, text);

    text[0]:=Chr(l);
    IF mode = 5 THEN
      text := text2;
    GETIN := text;
  END;

  FUNCTION Stick (number : BYTE) : BYTE;

  VAR
   reg        : REGISTERS;
   dirx, diry,
   direc      : BYTE;

  BEGIN

   direc := $F;

   reg.ah := $84;
   reg.dx := 1;
   Intr($15, reg);

   IF number = 0 THEN
   BEGIN
     dirx := reg.ax;
     diry := reg.bx;
   END
   ELSE
   BEGIN
     dirx := reg.cx;
     diry := reg.dx
   END;

   IF dirx < 60 THEN
    direc := direc - 4;
   IF dirx > 170 THEN
    direc := direc - 8;
   IF diry < 60 THEN
    direc := direc - 1;
   IF diry > 170 THEN
    direc := direc - 2;

   stick := direc;

  END;

  FUNCTION Strig (number : BYTE) : BYTE;

  VAR
   reg : REGISTERS;
   st  : BYTE;

  BEGIN

   reg.ah := $84;
   reg.dx := 0;
   Intr($15, reg);

   st := reg.al AND $F0;

   IF number = 0 THEN
    IF (st AND $30)<>$30 THEN
     st := 0;

   IF number = 1 THEN
     IF (st AND $C0)<>$C0 THEN
      st := 0;

   IF st = 0 THEN
     strig := st
   ELSE
     strig := 1;

  END;


   FUNCTION WildCard (str, cmp : STRING) : BOOLEAN;
   CONST
     jo1 = '*';
     jo2 = '?';

   VAR
     equ, flex : BOOLEAN;
     ch        : CHAR;
     strpos,
     cmppos,
     p         : INTEGER;

     strb      : STRING;

  FUNCTION InStr (source, search : STRING; p : BYTE) : BYTE;
  VAR
    str : STRING;
  BEGIN
    str := Copy (source, p, Length (source) - p + 1);
    InStr := Pos (search, str) + p - 1;
  END;

  BEGIN
    strpos := 1;
    cmppos := 1;
    equ    := TRUE;
    flex   := FALSE;

    WHILE equ AND (strpos <= Length(str)) DO
    BEGIN
      ch := str [strpos];

      IF ch = jo1 THEN
        flex := TRUE;

      IF ch = jo2 THEN
        Inc (cmppos);

      IF NOT (ch IN [jo1, jo2]) THEN
        IF flex THEN
        BEGIN
          strb := str[strpos];

          WHILE NOT (str [strpos + 1] IN [jo1, jo2]) AND (strpos < Length (str)) DO
          BEGIN
            Inc (strpos);
            strb := strb + str[strpos];
          END;

          p := InStr (cmp, strb, cmppos);

          IF p > 0 THEN
            cmppos := p + Length (strb)
          ELSE
            equ := FALSE;

          flex := FALSE;

        END

      ELSE

        IF cmp [cmppos] = ch THEN
          Inc (cmppos)
        ELSE
          equ := FALSE;

      Inc (strpos);

      IF (cmppos > Length (cmp)) AND (strpos <= Length (str)) THEN
        IF (strpos = Length (str)) AND (str[strpos] = jo1) THEN
          Inc (strpos)
        ELSE
          equ := FALSE;

    END;

    IF equ THEN
      IF (cmppos <= Length (cmp)) AND NOT FLEX THEN
        equ := FALSE;

    WildCard := equ;

  END;

  PROCEDURE Invert (x1, y1, x2, y2: WORD);

  VAR
    y           : WORD;

  BEGIN
    SetWriteMode (XORPUT);

    FOR y := y1 TO y2 DO
      Line (x1, y, x2, y);

    SetWriteMode (COPYPUT);
  END;

  PROCEDURE ClearText (x1, y1, x2, y2 : WORD);
  VAR
    u,z,y : WORD;
  BEGIN
    FOR z := y1 TO y2 DO
    BEGIN
      y := z-1;
      FOR u := x1-1TO x2-1 DO
        Mem[screenmem:(y*160)+(u*2)]:=0;
    END;
  END;

  PROCEDURE FillTextBox (x1, y1, x2, y2 : WORD; chr : CHAR);
  VAR
    u,z,y : WORD;
    off   : WORD;
  BEGIN
    FOR z := y1 TO y2 DO
    BEGIN
      y := z-1;
      FOR u := x1-1TO x2-1 DO
      BEGIN
        off := (y*160)+(u*2);
        Mem[screenmem:off]:=Ord (chr);
        Mem[screenmem:off+1]:=crt.TEXTATTR;
      END;
    END;
  END;

  PROCEDURE ClearBox (x1, y1, x2, y2 : WORD);
  BEGIN
    SetFillStyle (SOLIDFILL, BLACK);
    Bar (x1, y1, x2, y2);
  END;

  PROCEDURE Set43Lines;
  VAR
    r : REGISTERS;
  BEGIN
    r.ax := $1112;
    r.bl := 0;
    Intr ($10,r);
    Window (1,1,43,80);
  END;

  FUNCTION NextChar (s : STRING; p : BYTE): BYTE;
  BEGIN
    WHILE (s[p] = ' ') AND (p < $FF) DO
      Inc (p);
    NextChar := p;
  END;

END.
