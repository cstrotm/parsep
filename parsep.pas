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

  Programname    : Parse Plus
  Filename       : PASRSEP.PAS
  Author         : Carsten Strotmann
  License        : GPLv3

*)

PROGRAM ParseP;

USES CRT, DOS, PSCCRT, PSCDOS, WINDOW;

TYPE
  txtelem = RECORD
              num    : WORD;      {Zeilennummer}
              prev,               {Zeiger auf VorgÑnger}
              next   : POINTER;   {Zeiger auf Nachfolger}
              s      : STRING;    {Textstring}
            END;
  txtprt = ^txtelem;

CONST
  parsep_vers = 'Freeware Version 0.70 26/05/97';
  rc_no_error = 0;
  rc_no_memory = 1;
  rc_file_not_found = 9;

  st_end_cmdstruc = 'Kommando-Speicherstruktur wird gelîscht...';
  st_end_txtstruc = 'Text-Speicherstruktur wird gelîscht...';
  st_new_cmdstruc = 'Kommando-Speicherstruktur wird erstellt...';
  st_new_txtstruc = 'Text-Speicherstruktur wird erstellt...';
  st_save_file    = 'Datei wird zurÅckgeschrieben...';
  st_backup_file  = 'Backup der Datei wird erstellt...';
  st_end_pgm      = 'Programm wird beendet...';
  st_cmd_rep      = 'Kommando "Replace" ausgefÅhrt!';
  st_cmd_rew      = 'Kommando "Replace Word" ausgefÅhrt!';
  st_cmd_reo      = 'Kommando "Replace Once" ausgefÅhrt!';
  st_cmd_inp      = 'Kommando "Input" ausgefÅhrt!';
  st_cmd_del      = 'Kommando "Delete" ausgefÅhrt!';
  st_cmd_dew      = 'Kommando "Delete Word" ausgefÅhrt!';
  st_cmd_deo      = 'Kommando "Delete Once" ausgefÅhrt!';
  st_cmd_dll      = 'Kommando "Delete Line" ausgefÅhrt!';
  st_cmd_dlw      = 'Kommando "Delete Line Word" ausgefÅhrt!';
  st_cmd_dlo      = 'Kommando "Delete Line Once" ausgefÅhrt!';
  st_cmd_isl      = 'Kommando "Insert Line" ausgefÅhrt!';

  txtmod  : BYTE = 0; {0 = Full Window, 1= StdIO, 2= Quit}
  backup  : BOOLEAN = TRUE;
  cmd     : STRING[3] = '';
  param1  : STRING = '';
  param2  : STRING = '';
  numrep  : WORD = 0;
  numrew  : WORD = 0;
  numreo  : WORD = 0;
  numdel  : WORD = 0;
  numdew  : WORD = 0;
  numdeo  : WORD = 0;
  numdll  : WORD = 0;
  numdlw  : WORD = 0;
  numdlo  : WORD = 0;
  numina  : WORD = 0;
  numinp  : WORD = 0;
  numisl  : WORD = 0;
  numini  : WORD = 0;

  debug   : BOOLEAN = FALSE;

VAR
  memfree   : LONGINT;
  infile    : TEXT;
  s_infile  : STRING;
  numparam  : BYTE;
  txtstruc,
  cmdstruc,
  aktstruc  : txtprt;
  start_txt,
  start_cmd,
  akt_txt,
  akt_cmd   : POINTER;
  memwin,
  statwin,
  infwin,
  parsewin  : BYTE;


  PROCEDURE ExitPgm; FORWARD;

  FUNCTION GetInIP (x,y,len,mode : BYTE; txt : STRING): STRING;

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

    FOR u := l+1 TO len DO
      text[u] := lin;

    gr := FALSE;

    C_Off;

    pos := 0;
    ins := TRUE;
    crsr := 72;

    CASE mode OF
      0 : value := ['0'..'9','.',','];
      1 : value := [' '..'z'];
      3 : value := [' '..'z'];
      4 : value := ['0'..'9'];
    END;

    REPEAT
      text[0]:=Chr(len);

      CASE mode OF
        3 : BEGIN
              FillTextBox (x,y,x+l,y,'*');
              FillTextBox (x+l,y,x+len,y,' ');
            END;
        1 : WritePos (x, y, text);
        4 : WritePos (x, y, text);
        2 : WritePos (x, y, text);
      END;

      IF (pos<len) AND NOT gr THEN
          TextAttr (x+pos,y,1,(crt.textattr XOR $FF)+$80);

      REPEAT
        WritePos (x+pos,y,'€');
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

      IF (k = 331) AND (pos > 0) THEN Dec(pos);
      IF (k = 333) AND (pos < l) THEN Inc(pos);
      IF k = 338 THEN ins := NOT ins;

    UNTIL (k = 13) OR (k = 9) OR (k = 46);

    IF NOT gr THEN
      IF mode <> 3 THEN WritePos (x, y, text);

    text[0]:=Chr(l);
    GETINIP := text;
  END;

  FUNCTION WinGetInIP (num,x,y,len,mode : BYTE; str : STRING):STRING;
  BEGIN
    winact := winstart;
    WHILE (winact^.num <> num) AND (winact^.next <> NIL) DO
      winact := winact^.next;

    IF len > (winact^.xlen-x) THEN
      len := winact^.xlen-x;

    x := x+winact^.xpos;
    y := y+winact^.ypos;

    WinGetInIP := GetInIP (x,y,len,mode,str);
  END;

  PROCEDURE Status (s : STRING);
  BEGIN
    IF txtmod = 0 THEN
    BEGIN
      TextColor(WHITE);
      TextBackGround(BLUE);
      WinClear(statwin);
      WriteWinCenter(statwin,1,s);
      Str(numrep:3, s);
      WriteWinCenter(infwin,1,'REP: '+s);
      Str(numrew:3, s);
      WriteWinCenter(infwin,2,'REW: '+s);
      Str(numreo:3, s);
      WriteWinCenter(infwin,3,'REO: '+s);
      Str(numdel:3, s);
      WriteWinCenter(infwin,4,'DEL: '+s);
      Str(numdew:3, s);
      WriteWinCenter(infwin,5,'DEW: '+s);
      Str(numdeo:3, s);
      WriteWinCenter(infwin,6,'DEO: '+s);
      Str(numdll:3, s);
      WriteWinCenter(infwin,7,'DLL: '+s);
      Str(numdlw:3, s);
      WriteWinCenter(infwin,8,'DLW: '+s);
      Str(numdlo:3, s);
      WriteWinCenter(infwin,9,'DLO: '+s);
      Str(numini:3, s);
      WriteWinCenter(infwin,10,'INI: '+s);
      Str(numinp:3, s);
      WriteWinCenter(infwin,11,'INP: '+s);
      Str(numisl:3, s);
      WriteWinCenter(infwin,12,'ISL: '+s);
      RestoreTextAttr;
    END;
    IF txtmod = 1 THEN
     WriteLn (s);

    IF debug THEN Key;
  END;

  PROCEDURE CheckMem (mem: WORD);
  VAR
    s: STRING[6];
  BEGIN
    IF MemAvail <= mem THEN
    BEGIN
      {Error No_MEM}
      ExitPgm;
      Halt (rc_no_memory);
    END;
    IF txtmod = 0 THEN
    BEGIN
      Str(MemAvail DIV 1024, s);
      SaveTextAttr;
      TextColor(BLUE);
      TextBackGround(GREEN);
      WinClear(memwin);
      WriteWinCenter (memwin, 1, s + 'KB');
      RestoreTextAttr;
    END;
  END;


  PROCEDURE ExitPgm;
  BEGIN
    {TXT Struktur abbauen}
    Status (st_end_txtstruc);
    aktstruc := start_txt;
    WHILE aktstruc <> NIL DO
    BEGIN
      akt_txt := aktstruc^.next;
      Dispose(aktstruc);
      CheckMem(1);
      aktstruc := akt_txt;
    END;

    {CMD Struktur abbauen}
    Status (st_end_cmdstruc);
    aktstruc := start_cmd;
    WHILE aktstruc <> NIL DO
    BEGIN
      akt_cmd := aktstruc^.next;
      Dispose(aktstruc);
      CheckMem(1);
      aktstruc := akt_cmd;
    END;

    Status (st_end_pgm);

    IF txtmod = 0 THEN
    BEGIN
      CloseWin (infwin);
      CloseWin (statwin);
      CloseWin (memwin);
      CloseWin (parsewin);
    END;
  END;

  PROCEDURE WriteHelp;
  { Gibt Hilfetext aus}
  BEGIN
  END;

  PROCEDURE Init;
  {Initialisiert Datenstrukturen, wertet Parameter aus}
  VAR
    s,
    si,
    z : STRING;
    p,
    num,
    u : BYTE;
  BEGIN
    start_txt := NIL;
    start_cmd := NIL;

    numparam := ParamCount;
    IF numparam = 0 THEN
    BEGIN
      WriteHelp;
      ExitPgm;
      Halt(rc_no_error);
    END;

    s := '';
    FOR u := 1 TO numparam DO
      s := s + ParamStr(u) + ' ';

    IF Pos(' /T', s) > 0 THEN
    BEGIN
      txtmod := 1;
      Dec (numparam);
    END;

    IF Pos(' /Q', s) > 0 THEN
    BEGIN
      txtmod := 2;
      Dec (numparam);
    END;

    IF Pos(' /DEBUG', s) > 0 THEN
    BEGIN
      debug := TRUE;
      Dec (numparam);
    END;

    IF Pos(' /!B', s) > 0 THEN
    BEGIN
      backup := FALSE;
      Dec (numparam);
    END;

    IF txtmod = 0 THEN
    BEGIN
      SaveTextAttr;
      TextColor(WHITE);
      TextBackground(GREEN);
      parsewin := OpenWin(1,1,50,5,'About');
      WriteWinCenter (parsewin,1,'Parse+ (c) 1997 PhîniX Softcrew');
      WriteWinCenter (parsewin,2, parsep_vers);
      WriteWinCenter (parsewin,3,'Carsten Strotmann');
      TextColor(BLUE);
      TextBackground(GREEN);
      memwin := OpenWin(70,1,79,3,'Memory');
      CheckMem(500);
      TextColor(WHITE);
      TextBackGround(BLUE);
      statwin := OpenWin(1,22,79,24,'Status');
      infwin  := OpenWin(68,7,79,21,'Info');
      RestoreTextAttr;
    END;

    IF txtmod = 1 THEN
    BEGIN
      WriteLn;
      WriteLn (output,'Parse+ (c) 1997 PhîniX Softcrew');
      WriteLn (output, parsep_vers);
      WriteLn (output,'Carsten Strotmann');
      WriteLn;
    END;

    New (txtstruc);
    start_txt := txtstruc;
    txtstruc^.prev := NIL;
    txtstruc^.next := NIL;
    txtstruc^.num  := 0;
    txtstruc^.s    := '';

    IF numparam = 1 THEN
    BEGIN
      s_infile := ParamStr(1);
      IF (s_infile[2] = '?') OR (s_infile[1] = '?') THEN
      BEGIN
        WriteHelp;
        ExitPgm;
        Halt(rc_no_error);
      END;
      IF Exist(s_infile) THEN
      BEGIN
        Assign (infile, s_infile);
        ReSet (infile);
        num := 1;
        Status (st_new_cmdstruc);
        WHILE NOT EOF(infile) DO
        BEGIN
          CheckMem(SizeOf(cmdstruc));
          ReadLn (infile, si);


          FOR u := 1 TO Byte(si[0]) DO
            IF si[u] = ' ' THEN si[u] := '|';
          si := si + '|';

          IF num = 1 THEN
          BEGIN
            CheckMem(SizeOf(cmdstruc));
            New (cmdstruc);
            start_cmd := cmdstruc;
            akt_cmd   := cmdstruc;
            aktstruc  := cmdstruc;
            aktstruc^.prev := NIL;
            aktstruc^.next := NIL;
            aktstruc^.num  := num;
            aktstruc^.s    := si;
          END
          ELSE
          BEGIN
            CheckMem(SizeOf(cmdstruc));
            New (cmdstruc);
            aktstruc^.next := cmdstruc;
            aktstruc  := cmdstruc;
            aktstruc^.prev := akt_cmd;
            akt_cmd   := cmdstruc;
            aktstruc^.next := NIL;
            aktstruc^.num  := num;
            aktstruc^.s    := si;
          END;
          Inc (num);
        END;
        Close (infile);
      END
      ELSE
      BEGIN
        ExitPgm;
        Halt (rc_file_not_found);
      END;
    END
    ELSE
    BEGIN
      s := '';
      FOR u := 1 TO numparam DO
      BEGIN
        z := ParamStr(u);
        IF z[1] <> '/' THEN
          s := s + z + '|';
      END;

      p := Pos(':',s);
      cmd := Copy(s, 1, p-1);
      s := Copy(s, p+1, Length(s)-p);

      p := Pos('|',s);
      param1 := Copy(s, 1, p-1);
      s := Copy(s, p+1, Length(s)-p);

      p := Pos('|',s);
      param2 := Copy(s, 1, p-1);
      s := Copy(s, p+1, Length(s)-p);

      IF Length(s) = 0 THEN
        s_infile := param2
      ELSE
      BEGIN
        p := Pos('|',s);
        s_infile := Copy(s, 1, p-1);
        s := Copy(s, p+1, Length(s)-p);
      END;

      Status (st_new_cmdstruc);
      CheckMem(SizeOf(cmdstruc));
      New (cmdstruc);
      start_cmd := cmdstruc;
      akt_cmd   := cmdstruc;
      aktstruc  := cmdstruc;
      aktstruc^.prev := NIL;
      aktstruc^.next := NIL;
      aktstruc^.num  := 0;
      aktstruc^.s    := 'FIL:' + s_infile + '|';

      CheckMem(SizeOf(cmdstruc));
      New (cmdstruc);
      aktstruc := akt_cmd;
      aktstruc^.next := cmdstruc;
      cmdstruc^.prev := aktstruc;
      aktstruc       := cmdstruc;
      aktstruc^.next := NIL;
      aktstruc^.num  := 1;
      aktstruc^.s    := cmd + ':' + param1 + '|' + param2 + '|';

    END;
    s_infile := '';

  END;

  PROCEDURE Save;
  BEGIN
    IF Exist(s_infile) THEN
    BEGIN
      Status (st_save_file);
      Assign(infile, s_infile);
      ReWrite(infile);
      aktstruc := start_txt;
      WHILE aktstruc <> NIL DO
      BEGIN
        WriteLn (infile, aktstruc^.s);
        aktstruc := aktstruc^.next;
      END;
      Close(infile);
    END;
  END;

  PROCEDURE GetFile(s : STRING);
  VAR
    lin,
    u   : WORD;
    si,
    bs  : STRING;
    dir : DIRSTR;
    name: NAMESTR;
    ext : EXTSTR;
    outfile : TEXT;
  BEGIN
    aktstruc := start_cmd;
    IF aktstruc <> NIL THEN
      Save;

    u := Pos('|', s);
    s := Copy(s, 1, u - 1);

    s := FExpand(s);
    s_infile := s;
    lin := 0;
    IF Exist(s) THEN
    BEGIN
      {TXT Struktur abbauen}
      aktstruc := start_txt;
      WHILE aktstruc <> NIL DO
      BEGIN
        akt_txt := aktstruc^.next;
        Dispose(aktstruc);
        CheckMem(1);
        aktstruc := akt_txt;
      END;

      Status (st_new_txtstruc + '(' + s + ')');
      Assign(infile, s);
      ReSet(infile);
      WHILE NOT Eof(infile) DO
      BEGIN
        ReadLn(infile, si);
        IF lin = 0 THEN
        BEGIN
          CheckMem(SizeOf(txtstruc));
          New (txtstruc);
          start_txt := txtstruc;
          akt_txt   := txtstruc;
          txtstruc^.prev := NIL;
          txtstruc^.next := NIL;
          txtstruc^.num  := lin;
          txtstruc^.s    := si;
          aktstruc       := txtstruc;
        END
        ELSE
        BEGIN
          CheckMem(SizeOf(txtstruc));
          New (txtstruc);
          txtstruc^.next := aktstruc^.next;
          aktstruc^.next := txtstruc;
          txtstruc^.prev := akt_txt;
          txtstruc^.num  := lin;
          txtstruc^.s    := si;
          aktstruc       := txtstruc;
          akt_txt        := txtstruc;
        END;
        Inc(lin);
      END;
      Close (infile);

      IF backup THEN
      BEGIN
        lin := 0;
        FSplit(s, dir, name, ext);
        REPEAT
          Str(lin:3,bs);
          FOR u := 1 TO 3 DO
            IF bs[u] = ' ' THEN bs[u] := '0';
          s := dir + name + '.' + bs;
          Inc(lin);
        UNTIL NOT Exist(s) OR (lin = 999);
        IF lin = 999 THEN
          {Kann kein Backup anlegen}
        ELSE
        BEGIN
          Status (st_backup_file + '(' + s + ')');
          Assign(outfile, s);
          ReWrite(outfile);
          aktstruc := start_txt;
          WHILE aktstruc <> NIL DO
          BEGIN
            WriteLn (outfile, aktstruc^.s);
            aktstruc := aktstruc^.next;
          END;
          Close(outfile);
        END;
      END;
    END
    ELSE
    BEGIN
      ExitPgm;
      Halt(rc_file_not_found);
    {Fehler: Datei nicht gefunden!}
    END;
  END;

  FUNCTION Strip (s: STRING): STRING;
  BEGIN
    IF s[1] = '"' THEN
      s := Copy(s, 2, Length(s)-1);
    IF s[Length(s)] = '"' THEN
      s := Copy(s, 1, Length(s)-1);
    Strip := s;
  END;


  PROCEDURE Replace (s : STRING);

    FUNCTION InputIP(s : STRING): STRING;
    VAR
      ipstr   : STRING[15];
      ip      : ARRAY[0..3] OF STRING[3];
      win, x, i : BYTE;
    BEGIN
      FOR i := 0 TO 3 DO
        ip[i] := '';

      i := 0;
      x := 20;
      IF Length(s) > x - 2 THEN x := Length(s) + 2;
      win := OpenWin(2,7,20,10,s);
      REPEAT
        ip[i] := WinGetIn(win,1,1,3,0,ip[i]);
        IF (getin_lastkey = 9) OR (getin_lastkey = 46) THEN
        BEGIN
          Inc(i);
          IF i > 3 THEN i := 0;
        END;
        ipstr := ip[0] + '.' + ip[1] + '.' + ip[2] + '.' + ip[3];
        WriteWinPos(win,1,1,ipstr);
      UNTIL getin_lastkey = 13;
      CloseWin(win);
      InputIP := ipstr;
    END;

    FUNCTION GetInput(s : STRING): STRING;
    VAR
      win,
      x, y : BYTE;
      str,
      str2 : STRING;
    BEGIN
      SaveTextAttr;
      x := WhereX;
      y := WhereY;
      TextColor(WHITE);
      TextBackground(CYAN);
      IF cmd = 'INP' THEN
      BEGIN
        win := OpenWin(2,7,78,10,s);
        str := WinGetIn(win,1,1,70,1,'');
        CloseWin(win);
      END;
      IF cmd = 'IPI' THEN
        str := InputIP (s);
      IF cmd = 'IPP' THEN
      REPEAT
        win := OpenWin(2,7,78,10,s);
        str := WinGetIn(win,1,1,70,3,'');
        CloseWin (win);
        win := OpenWin(2,7,78,10,s + ' (öberprÅfung)' );
        str2 := WinGetIn(win,1,1,70,3,'');
        CloseWin (win);
        IF str <> str2 THEN Beep;
      UNTIL str = str2;
      GotoXY (x, y);
      GetInput := str;
      RestoreTextAttr;
    END;

    FUNCTION GetInputList(s : STRING): STRING;
    VAR
      win,
      p,x ,y   : BYTE;
      str   : STRING;
      sra   : ARRAY[0..20] OF STRING;
    BEGIN
      p  := Pos(';',s);
      str := Copy(s,1,p-1);
      s := Copy(s,p+1, Length(s));

      SaveTextAttr;
      x := WhereX;
      y := WhereY;
      TextColor(WHITE);
      TextBackground(CYAN);
      str := ListBox(2,5,0,6,str,s);
      GetInputList := str;
      GotoXY (x, y);
      RestoreTextAttr;
    END;

  VAR
    p : BYTE;
    part1,
    part2 : STRING;
    endp  : BOOLEAN;
  BEGIN
    endp := FALSE;
    p := Pos('|',s);
    param1 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    IF cmd = 'REW' THEN
      param1 := ' ' + param1 + ' ';

    p := Pos('|',s);
    param2 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    IF (cmd = 'INP') OR (cmd = 'IPP') OR (cmd = 'IPI') THEN
      param2 := GetInput(param2);

    IF cmd = 'INL' THEN
      param2 := GetInputList(param2);

    aktstruc := start_txt;

    WHILE aktstruc <> NIL DO
    BEGIN
      part1 := '';
      part2 := '';
      s := aktstruc^.s;
      aktstruc^.s := '';
      REPEAT
        p := Pos(param1, s);
        IF p > 0 THEN
        BEGIN
          part1 := Copy( s, 1, p-1);
          part2 := Copy( s, p + Length(param1), Length(s) - p + Length(param1));
          aktstruc^.s := aktstruc^.s + part1 + param2;
          s := part2;
          IF cmd = 'REP' THEN Inc(numrep);
          IF cmd = 'REW' THEN Inc(numrew);
          IF cmd = 'REO' THEN Inc(numreo);
          IF (cmd = 'INP') OR (cmd = 'INL') or (cmd = 'IPP') THEN Inc(numinp);

          IF cmd = 'REO' THEN
          BEGIN
            endp := TRUE;
            p := 0;
          END;
        END;
      UNTIL p = 0;
      aktstruc^.s := aktstruc^.s + s;

      IF aktstruc <> NIL THEN
        aktstruc := aktstruc^.next;
      IF endp THEN
        aktstruc := NIL;
    END;
  END;

  PROCEDURE Delete (s : STRING);

  VAR
    p : BYTE;
    part1,
    part2 : STRING;
    endp  : BOOLEAN;
  BEGIN
    endp := FALSE;
    p := Pos('|',s);
    param1 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    IF param1 = 'DEW' THEN
      param1 := ' ' + param1 + ' ';

    param2 := '';

    aktstruc := start_txt;

    WHILE aktstruc <> NIL DO
    BEGIN
      part1 := '';
      part2 := '';
      s := aktstruc^.s;
      aktstruc^.s := '';
      REPEAT
        p := Pos(param1, s);
        IF p > 0 THEN
        BEGIN
          part1 := Copy( s, 1, p-1);
          part2 := Copy( s, p + Length(param1), Length(s) - p + Length(param1));
          aktstruc^.s := aktstruc^.s + part1 + param2;
          s := part2;
          IF cmd = 'DEL' THEN Inc(numdel);
          IF cmd = 'DEW' THEN Inc(numdew);
          IF cmd = 'DEO' THEN Inc(numdeo);
          IF cmd = 'DEO' THEN
          BEGIN
            endp := TRUE;
            p := 0;
          END;
        END;
      UNTIL p = 0;
      aktstruc^.s := aktstruc^.s + s;

      IF aktstruc <> NIL THEN
        aktstruc := aktstruc^.next;
      IF endp THEN
        aktstruc := NIL;
    END;
  END;

  PROCEDURE DeleteLine (s : STRING);

  VAR
    p : BYTE;
    part1,
    part2 : STRING;
    endp  : BOOLEAN;
  BEGIN
    endp := FALSE;
    p := Pos('|',s);
    param1 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    IF param1 = 'DLW' THEN
      param1 := ' ' + param1 + ' ';

    param2 := '';

    aktstruc := start_txt;

    WHILE aktstruc <> NIL DO
    BEGIN
      part1 := '';
      part2 := '';
      s := aktstruc^.s;

      p := Pos(param1, s);
      IF p > 0 THEN
      BEGIN
        IF aktstruc^.prev = NIL THEN {erste Zeile}
        BEGIN
          start_txt := aktstruc^.next;
          Dispose (aktstruc);
          aktstruc := start_txt;
          aktstruc^.prev := NIL;
        END
        ELSE
        BEGIN
          txtstruc := aktstruc^.prev;
          txtstruc^.next := aktstruc^.next;
          txtstruc := aktstruc^.next;
          IF txtstruc <> NIL THEN
            txtstruc^.prev := aktstruc^.prev;

          Dispose (aktstruc);
          aktstruc := txtstruc;
        END;
        CheckMem (1);

        IF cmd = 'DLL' THEN Inc(numdll);
        IF cmd = 'DLW' THEN Inc(numdlw);
        IF cmd = 'DLO' THEN Inc(numdlo);
        IF cmd = 'DLO' THEN
        BEGIN
          endp := TRUE;
        END;
      END;

      IF (aktstruc <> NIL) AND (p = 0) THEN
        aktstruc := aktstruc^.next;
      IF endp THEN
        aktstruc := NIL;
    END;
  END;

  PROCEDURE InsertLine (s : STRING);

  VAR
    p : BYTE;
    part1,
    part2 : STRING;
    endp  : BOOLEAN;
  BEGIN
    endp := FALSE;
    p := Pos('|',s);
    param1 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    p := Pos('|',s);
    param2 := Strip(Copy(s, 1, p-1));
    s := Copy(s, p+1, Length(s)-p);

    aktstruc := start_txt;

    WHILE aktstruc <> NIL DO
    BEGIN
      part1 := '';
      part2 := '';
      s := aktstruc^.s;

      p := Pos(param1, s);
      IF p > 0 THEN
      BEGIN
        CheckMem(SizeOf(txtstruc));
        New (txtstruc);
        txtstruc^.next := aktstruc^.next;
        aktstruc^.next := txtstruc;
        txtstruc^.prev := aktstruc;
        txtstruc^.num  := 0;
        txtstruc^.s    := param2;
        aktstruc       := txtstruc;
        akt_txt        := txtstruc;
        IF cmd = 'ISL' THEN Inc(numisl);
      END;

      IF (aktstruc <> NIL) AND (p = 0) THEN
        aktstruc := aktstruc^.next;
    END;
  END;

  PROCEDURE Process;
  VAR
    p : BYTE;
    s : STRING;
    ptr : POINTER;

  BEGIN
    aktstruc := start_cmd;
    WHILE aktstruc <> NIL DO
    BEGIN
      ptr := aktstruc;
      s := aktstruc^.s;
      p := Pos(':',s);
      cmd := UpString(Copy(s, 1, p-1));
      s := Copy(s, p+1, Length(s)-p);
      IF cmd = 'FIL' THEN
          GetFile(s);
      IF cmd = 'REP' THEN
      BEGIN
        Replace(s);
        Status (st_cmd_rep);
      END;
      IF cmd = 'REW' THEN
      BEGIN
        Replace(s);
        Status (st_cmd_rew);
      END;
      IF cmd = 'REO' THEN
      BEGIN
        Replace(s);
        Status (st_cmd_reo);
      END;
      IF (cmd = 'INP') OR (cmd = 'INL') OR (cmd = 'IPP') OR (cmd = 'IPI') THEN
      BEGIN
        Replace(s);
        Status (st_cmd_inp);
      END;
      IF cmd = 'DEL' THEN
      BEGIN
        Delete(s);
        Status (st_cmd_del);
      END;
      IF cmd = 'DEW' THEN
      BEGIN
        Delete(s);
        Status (st_cmd_dew);
      END;
      IF cmd = 'DEO' THEN
      BEGIN
        Delete(s);
        Status (st_cmd_deo);
      END;
      IF cmd = 'DLL' THEN
      BEGIN
        DeleteLine(s);
        Status (st_cmd_dll);
      END;
      IF cmd = 'DLO' THEN
      BEGIN
        DeleteLine(s);
        Status (st_cmd_dll);
      END;
      IF cmd = 'ISL' THEN
      BEGIN
        InsertLine(s);
        Status (st_cmd_isl);
      END;

      aktstruc := ptr;
      aktstruc := aktstruc^.next;
    END;
  END;


BEGIN
  Init;
  Process;
  Save;
  ExitPgm;
END.
