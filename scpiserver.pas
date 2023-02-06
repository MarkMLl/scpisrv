(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ScpiServer;

(* Implement a simple execution framework for an SCPI server. This uses a       *)
(* background thread to receive messages from a TCP/IP socket or stdin (a poll  *)
(* facility is available if threads are not being used), splits concatenated    *)
(* commands, expands each to take into account any assumed node prefix and      *)
(* enqueues the result with a thread-safe lock.                                 *)
(*                                                                              *)
(* A different thread (e.g. the main GUI thread) may poll for any enqueued      *)
(* commands, which has the effect of distributing any waiting as determined by  *)
(* specified filters. Finally, a response may be returned to the originating    *)
(* client.                                                      MarkMLl.        *)

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Sockets, SyncObjs;

type
  TScpiServer= class;
  TBlocking= (Blocking, NonBlocking);
  ScpiProc= function(scpi: TScpiServer; const command: AnsiString): boolean;
  AvailableProc= procedure(scpi: TScpiServer; available: integer);
  LineStates= (LeftMargin, Reading, SeenIac, SeenIacReq);

  TScpiServer= class(TThread)
  strict private
    portNumber: integer;
    lineState: LineStates;
    fPrompt: boolean;
    fSocket: TSocket;
    fListening: boolean;
    fClient: TSocket;
    iacReq: AnsiChar;
    lineBuffer: AnsiString;
    helpPattern: AnsiString;
    helpPatternMixed: AnsiString;
    syntaxPattern: AnsiString;
    syntaxPatternMixed: AnsiString;
    longSymbols, shortSymbols: TStringList;
    defaultProc: ScpiProc;
    critSect: TCriticalSection;
    msgQueue: TStringList;
    fOnAvailable: AvailableProc;
    registered: TStringList;
    registeredMixed: TStringList;
    function prepareSocket(): boolean;

    (* This is a blocking call to read one 8-bit character at a time, silently
      reverting to TCP Listen state etc. as required. Note that this does not filter
      out e.g. Telnet IAC sequences.

      If non-blocking, this will return zero if no character is available.
    *)
    function getNextChar(block: TBlocking= Blocking): AnsiChar;

    (* This is a blocking call to read and process a single character. Return true
      when a complete line is enqueued.
    *)
    function readAndEnqueue(block: TBlocking= Blocking): boolean;

    procedure enqueue(msg: AnsiString);
    procedure clearQueue;

    (* Make sure that every node in the <Program Message Unit> appears in either the
      short or long symbol list. If it is a short symbol expand it to a long one,
      raise EInvalidSCPI with an appropriate message on error.
    *)
    function deAbbreviate(programMessageUnit: TStringList;
                                        exceptionOnError: boolean= false): TStringList;

    (* Make sure that every node in the <Program Message> appears in either the
      short or long symbol list. If it is a short symbol expand it to a long one,
      raise EInvalidSCPI with an appropriate message on error.
    *)
    function deAbbreviate(programMessage: TList; exceptionOnError: boolean= false): TList;

    procedure onAvailableShim;
  protected

    (* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
    *)
    procedure Execute; override;

    constructor Create(CreateSuspended: Boolean; port: integer= -1);
  public
    BlankIsHelp: boolean;
    HelpIsHelp: boolean;
    HelpQIsHelp: boolean;

    (* Initially false, this must be set true before OnAvailable is operative and
      is reset false to prevent repeated entry of the handler. OnAvailable will not
      fire again until this is set true.
    *)
    AvailableEventPrimed: boolean;

    constructor Create(port: integer= -1);
    destructor Destroy; override;

    (* Look at every portion of the command passed as the parameter. If it does not
      exist in the short and long symbol lists then insert it, if it does exist
      then change it from the abbreviated (short) to the long form.

      On error raise an EInvalidAbbreviation.
    *)
    function DeAbbreviate(const programMessage: AnsiString;
                                        exceptionOnError: boolean= false): AnsiString;

    (* The pattern is a conventional combination of short- and long-format names.
      There are two special cases here: a blank pattern with a non-nil proc is
      handled specially as the default, a non-blank pattern with a nil proc
      is associated internally with automatic help generation.

      Assume that the order of registration is preserved, and becomes the order
      of matching. Also assume that query and non-query variants of each command
      are provided where appropriate, rendering Keysight's /nquery/ and /qonly/
      qualifiers redundant.
    *)
    procedure Register(const pattern: AnsiString; proc: ScpiProc);

    (* Start up the background thread etc., generally after all registration has
      been done. Do not use this if input is from stdin and a prompt is required.
    *)
    function Run(prompt: boolean= false): boolean;

    (* Return the number of commands enqueued.
    *)
    function CommandsAvailable(): integer;

    (* Using the main thread, read input and enqueue a non-blank message returning
      true. This is a blocking call and returns true when a line is enqueued and
      false when any other character has been read (even if discarded). It is an
      alternative to running a background thread, and must be used if a prompt is
      required when reading stdin.
    *)
    function Poll(prompt: boolean= false; block: TBlocking= Blocking): boolean;

    (* Using the main thread, read input and enqueue a non-blank message returning
      true. This is a blocking call and returns true when a line is enqueued and
      false when any other character has been read (even if discarded). It is an
      alternative to running a background thread, and must be used if a prompt is
      required when reading stdin.
    *)
    function Poll(block: TBlocking= Blocking): boolean;

    (* Return true if a message has been dequeued and dispatched to a registered
      handler.
    *)
    function Dispatch(): boolean;

    (* Return a response to a client, with optional CRLF termination.
    *)
    procedure Respond(const msg: ansistring; eol: boolean= false);

    (* The enqueued received data exceeeds the byte threshold. This is called in the
      context of the background thread, and resets AvailableEventPrimed false.
    *)
    property OnAvailable: AvailableProc write fOnAvailable;

    property Prompt: boolean read fPrompt;
  end;


implementation

uses
  StrUtils, UnixType, BaseUnix {} , Errors {} , ScpiParser ;

type
  EScpiForcedTermination= CLASS(Exception);

const
  INVALID_SOCKET= -1;
  iac= #255;
  iacDo= #253;
  iacDont= #254;
  iacWill= #251;
  iacWont= #252;
  iacSb=  #250;
  iacSe= #240;


function threadManagerInstalled(): boolean;

var
  tm: TThreadManager;

begin
  result := false;
  if GetThreadManager(tm{%H-}) then
    result := Assigned(tm.InitManager)
end { threadManagerInstalled } ;


constructor TScpiServer.Create(CreateSuspended: Boolean; port: integer= -1);

begin
  if threadManagerInstalled() then
    inherited Create(CreateSuspended);
  portNumber := port;
  fSocket := INVALID_SOCKET;
  fClient := INVALID_SOCKET;
  fListening := true;
  fPrompt := false;
  helpPattern := '';
  helpPatternMixed := '';
  syntaxPattern := '';
  syntaxPatternMixed := '';
  defaultProc := nil;
  longSymbols := TStringList.Create;
  longSymbols.CaseSensitive := false;
  longSymbols.Sorted := true;
  longSymbols.Duplicates := DupError;
  shortSymbols := TStringList.Create;
  shortSymbols.CaseSensitive := false;
  shortSymbols.Sorted := true;
  shortSymbols.Duplicates := DupError;
  critsect := TCriticalSection.Create;
  msgQueue := TStringList.Create;
  fOnAvailable := nil;
  fOnAvailable := nil;
  AvailableEventPrimed := false;
  registered := TStringList.Create;
  registeredMixed := TStringList.Create;
  BlankIsHelp := false;
  HelpIsHelp := false;
  HelpQIsHelp := false;
  clearQueue
end { TScpiServer.Create } ;


constructor TScpiServer.Create(port: integer= -1);

begin
  Create(true, port)
end { TScpiServer.Create } ;


destructor TScpiServer.Destroy;

begin
  if threadManagerInstalled() then begin
    Terminate;

(* I was initially using                                                        *)
(*                                                                              *)
(*    PThread_kill(Handle, SIGHUP);      Anything stronger than HUP here        *)
(*    WaitFor                            would affect the entire process.       *)
(*                                                                              *)
(* here, but the kill caused major problems in a program which was also hooking *)
(* HUP and the WaitFor was just... well, it didn't work once I moved away from  *)
(* using HUP for non-obvious reasons.                                           *)

    fpShutdown(fClient, 2);
    CloseSocket(fClient);
    fpShutdown(fSocket, 2);
    CloseSocket(fSocket);
    repeat
      Sleep(1)
    until Finished
  end;
  critSect.Free;
  msgQueue.Free;
  registered.Free;
  registeredMixed.Free;
  longSymbols.Free;
  shortSymbols.Free;
  if fClient <> INVALID_SOCKET then begin
    fpShutdown(fClient, 2);
    CloseSocket(fClient)
  end;
  if fSocket <> INVALID_SOCKET then begin
    fpShutdown(fSocket, 2);
    CloseSocket(fSocket)
  end;
  if threadManagerInstalled() then
    inherited Destroy
end { TScpiServer.Destroy } ;


//############################################################################
//      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(* Make sure that every node in the <Program Message Unit> appears in either the
short or long symbol list. If it is a short symbol expand it to a long one,
raise EInvalidSCPI with an appropriate message on error.
*)
function TScpiServer.deAbbreviate(programMessageUnit: TStringList;
                                        exceptionOnError: boolean= false): TStringList;

var
  i, j: integer;
  node, suffix, hash, longName, shortName: AnsiString;

begin

(* Assume that the final node will either be a value (without leading space),   *)
(* ?MAX etc. (with no embedded space), ? by itself, or * as a wildcard. In all  *)
(* cases leave it untouched.                                                    *)

  for i := 0 to programMessageUnit.Count - 2 do begin

(* The penultimate node might have a numeric suffix, or # as a placeholder.     *)
(* Preserve this, any decision as to how to interpret # will be made later.     *)

    node := programMessageUnit[i];
    suffix := '';
    hash := '';
    if i = programMessageUnit.Count - 2 then begin
      j := PosSet(['#', '0'..'9'], node);
      if j > 0 then begin
        suffix := Copy(node, j, MaxInt);
        SetLength(node, j - 1);
        hash := '#';
      end;
      node += hash
    end;

(* For the remainder, attempt a case-insensitive match in the long symbol list, *)
(* followed by a case-insensitive match in the short symbol list. If the node   *)
(* is not found then noting the SCPI capitalisation convention enter it into    *)
(* the long and short symbol list, in either case ensure that the PMU contains  *)
(* the long symbol.                                                             *)

    if longSymbols.IndexOf(node) < 0 then begin
      j := shortSymbols.IndexOfName(node);
      if j >= 0 then
        node := shortSymbols.ValueFromIndex[j]
      else begin
        longName := UpperCase(node);
        shortName := node;
        while (shortName <> '') and not (shortName[Length(shortName)] in ['A'..'Z']) do
          SetLength(shortName, Length(shortName) - 1);
        if (shortName = '') and exceptionOnError then
          raise EInvalidSCPI.Create('Malformed short name "' + node + '"');
        if suffix = '' then
          hash := ''
        else
          hash := '#';
        try
          longSymbols.Add(longname);
          shortSymbols.AddPair(shortName + hash, longName)
        except
          raise EInvalidSCPI.Create('Unable to add "' + shortname + hash + '" as abbreviation of "' + longName)
        end;
        node := longName
      end
    end else
      node := UpperCase(node);

(* Replace any numeric suffix or the # placeholder. The caller will be able to  *)
(* de-tail this to a flat PMU, with all nodes capitalised and in their long     *)
(* form.                                                                        *)

    if suffix = '' then
      programMessageUnit[i] := node
    else begin
      SetLength(node, Length(node) - 1);        (* Assume trailing #            *)
      programMessageUnit[i] := node + suffix
    end
  end
end { TScpiServer.deAbbreviate } ;


(* Make sure that every node in the <Program Message> appears in either the
short or long symbol list. If it is a short symbol expand it to a long one,
raise EInvalidSCPI with an appropriate message on error.
*)
function TScpiServer.deAbbreviate(programMessage: TList; exceptionOnError: boolean= false): TList;

var
  i: integer;

begin
  result := programMessage;
  for i := 0 to programMessage.Count - 1 do
    deAbbreviate(TStringList(programMessage[i]), exceptionOnError)
end { TScpiServer.deAbbreviate } ;


(* Look at every portion of the command passed as the parameter. If it does not
exist in the short and long symbol lists then insert it, if it does exist
then change it from the abbreviated (short) to the long form.

On error raise an EInvalidAbbreviation.
*)
function TScpiServer.DeAbbreviate(const programMessage: AnsiString;
                                        exceptionOnError: boolean= false): AnsiString;

var
  pm: TList;

begin

(* Parse the Program Message into a sequence of one or more root-relative       *)
(* Program Message Units, and process each.                                     *)

  pm := ReRoot(DeTail(DeConstruct(programMessage)));
  try
    deAbbreviate(pm, exceptionOnError);
    ReTail(pm)
  finally

(* Return the content of and free the de-abbreviated Program Message.           *)

    result := ReConstruct(pm)
  end
end { TScpiServer.DeAbbreviate } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(* This is a blocking call to read one 8-bit character at a time, silently
  reverting to TCP Listen state etc. as required. Note that this does not filter
  out e.g. Telnet IAC sequences.

  If non-blocking, this will return zero if no character is available.
*)
function TScpiServer.getNextChar(block: TBlocking= Blocking): AnsiChar;

label
  listenAgain;

const
  promptStr= '> ';

var
  sockAddr: TInetSockAddr;
  sockLen: longint;
  readSet: TFDSet;
  timeout: TTimeVal;
  flags: cint;

begin
  result := #$00;
  if lineState = LeftMargin then begin
    if fPrompt and (portNumber < 0) then begin
      Flush(output);
      fpWrite(1, @promptStr[1], 2);
      Flush(output)
    end;
    lineState := Reading
  end;
  if portNumber < 0 then begin
    repeat
      if Terminated then
        Raise EScpiForcedTermination.Create('Forced thread termination while receiving');
      if block = NonBlocking then begin
        fpFD_ZERO(readSet);
        fpFD_SET(0, readSet);
        timeout.tv_sec := 0;
        timeout.tv_usec := 10000;
        flags := fpSelect(1, @readSet, nil, nil, @timeout);
        if flags < 0 then               (* Error, check for termination         *)
          continue
        else
          if flags = 0 then             (* No character, return zero            *)
            exit
      end
    until fpRead(0, result{%H-}, 1) = 1; (* Note: blocks but see "get out clause" above *)
    if result = #$0a then
      result := #$0d                    (* For compatibility with Telnet connection *)
  end else begin
listenAgain:
    while fListening do begin
      while fpListen(fSocket, 1) <> 0 do
        if Terminated then
          Raise EScpiForcedTermination.Create('Forced thread termination while listening');
      if block = Blocking then begin
        timeout.tv_sec := 0;
        timeout.tv_usec := 0
      end else begin
        timeout.tv_sec := 0;
        timeout.tv_usec := 10000
      end;
      if fpSetSockOpt(fSocket, SOL_SOCKET, SO_RCVTIMEO, @timeout, SizeOf(timeout)) < 0 then begin
{$if declared(StrError) }
        WriteLn(stderr, 'SetSockopt error ' + IntToStr(SocketError) + ': ' + StrError(SocketError))
{$endif declared        }
      end;
      sockLen := SizeOf(sockAddr);      (* Note: might or might not block       *)
      fClient := fpAccept(fSocket, @sockAddr, @sockLen);
      if (block = NonBlocking) and (fClient < 0) then
        exit;                           (* No incoming connection, return zero  *)
      if Terminated then
        Raise EScpiForcedTermination.Create('Forced thread termination while accepting');
      if fClient >= 0 then
        fListening := false
    end;
    if block = Blocking then begin
      flags := 0;
      sockLen := 1                      (* Terminate if < 1 character received  *)
    end else begin
      flags := MSG_DONTWAIT;
      sockLen := 0                      (* Terminate if result is -ve           *)
    end;
    flags := fpRecv(fClient, @result, 1, flags); (* Note: might or might not block *)
    if flags < sockLen then begin       (* Result visible for debugging         *)
      if (block = NonBlocking) and (SocketError = ESysEAGAIN) then
        exit(#$00);
      if Terminated then
        Raise EScpiForcedTermination.Create('Forced thread termination while receiving');
      CloseSocket(fClient);
      fClient := INVALID_SOCKET;
      fListening := true;
      lineState := LeftMargin;
      clearQueue;
      goto listenAgain
    end
  end
end { TScpiServer.getNextChar } ;


procedure TScpiServer.onAvailableShim;

begin
  fOnAvailable(self, msgQueue.Count)
end { TScpiServer.onAvailableShim } ;


procedure TScpiServer.enqueue(msg: AnsiString);

var
  pm: TList;
  i: integer;
  pmu: AnsiString;

begin

(* Handle the "user friendly" stuff first to avoid having to make special cases *)
(* for non-standard messages. Why does that term always make me think of a cat  *)
(* rubbing fur onto my leg?                                                     *)

  if BlankIsHelp and (Trim(msg) = '') then
    msg := helpPattern;
  if HelpIsHelp and (Trim(msg) = 'help') then
    msg := helpPattern;
  if HelpQIsHelp and (Trim(msg) = 'help?') then
    msg := helpPattern;
  if Trim(msg) = '' then
    exit;

(* Parse the Program Message into a sequence of de-abbreviated root-relative    *)
(* Program Message Units, and enqueue each.                                     *)

  pm := ReTail(deAbbreviate(ReRoot(DeTail(DeConstruct(msg)))));
  try
    for i := 0 to pm.Count - 1 do begin
      pmu := TStringList(pm[i])[0];
      critSect.Enter;
      try
        msgQueue.Append(pmu);
        if Assigned(fOnAvailable) and AvailableEventPrimed then begin
          AvailableEventPrimed := false;
          if GetCurrentThreadId() = MainThreadId then
            fOnAvailable(self, msgQueue.Count)
          else
            Synchronize(@onAvailableShim)       (* msgQueue.Count               *)
        end
      finally
        critSect.Leave
      end
    end
  finally
    pmu := ReConstruct(pm)              (* Re-tailed etc. PM available for debugging *)
  end
end { TScpiServer.enqueue } ;


procedure TScpiServer.clearQueue;

begin
  lineState := LeftMargin;
  lineBuffer := '';
  critSect.Enter;
  try
    msgQueue.Clear
  finally
    critSect.Leave
  end
end { TScpiServer.clearQueue } ;


(* This is a blocking call to read and process a single character. Return true
  when a complete line is enqueued.
*)
function TScpiServer.readAndEnqueue(block: TBlocking= Blocking): boolean;

var
  c: AnsiChar;


  procedure sendTelnetResponse(option: AnsiChar);

  (* All fairly obvious stuff, but summarised nicely at                         *)
  (* https://stackoverflow.com/questions/10413963/telnet-iac-command-answering  *)

  var
    response: AnsiString;

  begin
    response := iac;
    case option of

(* The client will probably respond to our IAC DO SUPPRESS_GO_AHEAD with an IAC *)
(* WILL SUPPRESS_GO_AHEAD which we ignore. It might itself send IAC DO          *)
(* SUPPRESS_GO_AHEAD, to which we should also respond with an IAC WILL          *)
(* SUPPRESS_GO_AHEAD. Other combinations may be ignored.                        *)

      #3: case iacReq of
            iacDo:   response += iacWill;
            iacDont: response += iacWont
          otherwise
            exit
          end
    otherwise
      case iacReq of
        iacDo,
        iacDont: response += iacDont;
        iacWill,
        iacWont: response += iacWont
      otherwise
// TODO : Consider response to erroneously-sent iacSb etc.
        exit
      end
    end;
    response += option;
    Respond(response);

(* If we've just responded to a Telnet client telling us to suppress go-ahead,  *)
(* then also tell it to suppress go-ahead.                                      *)

    if option = #3 then
      Respond(iac + iacDo + #3)         (* IAC DO SUPPRESS_GO_AHEAD             *)
  end { sendTelnetResponse } ;


begin
  result := false;
  c := getNextChar(block);             (* Blocking, might LeftMargin -> Reading *)
  case lineState of
    LeftMargin,
    Reading:     begin
                   case c of            (* In approximate numeric order:        *)
                     #$00..             (* Low control characters, ignore       *)
                     #$07: ;
                     #$08,              (* Backspace and delete                 *)
                     #$7f: if lineBuffer <> '' then
                             SetLength(lineBuffer, Length(lineBuffer) - 1);
                     #$09,              (* Mid control characters, ignore       *)
                     #$0a,
                     #$0b,
                     #$0c: ;

(* In line mode, a Telnet message will be terminated by 0x0d 0x0a. In character *)
(* mode, it will be terminated by 0x0d 0x00. Hence it is always the 0x0d (CR)   *)
(* which is significant, and both 0x00 (NUL) and 0x0a (NL) should be ignored.   *)

                     #$0d: begin
                             enqueue(lineBuffer);
                             lineBuffer := '';
                             lineState := LeftMargin;
                             result := true
                           end;
                     #$0e..             (* High control characters, ignore      *)
                     #$1f: ;
                     #$80..             (* Extended ASCII, ignore               *)
                     #$fe: ;
                     #$ff: lineState := SeenIac; (* Telnet IAC, handle specially *)
                   otherwise            (* Printable characters, accumulate     *)
                     if (lineBuffer = '') or (c <> ' ') then
                       lineBuffer += c
                     else               (* Special case: suppress repeated blanks *)
                       if lineBuffer[Length(lineBuffer)] <> ' ' then
                         lineBuffer += c
                   end
                 end;
    SeenIac:     begin
                   iacReq := c;
                   lineState := SeenIacReq
                 end;
    SeenIacReq:  begin
                   sendTelnetResponse(c);
                   lineState := Reading
                 end
  otherwise
  end
end { readAndEnqueue } ;


(* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
*)
procedure TScpiServer.Execute;

begin
  repeat
    try
      readAndEnqueue()
    except
      on E: EScpiForcedTermination do
        break
    else
      raise
    end
  until Terminated
end { TScpiServer.Execute } ;


(* The pattern is a conventional combination of short- and long-format names.
  There are two special cases here: a blank pattern with a non-nil proc is
  handled specially as the default, a non-blank pattern with a nil proc
  is associated internally with automatic help generation.

  Assume that the order of registration is preserved, and becomes the order
  of matching. Also assume that query and non-query variants of each command
  are provided where appropriate, rendering Keysight's /nquery/ and /qonly/
  qualifiers redundant.
*)
procedure TScpiServer.Register(const pattern: AnsiString; proc: ScpiProc);

var
  actual: AnsiString;
  a, p: integer;

begin
  if (pattern = '') and Assigned(proc) then
    defaultProc := proc
  else
    if (pattern <> '') and not Assigned(proc) then begin

(* The commands to get overall help (a list of headers) and the syntax of each  *)
(* are handled specially.                                                       *)

      syntaxPatternMixed := StringReplace(pattern, ':HEADers?', ':SYNTax?', [rfReplaceAll]);
      syntaxPattern := deAbbreviate(syntaxPatternMixed, true);
      helpPatternMixed := pattern;
      helpPattern := deAbbreviate(helpPatternMixed, true)
    end else begin

(* Everything else is added to a list, the order being significant.             *)

      actual := deAbbreviate(pattern, true); (* Visible for debugging           *)
      a := registered.AddObject(actual, TObject(proc));
      p := registeredMixed.Add(pattern);
      Assert(p = a, 'Inconsistent registered list entries')
    end
end { TScpiServer.Register } ;


function TScpiServer.prepareSocket(): boolean;

var
  sockAddr: TInetSockAddr;
  sz: integer;

begin
  result := true;
  if portNumber >= 0 then begin
    fSocket := fpSocket(PF_INET, SOCK_STREAM, 0);
    if fSocket < 0 then begin
      fSocket := INVALID_SOCKET;
      exit(false)
    end;

(* If the program terminates itself as the result of e.g. a received *HALT, we  *)
(* need to make sure that the TCP stack cleans things up promptly. Treat a      *)
(* failure here as an irritant which might result in a problem if the program   *)
(* is restarted within (in the case of Linux 6.0) a minute, but not as an error *)
(* which merits possibly-confusing output.                                      *)

    sz := 1;
    if fpSetSockOpt(fSocket, SOL_SOCKET, SO_REUSEADDR, @sz, SizeOf(sz)) < 0 then begin
      sz := SocketError;
{$if declared(StrError) }
      WriteLn(stderr, 'SetSockopt error ' + IntToStr(SocketError) + ': ' + StrError(SocketError))
{$endif declared        }
    end;
    FillChar(sockAddr{%H-}, SizeOf(sockAddr), 0);
    sockAddr.sin_family:= AF_INET;
    sockAddr.sin_port:= hToNS(portNumber);
    sockAddr.sin_addr.s_addr:= 0;
    if fpBind(fSocket, @sockAddr, SizeOf(sockAddr)) < 0 then
      exit(false)
  end
end { TScpiServer.prepareSocket } ;


(* Start up the background thread etc., generally after all registration has
  been done. Do not use this if input is from stdin and a prompt is required.
*)
function TScpiServer.Run(prompt: boolean= false): boolean;

begin
(*  WriteLn(StdErr, '======= Symbols ======');
  WriteLn(StdErr, shortSymbols.Text);
  WriteLn(StdErr, '===== Registered =====');
  WriteLn(StdErr, registered.Text);
  WriteLn(StdErr, '======================'); *)
  result := true;
  fPrompt := prompt;
  if fprompt and (portNumber < 0) then
    exit(false);

(* A requested prompt is actually ignored in all other cases. The check above   *)
(* was to accommodate the situation where the programmer actually wants one in  *)
(* which case he shouldn't be using Run().                                      *)

  fPrompt := false;
  if not prepareSocket() then
    exit(false);
  if threadManagerInstalled() then
    Start
end { TScpiServer.Run } ;


(* Return the number of commands enqueued.
*)
function TScpiServer.CommandsAvailable(): integer;

begin
  result := msgQueue.Count
end { TScpiServer.CommandsAvailable } ;


(* Using the main thread, read input and enqueue a non-blank message returning
  true. This is a blocking call and returns true when a line is enqueued and
  false when any other character has been read (even if discarded). It is an
  alternative to running a background thread, and must be used if a prompt is
  required when reading stdin.
*)
function TScpiServer.Poll(prompt: boolean= false; block: TBlocking= Blocking): boolean;

// TODO : Explore the feasibility of non-blocking Poll() behaviour and/or a timeout.
// The issue here is that in (a program supporting) an instrument that is e.g.
// constantly pushing data over a serial interface, since Poll() is blocking
// nothing else will happen if there is no SCPI activity. This is compounded
// by the fact that getNextByte() is handling transitions to and from the
// listening state, so not using a thread in this case is- quite literally- a
// non-starter.
//
// In more detail, and loosely: listen() defines a queue length, accept() reads
// a connection from that queue. It should apparently be possible to set fSocket
// non-blocking which affects accept(), after which the newly-returned fClient
// might or might not be blocking (we probably want it non-blocking for
// compatibility with the PIN reader in the Telnet server).
//
// https://www.scottklement.com/rpg/socktimeout/timingOut.html claims that
// select() works with accept(): presumably this is in effect a read operation.

begin
  fPrompt := prompt;
  if fSocket = INVALID_SOCKET then
    if not prepareSocket() then
      exit(false);
  result := readAndEnqueue(block)
end { TScpiServer.Poll } ;


(* Using the main thread, read input and enqueue a non-blank message returning
  true. This is a blocking call and returns true when a line is enqueued and
  false when any other character has been read (even if discarded). It is an
  alternative to running a background thread, and must be used if a prompt is
  required when reading stdin.
*)
function TScpiServer.Poll(block: TBlocking= Blocking): boolean;

begin
  result := Poll(false, block)
end { TScpiServer.Poll } ;


(* Return true if a message has been dequeued and dispatched to a registered handler.
*)
function TScpiServer.Dispatch(): boolean;

var
  msg: AnsiString= '';
  i: integer;
  pm: TList;


  (* The message, unless unusually short, will have an appended value (possibly ?
    indicating a query) separated by a space, or * as a wildcard. Immediately
    preceding this there might be a number selecting a channel etc., matching #
    as a placeholder in the registered commands.
  *)
  function commandMatch(const msg: AnsiString; index: integer): boolean;

  var
    headers, suffix, value, pattern: AnsiString;
    wildcard: boolean;
    i: integer;

  begin
// TODO : Move all preparation out, since this is called in a loop.
    headers := Trim(msg);
    value := '';
    suffix := '';
    if headers[Length(headers)] = '?' then begin
      value := '?';
      SetLength(headers, Length(headers) - 1);
      headers := Trim(headers)
    end else begin
      i := Pos(' ', headers);
      if i < 1 then
        value := ''
      else begin
        value := Copy(headers, i + 1, MaxInt);
        SetLength(headers, i - 1)
      end
    end;
    if headers = '' then
      exit(false);
    wildcard := headers[Length(headers)] = '*';
    if wildcard then
      SetLength(headers, Length(headers) - 1);
    if headers = '' then
      exit(false);
    i := PosSet(['0'..'9'], headers);
    if i > 0 then begin
      suffix := Copy(headers, i, MaxInt);
      SetLength(headers, i - 1)
    end;
    if headers = '' then
      exit(false);
    headers := UpperCase(headers);

(* That should have the capitalised headers, possible numeric suffix, value or  *)
(* ?, and possible wildcard broken out. Try to match the exact case, i.e. with  *)
(* the numeric suffix (if any) as given, first.                                 *)

    i := Length(headers);
    headers += suffix;
    if value = '?' then
      headers += '?';
    pattern := registered[index];       (* Visible for debugging                *)
    if not wildcard then
      result := headers = pattern
    else
      result := Pos(pattern, headers) = 1;
    if result then
      exit;

(* If there is no numeric suffix and that failed, then exit.                    *)

    if (suffix = '') and not result then
      exit;

(* Replace the numeric suffix with # as a wildcard, and redo.                   *)

    SetLength(headers, i);
    headers += '#';
    if value = '?' then
      headers += '?';
    pattern := registered[index];       (* Visible for debugging                *)
    if not wildcard then
      result := headers = pattern
    else
      result := Pos(pattern, headers) = 1
  end { commandMatch } ;


begin
  result := false;
  critSect.Enter;
  try
    if msgQueue.Count > 0 then begin
      msg := msgQueue[0];
      msgQueue.Delete(0)
    end else
      exit(false)                       (* Via finally section                  *)
  finally
    critSect.Leave
  end;

(* <Program Messages> received from the application were broken into individual *)
(* <Program Message Units>, made root-relative, and de-abbreviated before being *)
(* enqueued. As a result, while we might need to handle a value parameter (or ? *)
(* representing a query, or a wildcard) here the greater part of the parsing    *)
(* has been done already.                                                       *)

(********************************************************************************)
(*                                                                              *)
(* Note that in all cases the responses to consecutive commands are separated   *)
(* by line breaks, assumed to be 0x0d 0x0a (CR LF), with minimal furniture.     *)
(*                                                                              *)
(********************************************************************************)

  if portNumber >= 0 then
    fPrompt := false;
  if msg <> '' then begin

(* The "HEADers" and "SYNTax" handlers operate in the scope of the SCPI server  *)
(* object, rather than using simple handler functions defined at the level of   *)
(* the application.                                                             *)
(*                                                                              *)
(* SYSTem:HELP:HEADers? results in a list of registered commands.               *)

    if msg = helpPattern then begin
      for i := 0 to Registered.Count - 1 do begin
        if fPrompt  then
          Respond('  ', false);
        Respond(RegisteredMixed[i], true)
      end;
      if fPrompt then
        Respond('  ', false);
      Respond(helpPatternMixed, true);
      if syntaxPattern <> '' then begin
        if fPrompt then
          Respond('  ', false);
        Respond(syntaxPatternMixed, true)
      end;
      Respond('', true);
      exit(true)
    end;

(* If the message starts off with the syntax pattern (typically something like  *)
(* SYSTEM:HELP:SYNTAX? ) then treat the value as a command which needs to be    *)
(* expanded to its full form. This is the only case where the value is changed  *)
(* before being passed to a registered handler.                                 *)

    if Pos(syntaxPattern + ' ', msg) = 1 then begin
      Delete(msg, 1, Length(syntaxPattern) + 1);
      pm := ReTail(deAbbreviate(ReRoot(DeTail(DeConstruct(msg)))));
      msg := ReConstruct(pm);           (* Borrowed from Enqueue() above        *)
      msg := syntaxPattern + ' ' + msg
    end;

(* SYSTem:HELP:SYNTAX? results in each of the registered commands being passed  *)
(* ' SYNTAX' (note leading space) as its parameter. Hence                       *)
(*                                                                              *)
(* Returns the syntax of the specified command                                  *)
(*                                                                              *)
(* Syntax SYSTem:HELP:SYNTax? <command_header>                                  *)
(*                                                                              *)
(* Example SYST:HELP:SYNT? INP                                                  *)
(*                                                                              *)
(* The above command returns                                                    *)
(*                                                                              *)
(*      INPut#:COUPling GND|DC\n                                                *)
(*      INPut#:COUPling? [OPTions|DEFault]\n                                    *)
(*      INPut#:GAIN MINimum|MAXimum|                                            *)
(*                  UP|DOWN|DEFault|<value>\n                                   *)
(*      INPut#:GAIN? [OPTions|DEFault]\n                                        *)
(*                                                                              *)
(* Assume that the pattern to be matched here is strictly that returned by an   *)
(* earlier SYSTem:HELP:HEADers? command. Note the special cases here to handle  *)
(* the HEADers and SYNTax commands themselves.                                  *)

    if msg = syntaxPattern + ' ' + helpPattern then begin
      if fPrompt then
        Respond('  ', false);
      Respond(helpPattern, true);
      Respond('', true);
      exit(true)
    end;
    if msg = syntaxPattern + ' ' + syntaxPattern then begin
      if fPrompt then
        Respond('  ', false);
      Respond(syntaxPattern + ' <command>', true);
      Respond('', true);
      exit(true)
    end;
    if Pos(syntaxPattern, msg) = 1 then begin
      for i := 0 to Registered.Count - 1 do begin
        if Pos(' ' + registered[i], msg) > 0 then
          result := ScpiProc(registered.Objects[i])(self, registeredMixed[i] + ' SYNTAX');
        if result then
          break
      end;
      if Assigned(defaultProc) and not result then
        result := defaultProc(self, msg + ' SYNTAX');

(* The handler invoked above might output multiple lines of text, so allowance  *)
(* is made for leading spaces in case a prompt is being issued. However since   *)
(* in theory multiple handlers might be invoked, a terminating blank line is    *)
(* issued here.                                                                 *)

      Respond('', true);
      exit(true)
    end;

(* Anything else: the message, unless unusually short, will have an appended    *)
(* value (possibly ? indicating a query) separated by a space, or * as a        *)
(* wildcard. Immediately preceding this there might be a number selecting a     *)
(* channel etc., matching # as a placeholder in the registered commands.        *)

    for i := 0 to registered.Count - 1 do begin
      if commandMatch(msg, i) then begin
        result := ScpiProc(registered.Objects[i])(self, msg);
        if result then
          break
      end
    end;
    if Assigned(defaultProc) and not result then
      result := defaultProc(self, msg)
  end                                   (* Otherwise return false               *)
end { TScpiServer.Dispatch } ;


(* Return a response to a client, with optional CRLF termination.
*)
procedure TScpiServer.Respond(const msg: ansistring; eol: boolean= false);

const
  crlf= #$0d#$0a;

begin
  if portNumber < 0 then begin
    Write(msg);
    if eol then
      WriteLn;
    Flush(output)
  end else begin
    if msg <> '' then
      fpSend(fClient, @msg[1], Length(msg), 0);
    if eol then
      fpSend(fClient, @crlf[1], Length(crlf), 0)
  end
end { TScpiServer.Respond } ;


end.

