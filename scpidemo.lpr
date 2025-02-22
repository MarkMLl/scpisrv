(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

program ScpiDemo;

(* This is a minimal demonstration program for an SCPI server, note the scpiDo  *)
(* routines immediately below which are what do the actual work. MarkMLl.       *)

{$mode objfpc}{$H+}

(* Below: this must work both with and without an imported thread manager. The  *)
(* only thing that should fail if there is no thread support is Run(), scpi     *)
(* object creation and Poll() etc. should still be functional.                  *)
(*                                                                              *)
(* This program should be tested both with and without using a background       *)
(* thread, and in both cases with and without a requested prompt.               *)

{ define USETHREAD }                    // <===== TEST PARAMETER

uses
{$ifdef USETHREAD }
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
{$endif USETHREAD }
  Classes
  { you can add units after this }
  , SysUtils, ScpiServer, ScpiParser;

const
  prompt= false;                        // <===== TEST PARAMETER

var
  scpiPort: integer= -1;
  scpi: TScpiServer= nil;


{$macro on  }
{$define IS_SCPI_SYNTAX__:= (Pos(' SYNTAX', command) > 0) and (Pos(' SYNTAX', command) = Length(command) - Length(' SYNTAX') + 1) }
{$define SCPI_COMMAND_NO_SYNTAX__:= Copy(command, 1, Length(command) - Length(' SYNTAX')) }
{$define SYNTAX_REQUESTED_FOR__:= Copy(SCPI_COMMAND_NO_SYNTAX__, Pos(' ', SCPI_COMMAND_NO_SYNTAX__) + 1, MaxInt) }

function scpiDoNothing(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    if Pos(' ', SCPI_COMMAND_NO_SYNTAX__) > 0 then
      scpi.Respond('No syntax for "' + SYNTAX_REQUESTED_FOR__ + '"', true)
    else
      scpi.Respond('No syntax for "' + SCPI_COMMAND_NO_SYNTAX__ + '"', true)
  end else
    scpi.Respond('Do not understand "' + command + '"', true)
end { scpiDoNothing } ;


function scpiDoHalt(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else begin
    scpi.Destroy;
    Halt
  end
end { scpiDoHalt } ;


(* Response should be four fields with manufacturer, model (without "Model "
  text etc.), serial number, and firmware/revision info of all subsystems.

  Assume that response fields may be empty but must remain comma-delimited, and
  that trailing (but not embedded) spurious commas may be suppressed.
*)
function scpiDoIdentify(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__, true)
  end else
    scpi.Respond('Demo program', true)
end { scpiDoIdentify } ;


function scpiReportFunction(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond('Cannot report function (no instrument attached)', true)
  end
end { scpiReportFunction } ;


function scpiReportValue(scpi: TScpiServer; const {%H-}command: AnsiString): boolean;

begin
  result := true;
  if IS_SCPI_SYNTAX__ then begin
    if scpi.Prompt then
      scpi.Respond('  ', false);
    scpi.Respond(SCPI_COMMAND_NO_SYNTAX__ + ' (No instrument attached.)', true)
  end else begin
    scpi.Respond('Cannot report value (no instrument attached)', true)
  end
end { scpiReportValue } ;


begin
  if (ParamCount() > 0) and (Pos('-version', LowerCase(ParamStr(1))) > 0) then begin
    WriteLn('No version information.');
    WriteLn;
    Halt
  end;
  if ParamCount() > 0 then
    if Lowercase(ParamStr(1)) = 'scpi-telnet' then
      scpiPort := 5024                  (* Gospel according to NMap             *)
    else
      if Lowercase(ParamStr(1)) = 'scpi-raw' then
        scpiPort := 5025                (* Gospel according to NMap             *)
      else
        if (not TryStrToInt(ParamStr(1), scpiPort)) or
                    (scpiPort < 0) or (scpiPort > 65535) then begin
          WriteLn(erroutput, 'Bad SCPI port number');
          Halt(1)                       (* Bad SCPI port                        *)
        end;
  if scpiPort = -1 then
    WriteLn(erroutput, '# Starting SCPI daemon on standard I/O')
  else
    WriteLn(erroutput, '# Starting SCPI daemon on port ', scpiPort);

(* Even though we're not showing this host's IP addresses yet, we can usefully  *)
(* display the port number early so that if the daemon can't be started we know *)
(* the potential clash.                                                         *)

  scpi := TScpiServer.Create(scpiPort);
  if Assigned(scpi) then begin
    if (scpiPort > -1) and (scpi.OwnAddr <> '') then
      if Pos(' ', scpi.OwnAddr) > 0 then
        WriteLn(StdErr, '# Listening on IP addresses ', scpi.OwnAddr)
      else
        WriteLn(StdErr, '# Listening on IP address ', scpi.OwnAddr);
    scpi.BlankIsHelp := true;
    scpi.HelpIsHelp := true;
    scpi.HelpQIsHelp := true;
    scpi.Register('', @scpiDoNothing);  (* Default does nothing                 *)
    scpi.Register('SYSTem:HELP:HEADers?', nil); (* Help and syntax handler      *)
    scpi.Register('*IDN', @scpiDoIdentify); (* This is mandatory                *)
    scpi.Register('*HALT', @scpiDoHalt);

(* There are more * commands which are mandatory for any SCPI implementation,   *)
(* plus some that are optional. I don't know which of those absolutely /have/   *)
(* to be implemented for minimum functionality, there's a list on the Wp page   *)
(* and the three links                                                          *)
(*
http://www.av.it.pt/medidas/data/Manuais%20&%20Tutoriais/40b%20-%20VNA%20-%20ZVB20/CD/documents/Help_Files/WebHelp_ZVB/Remote_Control/Status_Reporting_System/status_registers.htm
http://www.av.it.pt/medidas/data/Manuais%20&%20Tutoriais/40b%20-%20VNA%20-%20ZVB20/CD/documents/Help_Files/WebHelp_ZVB/Remote_Control/Status_Reporting_System/Status_Reporting_System.htm#Structure *)
https://www.icselect.com/pdfs/ab48_11%20GPIB-101.pdf
(*                                                                              *)
(* look useful.                                                                 *)
(*                                                                              *)
(* I also don't know what the correct response for an unimplemented command is, *)
(* at present I'm ignoring it but at the very least I'd expect it to set a      *)
(* status register bit.                                                         *)

(* Placeholders for reporting the current function to which a hypothetical      *)
(* multimeter is set, and for responding to all measurement requests.           *)

    scpi.Register('SENSe:FUNCtion?', @scpiReportFunction);
    scpi.Register('SENSe:MEASure:*', @scpiReportValue);

// ...

(* Depending on whether a prompt is required, use either a background thread or *)
(* foreground polls. In practice, background threads might be disabled to mimic *)
(* the structure of a simple command-line program which typically makes import  *)
(* of cthreads conditional on the presence of the LCL.                          *)

{$ifdef USETHREAD }
    if not prompt then
      if scpi.Run(prompt) then begin

(* If this were a GUI-based program Dispatch() would probably be called from an *)
(* IdleTimer event.                                                             *)

        repeat
          while scpi.Dispatch() do
            Sleep(10);
          Sleep(100)
        until scpi.Finished
      end else
        if scpiPort < 0 then
          WriteLn(erroutput, '# Unable to run SCPI server on stdin')
        else
          WriteLn(erroutput, '# Unable to run SCPI server on port ', scpiPort)
    else
{$endif USETHREAD }
      {%H-}repeat
        if scpi.Poll(prompt { , NonBlocking } ) then
          while scpi.Dispatch() do
            Sleep(10)
        else
          Sleep(10)
      until scpi.Finished;
    FreeAndNil(scpi)
  end else
    WriteLn(erroutput, '# Unable to create SCPI server')
end.

