(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit IPAddressUtils;

(* Manipulate IP addresses.                                     MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

(* Get the current host's IP addresses.

   To say that this is messy is an understatement. Even in Linux there are at
  least three ways of doing this excluding capturing  ip address ls  output etc.:

   getifaddrs()       Absent in Linux 2.4, present from at least 2.6 to at least 6.0.

   /proc/net/fib_trie Absent in Linux 2.4, present from at least 2.6 to at least 6.0.

   In addition this might be relevant:

   SIOCGIFADDR        Originally BSD, present in Linux from at least 2.4 to at least 6.0.

   However I believe there are limitations to this last case, e.g. the network
  device must be specified rather than there being an "all devices" option which
  would be appropriate when listen() has been passed 0.0.0.0 i.e. "everything".
*)
function GetOwnIPAddresses(excludeZero: boolean; randomOrder: boolean= false): AnsiString;


implementation


(* Read the content of /proc/net/fib_trie, looking specifically for lines that
  precede '32 host' markers. Return nil on error.
*)
function readFibTrie(): TStringList;

var
  fibTrie: text;
  lastLine: AnsiString= '';
  currentLine, tidied: AnsiString;


  function tidy(const line: AnsiString): AnsiString;

  begin
    result := line;
    while (result <> '') and not (result[1] in ['0'..'9', '.']) do
      Delete(result, 1, 1);
    result := Trim(result);
  end { tidy } ;


begin
  if not FileExists('/proc/net/fib_trie') then
    exit(nil);
  result := TStringList.Create;
  try
    Assign(fibTrie, '/proc/net/fib_trie');
    Reset(fibTrie);
    try
      while not Eof(fibTrie) do begin
        ReadLn(fibTrie, currentLine);
        if Pos('32 host', currentLine) > 0 then begin (* Is the last line what we want? *)
          tidied := tidy(LastLine);
          if result.Indexof(tidied) < 0 then (* Is it one we've not seen before? *)
            result.Append(tidied)
        end;
        lastLine := currentLine
      end
    finally
      Close(fibTrie)
    end
  except
    FreeAndNil(result)
  end
end { readFibTrie } ;


(* Get the current host's IP addresses.

   To say that this is messy is an understatement. Even in Linux there are at
  least three ways of doing this excluding capturing  ip address ls  output etc.:

   getifaddrs()       Absent in Linux 2.4, present from at least 2.6 to at least 6.0.

   /proc/net/fib_trie Absent in Linux 2.4, present from at least 2.6 to at least 6.0.

   In addition this might be relevant:

   SIOCGIFADDR        Originally BSD, present in Linux from at least 2.4 to at least 6.0.

   However I believe there are limitations to this last case, e.g. the network
  device must be specified rather than there being an "all devices" option which
  would be appropriate when listen() has been passed 0.0.0.0 i.e. "everything".
*)
function GetOwnIPAddresses(excludeZero: boolean; randomOrder: boolean= false): AnsiString;

const
  excludeLocalhost= false;

var
  addressList: TStringList;
  i: integer;

begin
  result := '';
  addressList := readFibTrie();
  if Assigned(addressList) then
    try
      i := 0;
      while addressList.Count > 0 do begin
        if randomOrder then
          i := Random(addressList.Count);
        if ((addressList[i] <> '127.0.0.1') or not excludeLocalHost) and
                        ((addressList[i] <> '0.0.0.0') or not excludeZero) then begin
          if result <> '' then
            result += ', ';
          result += addressList[i]
        end;
        addressList.Delete(i)
      end
    finally
      addressList.Free
    end
end { GetOwnIPAddresses } ;


end.

