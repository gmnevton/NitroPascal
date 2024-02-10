//
// Nitro Pascal Compiler
// version 1.0
//
// MD5
//

unit npc_md5;

interface

uses
  Sysutils;

type
  TNPCMD5 = packed record
    A: Cardinal;
    B: Cardinal;
    C: Cardinal;
    D: Cardinal;
  end;

const
  EmptyTokenMD5: TNPCMD5 = (A: 0; B: 0; C: 0; D: 0);

function BytesAsNPCMD5(const ABytes: TBytes): TNPCMD5;

implementation

type
  TCardinalRec = packed record
    case Integer of
      0: (Value: Cardinal);
      1: (Bytes: Array[0..3] of Byte);
  end;

function FromBytesToCardinal(const AByte4, AByte3, AByte2, AByte1: Byte): Cardinal; inline;
begin
  result := AByte1 + (AByte2 shl 8) + (AByte3 shl 16) + (AByte4 shl 24);
end;

procedure FromCardinalToBytes(const AInput: Cardinal; out AByte1, AByte2, AByte3, AByte4: Byte); inline;
begin
  AByte1 := Byte(AInput);
  AByte2 := Byte(AInput shr 8);
  AByte3 := Byte(AInput shr 16);
  AByte4 := Byte(AInput shr 24);
end;

function BytesAsNPCMD5(const ABytes: TBytes): TNPCMD5;
begin
  Result := EmptyTokenMD5;
  if Length(ABytes) <> 16 then
    Exit;
  //
  Result.A := FromBytesToCardinal(ABytes[0], ABytes[1], ABytes[2], ABytes[3]);
  Result.B := FromBytesToCardinal(ABytes[4], ABytes[5], ABytes[6], ABytes[7]);
  Result.C := FromBytesToCardinal(ABytes[8], ABytes[9], ABytes[10], ABytes[11]);
  Result.D := FromBytesToCardinal(ABytes[12], ABytes[13], ABytes[14], ABytes[15]);
end;

end.