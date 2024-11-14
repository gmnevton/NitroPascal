//
// Nitro Pascal Compiler
// version 1.0
//
// Reserved words
//

unit npc_reserved_words;

interface

uses
  npc_md5;

type
  TNPCReservedIdents = (
    ri_and,
    ri_array,
    ri_as,
    ri_asm,
    ri_begin,
    ri_break,
    ri_case,
    ri_class,
    ri_const,
    ri_constructor,
    ri_destructor,
    ri_dispinterface,
    ri_div,
    ri_do,
//    ri_downto,
    ri_else,
    ri_end,
    ri_except,
    ri_export,
    ri_exports,
    ri_file,
    ri_finalization,
    ri_finally,
    ri_for,
    ri_function,
    ri_goto,
    ri_if,
    ri_implementation,
    ri_in,
    ri_inherited,
    ri_initialization,
    ri_inline,
    ri_interface,
    ri_import,
    ri_imports,
    ri_is,
    ri_label,
    ri_library,
    ri_mod,
    ri_nil,
    ri_not,
    ri_object,
    ri_of,
    ri_or,
    ri_packed,
    ri_procedure,
//    ri_program,
    ri_project,
    ri_property,
    ri_raise,
    ri_record,
    ri_repeat,
    ri_resourcestring,
    ri_set,
    ri_shl,
    ri_shr,
    ri_string,
    ri_then,
    ri_threadvar,
    ri_to,
    ri_try,
    ri_type,
    ri_unit,
    ri_until,
    ri_uses,
    ri_var,
    ri_while,
    ri_with,
    ri_xor
  );

  TNPCReservedIdentsType = packed record
    Ident: String[14];
    MD5: TNPCMD5;
  end;

const
  NPCReservedIdentifiers: Array[TNPCReservedIdents] of TNPCReservedIdentsType = (
    (Ident: 'and';            MD5: (A: $be5d5d37; B: $542d75f9; C: $3a870944; D: $59f76678)), // be5d5d37 542d75f9 3a870944 59f76678
    (Ident: 'array';          MD5: (A: $f1f713c9; B: $e000f5d3; C: $f280adbd; D: $124df4f5)), // f1f713c9 e000f5d3 f280adbd 124df4f5
    (Ident: 'as';             MD5: (A: $f970e276; B: $7d0cfe75; C: $876ea857; D: $f92e319b)), // f970e276 7d0cfe75 876ea857 f92e319b
    (Ident: 'asm';            MD5: (A: $7c5cc52f; B: $db69c634; C: $bf9c216c; D: $1c2d001b)), // 7c5cc52f db69c634 bf9c216c 1c2d001b
    (Ident: 'begin';          MD5: (A: $8d589afa; B: $4dfaeeed; C: $85fff5aa; D: $78e5ff6a)), // 8d589afa 4dfaeeed 85fff5aa 78e5ff6a
    (Ident: 'break';          MD5: (A: $1cbfb724; B: $ceee46cd; C: $879df7c7; D: $cfbe7dca)), // 1cbfb724 ceee46cd 879df7c7 cfbe7dca
    (Ident: 'case';           MD5: (A: $cd14c323; B: $902024e7; C: $2c850aa8; D: $28d634a7)), // cd14c323 902024e7 2c850aa8 28d634a7
    (Ident: 'class';          MD5: (A: $a2f2ed4f; B: $8ebc2cbb; C: $4c21a29d; D: $c40ab61d)), // a2f2ed4f 8ebc2cbb 4c21a29d c40ab61d
    (Ident: 'const';          MD5: (A: $6680dba0; B: $0f3a88f6; C: $6f8029a9; D: $3d71d93c)), // 6680dba0 0f3a88f6 6f8029a9 3d71d93c
    (Ident: 'constructor';    MD5: (A: $6ca26837; B: $1eeb5d93; C: $eefeb68f; D: $96157666)), // 6ca26837 1eeb5d93 eefeb68f 96157666
    (Ident: 'destructor';     MD5: (A: $334baa9d; B: $da4aaf36; C: $7f0efb87; D: $b12c824e)), // 334baa9d da4aaf36 7f0efb87 b12c824e
    (Ident: 'dispinterface';  MD5: (A: $e7fe19e8; B: $f505b9f1; C: $c6f568eb; D: $58a49abe)), // e7fe19e8 f505b9f1 c6f568eb 58a49abe
    (Ident: 'div';            MD5: (A: $38696558; B: $dc98494c; C: $08d951c0; D: $52900a2a)), // 38696558 dc98494c 08d951c0 52900a2a
    (Ident: 'do';             MD5: (A: $d4579b26; B: $88d67523; C: $5f402f6b; D: $4b43bcbf)), // d4579b26 88d67523 5f402f6b 4b43bcbf
//    (Ident: 'downto';         MD5: (A: $8e03fd4a; B: $bf5e3809; C: $86347067; D: $91bd6bba)), // 8e03fd4a bf5e3809 86347067 91bd6bba
    (Ident: 'else';           MD5: (A: $2954e92a; B: $9b4d0e99; C: $8fe4893f; D: $8141649a)), // 2954e92a 9b4d0e99 8fe4893f 8141649a
    (Ident: 'end';            MD5: (A: $7f021a14; B: $15b86f2d; C: $013b2618; D: $fb31ae53)), // 7f021a14 15b86f2d 013b2618 fb31ae53
    (Ident: 'except';         MD5: (A: $d1cc652b; B: $86b9858f; C: $e7805851; D: $1716e92f)), // d1cc652b 86b9858f e7805851 1716e92f
    (Ident: 'export';         MD5: (A: $b2507468; B: $f9515635; C: $8fa490fd; D: $543ad2f0)), // b2507468 f9515635 8fa490fd 543ad2f0
    (Ident: 'exports';        MD5: (A: $2be75656; B: $0ea379b2; C: $23d85159; D: $24d4ffad)), // 2be75656 0ea379b2 23d85159 24d4ffad
    (Ident: 'file';           MD5: (A: $8c7dd922; B: $ad47494f; C: $c02c388e; D: $12c00eac)), // 8c7dd922 ad47494f c02c388e 12c00eac
    (Ident: 'finalization';   MD5: (A: $f3e88483; B: $6172a769; C: $a69b0fe0; D: $c246ce5f)), // f3e88483 6172a769 a69b0fe0 c246ce5f
    (Ident: 'finally';        MD5: (A: $2025c244; B: $29eb6fb6; C: $d4a2867d; D: $478db1a2)), // 2025c244 29eb6fb6 d4a2867d 478db1a2
    (Ident: 'for';            MD5: (A: $d5566982; B: $2f1a8cf7; C: $2ec1911e; D: $462a54eb)), // d5566982 2f1a8cf7 2ec1911e 462a54eb
    (Ident: 'function';       MD5: (A: $c1c42526; B: $8e68385d; C: $1ab5074c; D: $17a94f14)), // c1c42526 8e68385d 1ab5074c 17a94f14
    (Ident: 'goto';           MD5: (A: $de94e676; B: $c0358eef; C: $ea4794f0; D: $3d6bda4f)), // de94e676 c0358eef ea4794f0 3d6bda4f
    (Ident: 'if';             MD5: (A: $39c8942e; B: $1038872a; C: $822c0dc7; D: $5eedbde3)), // 39c8942e 1038872a 822c0dc7 5eedbde3
    (Ident: 'implementation'; MD5: (A: $b6fa1536; B: $0b4cdce4; C: $806bf9a0; D: $580c8f0f)), // b6fa1536 0b4cdce4 806bf9a0 580c8f0f
    (Ident: 'in';             MD5: (A: $13b5bfe9; B: $6f3e2fe4; C: $11c9f66f; D: $4a582adf)), // 13b5bfe9 6f3e2fe4 11c9f66f 4a582adf
    (Ident: 'inherited';      MD5: (A: $cae4eae4; B: $c3b46142; C: $380966d0; D: $0cc5e446)), // cae4eae4 c3b46142 380966d0 0cc5e446
    (Ident: 'initialization'; MD5: (A: $2d56c84e; B: $16cd8283; C: $af0b9c9c; D: $0c661405)), // 2d56c84e 16cd8283 af0b9c9c 0c661405
    (Ident: 'inline';         MD5: (A: $03fdad15; B: $5b754888; C: $4584c7c3; D: $9b0c5cd2)), // 03fdad15 5b754888 4584c7c3 9b0c5cd2
    (Ident: 'interface';      MD5: (A: $8eb58dd5; B: $e328e978; C: $169c7b0c; D: $bd30d43f)), // 8eb58dd5 e328e978 169c7b0c bd30d43f
    (Ident: 'import';         MD5: (A: $93473a73; B: $44419b15; C: $c4219cc2; D: $b6c64c6f)), // 93473a73 44419b15 c4219cc2 b6c64c6f
    (Ident: 'imports';        MD5: (A: $a6878bf5; B: $d8583676; C: $43a1ef28; D: $71d36f47)), // a6878bf5 d8583676 43a1ef28 71d36f47
    (Ident: 'is';             MD5: (A: $a2a551a6; B: $458a8de2; C: $2446cc76; D: $d639a9e9)), // a2a551a6 458a8de2 2446cc76 d639a9e9
    (Ident: 'label';          MD5: (A: $d304ba20; B: $e96d8741; C: $1588eeab; D: $ac850e34)), // d304ba20 e96d8741 1588eeab ac850e34
    (Ident: 'library';        MD5: (A: $d521f765; B: $a49c7250; C: $7257a262; D: $0612ee96)), // d521f765 a49c7250 7257a262 0612ee96
    (Ident: 'mod';            MD5: (A: $ad148a3c; B: $a8bd0ef3; C: $b48c5245; D: $4c493ec5)), // ad148a3c a8bd0ef3 b48c5245 4c493ec5
    (Ident: 'nil';            MD5: (A: $852438d0; B: $26c018c4; C: $307b9164; D: $06f98c62)), // 852438d0 26c018c4 307b9164 06f98c62
    (Ident: 'not';            MD5: (A: $d529e941; B: $509eb9e9; C: $b9cfaeae; D: $1fe7ca23)), // d529e941 509eb9e9 b9cfaeae 1fe7ca23
    (Ident: 'object';         MD5: (A: $a8cfde63; B: $31bd59eb; C: $2ac96f89; D: $11c4b666)), // a8cfde63 31bd59eb 2ac96f89 11c4b666
    (Ident: 'of';             MD5: (A: $8bf8854b; B: $ebe10818; C: $3caeb845; D: $c7676ae4)), // 8bf8854b ebe10818 3caeb845 c7676ae4
    (Ident: 'or';             MD5: (A: $e81c4e4f; B: $2b7b93b4; C: $81e13a85; D: $53c2ae1b)), // e81c4e4f 2b7b93b4 81e13a85 53c2ae1b
    (Ident: 'packed';         MD5: (A: $f59dcd30; B: $6ec32930; C: $f1e78a1d; D: $82280b48)), // f59dcd30 6ec32930 f1e78a1d 82280b48
    (Ident: 'procedure';      MD5: (A: $66260990; B: $8ab8e0f3; C: $72d83dea; D: $3511370b)), // 66260990 8ab8e0f3 72d83dea 3511370b
//    (Ident: 'program';        MD5: (A: $a9c449d4; B: $fa44e9e5; C: $a41c574a; D: $e55ce4d9)), // a9c449d4 fa44e9e5 a41c574a e55ce4d9
    (Ident: 'project';        MD5: (A: $46f86faa; B: $6bbf9ac9; C: $4a7e4595; D: $09a20ed0)), // 46f86faa 6bbf9ac9 4a7e4595 09a20ed0
    (Ident: 'property';       MD5: (A: $1a8db4c9; B: $96d8ed82; C: $89da5f95; D: $7879ab94)), // 1a8db4c9 96d8ed82 89da5f95 7879ab94
    (Ident: 'raise';          MD5: (A: $09024528; B: $9ce5a188; C: $ea08adfb; D: $0defd1cb)), // 09024528 9ce5a188 ea08adfb 0defd1cb
    (Ident: 'record';         MD5: (A: $de17f0f2; B: $4b49f836; C: $4187891f; D: $8550ffbb)), // de17f0f2 4b49f836 4187891f 8550ffbb
    (Ident: 'repeat';         MD5: (A: $32cf6da1; B: $34a8b268; C: $cf4ab6b7; D: $9a9a5ad9)), // 32cf6da1 34a8b268 cf4ab6b7 9a9a5ad9
    (Ident: 'resourcestring'; MD5: (A: $d9e8974b; B: $988b6a8f; C: $141bf88c; D: $fe8b4a34)), // d9e8974b 988b6a8f 141bf88c fe8b4a34
    (Ident: 'set';            MD5: (A: $cdaeeeba; B: $9b4a4c5e; C: $bf042c02; D: $15a7bb0e)), // cdaeeeba 9b4a4c5e bf042c02 15a7bb0e
    (Ident: 'shl';            MD5: (A: $bc192716; B: $b0e1d5c9; C: $6a67961d; D: $d0d3f9da)), // bc192716 b0e1d5c9 6a67961d d0d3f9da
    (Ident: 'shr';            MD5: (A: $82cfb068; B: $052c46d3; C: $6d9050ce; D: $1fcbaf2e)), // 82cfb068 052c46d3 6d9050ce 1fcbaf2e
    (Ident: 'string';         MD5: (A: $b45cffe0; B: $84dd3d20; C: $d928bee8; D: $5e7b0f21)), // b45cffe0 84dd3d20 d928bee8 5e7b0f21
    (Ident: 'then';           MD5: (A: $0e5243d9; B: $965540f6; C: $2aac19a9; D: $85f3f33e)), // 0e5243d9 965540f6 2aac19a9 85f3f33e
    (Ident: 'threadvar';      MD5: (A: $5a52486d; B: $b55c0b94; C: $1a908ee3; D: $4a367096)), // 5a52486d b55c0b94 1a908ee3 4a367096
    (Ident: 'to';             MD5: (A: $01b6e203; B: $44b68835; C: $c5ed1dde; D: $df20d531)), // 01b6e203 44b68835 c5ed1dde df20d531
    (Ident: 'try';            MD5: (A: $080f651e; B: $3fcca17d; C: $f3a47c2c; D: $ecfcb880)), // 080f651e 3fcca17d f3a47c2c ecfcb880
    (Ident: 'type';           MD5: (A: $599dcce2; B: $998a6b40; C: $b1e38e8c; D: $6006cb0a)), // 599dcce2 998a6b40 b1e38e8c 6006cb0a
    (Ident: 'unit';           MD5: (A: $3e34bdeb; B: $d9bd5edd; C: $a27e8728; D: $904a2552)), // 3e34bdeb d9bd5edd a27e8728 904a2552
    (Ident: 'until';          MD5: (A: $3103a7d8; B: $f63cb184; C: $287c558c; D: $a51c6160)), // 3103a7d8 f63cb184 287c558c a51c6160
    (Ident: 'uses';           MD5: (A: $7febea91; B: $979ea98f; C: $4b544f5a; D: $672532da)), // 7febea91 979ea98f 4b544f5a 672532da
    (Ident: 'var';            MD5: (A: $b2145aac; B: $704ce76d; C: $be1ac7ad; D: $ac535b23)), // b2145aac 704ce76d be1ac7ad ac535b23
    (Ident: 'while';          MD5: (A: $901889f4; B: $f34f8ca1; C: $8ac2f53d; D: $1fed346e)), // 901889f4 f34f8ca1 8ac2f53d 1fed346e
    (Ident: 'with';           MD5: (A: $23a58bf9; B: $274bedb1; C: $9375e527; D: $a0744fa9)), // 23a58bf9 274bedb1 9375e527 a0744fa9
    (Ident: 'xor';            MD5: (A: $a3929604; B: $21913165; C: $197845f3; D: $4bf5d1a8))  // a3929604 21913165 197845f3 4bf5d1a8
  );

function IsReservedWord(const AValue: String): Boolean; overload;
function IsReservedWord(const AValue: TNPCMD5): Boolean; overload;
function MD5ToReservedWord(const AValue: TNPCMD5): TNPCReservedIdents;

implementation

uses
  SysUtils;
//  Hash,
//  npc_md5;

function IsReservedWord(const AValue: String): Boolean;
var
  i: TNPCReservedIdents;
begin
  Result := False;
  for i:=Low(TNPCReservedIdents) to High(TNPCReservedIdents) do begin
    if SameText(AValue, NPCReservedIdentifiers[i].Ident) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function IsReservedWord(const AValue: TNPCMD5): Boolean;
var
  i: TNPCReservedIdents;
begin
  Result := False;
  for i:=Low(TNPCReservedIdents) to High(TNPCReservedIdents) do begin
    if (AValue.A = NPCReservedIdentifiers[i].MD5.A) and
       (AValue.B = NPCReservedIdentifiers[i].MD5.B) and
       (AValue.C = NPCReservedIdentifiers[i].MD5.C) and
       (AValue.D = NPCReservedIdentifiers[i].MD5.D) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function MD5ToReservedWord(const AValue: TNPCMD5): TNPCReservedIdents;
var
  i: TNPCReservedIdents;
begin
  Result := TNPCReservedIdents(-1);
  for i:=Low(TNPCReservedIdents) to High(TNPCReservedIdents) do begin
    if (AValue.A = NPCReservedIdentifiers[i].MD5.A) and
       (AValue.B = NPCReservedIdentifiers[i].MD5.B) and
       (AValue.C = NPCReservedIdentifiers[i].MD5.C) and
       (AValue.D = NPCReservedIdentifiers[i].MD5.D) then begin
      Result := i;
      Exit;
    end;
  end;
end;

//procedure InitReservedIdentifiers;
//var
//  i: Integer;
//begin
//  for i:=0 to High(TNPCReservedIdentifiers) do
//    TNPCReservedIdentifiers[i].MD5 := BytesAsNPCMD5(THashMD5.GetHashBytes(TNPCReservedIdentifiers[i].Ident));
//end;

//initialization
//  InitReservedIdentifiers;

end.