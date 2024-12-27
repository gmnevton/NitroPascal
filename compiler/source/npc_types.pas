//
// Nitro Pascal Compiler
// version 1.0
//
// Language types
//

unit npc_types;

interface

uses
  SysUtils,
  Classes,
  npc_location;

type
  TNPCType = (
    _UNKNOWN,
    
    _BOOLEAN,
    _INTEGER,
    _FLOAT,

    _UINT8,
    _UINT16,
    _UINT32,
    _UINT64,

    _SINT8,
    _SINT16,
    _SINT32,
    _SINT64,

    _FLOAT32,
    _FLOAT64,

    _STRING,

    _NULL,
    _POINTER,
    _PROCEDURE,

    _SET,
    _ARRAY,
    _RECORD,

    _VARIABLE,
    _ANY,
    _TYPE,

    _MAX_TYPES
  );

  TNPCTypeInfo = class
  public
    &Type: TNPCType;
    RunTimeSize: UInt64; // ??? why so big ???
    Name: ShortString;
    Location: TNPCLocation;
  end;

  TNPCType_Boolean = class(TNPCTypeInfo)
  public

  end;

  TNPCType_Integer = class(TNPCTypeInfo)
  public
    IsSigned: Boolean;
  end;

  TNPCType_Pointer = class(TNPCTypeInfo)
  public
    PointerTo: TNPCTypeInfo;
  end;

  TNPCProcedureFlags = (
    pf_IsPolymorphic,
    pf_IsForeign,
    pf_IsCallBack
  );

  TNPCType_Procedure = class(TNPCTypeInfo)
  public
    ArgumentTypes: Array of Pointer;
    ReturnTypes: Array of Pointer;
    Flags: TNPCProcedureFlags;
  end;

  TNPCRecordFlags = (
    rf_RecordIsIncomplete,
    rf_SetIsIncomplete
  );

  TNPCRecordMember = class;

  TNPCType_Record = class(TNPCTypeInfo)
  public
    Members: Array of TNPCRecordMember;
    Flags: TNPCRecordFlags;

    InitializerProc: Pointer;
    ConstructorProc: Pointer;
    DestructorProc: Pointer;
  end;

  TNPCRecordMember = class
  public
    &Type: TNPCTypeInfo;
    Name: ShortString;
    ByteOffset: UInt64;
    Flags: Word;
    //Notes: Array of Pointer;

  end;

implementation

end.

