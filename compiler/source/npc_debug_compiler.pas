//
// Nitro Pascal Compiler
// version 1.0
//
// Trace compiler runtime errors
//

unit npc_debug_compiler;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Contnrs;

type
{$IFDEF CPU64}
  TDebugBaseAddr = QWord;
{$ELSE}
  TDebugBaseAddr = Cardinal;
{$ENDIF}
  TDebugAddr = TDebugBaseAddr;
  PDebugAddr = ^TDebugAddr;

  TDebugModuleInfo = class(TObject)
  private
    FSize: Cardinal;
    FStartAddr: Pointer;
    FEndAddr: Pointer;
    FSystemModule: Boolean;
  public
    property Size: Cardinal read FSize;
    property StartAddr: Pointer read FStartAddr;
    property EndAddr: Pointer read FEndAddr;
    property SystemModule: Boolean read FSystemModule;
  end;

  TDebugModuleInfoList = class(TObjectList)
  private
    FDynamicBuild: Boolean;
    FSystemModulesOnly: Boolean;
    function GetItems(Index: Integer): TDebugModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TDebugModuleInfo;
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TDebugModuleInfo;
  public
    constructor Create(ADynamicBuild, ASystemModulesOnly: Boolean);
    //
    function AddModule(Module: HMODULE; SystemModule: Boolean): Boolean;
    function IsSystemModuleAddress(Addr: Pointer): Boolean;
    function IsValidModuleAddress(Addr: Pointer): Boolean;
    //
    property DynamicBuild: Boolean read FDynamicBuild;
    property Items[Index: Integer]: TDebugModuleInfo read GetItems;
    property ModuleFromAddress[Addr: Pointer]: TDebugModuleInfo read GetModuleFromAddress;
  end;

  PDWORD_PTRArray = ^TDWORD_PTRArray;
  TDWORD_PTRArray = Array[0..(MaxInt - $F) div SizeOf(DWORD_PTR)] of DWORD_PTR;

  TByteArray = Array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PByteArray = ^TByteArray;

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: TDebugAddr;
    CallerAddr: TDebugAddr;
  end;

  PStackInfo = ^TStackInfo;
  TStackInfo = record
    CallerAddr: TDebugAddr;
    Level: DWORD;
    CallerFrame: TDebugAddr;
    DumpSize: DWORD;
    ParamSize: DWORD;
    ParamPtr: PDWORD_PTRArray;
    case Integer of
      0: (StackFrame: PStackFrame);
      1: (DumpPtr: PByteArray);
  end;

  TStackInfoItem = class(TObject)
  private
    FStackInfo: TStackInfo;
    function GetCallerAddr: Pointer;
    function GetLogicalAddress: TDebugAddr;
  public
    property CallerAddr: Pointer read GetCallerAddr;
    property LogicalAddress: TDebugAddr read GetLogicalAddress;
    property StackInfo: TStackInfo read FStackInfo;
  end;

  TStackBaseList = class(TObjectList)
  private
    FThreadID: DWORD;
    FTimeStamp: TDateTime;
  protected
    FOnDestroy: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    //
    property ThreadID: DWORD read FThreadID;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

  TStackInfoList = class(TStackBaseList)
  private
    FIgnoreLevels: DWORD;
    TopOfStack: TDebugAddr;
    BaseOfStack: TDebugAddr;
    FStackData: PPointer;
    FFramePointer: Pointer;
    FModuleInfoList: TDebugModuleInfoList;
    FCorrectOnAccess: Boolean;
    FSkipFirstItem: Boolean;
    FDelayedTrace: Boolean;
    FInStackTracing: Boolean;
    FRaw: Boolean;
    FStackOffset: Int64;
    function GetItems(Index: Integer): TStackInfoItem;
    function NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
    procedure StoreToList(const StackInfo: TStackInfo);
    procedure TraceStackFrames;
    procedure TraceStackRaw;
    procedure DelayStoreStack;
    function ValidCallSite(CodeAddr: TDebugAddr; out CallInstructionSize: Cardinal): Boolean;
    function ValidStackAddr(StackAddr: TDebugAddr): Boolean;
    function GetCount: Integer;
    procedure CorrectOnAccess(ASkipFirstItem: Boolean);
  public
    constructor Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack: Pointer); overload;
    constructor Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack, ATopOfStack: Pointer); overload;
    destructor Destroy; override;
    //
    procedure ForceStackTracing;
    procedure AddToStrings(Strings: TStrings; IncludeModuleName: Boolean = False; IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False; IncludeVAddress: Boolean = False);
    //
    property DelayedTrace: Boolean read FDelayedTrace;
    property Items[Index: Integer]: TStackInfoItem read GetItems; default;
    property IgnoreLevels: DWORD read FIgnoreLevels;
    property Count: Integer read GetCount;
    property Raw: Boolean read FRaw;
  end;

  EDebugError = class(Exception);

  EDebugWin32Error = class(EDebugError)
  private
    FLastError: DWORD;
    FLastErrorMsg: String;
  public
    constructor Create(const Msg: String);
    constructor CreateFmt(const Msg: String; const Args: Array of const);
    constructor CreateRes(Ident: Integer); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: Array of const); overload;
    //
    property LastError: DWORD read FLastError;
    property LastErrorMsg: String read FLastErrorMsg;
  end;

  EFileMappingError = class(EDebugWin32Error);
  EFileMappingViewError = class(EDebugWin32Error);

function CreateStackList(Raw: Boolean; AIgnoreLevels: DWORD; FirstCaller: Pointer): TStackInfoList;

// Global exceptional stack tracker enable routines and variables
type
  TDebugStackTrackingOption = (
    stStack,
    stExceptFrame,
    stRawMode,
    stAllModules,
    stStaticModuleList,
    stDelayedTrace,
    stTraceAllExceptions,
    stMainThreadOnly,
    stDisableIfDebuggerAttached
  );
  TDebugStackTrackingOptions = set of TDebugStackTrackingOption;

var
  DebugStackTrackingOptions: TDebugStackTrackingOptions = [stStack, stTraceAllExceptions, stDisableIfDebuggerAttached];
  DebugInfoSymbolPaths: String = '';

resourcestring
  resWin32Error            = 'Win32 error: %s (%u)%s%s';

  resCreateFileMapping     = 'Failed to create FileMapping of "%s".';
  resCreateFileMappingView = 'Failed to create FileMappingView of "%s".';
  resFailedToObtainSize    = 'Failed to obtain size of file "%s".';

implementation

uses
  Character,
  TLHelp32,
  PsApi;

{$IFOPT Q+} {$DEFINE OVERFLOWCHECKS_ON} {$ENDIF}
{$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$ENDIF}

type
  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

  TUTF8String = AnsiString;
  TUTF16String = WideString;
  TPeTarget = (taUnknown, taWin32, taWin64);
{$IFDEF CPU64}
  TBaseSizeInt = NativeInt;
{$ELSE}
  TBaseSizeInt = Integer;
{$ENDIF CPU64}
  SizeInt = TBaseSizeInt;

  TDynArraySortCompare = function (Item1, Item2: Pointer): Integer;
  TDynByteArray        = Array of Byte;

var
  IsWinNT: Boolean = False;

const
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);
  NativeCrLf           = String(#13#10);
  NativeLineBreak      = NativeCrLf;

  // Misc. often used character definitions
  NativeNull = Char(#0);
  NativeSoh = Char(#1);
  NativeStx = Char(#2);
  NativeEtx = Char(#3);
  NativeEot = Char(#4);
  NativeEnq = Char(#5);
  NativeAck = Char(#6);
  NativeBell = Char(#7);
  NativeBackspace = Char(#8);
  NativeTab = Char(#9);

  NativeVerticalTab = Char(#11);
  NativeFormFeed = Char(#12);

  NativeSo = Char(#14);
  NativeSi = Char(#15);
  NativeDle = Char(#16);
  NativeDc1 = Char(#17);
  NativeDc2 = Char(#18);
  NativeDc3 = Char(#19);
  NativeDc4 = Char(#20);
  NativeNak = Char(#21);
  NativeSyn = Char(#22);
  NativeEtb = Char(#23);
  NativeCan = Char(#24);
  NativeEm = Char(#25);
  NativeEndOfFile = Char(#26);
  NativeEscape = Char(#27);
  NativeFs = Char(#28);
  NativeGs = Char(#29);
  NativeRs = Char(#30);
  NativeUs = Char(#31);
  NativeSpace = Char(' ');
  NativeComma = Char(',');
  NativeBackslash = Char('\');
  NativeForwardSlash = Char('/');

  NativeDoubleQuote = Char('"');
  NativeSingleQuote = Char('''');

  EnvironmentVarNtSymbolPath = '_NT_SYMBOL_PATH';                    // do not localize
  EnvironmentVarAlternateNtSymbolPath = '_NT_ALTERNATE_SYMBOL_PATH'; // do not localize

procedure ResetMemory(var P; Size: Longint);
begin
  if Size > 0 then begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
var
  OldProtect, Dummy: Cardinal;
begin
  WrittenBytes := 0;
  if Size > 0 then begin
    // (outchy) VirtualProtect for DEP issues
    OldProtect := 0;
    Result := VirtualProtect(BaseAddress, Size, PAGE_EXECUTE_READWRITE, OldProtect);
    if Result then try
      Move(Buffer^, BaseAddress^, Size);
      WrittenBytes := Size;
      if OldProtect in [PAGE_EXECUTE, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_EXECUTE_WRITECOPY] then
        FlushInstructionCache(GetCurrentProcess, BaseAddress, Size);
    finally
      Dummy := 0;
      VirtualProtect(BaseAddress, Size, OldProtect, Dummy);
    end;
  end;
  Result := WrittenBytes = Size;
end;

function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean): Boolean;

  procedure AddToList(ProcessHandle: THandle; Module: HMODULE);
  var
    FileName: Array [0..MAX_PATH] of Char;
    ModuleInfo: TModuleInfo;
  begin
    ModuleInfo.EntryPoint := Nil;
    if GetModuleInformation(ProcessHandle, Module, @ModuleInfo, SizeOf(ModuleInfo)) then begin
      if HandlesOnly then
        List.AddObject('', Pointer(ModuleInfo.lpBaseOfDll))
      else if GetModuleFileNameEx(ProcessHandle, Module, Filename, SizeOf(Filename)) > 0 then
        List.AddObject(FileName, Pointer(ModuleInfo.lpBaseOfDll));
    end;
  end;

  function EnumModulesVQ(ProcessHandle: THandle): Boolean;
  var
    MemInfo: TMemoryBasicInformation;
    Base: PChar;
    LastAllocBase: Pointer;
    Res: DWORD;
  begin
    Base := Nil;
    LastAllocBase := Nil;
    ResetMemory(MemInfo, SizeOf(MemInfo));
    Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    Result := (Res = SizeOf(MemInfo));
    while Res = SizeOf(MemInfo) do begin
      if MemInfo.AllocationBase <> LastAllocBase then begin
        if MemInfo.Type_9 = MEM_IMAGE then
          AddToList(ProcessHandle, HMODULE(MemInfo.AllocationBase));
        LastAllocBase := MemInfo.AllocationBase;
      end;
      Inc(Base, MemInfo.RegionSize);
      Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    end;
  end;

  function EnumModulesPS: Boolean;
  var
    ProcessHandle: THandle;
    Needed: DWORD;
    Modules: Array of THandle;
    I, Cnt: Integer;
  begin
    Result := False;
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
    if ProcessHandle <> 0 then try
      Needed := 0;
      Result := EnumProcessModules(ProcessHandle, Nil, 0, Needed);
      if Result then begin
        Cnt := Needed div SizeOf(HMODULE);
        SetLength(Modules, Cnt);
        if EnumProcessModules(ProcessHandle, @Modules[0], Needed, Needed) then
          for I := 0 to Cnt - 1 do
            AddToList(ProcessHandle, Modules[I]);
      end
      else
        Result := EnumModulesVQ(ProcessHandle);
    finally
      CloseHandle(ProcessHandle);
    end;
  end;

 { TODO: Check return value of CreateToolhelp32Snapshot on Windows NT (0?) }

  function EnumModulesTH: Boolean;
  var
    SnapProcHandle: THandle;
    Module: TModuleEntry32;
    Next: Boolean;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then try
      ResetMemory(Module, SizeOf(Module));
      Module.dwSize := SizeOf(Module);
      Next := Module32First(SnapProcHandle, Module);
      while Next do begin
        if HandlesOnly then
          List.AddObject('', Pointer(Module.hModule))
        else
          List.AddObject(Module.szExePath, Pointer(Module.hModule));
        Next := Module32Next(SnapProcHandle, Module);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

begin
  List.BeginUpdate;
  try
    if IsWinNT then
      Result := EnumModulesPS
    else
      Result := EnumModulesTH;
  finally
    List.EndUpdate;
  end;
end;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  MI.AllocationBase := Nil;
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

function PeMapImgNtHeaders32(const BaseAddress: Pointer): PImageNtHeaders32;
begin
  Result := Nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders32(TDebugAddr(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders32)) or (Result^.Signature <> IMAGE_NT_SIGNATURE) then
    Result := Nil;
end;

function PeMapImgNtHeaders64(const BaseAddress: Pointer): PImageNtHeaders64;
begin
  Result := Nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders64(TDebugAddr(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders64)) or (Result^.Signature <> IMAGE_NT_SIGNATURE) then
    Result := Nil;
end;

function PeMapImgTarget(const BaseAddress: Pointer): TPeTarget;
var
  ImageNtHeaders: PImageNtHeaders32;
begin
  Result := taUnknown;

  ImageNtHeaders := PeMapImgNtHeaders32(BaseAddress);
  if Assigned(ImageNtHeaders) then
    case ImageNtHeaders.FileHeader.Machine of
      IMAGE_FILE_MACHINE_I386 : Result := taWin32;
      IMAGE_FILE_MACHINE_AMD64: Result := taWin64;
    end;
end;

function PeMapImgSize32(const BaseAddress: Pointer): DWORD;
var
  NtHeaders32: PImageNtHeaders32;
begin
  Result := 0;
  NtHeaders32 := PeMapImgNtHeaders32(BaseAddress);
  if Assigned(NtHeaders32) then
    Result := NtHeaders32^.OptionalHeader.SizeOfImage;
end;

function PeMapImgSize64(const BaseAddress: Pointer): DWORD;
var
  NtHeaders64: PImageNtHeaders64;
begin
  Result := 0;
  NtHeaders64 := PeMapImgNtHeaders64(BaseAddress);
  if Assigned(NtHeaders64) then
    Result := NtHeaders64^.OptionalHeader.SizeOfImage;
end;

function PeMapImgSize(const BaseAddress: Pointer): DWORD;
begin
  case PeMapImgTarget(BaseAddress) of
    taWin32: Result := PeMapImgSize32(BaseAddress);
    taWin64: Result := PeMapImgSize64(BaseAddress);
  else //taUnknown:
    Result := 0;
  end;
end;

function IsSystemModule(const Module: HMODULE): Boolean;
var
  CurModule: PLibModule;
begin
  Result := False;
  if Module <> 0 then begin
    CurModule := LibModuleList;
    while CurModule <> Nil do begin
      if CurModule.Instance = Module then begin
        Result := True;
        Break;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;

function GetStackTop: TDebugAddr;
asm
{$IFDEF CPU32}
  MOV     EAX, FS:[0].NT_TIB32.StackBase
{$ENDIF CPU32}
{$IFDEF CPU64}
  MOV     RAX, FS:[0].NT_TIB64.StackBase
{$ENDIF CPU64}
end;

function GetFramePointer: Pointer;
asm
{$IFDEF CPU32}
  MOV     EAX, EBP
{$ENDIF CPU32}
{$IFDEF CPU64}
  MOV     RAX, RBP
{$ENDIF CPU64}
end;

function GetStackPointer: Pointer;
asm
{$IFDEF CPU32}
  MOV     EAX, ESP
{$ENDIF CPU32}
{$IFDEF CPU64}
  MOV     RAX, RSP
{$ENDIF CPU64}
end;

function GetExceptionPointer: Pointer;
asm
{$IFDEF CPU32}
  XOR     EAX, EAX
  MOV     EAX, FS:[EAX]
{$ENDIF CPU32}
{$IFDEF CPU64}
  XOR     RAX, RAX
  MOV     RAX, FS:[RAX]
{$ENDIF CPU64}
end;

function LockedCompareExchange(var Target: Pointer; Exch, Comp: Pointer): Pointer;
asm
{$IFDEF CPU32}
  // --> EAX Target
  //     EDX Exch
  //     ECX Comp
  // <-- EAX Result
  XCHG    EAX, ECX
  //     EAX Comp
  //     EDX Exch
  //     ECX Target
  LOCK CMPXCHG [ECX], EDX
{$ENDIF CPU32}
{$IFDEF CPU64}
  // --> RCX Target
  //     RDX Exch
  //     R8  Comp
  // <-- RAX Result
  MOV     RAX, R8
  //     RCX Target
  //     RDX Exch
  //     RAX Comp
  LOCK CMPXCHG [RCX], RDX
{$ENDIF CPU64}
end;

function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare; ValuePtr: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ArrayPtr <> Nil then begin
    L := 0;
    H := PInteger(TDebugAddr(ArrayPtr) - 4)^ - 1;
    B := False;
    while L <= H do begin
      I := (L + H) shr 1;
      C := SortFunc(Pointer(TDebugAddr(ArrayPtr) + (Cardinal(I) * ElementSize)), ValuePtr);
      if C < 0 then
        L := I + 1
      else begin
        H := I - 1;
        if C = 0 then begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else if Nearest and (H >= 0) then
      Result := H;
  end;
end;

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
var
  TempBuf: TDynByteArray;

  function ArrayItemPointer(Item: Integer): Pointer;
  begin
    Result := Pointer(TDebugAddr(ArrayPtr) + (Cardinal(Item) * ElementSize));
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, T: Integer;
    P, IPtr, JPtr: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := ArrayItemPointer((L + R) shr 1);
      repeat
        while SortFunc(ArrayItemPointer(I), P) < 0 do
          Inc(I);
        while SortFunc(ArrayItemPointer(J), P) > 0 do
          Dec(J);
        if I <= J then begin
          IPtr := ArrayItemPointer(I);
          JPtr := ArrayItemPointer(J);
          case ElementSize of
            SizeOf(Byte): begin
              T := PByte(IPtr)^;
              PByte(IPtr)^ := PByte(JPtr)^;
              PByte(JPtr)^ := T;
            end;
            SizeOf(Word): begin
              T := PWord(IPtr)^;
              PWord(IPtr)^ := PWord(JPtr)^;
              PWord(JPtr)^ := T;
            end;
            SizeOf(Integer): begin
              T := PInteger(IPtr)^;
              PInteger(IPtr)^ := PInteger(JPtr)^;
              PInteger(JPtr)^ := T;
            end;
          else
            Move(IPtr^, TempBuf[0], ElementSize);
            Move(JPtr^, IPtr^, ElementSize);
            Move(TempBuf[0], JPtr^, ElementSize);
          end;
          if P = IPtr then
            P := JPtr
          else if P = JPtr then
            P := IPtr;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ArrayPtr <> Nil then begin
    SetLength(TempBuf, ElementSize);
    QuickSort(0, PInteger(TDebugAddr(ArrayPtr) - 4)^ - 1);
  end;
end;

{ EDebugWin32Error }

constructor EDebugWin32Error.Create(const Msg: String);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@resWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, Msg]);
end;

constructor EDebugWin32Error.CreateFmt(const Msg: String; const Args: Array of const);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@resWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, Format(Msg, Args)]);
end;

constructor EDebugWin32Error.CreateRes(Ident: Integer);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@resWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, LoadStr(Ident)]);
end;

constructor EDebugWin32Error.CreateRes(ResStringRec: PResStringRec);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@resWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, LoadResString(ResStringRec)]);
end;

constructor EDebugWin32Error.CreateResFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  FLastError := GetLastError;
  FLastErrorMsg := SysErrorMessage(FLastError);
  inherited CreateResFmt(@resWin32Error, [FLastErrorMsg, FLastError, NativeLineBreak, Format(LoadResString(ResStringRec), Args)]);
end;

// Exceptions hooking in libraries
type
  TModuleArray = Array of HMODULE;

  TDebugCriticalSection = class(TObject)
  private
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    class procedure CreateAndEnter(var CS: TDebugCriticalSection);
    //
    procedure Enter;
    procedure Leave;
  end;

  TGlobalModulesList = class(TObject)
  private
    FAddedModules: TStringList;
    FHookedModules: TModuleArray;
    FLock: TDebugCriticalSection;
    FModulesList: TDebugModuleInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure AddModule(const ModuleName: String);
    function CreateModulesList: TDebugModuleInfoList;
    procedure FreeModulesList(var ModulesList: TDebugModuleInfoList);
    function ValidateAddress(Addr: Pointer): Boolean;
  end;

  PExceptionArguments = ^TExceptionArguments;
  TExceptionArguments = record
    ExceptAddr: Pointer;
    ExceptObj: Exception;
  end;

  TExceptFilterProc = function(ExceptRecord: PExceptionRecord): Exception;
  TExceptNotifyProc = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  TExceptNotifyProcEx = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
  TExceptNotifyMethod = procedure(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean) of object;

  TDebugExceptNotifyPriority = (npNormal, npFirstChain);

  TFilterItem = class(TObject)
  private
    FExceptFilterProc: TExceptFilterProc;
    FPriority: TDebugExceptNotifyPriority;
  public
    constructor Create(const ExceptFilterProc: TExceptFilterProc; APriority: TDebugExceptNotifyPriority);
    //
    function DoFilterException(ExceptRecord: PExceptionRecord; out ExceptObj: Exception): Boolean;
    //
    property Priority: TDebugExceptNotifyPriority read FPriority;
  end;

  TNotifierItem = class(TObject)
  private
    FNotifyMethod: TExceptNotifyMethod;
    FNotifyProc: TExceptNotifyProc;
    FNotifyProcEx: TExceptNotifyProcEx;
    FPriority: TDebugExceptNotifyPriority;
  public
    constructor Create(const NotifyProc: TExceptNotifyProc; Priority: TDebugExceptNotifyPriority); overload;
    constructor Create(const NotifyProc: TExceptNotifyProcEx; Priority: TDebugExceptNotifyPriority); overload;
    constructor Create(const NotifyMethod: TExceptNotifyMethod; Priority: TDebugExceptNotifyPriority); overload;
    //
    procedure DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
    //
    property Priority: TDebugExceptNotifyPriority read FPriority;
  end;

  TPeMapImgHookItem = class(TObject)
  private
    FBaseAddress: Pointer;
    FFunctionName: String;
    FModuleName: String;
    FNewAddress: Pointer;
    FOriginalAddress: Pointer;
    FList: TObjectList;
  protected
    function InternalUnhook: Boolean;
  public
    constructor Create(AList: TObjectList; const AFunctionName: String; const AModuleName: String; ABaseAddress, ANewAddress, AOriginalAddress: Pointer);
    destructor Destroy; override;
    //
    function Unhook: Boolean;
    //
    property BaseAddress: Pointer read FBaseAddress;
    property FunctionName: String read FFunctionName;
    property ModuleName: String read FModuleName;
    property NewAddress: Pointer read FNewAddress;
    property OriginalAddress: Pointer read FOriginalAddress;
  end;

  TPeMapImgHooks = class(TObjectList)
  private
    function GetItems(Index: Integer): TPeMapImgHookItem;
    function GetItemFromOriginalAddress(OriginalAddress: Pointer): TPeMapImgHookItem;
    function GetItemFromNewAddress(NewAddress: Pointer): TPeMapImgHookItem;
  public
    function HookImport(Base: Pointer; const ModuleName: String; const FunctionName: String; NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
    class function IsWin9xDebugThunk(P: Pointer): Boolean;
    class function ReplaceImport(Base: Pointer; const ModuleName: String; FromProc, ToProc: Pointer): Boolean;
    class function SystemBase: Pointer;
    procedure UnhookAll;
    function UnhookByNewAddress(NewAddress: Pointer): Boolean;
    procedure UnhookByBaseAddress(BaseAddress: Pointer);
    //
    property Items[Index: Integer]: TPeMapImgHookItem read GetItems; default;
    property ItemFromOriginalAddress[OriginalAddress: Pointer]: TPeMapImgHookItem read GetItemFromOriginalAddress;
    property ItemFromNewAddress[NewAddress: Pointer]: TPeMapImgHookItem read GetItemFromNewAddress;
  end;

  TDebugInfoSource = class;

  PDebugLocationInfo = ^TDebugLocationInfo;
  TDebugLocationInfo = record
    Address: Pointer;               // Error address
    UnitName: String;               // Name of Delphi unit
    ProcedureName: String;          // Procedure name
    OffsetFromProcName: Integer;    // Offset from Address to ProcedureName symbol location
    LineNumber: Integer;            // Line number
    OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
    SourceName: String;             // Module file name
    DebugInfo: TDebugInfoSource;    // Location object
    BinaryFileName: String;         // Name of the binary file containing the symbol
  end;

  TDebugInfoSource = class(TObject)
  private
    FModule: HMODULE;
    function GetFileName: TFileName;
  protected
    function VAFromAddr(const Addr: Pointer): DWORD; virtual;
  public
    constructor Create(AModule: HMODULE); virtual;
    //
    function InitializeSource: Boolean; virtual; abstract;
    function GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean; virtual; abstract;
    //
    property Module: HMODULE read FModule;
    property FileName: TFileName read GetFileName;
  end;

  TDebugInfoSourceClass = class of TDebugInfoSource;

  TFileMappingStream = class(TCustomMemoryStream)
  private
    FFileHandle: THandle;
    FMapping: THandle;
  protected
    procedure Close;
  public
    constructor Create(const FileName: String; FileMode: Word = fmOpenRead or fmShareDenyWrite);
    destructor Destroy; override;
    //
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  PDebugMapAddress = ^TDebugMapAddress;
  TDebugMapAddress = packed record
    Segment: Word;
    Offset: TDebugAddr;
  end;

  PDebugMapString = PAnsiChar;

  TAbstractDebugMapParser = class(TObject)
  private
    FLinkerBug: Boolean;
    FLinkerBugUnitName: PDebugMapString;
    FStream: TFileMappingStream;
    function GetLinkerBugUnitName: String;
  protected
    FModule: HMODULE;
    FLastUnitName: PDebugMapString;
    FLastUnitFileName: PDebugMapString;
    procedure ClassTableItem(const Address: TDebugMapAddress; Len: Integer; SectionName, GroupName: PDebugMapString); virtual; abstract;
    procedure SegmentItem(const Address: TDebugMapAddress; Len: Integer; GroupName, UnitName: PDebugMapString); virtual; abstract;
    procedure PublicsByNameItem(const Address: TDebugMapAddress; Name: PDebugMapString); virtual; abstract;
    procedure PublicsByValueItem(const Address: TDebugMapAddress; Name: PDebugMapString); virtual; abstract;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PDebugMapString); virtual; abstract;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TDebugMapAddress); virtual; abstract;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); overload; virtual;
    constructor Create(const MapFileName: TFileName); overload;
    destructor Destroy; override;
    //
    procedure Parse;
    class function MapStringToFileName(MapString: PDebugMapString): String;
    class function MapStringToModuleName(MapString: PDebugMapString): String;
    class function MapStringToStr(MapString: PDebugMapString; IgnoreSpaces: Boolean = False): String;
    //
    property LinkerBug: Boolean read FLinkerBug;
    property LinkerBugUnitName: String read GetLinkerBugUnitName;
    property Stream: TFileMappingStream read FStream;
  end;

  TDebugMapStringCache = record
    CachedValue: String;
    RawValue: PDebugMapString;
  end;

  PDebugMapSegmentClass = ^TDebugMapSegmentClass;
  TDebugMapSegmentClass = record
    Segment: Word; // segment ID
    Start: DWORD;  // start as in the map file
    Addr: DWORD;   // start as in process memory
    VA: DWORD;     // position relative to module base adress
    Len: DWORD;    // segment length
    SectionName: TDebugMapStringCache;
    GroupName: TDebugMapStringCache;
  end;

  PDebugMapLineNumber = ^TDebugMapLineNumber;
  TDebugMapLineNumber = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    LineNumber: Integer;
  end;

  PDebugMapProcName = ^TDebugMapProcName;
  TDebugMapProcName = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    ProcName: TDebugMapStringCache;
  end;

  PDebugMapSegment = ^TDebugMapSegment;
  TDebugMapSegment = record
    Segment: Word;
    StartVA: DWORD; // VA relative to (module base address + $10000)
    EndVA: DWORD;
    UnitName: TDebugMapStringCache;
  end;

  TDebugMapScanner = class(TAbstractDebugMapParser)
  private
    FSegmentClasses: Array of TDebugMapSegmentClass;
    FLineNumbers: Array of TDebugMapLineNumber;
    FProcNames: Array of TDebugMapProcName;
    FSegments: Array of TDebugMapSegment;
    FSourceNames: Array of TDebugMapProcName;
    FLineNumbersCnt: Integer;
    FLineNumberErrors: Integer;
    FNewUnitFileName: PDebugMapString;
    FProcNamesCnt: Integer;
    FSegmentCnt: Integer;
  protected
    function MAPAddrToVA(const Addr: DWORD): DWORD;
    procedure ClassTableItem(const Address: TDebugMapAddress; Len: Integer; SectionName, GroupName: PDebugMapString); override;
    procedure SegmentItem(const Address: TDebugMapAddress; Len: Integer; GroupName, UnitName: PDebugMapString); override;
    procedure PublicsByNameItem(const Address: TDebugMapAddress; Name: PDebugMapString); override;
    procedure PublicsByValueItem(const Address: TDebugMapAddress; Name: PDebugMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TDebugMapAddress); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PDebugMapString); override;
    procedure Scan;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); override;

    class function MapStringCacheToFileName(var MapString: TDebugMapStringCache): String;
    class function MapStringCacheToModuleName(var MapString: TDebugMapStringCache): String;
    class function MapStringCacheToStr(var MapString: TDebugMapStringCache; IgnoreSpaces: Boolean = False): String;

    // Addr are virtual addresses relative to (module base address + $10000)
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer; overload;
    function ModuleNameFromAddr(Addr: DWORD): String;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function ProcNameFromAddr(Addr: DWORD): String; overload;
    function ProcNameFromAddr(Addr: DWORD; out Offset: Integer): String; overload;
    function SourceNameFromAddr(Addr: DWORD): String;
    property LineNumberErrors: Integer read FLineNumberErrors;
  end;

  TDebugInfoMap = class(TDebugInfoSource)
  private
    FScanner: TDebugMapScanner;
  public
    destructor Destroy; override;
    //
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean; override;
  end;

  TDebugInfoSymbols = class(TDebugInfoSource)
  public
    class function LoadDebugFunctions: Boolean;
    class function UnloadDebugFunctions: Boolean;
    class function InitializeDebugSymbols: Boolean;
    class function CleanupDebugSymbols: Boolean;
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean; override;
  end;

  PMapLineNumber = ^TMapLineNumber;
  TMapLineNumber = record
    Segment: Word;
    VA: DWORD; // VA relative to (module base address + $10000)
    LineNumber: Integer;
  end;

  TBinDbgNameCache = record
    Addr: DWORD;
    FirstWord: Integer;
    SecondWord: Integer;
  end;

  TBinDebugScanner = class(TObject)
  private
    FCacheData: Boolean;
    FStream: TCustomMemoryStream;
    FValidFormat: Boolean;
    FLineNumbers: Array of TMapLineNumber;
    FProcNames: Array of TBinDbgNameCache;
    function GetModuleName: String;
  protected
    procedure CacheLineNumbers;
    procedure CacheProcNames;
    procedure CheckFormat;
    function DataToStr(A: Integer): String;
    function MakePtr(A: Integer): Pointer;
    function ReadValue(var P: Pointer; var Value: Integer): Boolean;
  public
    constructor Create(AStream: TCustomMemoryStream; CacheData: Boolean);
    //
    function IsModuleNameValid(const Name: TFileName): Boolean;
    function LineNumberFromAddr(Addr: DWORD): Integer; overload;
    function LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer; overload;
    function ProcNameFromAddr(Addr: DWORD): String; overload;
    function ProcNameFromAddr(Addr: DWORD; out Offset: Integer): String; overload;
    function ModuleNameFromAddr(Addr: DWORD): String;
    function ModuleStartFromAddr(Addr: DWORD): DWORD;
    function SourceNameFromAddr(Addr: DWORD): String;
    //
    property ModuleName: String read GetModuleName;
    property ValidFormat: Boolean read FValidFormat;
  end;

  TDebugInfoList = class(TObjectList)
  private
    function GetItemFromModule(const Module: HMODULE): TDebugInfoSource;
    function GetItems(Index: Integer): TDebugInfoSource;
  protected
    function CreateDebugInfo(const Module: HMODULE): TDebugInfoSource;
  public
    class procedure RegisterDebugInfoSource(const InfoSourceClass: TDebugInfoSourceClass);
    class procedure UnRegisterDebugInfoSource(const InfoSourceClass: TDebugInfoSourceClass);
    class procedure RegisterDebugInfoSourceFirst(const InfoSourceClass: TDebugInfoSourceClass);
    class procedure NeedInfoSourceClassList;
    function GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean;
    //
    property ItemFromModule[const Module: HMODULE]: TDebugInfoSource read GetItemFromModule;
    property Items[Index: Integer]: TDebugInfoSource read GetItems;
  end;

  TExceptFrameKind = (
    efkUnknown,
    efkFinally,
    efkAnyException,
    efkOnException,
    efkAutoException
  );

  TExcDescEntry = record // from System.pas
    VTable: Pointer;
    Handler: Pointer;
  end;

  PJmpInstruction = ^TJmpInstruction;
  TJmpInstruction = packed record // from System.pas
    OpCode: Byte;
    Distance: Longint;
  end;

  PExcDesc = ^TExcDesc;
  TExcDesc = packed record // from System.pas
    JMP: TJmpInstruction;
    case Integer of
      0: (
        Instructions: Array [0..0] of Byte
      );
      1: (
        Cnt: Integer;
        ExcTab: Array [0..0] of TExcDescEntry
      );
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame =  record // from System.pas
    Next: PExcFrame;
    Desc: PExcDesc;
    FramePointer: Pointer;
    case Integer of
      0: ();
      1: (
        ConstructedObject: Pointer
      );
      2: (
        SelfOfMethod: Pointer
      );
  end;

  PJmpTable = ^TJmpTable;
  TJmpTable = packed record
    OPCode: Word; // FF 25 = JMP DWORD PTR [$xxxxxxxx], encoded as $25FF
    Ptr: Pointer;
  end;

  TExceptFrame = class(TObject)
  private
    FFrameKind: TExceptFrameKind;
    FFrameLocation: Pointer;
    FCodeLocation: Pointer;
    FExcTab: Array of TExcDescEntry;
  protected
    procedure AnalyseExceptFrame(AExcDesc: PExcDesc);
  public
    constructor Create(AFrameLocation: Pointer; AExcDesc: PExcDesc);
    //
    function Handles(ExceptObj: TObject): Boolean;
    function HandlerInfo(ExceptObj: TObject; out HandlerAt: Pointer): Boolean;
    //
    property CodeLocation: Pointer read FCodeLocation;
    property FrameLocation: Pointer read FFrameLocation;
    property FrameKind: TExceptFrameKind read FFrameKind;
  end;

  TExceptFrameList = class(TStackBaseList)
  private
    FIgnoreLevels: Integer;
    function GetItems(Index: Integer): TExceptFrame;
  protected
    function AddFrame(AFrame: PExcFrame): TExceptFrame;
  public
    constructor Create(AIgnoreLevels: Integer);
    //
    procedure TraceExceptionFrames;
    //
    property Items[Index: Integer]: TExceptFrame read GetItems;
    property IgnoreLevels: Integer read FIgnoreLevels write FIgnoreLevels;
  end;

  TStackBaseListClass = class of TStackBaseList;

  TGlobalStackList = class(TThreadList)
  private
    FLockedTID: DWORD;
    FTIDLocked: Boolean;
    function GetExceptStackInfo(TID: DWORD): TStackInfoList;
    function GetLastExceptFrameList(TID: DWORD): TExceptFrameList;
    procedure ItemDestroyed(Sender: TObject);
  public
    destructor Destroy; override;
    //
    procedure AddObject(AObject: TStackBaseList);
    procedure Clear;
    procedure LockThreadID(TID: DWORD);
    procedure UnlockThreadID;
    function FindObject(TID: DWORD; AClass: TStackBaseListClass): TStackBaseList;
    //
    property ExceptStackInfo[TID: DWORD]: TStackInfoList read GetExceptStackInfo;
    property LastExceptFrameList[TID: DWORD]: TExceptFrameList read GetLastExceptFrameList;
  end;

  PDbgHeader = ^TDbgHeader;
  TDbgHeader = packed record
    Signature: DWORD;
    Version: Byte;
    Units: Integer;
    SourceNames: Integer;
    Symbols: Integer;
    LineNumbers: Integer;
    Words: Integer;
    ModuleName: Integer;
    CheckSum: Integer;
    CheckSumValid: Boolean;
  end;

threadvar
  Recursive: Boolean;
  NewResultExc: Exception;

var
  GlobalModulesList: TGlobalModulesList;
  ExceptionsHooked: Boolean;
  Kernel32_RaiseException: procedure (dwExceptionCode, dwExceptionFlags, nNumberOfArguments: DWORD; lpArguments: PDWORD); stdcall;
  SysUtils_ExceptObjProc: function (P: PExceptionRecord): Exception;
  Notifiers: TThreadList;
  Filters: TThreadList;
  DebugInfoList: TDebugInfoList = Nil;
  InfoSourceClassList: TList = Nil;
  DebugInfoCritSect: TDebugCriticalSection;
  GlobalStackList: TGlobalStackList;

const
  ModuleCodeOffset = $1000;
  MaxStackTraceItems = 4096;

procedure NeedDebugInfoList;
begin
  if DebugInfoList = Nil then
    DebugInfoList := TDebugInfoList.Create;
end;

function GetModulePath(const Module: HMODULE): String;
var
  L: Integer;
begin
  L := MAX_PATH + 1;
  SetLength(Result, L);
  L := Windows.GetModuleFileName(Module, Pointer(Result), L);
  SetLength(Result, L);
end;

function ValidCodeAddr(CodeAddr: DWORD; ModuleList: TDebugModuleInfoList): Boolean;
begin
  if stAllModules in DebugStackTrackingOptions then
    Result := ModuleList.IsValidModuleAddress(Pointer(CodeAddr))
  else
    Result := ModuleList.IsSystemModuleAddress(Pointer(CodeAddr));
end;

procedure FreeThreadObjList(var TheList: TThreadList);
var
  I: Integer;
begin
  with TheList.LockList do
    try
      for I := 0 to Count - 1 do
        TObject(Items[I]).Free;
    finally
      TheList.UnlockList;
    end;
  FreeAndNil(TheList);
end;

function DebugHookedExceptModulesList(out ModulesList: TModuleArray): Boolean;
begin
{$IFDEF HOOK_DLL_EXCEPTIONS}
  Result := Assigned(HookExceptModuleList);
  if Result then
    HookExceptModuleList.List(ModulesList);
{$ELSE HOOK_DLL_EXCEPTIONS}
  Result := False;
  SetLength(ModulesList, 0);
{$ENDIF HOOK_DLL_EXCEPTIONS}
end;

function DoExceptFilter(ExceptRecord: PExceptionRecord): Exception;
var
  Priorities: TDebugExceptNotifyPriority;
  I: Integer;
begin
  if Recursive then
    Exit;
  if Assigned(Filters) then begin
    Recursive := True;
    try
      with Filters.LockList do try
        for Priorities := High(Priorities) downto Low(Priorities) do begin
          for I := 0 to Count - 1 do begin
            with TFilterItem(Items[I]) do begin
              if Priority = Priorities then begin
                if DoFilterException(ExceptRecord, Result) then
                  Exit;
              end;
            end;
          end;
        end;
      finally
        Filters.UnlockList;
      end;
      // Nobody wanted to handle the external exception. Call the default handler.
      Result := SysUtils_ExceptObjProc(ExceptRecord);
    finally
      Recursive := False;
    end;
  end;
end;

procedure DoExceptNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
var
  Priorities: TDebugExceptNotifyPriority;
  I: Integer;
begin
  if Recursive then
    Exit;
  if Assigned(Notifiers) then begin
    Recursive := True;
    NewResultExc := Nil;
    try
      with Notifiers.LockList do
      try
        if Count = 1 then begin
          with TNotifierItem(Items[0]) do
            DoNotify(ExceptObj, ExceptAddr, OSException, StackPointer);
        end
        else begin
          for Priorities := High(Priorities) downto Low(Priorities) do begin
            for I := 0 to Count - 1 do begin
              with TNotifierItem(Items[I]) do begin
                if Priority = Priorities then
                  DoNotify(ExceptObj, ExceptAddr, OSException, StackPointer);
              end;
            end;
          end;
        end;
      finally
        Notifiers.UnlockList;
      end;
    finally
      Recursive := False;
    end;
  end;
end;

procedure HookedRaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments: DWORD; Arguments: PExceptionArguments); stdcall;
const
  cDelphiException = $0EEDFADE;
  cNonContinuable = 1;                  // Delphi exceptions
  cNonContinuableException = $C0000025; // C++Builder exceptions (sounds like a bug)
  DelphiNumberOfArguments = 7;
  CBuilderNumberOfArguments = 8;
begin
  if ((ExceptionFlags = cNonContinuable) or (ExceptionFlags = cNonContinuableException)) and
     (ExceptionCode = cDelphiException) and (NumberOfArguments in [DelphiNumberOfArguments, CBuilderNumberOfArguments]) and
     (TDebugAddr(Arguments) = TDebugAddr(@Arguments) + SizeOf(Pointer)) then begin
    DoExceptNotify(Arguments.ExceptObj, Arguments.ExceptAddr, False, GetFramePointer);
  end;
  Kernel32_RaiseException(ExceptionCode, ExceptionFlags, NumberOfArguments, PDWORD(Arguments));
end;

function HookedExceptObjProc(P: PExceptionRecord): Exception;
var
  NewResultExcCache: Exception; // TLS optimization
begin
  Result := DoExceptFilter(P);
  DoExceptNotify(Result, P^.ExceptionAddress, True, GetFramePointer);
  NewResultExcCache := NewResultExc;
  if NewResultExcCache <> Nil then
    Result := NewResultExcCache;
end;

function DebugBelongsHookedCode(Address: Pointer): Boolean;
begin
  Result := (TDebugAddr(@HookedRaiseException) < TDebugAddr(@DebugBelongsHookedCode)) and
            (TDebugAddr(@HookedRaiseException) <= TDebugAddr(Address)) and
            (TDebugAddr(@DebugBelongsHookedCode) > TDebugAddr(Address));
end;

procedure CorrectExceptStackListTop(List: TStackInfoList; SkipFirstItem: Boolean);
var
  TopItem, I, FoundPos: Integer;
begin
  FoundPos := -1;
  if SkipFirstItem then
    TopItem := 1
  else
    TopItem := 0;
  with List do begin
    for I := Count - 1 downto TopItem do begin
      if DebugBelongsHookedCode(Items[I].CallerAddr) then begin
        FoundPos := I;
        Break;
      end;
    end;
    if FoundPos <> -1 then
      for I := FoundPos downto TopItem do
        Delete(I);
  end;
end;

function RaiseExceptionAddress: Pointer;
begin
  Result := GetProcAddress(GetModuleHandle(kernel32), 'RaiseException');
  Assert(Result <> Nil);
end;

function GetCppRtlBase: Pointer;
const
  { Successive RTLDLL version numbers in the remaining cases: CB2006 has cc3270mt.dll,
    CB2009 (= CB2006 + 2 releases) has cc3290mt.dll, CB2010 has cc32100mt.dll etc. }
  CppRtlVersion = 70 + Trunc(RtlVersion - 18.0) * 10;
begin
  Result := Pointer(GetModuleHandle(PChar(Format('cc32%dmt.dll', [CppRtlVersion]))));
  { 'Result = Nil' means that the C++ RTL has been linked statically or is not available at all;
    in this case TPeMapImgHooks.ReplaceImport() is a no-op. The base module is also being
    hooked separately, so we're covered. }
end;

function HasCppRtl: Boolean;
begin
  Result := GetCppRtlBase <> TPeMapImgHooks.SystemBase;
end;

function DebugHookExceptions: Boolean;
var
  RaiseExceptionAddressCache: Pointer;
begin
  RaiseExceptionAddressCache := RaiseExceptionAddress;
  { Detect C++Builder applications and C++ packages loaded into Delphi applications.
    Hook the C++ RTL regardless of ExceptionsHooked so that users can call JclHookException() after
    loading a C++ package which might pull in the C++ RTL DLL. }
  if HasCppRtl then
    TPeMapImgHooks.ReplaceImport(GetCppRtlBase, kernel32, RaiseExceptionAddressCache, @HookedRaiseException);
  if not ExceptionsHooked then begin
    Recursive := False;
    with TPeMapImgHooks do
      Result := ReplaceImport(SystemBase, kernel32, RaiseExceptionAddressCache, @HookedRaiseException);
    if Result then begin
      @Kernel32_RaiseException := RaiseExceptionAddressCache;
      SysUtils_ExceptObjProc := System.ExceptObjProc;
      System.ExceptObjProc := @HookedExceptObjProc;
    end;
    ExceptionsHooked := Result;
  end
  else
    Result := True;
end;

function DebugUnhookExceptions: Boolean;
begin
  if HasCppRtl then
    TPeMapImgHooks.ReplaceImport (GetCppRtlBase, kernel32, @HookedRaiseException, @Kernel32_RaiseException);
  if ExceptionsHooked then begin
    with TPeMapImgHooks do
      ReplaceImport(SystemBase, kernel32, @HookedRaiseException, @Kernel32_RaiseException);
    System.ExceptObjProc := @SysUtils_ExceptObjProc;
    @SysUtils_ExceptObjProc := Nil;
    @Kernel32_RaiseException := Nil;
    Result := True;
    ExceptionsHooked := False;
  end
  else
    Result := True;
end;

function GetLocationInfo(const Addr: Pointer): TDebugLocationInfo; overload;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      DebugInfoList.GetLocationInfo(Addr, Result)
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    Finalize(Result);
    ResetMemory(Result, SizeOf(Result));
  end;
end;

function GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean; overload;
begin
  try
    DebugInfoCritSect.Enter;
    try
      NeedDebugInfoList;
      Result := DebugInfoList.GetLocationInfo(Addr, Info);
    finally
      DebugInfoCritSect.Leave;
    end;
  except
    Result := False;
  end;
end;

function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset: Boolean; IncludeVAddress: Boolean): String;
var
  Info, StartProcInfo: TDebugLocationInfo;
  OffsetStr, StartProcOffsetStr, FixedProcedureName: String;
  Module : HMODULE;
begin
  OffsetStr := '';
  if GetLocationInfo(Addr, Info) then begin
    with Info do begin
      FixedProcedureName := ProcedureName;
      if Pos(UnitName + '.', FixedProcedureName) = 1 then
        FixedProcedureName := Copy(FixedProcedureName, Length(UnitName) + 2, Length(FixedProcedureName) - Length(UnitName) - 1);

      if LineNumber > 0 then begin
        if IncludeStartProcLineOffset and GetLocationInfo(Pointer(TDebugAddr(Info.Address) - Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
            StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
        else
          StartProcOffsetStr := '';

        if IncludeAddressOffset then begin
          if OffsetFromLineNumber >= 0 then
            OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
          else
            OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
        end;

        Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Addr, UnitName, FixedProcedureName, LineNumber, SourceName, StartProcOffsetStr, OffsetStr]);
      end
      else begin
        if IncludeAddressOffset then
          OffsetStr := Format(' + $%x', [OffsetFromProcName]);

        if Length(UnitName) > 0 then
          Result := Format('[%p] %s.%s%s', [Addr, UnitName, FixedProcedureName, OffsetStr])
        else
          Result := Format('[%p] %s%s', [Addr, FixedProcedureName, OffsetStr]);
      end;
    end
  end
  else begin
    Result := Format('[%p]', [Addr]);
    IncludeVAddress := True;
  end;

  if IncludeVAddress or IncludeModuleName then begin
    Module := ModuleFromAddr(Addr);
    if IncludeVAddress then begin
      OffsetStr :=  Format('(%p) ', [Pointer(TDebugAddr(Addr) - Module - ModuleCodeOffset)]);
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ExtractFileName(GetModulePath(Module))]), Result, 11);
  end;
end;

function SearchForStackPtrManipulation(StackPtr: Pointer; Proc: Pointer): Pointer; inline;
{var
  Addr: PByteArray;}
begin
{  Addr := Proc;
  while (Addr <> Nil) and (DWORD_PTR(Addr) > DWORD_PTR(Proc) - $100) and not IsBadReadPtr(Addr, 6) do
  begin
    if (Addr[0] = $55) and                                           // push ebp
       (Addr[1] = $8B) and (Addr[2] = $EC) then                      // mov ebp,esp
    begin
      if (Addr[3] = $83) and (Addr[4] = $C4) then                    // add esp,c8
      begin
        Result := Pointer(INT_PTR(StackPtr) - ShortInt(Addr[5]));
        Exit;
      end;
      Break;
    end;

    if (Addr[0] = $C2) and // ret $xxxx
         (((Addr[3] = $90) and (Addr[4] = $90) and (Addr[5] = $90)) or // nop
          ((Addr[3] = $CC) and (Addr[4] = $CC) and (Addr[5] = $CC))) then // int 3
      Break;

    if (Addr[0] = $C3) and // ret
         (((Addr[1] = $90) and (Addr[2] = $90) and (Addr[3] = $90)) or // nop
          ((Addr[1] = $CC) and (Addr[2] = $CC) and (Addr[3] = $CC))) then // int 3
      Break;

    if (Addr[0] = $E9) and // jmp rel-far
         (((Addr[5] = $90) and (Addr[6] = $90) and (Addr[7] = $90)) or // nop
          ((Addr[5] = $CC) and (Addr[6] = $CC) and (Addr[7] = $CC))) then // int 3
      Break;

    if (Addr[0] = $EB) and // jmp rel-near
         (((Addr[2] = $90) and (Addr[3] = $90) and (Addr[4] = $90)) or // nop
          ((Addr[2] = $CC) and (Addr[3] = $CC) and (Addr[4] = $CC))) then // int 3
      Break;

    Dec(DWORD_TR(Addr));
  end;}
  Result := StackPtr;
end;

function SystemTObjectInstance: TDebugAddr;
begin
  Result := ModuleFromAddr(Pointer(System.TObject));
end;

{ TGlobalModulesList }

constructor TGlobalModulesList.Create;
begin
  FLock := TDebugCriticalSection.Create;
end;

destructor TGlobalModulesList.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FModulesList);
  FreeAndNil(FAddedModules);
  inherited Destroy;
end;

procedure TGlobalModulesList.AddModule(const ModuleName: String);
var
  IsMultiThreaded: Boolean;
begin
  IsMultiThreaded := IsMultiThread;
  if IsMultiThreaded then
    FLock.Enter;
  try
    if not Assigned(FAddedModules) then begin
      FAddedModules := TStringList.Create;
      FAddedModules.Sorted := True;
      FAddedModules.Duplicates := dupIgnore;
    end;
    FAddedModules.Add(ModuleName);
  finally
    if IsMultiThreaded then
      FLock.Leave;
  end;
end;

function TGlobalModulesList.CreateModulesList: TDebugModuleInfoList;
var
  I: Integer;
  SystemModulesOnly: Boolean;
  IsMultiThreaded: Boolean;
  AddedModuleHandle: HMODULE;
begin
  IsMultiThreaded := IsMultiThread;
  if IsMultiThreaded then
    FLock.Enter;
  try
    if FModulesList = Nil then begin
      SystemModulesOnly := not (stAllModules in DebugStackTrackingOptions);
      Result := TDebugModuleInfoList.Create(False, SystemModulesOnly);
      // Add known Borland modules collected by DLL exception hooking code
      if SystemModulesOnly and DebugHookedExceptModulesList(FHookedModules) then begin
        for I := Low(FHookedModules) to High(FHookedModules) do
          Result.AddModule(FHookedModules[I], True);
      end;
      if Assigned(FAddedModules) then begin
        for I := 0 to FAddedModules.Count - 1 do begin
          AddedModuleHandle := GetModuleHandle(PChar(FAddedModules[I]));
          if (AddedModuleHandle <> 0) and not Assigned(Result.ModuleFromAddress[Pointer(AddedModuleHandle)]) then
            Result.AddModule(AddedModuleHandle, True);
        end;
      end;
      if stStaticModuleList in DebugStackTrackingOptions then
        FModulesList := Result;
    end
    else
      Result := FModulesList;
  finally
    if IsMultiThreaded then
      FLock.Leave;
  end;
end;

procedure TGlobalModulesList.FreeModulesList(var ModulesList: TDebugModuleInfoList);
var
  IsMultiThreaded: Boolean;
begin
  if FModulesList <> ModulesList then begin
    IsMultiThreaded := IsMultiThread;
    if IsMultiThreaded then
      FLock.Enter;
    try
      FreeAndNil(ModulesList);
    finally
      if IsMultiThreaded then
        FLock.Leave;
    end;
  end;
end;

function TGlobalModulesList.ValidateAddress(Addr: Pointer): Boolean;
var
  TempList: TDebugModuleInfoList;
begin
  TempList := CreateModulesList;
  try
    Result := TempList.IsValidModuleAddress(Addr);
  finally
    FreeModulesList(TempList);
  end;
end;

function ValidateModuleAddress(Addr: Pointer): Boolean;
begin
  Result := GlobalModulesList.ValidateAddress(Addr);
end;

{ TDebugModuleInfoList }

constructor TDebugModuleInfoList.Create(ADynamicBuild, ASystemModulesOnly: Boolean);
begin
  inherited Create(True);
  FDynamicBuild := ADynamicBuild;
  FSystemModulesOnly := ASystemModulesOnly;
  if not FDynamicBuild then
    BuildModulesList;
end;

function TDebugModuleInfoList.GetItems(Index: Integer): TDebugModuleInfo;
begin
  Result := TDebugModuleInfo(Get(Index));
end;

function TDebugModuleInfoList.GetModuleFromAddress(Addr: Pointer): TDebugModuleInfo;
var
  I: Integer;
  Item: TDebugModuleInfo;
begin
  Result := Nil;
  for I := 0 to Count - 1 do begin
    Item := Items[I];
    if (TDebugAddr(Item.StartAddr) <= TDebugAddr(Addr)) and (TDebugAddr(Item.EndAddr) > TDebugAddr(Addr)) then begin
      Result := Item;
      Break;
    end;
  end;
  if DynamicBuild and (Result = Nil) then
    Result := CreateItemForAddress(Addr, False);
end;

procedure TDebugModuleInfoList.BuildModulesList;
var
  List: TStringList;
  I: Integer;
  CurModule: PLibModule;
begin
  if FSystemModulesOnly then begin
    CurModule := LibModuleList;
    while CurModule <> Nil do begin
      CreateItemForAddress(Pointer(CurModule.Instance), True);
      CurModule := CurModule.Next;
    end;
  end
  else begin
    List := TStringList.Create;
    try
      LoadedModulesList(List, GetCurrentProcessId, True);
      for I := 0 to List.Count - 1 do
        CreateItemForAddress(List.Objects[I], False);
    finally
      List.Free;
    end;
  end;
  //Sort(SortByStartAddress);
end;

function TDebugModuleInfoList.CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TDebugModuleInfo;
var
  Module: HMODULE;
  ModuleSize: DWORD;
begin
  Result := Nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then begin
    ModuleSize := PeMapImgSize(Pointer(Module));
    if ModuleSize <> 0 then begin
      Result := TDebugModuleInfo.Create;
      Result.FSize := ModuleSize;
      Result.FStartAddr := Pointer(Module);
      Result.FEndAddr := Pointer(Module + ModuleSize - 1);
      if SystemModule then
        Result.FSystemModule := True
      else
        Result.FSystemModule := IsSystemModule(Module);
    end;
  end;
  if Result <> Nil then
    Add(Result);
end;

function TDebugModuleInfoList.AddModule(Module: HMODULE; SystemModule: Boolean): Boolean;
begin
  Result := not IsValidModuleAddress(Pointer(Module)) and (CreateItemForAddress(Pointer(Module), SystemModule) <> Nil);
end;

function TDebugModuleInfoList.IsSystemModuleAddress(Addr: Pointer): Boolean;
var
  Item: TDebugModuleInfo;
begin
  Item := ModuleFromAddress[Addr];
  Result := (Item <> Nil) and Item.SystemModule;
end;

function TDebugModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := ModuleFromAddress[Addr] <> Nil;
end;

{ TStackInfoItem }

function TStackInfoItem.GetCallerAddr: Pointer;
begin
  Result := Pointer(FStackInfo.CallerAddr);
end;

function TStackInfoItem.GetLogicalAddress: TDebugAddr;
begin
  Result := FStackInfo.CallerAddr - TDebugAddr(ModuleFromAddr(CallerAddr));
end;

{ TStackBaseList }

constructor TStackBaseList.Create;
begin
  inherited Create(True);
  FThreadID := GetCurrentThreadId;
  FTimeStamp := Now;
end;

destructor TStackBaseList.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited Destroy;
end;

{ TStackInfoList }

constructor TStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, False, Nil, Nil);
end;

constructor TStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, ADelayedTrace, Nil, Nil);
end;

constructor TStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack: Pointer);
begin
  Create(ARaw, AIgnoreLevels, AFirstCaller, ADelayedTrace, ABaseOfStack, Nil);
end;

constructor TStackInfoList.Create(ARaw: Boolean; AIgnoreLevels: DWORD; AFirstCaller: Pointer; ADelayedTrace: Boolean; ABaseOfStack, ATopOfStack: Pointer);
var
  Item: TStackInfoItem;
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  FDelayedTrace := ADelayedTrace;
  FRaw := ARaw;
  BaseOfStack := TDebugAddr(ABaseOfStack);
  FStackOffset := 0;
  FFramePointer := ABaseOfStack;

  if ATopOfStack = Nil then
    TopOfStack := GetStackTop
  else
    TopOfStack := TDebugAddr(ATopOfStack);

  FModuleInfoList := GlobalModulesList.CreateModulesList;
  if AFirstCaller <> Nil then begin
    Item := TStackInfoItem.Create;
    Item.FStackInfo.CallerAddr := TDebugAddr(AFirstCaller);
    Add(Item);
  end;
  if DelayedTrace then
    DelayStoreStack
  else if Raw then
    TraceStackRaw
  else
    TraceStackFrames;
end;

destructor TStackInfoList.Destroy;
begin
  if Assigned(FStackData) then
    FreeMem(FStackData);
  GlobalModulesList.FreeModulesList(FModuleInfoList);
  inherited Destroy;
end;

procedure TStackInfoList.ForceStackTracing;
begin
  if DelayedTrace and Assigned(FStackData) and not FInStackTracing then begin
    FInStackTracing := True;
    try
      if Raw then
        TraceStackRaw
      else
        TraceStackFrames;
      if FCorrectOnAccess then
        CorrectExceptStackListTop(Self, FSkipFirstItem);
    finally
      FInStackTracing := False;
      FDelayedTrace := False;
    end;
  end;
end;

function TStackInfoList.GetCount: Integer;
begin
  ForceStackTracing;
  Result := inherited Count;
end;

procedure TStackInfoList.CorrectOnAccess(ASkipFirstItem: Boolean);
begin
  FCorrectOnAccess := True;
  FSkipFirstItem := ASkipFirstItem;
end;

procedure TStackInfoList.AddToStrings(Strings: TStrings; IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset, IncludeVAddress: Boolean);
var
  I: Integer;
begin
  ForceStackTracing;
  Strings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Strings.Add(GetLocationInfoStr(Items[I].CallerAddr, IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset, IncludeVAddress));
  finally
    Strings.EndUpdate;
  end;
end;

function TStackInfoList.GetItems(Index: Integer): TStackInfoItem;
begin
  ForceStackTracing;
  Result := TStackInfoItem(Get(Index));
end;

function TStackInfoList.NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
var
  CallInstructionSize: Cardinal;
  StackFrameCallerFrame, NewFrame: TDebugAddr;
  StackFrameCallerAddr: TDebugAddr;
begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, the frame pointer and the return address on the stack
  // are valid addresses
  StackFrameCallerFrame := StackInfo.CallerFrame;
  while ValidStackAddr(TDebugAddr(StackFrame)) do begin
    // CallersEBP above the previous CallersEBP
    NewFrame := StackFrame^.CallerFrame;
    if NewFrame <= StackFrameCallerFrame then
      Break;
    StackFrameCallerFrame := NewFrame;

    // CallerAddr within current process space, code segment etc.
    // CallerFrame within current thread stack. Added Mar 12 2002 per Hallvard's suggestion
    StackFrameCallerAddr := StackFrame^.CallerAddr;
    if ValidCodeAddr(StackFrameCallerAddr, FModuleInfoList) and ValidStackAddr(StackFrameCallerFrame + FStackOffset) then begin
      Inc(StackInfo.Level);
      StackInfo.StackFrame := StackFrame;
      StackInfo.ParamPtr := PDWORD_PTRArray(TDebugAddr(StackFrame) + SizeOf(TStackFrame));

      if StackFrameCallerFrame > StackInfo.CallerFrame then
        StackInfo.CallerFrame := StackFrameCallerFrame
      else
        // the frame pointer points to an address that is below
        // the last frame pointer, so it must be invalid
        Break;

      // Calculate the address of caller by subtracting the CALL instruction size (if possible)
      if ValidCallSite(StackFrameCallerAddr, CallInstructionSize) then
        StackInfo.CallerAddr := StackFrameCallerAddr - CallInstructionSize
      else
        StackInfo.CallerAddr := StackFrameCallerAddr;
      // the stack may be messed up in big projects, avoid overflow in arithmetics
      if StackFrameCallerFrame < TDebugAddr(StackFrame) then
        Break;
      StackInfo.DumpSize := StackFrameCallerFrame - TDebugAddr(StackFrame);
      StackInfo.ParamSize := (StackInfo.DumpSize - SizeOf(TStackFrame)) div 4;
      if PStackFrame(StackFrame^.CallerFrame) = StackFrame then
        Break;
      // Step to the next stack frame by following the frame pointer
      StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
      Result := True;
      Exit;
    end;
    // Step to the next stack frame by following the frame pointer
    StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
  end;
  Result := False;
end;

procedure TStackInfoList.StoreToList(const StackInfo: TStackInfo);
var
  Item: TStackInfoItem;
begin
  if ((IgnoreLevels = Cardinal(-1)) and (StackInfo.Level > 0)) or (StackInfo.Level > (IgnoreLevels + 1)) then begin
    Item := TStackInfoItem.Create;
    Item.FStackInfo := StackInfo;
    Add(Item);
  end;
end;

procedure TStackInfoList.TraceStackFrames;
var
  StackFrame: PStackFrame;
  StackInfo: TStackInfo;
begin
  Capacity := 32; // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  // Start at level 0
  StackInfo.Level := 0;
  StackInfo.CallerFrame := 0;
  if DelayedTrace then
    // Get the current stack frame from the frame register
    StackFrame := FFramePointer
  else begin
    // We define the bottom of the valid stack to be the current ESP pointer
    if BaseOfStack = 0 then
      BaseOfStack := TDebugAddr(GetFramePointer);
    // Get a pointer to the current bottom of the stack
    StackFrame := PStackFrame(BaseOfStack);
  end;

  // We define the bottom of the valid stack to be the current frame Pointer
  // There is a TIB field called pvStackUserBase, but this includes more of the
  // stack than what would define valid stack frames.
  BaseOfStack := TDebugAddr(StackFrame) - 1;
  // Loop over and report all valid stackframes
  while NextStackFrame(StackFrame, StackInfo) and (inherited Count <> MaxStackTraceItems) do
    StoreToList(StackInfo);
end;

procedure TStackInfoList.TraceStackRaw;
var
  StackInfo: TStackInfo;
  StackPtr: PDebugAddr;
  PrevCaller: TDebugAddr;
  CallInstructionSize: Cardinal;
  StackTop: TDebugAddr;
begin
  Capacity := 32; // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  if DelayedTrace then begin
    if not Assigned(FStackData) then
      Exit;
    StackPtr := PDebugAddr(FStackData);
  end
  else begin
    // We define the bottom of the valid stack to be the current ESP pointer
    if BaseOfStack = 0 then
      BaseOfStack := TDebugAddr(GetStackPointer);
    // Get a pointer to the current bottom of the stack
    StackPtr := PDebugAddr(BaseOfStack);
  end;

  StackTop := TopOfStack;

  if Count > 0 then
    StackPtr := SearchForStackPtrManipulation(StackPtr, Pointer(Items[0].StackInfo.CallerAddr));

  // We will not be able to fill in all the fields in the StackInfo record,
  // so just blank it all out first
  ResetMemory(StackInfo, SizeOf(StackInfo));
  // Clear the previous call address
  PrevCaller := 0;
  // Loop through all of the valid stack space
  while (TDebugAddr(StackPtr) < StackTop) and (inherited Count <> MaxStackTraceItems) do begin
    // If the current DWORD on the stack refers to a valid call site...
    if ValidCallSite(StackPtr^, CallInstructionSize) and (StackPtr^ <> PrevCaller) then begin
      // then pick up the callers address
      StackInfo.CallerAddr := StackPtr^ - CallInstructionSize;
      // remember to callers address so that we don't report it repeatedly
      PrevCaller := StackPtr^;
      // increase the stack level
      Inc(StackInfo.Level);
      // then report it back to our caller
      StoreToList(StackInfo);
      StackPtr := SearchForStackPtrManipulation(StackPtr, Pointer(StackInfo.CallerAddr));
    end;
    // Look at the next DWORD on the stack
    Inc(StackPtr);
  end;

  if Assigned(FStackData) then begin
    FreeMem(FStackData);
    FStackData := Nil;
  end;
end;

procedure TStackInfoList.DelayStoreStack;
var
  StackPtr: PDebugAddr;
  StackDataSize: Cardinal;
begin
  if Assigned(FStackData) then begin
    FreeMem(FStackData);
    FStackData := Nil;
  end;
  // We define the bottom of the valid stack to be the current ESP pointer
  if BaseOfStack = 0 then begin
    BaseOfStack := TDebugAddr(GetStackPointer);
    FFramePointer := GetFramePointer;
  end;

  // Get a pointer to the current bottom of the stack
  StackPtr := PDebugAddr(BaseOfStack);
  if TDebugAddr(StackPtr) < TopOfStack then begin
    StackDataSize := TopOfStack - TDebugAddr(StackPtr);
    GetMem(FStackData, StackDataSize);
    System.Move(StackPtr^, FStackData^, StackDataSize);
    //CopyMemory(FStackData, StackPtr, StackDataSize);
  end;

  FStackOffset := Int64(FStackData) - Int64(StackPtr);
  FFramePointer := Pointer(TDebugAddr(FFramePointer) + FStackOffset);
  TopOfStack := TopOfStack + FStackOffset;
end;

function TStackInfoList.ValidCallSite(CodeAddr: TDebugAddr; out CallInstructionSize: Cardinal): Boolean;
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
  RM1, RM2, RM5: Byte;
begin
  // todo: 64 bit version

  // First check that the address is within range of our code segment!
  Result := CodeAddr > 8;
  if Result then begin
    C8P := PDWORD(CodeAddr - 8);
    C4P := PDWORD(CodeAddr - 4);
    Result := ValidCodeAddr(TDebugAddr(C8P), FModuleInfoList) and not IsBadReadPtr(C8P, 8);

    // Now check to see if the instruction preceding the return address
    // could be a valid CALL instruction
    if Result then begin
      try
        CodeDWORD8 := PDWORD(C8P)^;
        CodeDWORD4 := PDWORD(C4P)^;
        // CodeDWORD8 = (ReturnAddr-5):(ReturnAddr-6):(ReturnAddr-7):(ReturnAddr-8)
        // CodeDWORD4 = (ReturnAddr-1):(ReturnAddr-2):(ReturnAddr-3):(ReturnAddr-4)

        // ModR/M bytes contain the following bits:
        // Mod        = (76)
        // Reg/Opcode = (543)
        // R/M        = (210)
        RM1 := (CodeDWORD4 shr 24) and $7;
        RM2 := (CodeDWORD4 shr 16) and $7;
        //RM3 := (CodeDWORD4 shr 8)  and $7;
        //RM4 :=  CodeDWORD4         and $7;
        RM5 := (CodeDWORD8 shr 24) and $7;
        //RM6 := (CodeDWORD8 shr 16) and $7;
        //RM7 := (CodeDWORD8 shr 8)  and $7;

        // Check the instruction prior to the potential call site.
        // We consider it a valid call site if we find a CALL instruction there
        // Check the most common CALL variants first
        if ((CodeDWORD8 and $FF000000) = $E8000000) then
          // 5 bytes, "CALL NEAR REL32" (E8 cd)
          CallInstructionSize := 5
        else if ((CodeDWORD4 and $F8FF0000) = $10FF0000) and not (RM1 in [4, 5]) then
          // 2 bytes, "CALL NEAR [EAX]" (FF /2) where Reg = 010, Mod = 00, R/M <> 100 (1 extra byte)
          // and R/M <> 101 (4 extra bytes)
          CallInstructionSize := 2
        else if ((CodeDWORD4 and $F8FF0000) = $D0FF0000) then
          // 2 bytes, "CALL NEAR EAX" (FF /2) where Reg = 010 and Mod = 11
          CallInstructionSize := 2
        else if ((CodeDWORD4 and $00FFFF00) = $0014FF00) then
          // 3 bytes, "CALL NEAR [EAX+EAX*i]" (FF /2) where Reg = 010, Mod = 00 and RM = 100
          // SIB byte not validated
          CallInstructionSize := 3
        else if ((CodeDWORD4 and $00F8FF00) = $0050FF00) and (RM2 <> 4) then
          // 3 bytes, "CALL NEAR [EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM <> 100 (1 extra byte)
          CallInstructionSize := 3
        else if ((CodeDWORD4 and $0000FFFF) = $000054FF) then
          // 4 bytes, "CALL NEAR [EAX+EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM = 100
          // SIB byte not validated
          CallInstructionSize := 4
        else if ((CodeDWORD8 and $FFFF0000) = $15FF0000) then
          // 6 bytes, "CALL NEAR [$12345678]" (FF /2) where Reg = 010, Mod = 00 and RM = 101
          CallInstructionSize := 6
        else if ((CodeDWORD8 and $F8FF0000) = $90FF0000) and (RM5 <> 4) then
          // 6 bytes, "CALL NEAR [EAX+$12345678]" (FF /2) where Reg = 010, Mod = 10 and RM <> 100 (1 extra byte)
          CallInstructionSize := 6
        else if ((CodeDWORD8 and $00FFFF00) = $0094FF00) then
          // 7 bytes, "CALL NEAR [EAX+EAX+$1234567]" (FF /2) where Reg = 010, Mod = 10 and RM = 100
          CallInstructionSize := 7
        else if ((CodeDWORD8 and $0000FF00) = $00009A00) then
          // 7 bytes, "CALL FAR $1234:12345678" (9A ptr16:32)
          CallInstructionSize := 7
        else
          Result := False;
        // Because we're not doing a complete disassembly, we will potentially report
        // false positives. If there is odd code that uses the CALL 16:32 format, we
        // can also get false negatives.
      except
        Result := False;
      end;
    end;
  end;
end;

function TStackInfoList.ValidStackAddr(StackAddr: TDebugAddr): Boolean;
begin
  Result := (BaseOfStack < StackAddr) and (StackAddr < TopOfStack);
end;

function CreateStackList(Raw: Boolean; AIgnoreLevels: DWORD; FirstCaller: Pointer): TStackInfoList;
begin
  Result := TStackInfoList.Create(Raw, AIgnoreLevels, FirstCaller, False, Nil, Nil);
  GlobalStackList.AddObject(Result);
end;

{ TDebugCriticalSection }

constructor TDebugCriticalSection.Create;
begin
  inherited Create;
  Windows.InitializeCriticalSection(FCriticalSection);
end;

destructor TDebugCriticalSection.Destroy;
begin
  Windows.DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

class procedure TDebugCriticalSection.CreateAndEnter(var CS: TDebugCriticalSection);
var
  NewCritSect: TDebugCriticalSection;
begin
  NewCritSect := TDebugCriticalSection.Create;
  if LockedCompareExchange(Pointer(CS), Pointer(NewCritSect), Nil) <> Nil then begin
    // LoadInProgress was <> Nil -> no exchange took place, free the CS
    NewCritSect.Free;
  end;
  CS.Enter;
end;

procedure TDebugCriticalSection.Enter;
begin
  Windows.EnterCriticalSection(FCriticalSection);
end;

procedure TDebugCriticalSection.Leave;
begin
  Windows.LeaveCriticalSection(FCriticalSection);
end;

{ TFilterItem }

constructor TFilterItem.Create(const ExceptFilterProc: TExceptFilterProc; APriority: TDebugExceptNotifyPriority);
begin
  FExceptFilterProc := ExceptFilterProc;
  FPriority := APriority;
end;

function TFilterItem.DoFilterException(ExceptRecord: PExceptionRecord; out ExceptObj: Exception): Boolean;
begin
  if Assigned(FExceptFilterProc) then begin
    ExceptObj := FExceptFilterProc(ExceptRecord);
    Result := ExceptObj <> Nil;
  end
  else
    Result := False;
end;

{ TNotifierItem }

constructor TNotifierItem.Create(const NotifyProc: TExceptNotifyProc; Priority: TDebugExceptNotifyPriority);
begin
  inherited Create;
  FNotifyProc := NotifyProc;
  FPriority := Priority;
end;

constructor TNotifierItem.Create(const NotifyProc: TExceptNotifyProcEx; Priority: TDebugExceptNotifyPriority);
begin
  inherited Create;
  FNotifyProcEx := NotifyProc;
  FPriority := Priority;
end;

constructor TNotifierItem.Create(const NotifyMethod: TExceptNotifyMethod; Priority: TDebugExceptNotifyPriority);
begin
  inherited Create;
  FNotifyMethod := NotifyMethod;
  FPriority := Priority;
end;

procedure TNotifierItem.DoNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean; StackPointer: Pointer);
begin
  if Assigned(FNotifyProc) then
    FNotifyProc(ExceptObj, ExceptAddr, OSException)
  else if Assigned(FNotifyProcEx) then
    FNotifyProcEx(ExceptObj, ExceptAddr, OSException, StackPointer)
  else if Assigned(FNotifyMethod) then
    FNotifyMethod(ExceptObj, ExceptAddr, OSException);
end;

{ TPeMapImgHookItem }

constructor TPeMapImgHookItem.Create(AList: TObjectList; const AFunctionName, AModuleName: String; ABaseAddress, ANewAddress, AOriginalAddress: Pointer);
begin
  inherited Create;
  FList := AList;
  FFunctionName := AFunctionName;
  FModuleName := AModuleName;
  FBaseAddress := ABaseAddress;
  FNewAddress := ANewAddress;
  FOriginalAddress := AOriginalAddress;
end;

destructor TPeMapImgHookItem.Destroy;
begin
  if FBaseAddress <> Nil then
    InternalUnhook;
  inherited Destroy;
end;

function TPeMapImgHookItem.InternalUnhook: Boolean;
var
  Buf: TMemoryBasicInformation;
begin
  Buf.AllocationBase := Nil;
  if (VirtualQuery(FBaseAddress, Buf, SizeOf(Buf)) = SizeOf(Buf)) and (Buf.State and MEM_FREE = 0) then
    Result := TPeMapImgHooks.ReplaceImport(FBaseAddress, ModuleName, NewAddress, OriginalAddress)
  else
    Result := True; // PE image is not available anymore (DLL got unloaded)
  if Result then
    FBaseAddress := Nil;
end;

function TPeMapImgHookItem.Unhook: Boolean;
begin
  Result := InternalUnhook;
  if Result then
    FList.Remove(Self);
end;

{ TPeMapImgHooks }

type
  PWin9xDebugThunk32 = ^TWin9xDebugThunk32;
  TWin9xDebugThunk32 = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: DWORD; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: DWORD;  // Relative displacement (a Kernel32 address)
  end;

  PIMAGE_THUNK_DATA32 = ^IMAGE_THUNK_DATA32;
  {$EXTERNALSYM PIMAGE_THUNK_DATA32}
  _IMAGE_THUNK_DATA32 = record
    case Integer of
      0: (ForwarderString: DWORD);   // PBYTE
      1: (Function_: DWORD);         // PDWORD
      2: (Ordinal: DWORD);
      3: (AddressOfData: DWORD);     // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM _IMAGE_THUNK_DATA32}
  IMAGE_THUNK_DATA32 = _IMAGE_THUNK_DATA32;
  {$EXTERNALSYM IMAGE_THUNK_DATA32}
  TImageThunkData32 = IMAGE_THUNK_DATA32;
  PImageThunkData32 = PIMAGE_THUNK_DATA32;

const
  SurrogateHighStart = UCS4($D800);
  SurrogateHighEnd = UCS4($DBFF);
  SurrogateLowStart = UCS4($DC00);
  SurrogateLowEnd = UCS4($DFFF);

  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

  UCS4ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

procedure FlagInvalidSequence(var StrPos: SizeInt; Increment: SizeInt; out Ch: UCS4); overload;
begin
  Ch := UCS4ReplacementCharacter;
  Inc(StrPos, Increment);
end;

function UTF16GetNextChar(const S: TUTF16String; var StrPos: SizeInt): UCS4;
var
  StrLength: SizeInt;
  Ch: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd: begin
        // 2 bytes to read
        if StrPos < StrLength then begin
          Ch := UCS4(S[StrPos + 1]);
          if (Ch >= SurrogateLowStart) and (Ch <= SurrogateLowEnd) then begin
            Result := ((Result - SurrogateHighStart) shl HalfShift) +  (Ch - SurrogateLowStart) + HalfBase;
            Inc(StrPos, 2);
          end
          else
            FlagInvalidSequence(StrPos, 1, Result);
        end
        else
          FlagInvalidSequence(StrPos, 1, Result);
      end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 String:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags String being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: SizeInt; Ch: UCS4): Boolean;
var
  StrLength: SizeInt;
begin
  StrLength := Length(S);

  if Ch <= $7F then begin
    // 7 bits to store
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then begin
      S[StrPos] := AnsiChar(Ch);
      Inc(StrPos);
    end;
  end
  else if Ch <= $7FF then begin
    // 11 bits to store
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then begin
      S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
      S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 2);
    end;
  end
  else if Ch <= $FFFF then begin
    // 16 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then begin
      S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end
  else if Ch <= $1FFFFF then begin
    // 21 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then begin
      S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 4);
    end;
  end
  else if Ch <= $3FFFFFF then begin
    // 26 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then begin
      S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 5);
    end;
  end
  else if Ch <= MaximumUCS4 then begin
    // 31 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 3));
    if Result then begin
      S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 6);
    end;
  end
  else begin
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then begin
      S[StrPos] := AnsiChar($E0 or (UCS4ReplacementCharacter shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((UCS4ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((UCS4ReplacementCharacter and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end;
end;

function TryUTF16ToUTF8(const S: TUTF16String; out D: TUTF8String): Boolean;
var
  SrcIndex, SrcLength, DestIndex: SizeInt;
  Ch: UCS4;
begin
  Result := True;
  if Length(S) = 0 then
    D := ''
  else begin
    SrcLength := Length(S);
    SetLength(D, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while (SrcIndex > 0) and (SrcIndex <= SrcLength) do begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex > 0 then
        UTF8SetNextChar(D, DestIndex, Ch)
      else
        Result := False;
    end;
    if Result then
      SetLength(D, DestIndex - 1) // now fix up length
    else
      D := '';
  end;
end;

function TryStringToUTF8(const S: String; out D: TUTF8String): Boolean;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := TryUTF16ToUTF8(WS, D);
end;

function TPeMapImgHooks.GetItemFromNewAddress(NewAddress: Pointer): TPeMapImgHookItem;
var
  I: Integer;
begin
  Result := Nil;
  for I := 0 to Count - 1 do begin
    if Items[I].NewAddress = NewAddress then begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TPeMapImgHooks.GetItemFromOriginalAddress(OriginalAddress: Pointer): TPeMapImgHookItem;
var
  I: Integer;
begin
  Result := Nil;
  for I := 0 to Count - 1 do begin
    if Items[I].OriginalAddress = OriginalAddress then begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TPeMapImgHooks.GetItems(Index: Integer): TPeMapImgHookItem;
begin
  Result := TPeMapImgHookItem(Get(Index));
end;

function TPeMapImgHooks.HookImport(Base: Pointer; const ModuleName, FunctionName: String; NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
var
  ModuleHandle: THandle;
  OriginalItem: TPeMapImgHookItem;
  UTF8Name: TUTF8String;
begin
  ModuleHandle := GetModuleHandle(PChar(ModuleName));
  Result := (ModuleHandle <> 0);
  if not Result then begin
    SetLastError(ERROR_MOD_NOT_FOUND);
    Exit;
  end;
  if not TryStringToUTF8(FunctionName, UTF8Name) then
    UTF8Name := TUTF8String(FunctionName);
  OriginalAddress := GetProcAddress(ModuleHandle, PAnsiChar(UTF8Name));
  Result := (OriginalAddress <> Nil);
  if not Result then begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;
  OriginalItem := ItemFromOriginalAddress[OriginalAddress];
  Result := ((OriginalItem = Nil) or (OriginalItem.ModuleName = ModuleName)) and (NewAddress <> Nil) and (OriginalAddress <> NewAddress);
  if not Result then begin
    SetLastError(ERROR_ALREADY_EXISTS);
    Exit;
  end;
  if Result then
    Result := ReplaceImport(Base, ModuleName, OriginalAddress, NewAddress);
  if Result then begin
    Add(TPeMapImgHookItem.Create(Self, FunctionName, ModuleName, Base, NewAddress, OriginalAddress));
  end
  else
    SetLastError(ERROR_INVALID_PARAMETER);
end;

class function TPeMapImgHooks.IsWin9xDebugThunk(P: Pointer): Boolean;
begin
  with PWin9xDebugThunk32(P)^ do
    Result := (PUSH = $68) and (JMP = $E9);
end;

class function TPeMapImgHooks.ReplaceImport(Base: Pointer; const ModuleName: String; FromProc, ToProc: Pointer): Boolean;
var
  FromProcDebugThunk32, ImportThunk32: PWin9xDebugThunk32;
  IsThunked: Boolean;
  NtHeader32: PImageNtHeaders32;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName, RefName: PAnsiChar;
  ImportEntry32: PImageThunkData32;
  FoundProc: Boolean;
  WrittenBytes: Cardinal;
  UTF8Name: TUTF8String;
begin
  Result := False;
  FromProcDebugThunk32 := PWin9xDebugThunk32(FromProc);
  IsThunked := not IsWinNT and IsWin9xDebugThunk(FromProcDebugThunk32);
  NtHeader32 := PeMapImgNtHeaders32(Base);
  if NtHeader32 = Nil then
    Exit;
  ImportDir := NtHeader32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  ImportDesc := PImageImportDescriptor(TDebugAddr(Base) + ImportDir.VirtualAddress);
  if not TryStringToUTF8(ModuleName, UTF8Name) then
    UTF8Name := TUTF8String(ModuleName);
  RefName := PAnsiChar(UTF8Name);
  while (ImportDesc <> Nil) and (ImportDesc^.Name <> 0) do begin
    CurrName := PAnsiChar(Base) + ImportDesc^.Name;
    if StrIComp(CurrName, RefName) = 0 then begin
      ImportEntry32 := PImageThunkData32(TDebugAddr(Base) + ImportDesc^.FirstThunk);
      while ImportEntry32^.Function_ <> 0 do begin
        if IsThunked then begin
          ImportThunk32 := PWin9xDebugThunk32(ImportEntry32^.Function_);
          FoundProc := IsWin9xDebugThunk(ImportThunk32) and (ImportThunk32^.Addr = FromProcDebugThunk32^.Addr);
        end
        else
          FoundProc := Pointer(ImportEntry32^.Function_) = FromProc;
        if FoundProc then
          Result := WriteProtectedMemory(@ImportEntry32^.Function_, @ToProc, SizeOf(ToProc), WrittenBytes);
        Inc(ImportEntry32);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

class function TPeMapImgHooks.SystemBase: Pointer;
begin
  Result := Pointer(SystemTObjectInstance);
end;

procedure TPeMapImgHooks.UnhookAll;
var
  I: Integer;
begin
  I := 0;
  while I < Count do begin
    if not Items[I].Unhook then
      Inc(I);
  end;
end;

function TPeMapImgHooks.UnhookByNewAddress(NewAddress: Pointer): Boolean;
var
  Item: TPeMapImgHookItem;
begin
  Item := ItemFromNewAddress[NewAddress];
  Result := (Item <> Nil) and Item.Unhook;
end;

procedure TPeMapImgHooks.UnhookByBaseAddress(BaseAddress: Pointer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
    if Items[I].BaseAddress = BaseAddress then
      Items[I].Unhook;
  end;
end;

{ TDebugInfoSource }

constructor TDebugInfoSource.Create(AModule: HMODULE);
begin
  FModule := AModule;
end;

function TDebugInfoSource.GetFileName: TFileName;
begin
  Result := GetModulePath(FModule);
end;

function TDebugInfoSource.VAFromAddr(const Addr: Pointer): DWORD;
begin
  Result := DWORD(TDebugAddr(Addr) - FModule - ModuleCodeOffset);
end;

{ TDebugInfoList }

function TDebugInfoList.CreateDebugInfo(const Module: HMODULE): TDebugInfoSource;
var
  I: Integer;
begin
  NeedInfoSourceClassList;

  Result := Nil;
  for I := 0 to InfoSourceClassList.Count - 1 do begin
    Result := TDebugInfoSourceClass(InfoSourceClassList.Items[I]).Create(Module);
    try
      if Result.InitializeSource then
        Break
      else
        FreeAndNil(Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TDebugInfoList.GetItemFromModule(const Module: HMODULE): TDebugInfoSource;
var
  I: Integer;
  TempItem: TDebugInfoSource;
begin
  Result := Nil;
  if Module = 0 then
    Exit;

  for I := 0 to Count - 1 do begin
    TempItem := Items[I];
    if TempItem.Module = Module then begin
      Result := TempItem;
      Break;
    end;
  end;

  if Result = Nil then begin
    Result := CreateDebugInfo(Module);
    if Result <> Nil then
      Add(Result);
  end;
end;

function TDebugInfoList.GetItems(Index: Integer): TDebugInfoSource;
begin
  Result := TDebugInfoSource(Get(Index));
end;

function TDebugInfoList.GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean;
var
  Item: TDebugInfoSource;
begin
  ResetMemory(Info, SizeOf(Info));
  Item := ItemFromModule[ModuleFromAddr(Addr)];
  if Item <> Nil then
    Result := Item.GetLocationInfo(Addr, Info)
  else
    Result := False;
end;

class procedure TDebugInfoList.NeedInfoSourceClassList;
begin
  if not Assigned(InfoSourceClassList) then begin
    InfoSourceClassList := TList.Create;
//    {$IFNDEF DEBUG_NO_BINARY}
//    InfoSourceClassList.Add(Pointer(TDebugInfoBinary));
//    {$ENDIF !DEBUG_NO_BINARY}
//    {$IFNDEF DEBUG_NO_TD32}
//    InfoSourceClassList.Add(Pointer(TDebugInfoTD32));
//    {$ENDIF !DEBUG_NO_TD32}
    {$IFNDEF DEBUG_NO_MAP}
    InfoSourceClassList.Add(Pointer(TDebugInfoMap));
    {$ENDIF !DEBUG_NO_MAP}
    {$IFNDEF DEBUG_NO_SYMBOLS}
    InfoSourceClassList.Add(Pointer(TDebugInfoSymbols));
    {$ENDIF !DEBUG_NO_SYMBOLS}
//    {$IFNDEF DEBUG_NO_EXPORTS}
//    InfoSourceClassList.Add(Pointer(TDebugInfoExports));
//    {$ENDIF !DEBUG_NO_EXPORTS}
  end;
end;

class procedure TDebugInfoList.RegisterDebugInfoSource(const InfoSourceClass: TDebugInfoSourceClass);
begin
  NeedInfoSourceClassList;

  InfoSourceClassList.Add(Pointer(InfoSourceClass));
end;

class procedure TDebugInfoList.RegisterDebugInfoSourceFirst(const InfoSourceClass: TDebugInfoSourceClass);
begin
  NeedInfoSourceClassList;

  InfoSourceClassList.Insert(0, Pointer(InfoSourceClass));
end;

class procedure TDebugInfoList.UnRegisterDebugInfoSource(const InfoSourceClass: TDebugInfoSourceClass);
begin
  if Assigned(InfoSourceClassList) then
    InfoSourceClassList.Remove(Pointer(InfoSourceClass));
end;

{ TFileMappingStream }

constructor TFileMappingStream.Create(const FileName: String; FileMode: Word);
var
  Protect, Access, Size: DWORD;
  BaseAddress: Pointer;
begin
  inherited Create;
  FFileHandle := THandle(FileOpen(FileName, FileMode));
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  if (FileMode and $0F) = fmOpenReadWrite then begin
    Protect := PAGE_WRITECOPY;
    Access := FILE_MAP_COPY;
  end
  else begin
    Protect := PAGE_READONLY;
    Access := FILE_MAP_READ;
  end;
  FMapping := CreateFileMapping(FFileHandle, Nil, Protect, 0, 0, Nil);
  if FMapping = 0 then begin
    Close;
    raise EFileMappingError.CreateResFmt(@resCreateFileMapping, [FileName]);
  end;
  BaseAddress := MapViewOfFile(FMapping, Access, 0, 0, 0);
  if BaseAddress = Nil then begin
    Close;
    raise EFileMappingViewError.CreateResFmt(@resCreateFileMappingView, [FileName]);
  end;
  Size := GetFileSize(FFileHandle, Nil);
  if Size = DWORD(-1) then begin
    UnMapViewOfFile(BaseAddress);
    Close;
    raise EFileMappingViewError.CreateResFmt(@resFailedToObtainSize, [FileName]);
  end;
  SetPointer(BaseAddress, Size);
end;

destructor TFileMappingStream.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TFileMappingStream.Close;
begin
  if Memory <> Nil then begin
    UnMapViewOfFile(Memory);
    SetPointer(Nil, 0);
  end;
  if FMapping <> 0 then begin
    CloseHandle(FMapping);
    FMapping := 0;
  end;
  if FFileHandle <> INVALID_HANDLE_VALUE then begin
    FileClose(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TFileMappingStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then begin
    System.Move(Buffer, Pointer(TDebugAddr(Memory) + TDebugAddr(Position))^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;

{ TAbstractDebugMapParser }

function CharIsReturn(const C: Char): Boolean; inline;
begin
  Result := (C = NativeLineFeed) or (C = NativeCarriageReturn);
end;

function CharIsWhiteSpace(const C: Char): Boolean; inline;
begin
  case C of
    NativeTab,
    NativeLineFeed,
    NativeVerticalTab,
    NativeFormFeed,
    NativeCarriageReturn,
    NativeSpace:
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsDigit(const C: Char): Boolean; inline;
begin
  Result := Character.IsDigit(C);
end;

constructor TAbstractDebugMapParser.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create;
  FModule := Module;
  if FileExists(MapFileName) then
    FStream := TFileMappingStream.Create(MapFileName, fmOpenRead or fmShareDenyWrite);
end;

constructor TAbstractDebugMapParser.Create(const MapFileName: TFileName);
begin
  Create(MapFileName, 0);
end;

destructor TAbstractDebugMapParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TAbstractDebugMapParser.GetLinkerBugUnitName: String;
begin
  Result := MapStringToStr(FLinkerBugUnitName);
end;

class function TAbstractDebugMapParser.MapStringToFileName(MapString: PDebugMapString): String;
var
  PEnd: PDebugMapString;
begin
  if MapString = Nil then begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> '=') and not CharIsReturn(Char(PEnd^)) do
    Inc(PEnd);
  if (PEnd^ = '=') then begin
    while (PEnd >= MapString) and not (PEnd^ = NativeSpace) do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd - 1)^ = NativeSpace) do
      Dec(PEnd);
  end;
  SetString(Result, MapString, PEnd - MapString);
end;

class function TAbstractDebugMapParser.MapStringToModuleName(MapString: PDebugMapString): String;
var
  PStart, PEnd, PExtension: PDebugMapString;
begin
  if MapString = Nil then begin
    Result := '';
    Exit;
  end;
  PEnd := MapString;
  while (PEnd^ <> '=') and not CharIsReturn(Char(PEnd^)) do
    Inc(PEnd);
  if (PEnd^ = '=') then begin
    while (PEnd >= MapString) and not (PEnd^ = NativeSpace) do
      Dec(PEnd);
    while (PEnd >= MapString) and ((PEnd - 1)^ = NativeSpace) do
      Dec(PEnd);
  end;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '.') and (PExtension^ <> '|') do
    Dec(PExtension);
  if (StrLIComp(PExtension, '.pas ', 5) = 0) or (StrLIComp(PExtension, '.obj ', 5) = 0) then
    PEnd := PExtension;
  PExtension := PEnd;
  while (PExtension >= MapString) and (PExtension^ <> '|') and (PExtension^ <> '\') do
    Dec(PExtension);
  if PExtension >= MapString then
    PStart := PExtension + 1
  else
    PStart := MapString;
  SetString(Result, PStart, PEnd - PStart);
end;

class function TAbstractDebugMapParser.MapStringToStr(MapString: PDebugMapString; IgnoreSpaces: Boolean): String;
var
  P: PDebugMapString;
begin
  if MapString = Nil then begin
    Result := '';
    Exit;
  end;
  if MapString^ = '(' then begin
    Inc(MapString);
    P := MapString;
    while (P^ <> ')') and not CharIsReturn(Char(P^)) do
      Inc(P);
  end
  else begin
    P := MapString;
    if IgnoreSpaces then
      while (P^ <> '(') and not CharIsReturn(Char(P^)) do
        Inc(P)
    else
      while (P^ <> '(') and not CharIsWhiteSpace(Char(P^)) do
        Inc(P);
  end;
  SetString(Result, MapString, P - MapString);
end;

procedure TAbstractDebugMapParser.Parse;
const
  TableHeader          : array [0..3] of String = ('Start', 'Length', 'Name', 'Class');
  SegmentsHeader       : array [0..3] of String = ('Detailed', 'map', 'of', 'segments');
  PublicsByNameHeader  : array [0..3] of String = ('Address', 'Publics', 'by', 'Name');
  PublicsByValueHeader : array [0..3] of String = ('Address', 'Publics', 'by', 'Value');
  LineNumbersPrefix    : String = 'Line numbers for';
var
  CurrPos, EndPos: PDebugMapString;
  A: TDebugMapAddress;
  L: Integer;
  P1, P2: PDebugMapString;

  function Eof: Boolean;
  begin
    Result := (CurrPos >= EndPos);
  end;

  procedure SkipWhiteSpace;
  begin
    while not Eof and CharIsWhiteSpace(Char(CurrPos^)) do
      Inc(CurrPos);
  end;

  procedure SkipEndLine;
  begin
    while not Eof and not CharIsReturn(Char(CurrPos^)) do
      Inc(CurrPos);
    SkipWhiteSpace;
  end;

  function IsDecDigit: Boolean;
  begin
    Result := CharIsDigit(Char(CurrPos^));
  end;

  function ReadTextLine: String;
  var
    P: PDebugMapString;
  begin
    P := CurrPos;
    while (CurrPos^ <> NativeNull) and not CharIsReturn(Char(CurrPos^)) do
      Inc(CurrPos);
    SetString(Result, P, CurrPos - P);
  end;


  function ReadDecValue: Integer;
  begin
    Result := 0;
    while CharIsDigit(Char(CurrPos^)) do begin
      Result := Result * 10 + (Ord(CurrPos^) - Ord('0'));
      Inc(CurrPos);
    end;
  end;

  function ReadHexValue: DWORD;
  var
    C: Char;
  begin
    Result := 0;
    repeat
      C := Char(CurrPos^);
      case C of
        '0'..'9': Result := (Result shl 4) or DWORD(Ord(C) - Ord('0'));
        'A'..'F': Result := (Result shl 4) or DWORD(Ord(C) - Ord('A') + 10);
        'a'..'f': Result := (Result shl 4) or DWORD(Ord(C) - Ord('a') + 10);
        'H', 'h': begin
          Inc(CurrPos);
          Break;
        end;
      else
        Break;
      end;
      Inc(CurrPos);
    until False;
  end;

  function ReadAddress: TDebugMapAddress;
  begin
    Result.Segment := ReadHexValue;
    if CurrPos^ = ':' then begin
      Inc(CurrPos);
      Result.Offset := ReadHexValue;
    end
    else
      Result.Offset := 0;
  end;

  function ReadString: PDebugMapString;
  begin
    SkipWhiteSpace;
    Result := CurrPos;
    while not CharIsWhiteSpace(Char(CurrPos^)) do
      Inc(CurrPos);
  end;

  procedure FindParam(Param: AnsiChar);
  begin
    while not ((CurrPos^ = Param) and ((CurrPos + 1)^ = '=')) do
      Inc(CurrPos);
    Inc(CurrPos, 2);
  end;

  function SyncToHeader(const Header: Array of String): Boolean;
  var
    S: String;
    TokenIndex, OldPosition, CurrentPosition: Integer;
  begin
    Result := False;
    while not Eof do begin
      S := Trim(ReadTextLine);
      TokenIndex := Low(Header);
      CurrentPosition := 0;
      OldPosition := 0;
      while (TokenIndex <= High(Header)) do begin
        CurrentPosition := Pos(Header[TokenIndex],S);
        if (CurrentPosition <= OldPosition) then begin
          CurrentPosition := 0;
          Break;
        end;
        OldPosition := CurrentPosition;
        Inc(TokenIndex);
      end;
      Result := CurrentPosition <> 0;
      if Result then
        Break;
      SkipEndLine;
    end;
    if not Eof then
      SkipWhiteSpace;
  end;

  function SyncToPrefix(const Prefix: String): Boolean;
  var
    I: Integer;
    P: PDebugMapString;
    S: String;
  begin
    if Eof then begin
      Result := False;
      Exit;
    end;
    SkipWhiteSpace;
    I := Length(Prefix);
    P := CurrPos;
    while not Eof and (P^ <> NativeCarriageReturn) and (P^ <> NativeNull) and (I > 0) do begin
      Inc(P);
      Dec(I);
    end;
    SetString(S, CurrPos, Length(Prefix));
    Result := (S = Prefix);
    if Result then
      CurrPos := P;
    SkipWhiteSpace;
  end;

begin
  if FStream <> Nil then begin
    FLinkerBug := False;
    CurrPos := FStream.Memory;
    EndPos := CurrPos + FStream.Size;
    if SyncToHeader(TableHeader) then
      while IsDecDigit do begin
        A := ReadAddress;
        SkipWhiteSpace;
        L := ReadHexValue;
        P1 := ReadString;
        P2 := ReadString;
        SkipEndLine;
        ClassTableItem(A, L, P1, P2);
      end;
    if SyncToHeader(SegmentsHeader) then
      while IsDecDigit do begin
        A := ReadAddress;
        SkipWhiteSpace;
        L := ReadHexValue;
        FindParam('C');
        P1 := ReadString;
        FindParam('M');
        P2 := ReadString;
        SkipEndLine;
        SegmentItem(A, L, P1, P2);
      end;
    if SyncToHeader(PublicsByNameHeader) then
      while IsDecDigit do begin
        A := ReadAddress;
        P1 := ReadString;
        SkipEndLine; // compatibility with C++Builder MAP files
        PublicsByNameItem(A, P1);
      end;
    if SyncToHeader(PublicsByValueHeader) then
      while not Eof and IsDecDigit do begin
        A := ReadAddress;
        P1 := ReadString;
        SkipEndLine; // compatibility with C++Builder MAP files
        PublicsByValueItem(A, P1);
      end;
    while SyncToPrefix(LineNumbersPrefix) do begin
      FLastUnitName := CurrPos;
      FLastUnitFileName := CurrPos;
      while FLastUnitFileName^ <> '(' do
        Inc(FLastUnitFileName);
      SkipEndLine;
      LineNumberUnitItem(FLastUnitName, FLastUnitFileName);
      repeat
        SkipWhiteSpace;
        L := ReadDecValue;
        SkipWhiteSpace;
        A := ReadAddress;
        SkipWhiteSpace;
        LineNumbersItem(L, A);
      until not IsDecDigit;
    end;
  end;
end;

{ TDebugMapScanner }

function PeMapImgSections32(NtHeaders: PImageNtHeaders32): PImageSectionHeader;
begin
  if NtHeaders = Nil then
    Result := Nil
  else
    Result := PImageSectionHeader(TDebugAddr(@NtHeaders^.OptionalHeader) + NtHeaders^.FileHeader.SizeOfOptionalHeader);
end;

function PeMapImgSections64(NtHeaders: PImageNtHeaders64): PImageSectionHeader;
begin
  if NtHeaders = Nil then
    Result := Nil
  else
    Result := PImageSectionHeader(TDebugAddr(@NtHeaders^.OptionalHeader) + NtHeaders^.FileHeader.SizeOfOptionalHeader);
end;

function PeMapImgFindSection32(NtHeaders: PImageNtHeaders32; const SectionName: String): PImageSectionHeader;
var
  Header: PImageSectionHeader;
  I: Integer;
  P: PAnsiChar;
  UTF8Name: TUTF8String;
begin
  Result := Nil;
  if NtHeaders <> Nil then begin
    if not TryStringToUTF8(SectionName, UTF8Name) then
      UTF8Name := TUTF8String(SectionName);
    P := PAnsiChar(UTF8Name);
    Header := PeMapImgSections32(NtHeaders);
    for I := 1 to NtHeaders^.FileHeader.NumberOfSections do
      if StrLComp(PAnsiChar(@Header^.Name), P, IMAGE_SIZEOF_SHORT_NAME) = 0 then begin
        Result := Header;
        Break;
      end
      else
        Inc(Header);
  end;
end;

function PeMapImgFindSection64(NtHeaders: PImageNtHeaders64; const SectionName: String): PImageSectionHeader;
var
  Header: PImageSectionHeader;
  I: Integer;
  P: PAnsiChar;
  UTF8Name: TUTF8String;
begin
  Result := Nil;
  if NtHeaders <> Nil then begin
    if not TryStringToUTF8(SectionName, UTF8Name) then
      UTF8Name := TUTF8String(SectionName);
    P := PAnsiChar(UTF8Name);
    Header := PeMapImgSections64(NtHeaders);
    for I := 1 to NtHeaders^.FileHeader.NumberOfSections do
      if StrLComp(PAnsiChar(@Header^.Name), P, IMAGE_SIZEOF_SHORT_NAME) = 0 then begin
        Result := Header;
        Break;
      end
      else
        Inc(Header);
  end;
end;

function PeMapImgFindSectionFromModule(const BaseAddress: Pointer; const SectionName: String): PImageSectionHeader;

  function PeMapImgFindSectionFromModule32(const BaseAddress: Pointer; const SectionName: String): PImageSectionHeader;
  var
    NtHeaders32: PImageNtHeaders32;
  begin
    Result := Nil;
    NtHeaders32 := PeMapImgNtHeaders32(BaseAddress);
    if Assigned(NtHeaders32) then
      Result := PeMapImgFindSection32(NtHeaders32, SectionName);
  end;

  function PeMapImgFindSectionFromModule64(const BaseAddress: Pointer; const SectionName: String): PImageSectionHeader;
  var
    NtHeaders64: PImageNtHeaders64;
  begin
    Result := Nil;
    NtHeaders64 := PeMapImgNtHeaders64(BaseAddress);
    if Assigned(NtHeaders64) then
      Result := PeMapImgFindSection64(NtHeaders64, SectionName);
  end;

begin
  case PeMapImgTarget(BaseAddress) of
    taWin32: Result := PeMapImgFindSectionFromModule32(BaseAddress, SectionName);
    taWin64: Result := PeMapImgFindSectionFromModule64(BaseAddress, SectionName);
  else //taUnknown:
    Result := Nil;
  end;
end;

constructor TDebugMapScanner.Create(const MapFileName: TFileName; Module: HMODULE);
begin
  inherited Create(MapFileName, Module);
  Scan;
end;

function TDebugMapScanner.MAPAddrToVA(const Addr: DWORD): DWORD;
begin
  // MAP file format was changed in Delphi 2005
  // before Delphi 2005: segments started at offset 0
  //                     only one segment of code
  // after Delphi 2005: segments started at code base address (module base address + $10000)
  //                    2 segments of code
  if (Length(FSegmentClasses) > 0) and (FSegmentClasses[0].Start > 0) and (Addr >= FSegmentClasses[0].Start) then
    // Delphi 2005 and later
    // The first segment should be code starting at module base address + $10000
    Result := Addr - FSegmentClasses[0].Start
  else
    // before Delphi 2005
    Result := Addr;
end;

class function TDebugMapScanner.MapStringCacheToFileName(var MapString: TDebugMapStringCache): String;
begin
  Result := MapString.CachedValue;
  if Length(Result) = 0 then begin
    Result := MapStringToFileName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TDebugMapScanner.MapStringCacheToModuleName(var MapString: TDebugMapStringCache): String;
begin
  Result := MapString.CachedValue;
  if Length(Result) = 0 then begin
    Result := MapStringToModuleName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TDebugMapScanner.MapStringCacheToStr(var MapString: TDebugMapStringCache; IgnoreSpaces: Boolean): String;
begin
  Result := MapString.CachedValue;
  if Length(Result) = 0 then begin
    Result := MapStringToStr(MapString.RawValue, IgnoreSpaces);
    MapString.CachedValue := Result;
  end;
end;

procedure TDebugMapScanner.ClassTableItem(const Address: TDebugMapAddress; Len: Integer; SectionName, GroupName: PDebugMapString);
var
  C: Integer;
  SectionHeader: PImageSectionHeader;
begin
  C := Length(FSegmentClasses);
  SetLength(FSegmentClasses, C + 1);
  FSegmentClasses[C].Segment := Address.Segment;
  FSegmentClasses[C].Start := Address.Offset;
  FSegmentClasses[C].Addr := Address.Offset; // will be fixed below while considering module mapped address
  // test GroupName because SectionName = '.tls' in Delphi and '_tls' in BCB
  if StrLIComp(GroupName, 'TLS', 3) = 0 then
    FSegmentClasses[C].VA := FSegmentClasses[C].Start
  else
    FSegmentClasses[C].VA := MAPAddrToVA(FSegmentClasses[C].Start);
  FSegmentClasses[C].Len := Len;
  FSegmentClasses[C].SectionName.RawValue := SectionName;
  FSegmentClasses[C].GroupName.RawValue := GroupName;

  if FModule <> 0 then begin
    { Fix the section addresses }
    SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(SectionName));
    if SectionHeader = Nil then
      { before Delphi 2005 the class names where used for the section names }
      SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(GroupName));

    if SectionHeader <> Nil then begin
      FSegmentClasses[C].Addr := TDebugAddr(FModule) + SectionHeader.VirtualAddress;
      FSegmentClasses[C].VA := SectionHeader.VirtualAddress;
    end;
  end;
end;

function TDebugMapScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function Search_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PDebugMapLineNumber(Item1)^.VA) - PInteger(Item2)^;
end;

function TDebugMapScanner.LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := 0;
  Offset := 0;
  I := SearchDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Search_MapLineNumber, @Addr, True);
  if (I <> -1) and (FLineNumbers[I].VA >= ModuleStartAddr) then begin
    Result := FLineNumbers[I].LineNumber;
    Offset := Addr - FLineNumbers[I].VA;
  end;
end;

procedure TDebugMapScanner.LineNumbersItem(LineNumber: Integer; const Address: TDebugMapAddress);
var
  SegIndex, C: Integer;
  VA: DWORD;
  Added: Boolean;
begin
  Added := False;
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do begin
    if (FSegmentClasses[SegIndex].Segment = Address.Segment) and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then begin
      if StrLIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS', 3) = 0 then
        Va := Address.Offset
      else
        VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
    { Starting with Delphi 2005, "empty" units are listes with the last line and
      the VA 0001:00000000. When we would accept 0 VAs here, System.pas functions
      could be mapped to other units and line numbers. Discaring such items should
      have no impact on the correct information, because there can't be a function
      that starts at VA 0. }
      if VA = 0 then
        Continue;
      if FLineNumbersCnt mod 256 = 0 then
        SetLength(FLineNumbers, FLineNumbersCnt + 256);
      FLineNumbers[FLineNumbersCnt].Segment := FSegmentClasses[SegIndex].Segment;
      FLineNumbers[FLineNumbersCnt].VA := VA;
      FLineNumbers[FLineNumbersCnt].LineNumber := LineNumber;
      Inc(FLineNumbersCnt);
      Added := True;
      if FNewUnitFileName <> Nil then begin
        C := Length(FSourceNames);
        SetLength(FSourceNames, C + 1);
        FSourceNames[C].Segment := FSegmentClasses[SegIndex].Segment;
        FSourceNames[C].VA := VA;
        FSourceNames[C].ProcName.RawValue := FNewUnitFileName;
        FNewUnitFileName := Nil;
      end;
      Break;
    end;
  end;
  if not Added then
    Inc(FLineNumberErrors);
end;

procedure TDebugMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PDebugMapString);
begin
  FNewUnitFileName := UnitFileName;
end;

function TDebugMapScanner.ModuleNameFromAddr(Addr: DWORD): String;
var
  I: Integer;
begin
  Result := '';
  for I := Length(FSegments) - 1 downto 0 do
    if (FSegments[I].StartVA <= Addr) and (Addr < FSegments[I].EndVA) then begin
      Result := MapStringCacheToModuleName(FSegments[I].UnitName);
      Break;
    end;
end;

function TDebugMapScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  I: Integer;
begin
  Result := DWORD(-1);
  for I := Length(FSegments) - 1 downto 0 do
    if (FSegments[I].StartVA <= Addr) and (Addr < FSegments[I].EndVA) then begin
      Result := FSegments[I].StartVA;
      Break;
    end;
end;

function TDebugMapScanner.ProcNameFromAddr(Addr: DWORD): String;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function Search_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PDebugMapProcName(Item1)^.VA) - PInteger(Item2)^;
end;

function TDebugMapScanner.ProcNameFromAddr(Addr: DWORD; out Offset: Integer): String;
var
  I: Integer;
  ModuleStartAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  Result := '';
  Offset := 0;
  I := SearchDynArray(FProcNames, SizeOf(FProcNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FProcNames[I].VA >= ModuleStartAddr) then
  begin
    Result := MapStringCacheToStr(FProcNames[I].ProcName, True);
    Offset := Addr - FProcNames[I].VA;
  end;
end;

procedure TDebugMapScanner.PublicsByNameItem(const Address: TDebugMapAddress; Name: PDebugMapString);
begin
  { TODO : What to do? }
end;

procedure TDebugMapScanner.PublicsByValueItem(const Address: TDebugMapAddress; Name: PDebugMapString);
var
  SegIndex: Integer;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment) and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then begin
      if FProcNamesCnt mod 256 = 0 then
        SetLength(FProcNames, FProcNamesCnt + 256);
      FProcNames[FProcNamesCnt].Segment := FSegmentClasses[SegIndex].Segment;
      if StrLIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS', 3) = 0 then
        FProcNames[FProcNamesCnt].VA := Address.Offset
      else
        FProcNames[FProcNamesCnt].VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
      FProcNames[FProcNamesCnt].ProcName.RawValue := Name;
      Inc(FProcNamesCnt);
      Break;
    end;
end;

function Sort_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PDebugMapLineNumber(Item1)^.VA) - Integer(PDebugMapLineNumber(Item2)^.VA);
end;

function Sort_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PDebugMapProcName(Item1)^.VA) - Integer(PDebugMapProcName(Item2)^.VA);
end;

function Sort_MapSegment(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PDebugMapSegment(Item1)^.StartVA) - Integer(PDebugMapSegment(Item2)^.StartVA);
end;

procedure TDebugMapScanner.Scan;
begin
  FLineNumberErrors := 0;
  FSegmentCnt := 0;
  FProcNamesCnt := 0;
  Parse;
  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
  SetLength(FSegments, FSegmentCnt);
  SortDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Sort_MapLineNumber);
  SortDynArray(FProcNames, SizeOf(FProcNames[0]), Sort_MapProcName);
  SortDynArray(FSegments, SizeOf(FSegments[0]), Sort_MapSegment);
  SortDynArray(FSourceNames, SizeOf(FSourceNames[0]), Sort_MapProcName);
end;

procedure TDebugMapScanner.SegmentItem(const Address: TDebugMapAddress; Len: Integer; GroupName, UnitName: PDebugMapString);
var
  SegIndex: Integer;
  VA: DWORD;
begin
  for SegIndex := Low(FSegmentClasses) to High(FSegmentClasses) do
    if (FSegmentClasses[SegIndex].Segment = Address.Segment) and (DWORD(Address.Offset) < FSegmentClasses[SegIndex].Len) then begin
      if StrLIComp(FSegmentClasses[SegIndex].GroupName.RawValue, 'TLS', 3) = 0 then
        VA := Address.Offset
      else
        VA := MAPAddrToVA(Address.Offset + FSegmentClasses[SegIndex].Start);
      if FSegmentCnt mod 16 = 0 then
        SetLength(FSegments, FSegmentCnt + 16);
      FSegments[FSegmentCnt].Segment := FSegmentClasses[SegIndex].Segment;
      FSegments[FSegmentCnt].StartVA := VA;
      FSegments[FSegmentCnt].EndVA := VA + DWORD(Len);
      FSegments[FSegmentCnt].UnitName.RawValue := UnitName;
      Inc(FSegmentCnt);
      Break;
    end;
end;

function TDebugMapScanner.SourceNameFromAddr(Addr: DWORD): String;
var
  I: Integer;
  ModuleStartVA: DWORD;
begin
  // try with line numbers first (Delphi compliance)
  ModuleStartVA := ModuleStartFromAddr(Addr);
  Result := '';
  I := SearchDynArray(FSourceNames, SizeOf(FSourceNames[0]), Search_MapProcName, @Addr, True);
  if (I <> -1) and (FSourceNames[I].VA >= ModuleStartVA) then
    Result := MapStringCacheToStr(FSourceNames[I].ProcName);
  if Length(Result) = 0 then begin
    // try with module names (C++Builder compliance)
    for I := Length(FSegments) - 1 downto 0 do
      if (FSegments[I].StartVA <= Addr) and (Addr < FSegments[I].EndVA) then
    begin
      Result := MapStringCacheToFileName(FSegments[I].UnitName);
      Break;
    end;
  end;
end;

const
  MapFileExtension = '.map';  // do not localize

{ TDebugInfoMap }

destructor TDebugInfoMap.Destroy;
begin
  FreeAndNil(FScanner);
  inherited Destroy;
end;

function TDebugInfoMap.GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean;
var
  VA: DWORD;
begin
  VA := VAFromAddr(Addr);
  with FScanner do begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := Length(Info.UnitName) > 0;
    if Result then begin
      Info.Address := Addr;
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := Self;
      Info.BinaryFileName := FileName;
    end;
  end;
end;

function TDebugInfoMap.InitializeSource: Boolean;
var
  MapFileName: TFileName;
begin
  MapFileName := ChangeFileExt(FileName, MapFileExtension);
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TDebugMapScanner.Create(MapFileName, Module);
end;

{ TDebugInfoSymbols }

type
  {$EXTERNALSYM SYM_TYPE}
  SYM_TYPE = (
    SymNone,
    SymCoff,
    SymCv,
    SymPdb,
    SymExport,
    SymDeferred,
    SymSym                  { .sym file }
  );
  TSymType = SYM_TYPE;

  {$EXTERNALSYM PImagehlpSymbolA}
  PImagehlpSymbolA = ^TImagehlpSymbolA;
  {$EXTERNALSYM _IMAGEHLP_SYMBOLA}
  _IMAGEHLP_SYMBOLA = packed record
    SizeOfStruct: DWORD;                                { set to sizeof(IMAGEHLP_SYMBOL) }
    Address: DWORD;                                     { virtual address including dll base address }
    Size: DWORD;                                        { estimated size of symbol, can be zero }
    Flags: DWORD;                                       { info about the symbols, see the SYMF defines }
    MaxNameLength: DWORD;                               { maximum size of symbol name in 'Name' }
    Name: packed Array[0..0] of AnsiChar;               { symbol name (null terminated String) }
  end;
  {$EXTERNALSYM IMAGEHLP_SYMBOLA}
  IMAGEHLP_SYMBOLA = _IMAGEHLP_SYMBOLA;
  {$EXTERNALSYM TImagehlpSymbolA}
  TImagehlpSymbolA = _IMAGEHLP_SYMBOLA;

  {$EXTERNALSYM PImagehlpSymbolW}
  PImagehlpSymbolW = ^TImagehlpSymbolW;
  {$EXTERNALSYM _IMAGEHLP_SYMBOLW}
  _IMAGEHLP_SYMBOLW = packed record
    SizeOfStruct: DWORD;                                { set to sizeof(IMAGEHLP_SYMBOL) }
    Address: DWORD;                                     { virtual address including dll base address }
    Size: DWORD;                                        { estimated size of symbol, can be zero }
    Flags: DWORD;                                       { info about the symbols, see the SYMF defines }
    MaxNameLength: DWORD;                               { maximum size of symbol name in 'Name' }
    Name: packed Array[0..0] of WideChar;               { symbol name (null terminated String) }
  end;
  {$EXTERNALSYM IMAGEHLP_SYMBOLW}
  IMAGEHLP_SYMBOLW = _IMAGEHLP_SYMBOLW;
  {$EXTERNALSYM TImagehlpSymbolW}
  TImagehlpSymbolW = _IMAGEHLP_SYMBOLW;

  {$EXTERNALSYM PImagehlpModuleA}
  PImagehlpModuleA = ^TImagehlpModuleA;
  {$EXTERNALSYM _IMAGEHLP_MODULEA}
  _IMAGEHLP_MODULEA = record
    SizeOfStruct: DWORD;                                { set to sizeof(IMAGEHLP_MODULE) }
    BaseOfImage: DWORD;                                 { base load address of module }
    ImageSize: DWORD;                                   { virtual size of the loaded module }
    TimeDateStamp: DWORD;                               { date/time stamp from pe header }
    CheckSum: DWORD;                                    { checksum from the pe header }
    NumSyms: DWORD;                                     { number of symbols in the symbol table }
    SymType: TSymType;                                  { type of symbols loaded }
    ModuleName: packed Array[0..31] of AnsiChar;        { module name }
    ImageName: packed Array[0..255] of AnsiChar;        { image name }
    LoadedImageName: packed Array[0..255] of AnsiChar;  { symbol file name }
  end;
  {$EXTERNALSYM IMAGEHLP_MODULEA}
  IMAGEHLP_MODULEA = _IMAGEHLP_MODULEA;
  {$EXTERNALSYM TImagehlpModuleA}
  TImagehlpModuleA = _IMAGEHLP_MODULEA;

  {$EXTERNALSYM PImagehlpModuleW}
  PImagehlpModuleW = ^TImagehlpModuleW;
  {$EXTERNALSYM _IMAGEHLP_MODULEW}
  _IMAGEHLP_MODULEW = record
    SizeOfStruct: DWORD;                                { set to sizeof(IMAGEHLP_MODULE) }
    BaseOfImage: DWORD;                                 { base load address of module }
    ImageSize: DWORD;                                   { virtual size of the loaded module }
    TimeDateStamp: DWORD;                               { date/time stamp from pe header }
    CheckSum: DWORD;                                    { checksum from the pe header }
    NumSyms: DWORD;                                     { number of symbols in the symbol table }
    SymType: TSymType;                                  { type of symbols loaded }
    ModuleName: packed Array[0..31] of WideChar;        { module name }
    ImageName: packed Array[0..255] of WideChar;        { image name }
    LoadedImageName: packed Array[0..255] of WideChar;  { symbol file name }
  end;
  {$EXTERNALSYM IMAGEHLP_MODULEW}
  IMAGEHLP_MODULEW = _IMAGEHLP_MODULEW;
  {$EXTERNALSYM TImagehlpModuleW}
  TImagehlpModuleW = _IMAGEHLP_MODULEW;

_IMAGEHLP_LINEA = packed record
    SizeOfStruct: DWORD;           // set to sizeof(IMAGEHLP_LINE)
    Key: Pointer;                  // internal
    LineNumber: DWORD;             // line number in file
    FileName: PAnsiChar;           // full filename
    Address: DWORD;                // first instruction of line
  end;
  IMAGEHLP_LINEA = _IMAGEHLP_LINEA;
  PIMAGEHLP_LINEA = ^_IMAGEHLP_LINEA;
  TImageHlpLineA = _IMAGEHLP_LINEA;
  PImageHlpLineA = PIMAGEHLP_LINEA;

  _IMAGEHLP_LINEW = packed record
    SizeOfStruct: DWORD;           // set to sizeof(IMAGEHLP_LINE)
    Key: Pointer;                  // internal
    LineNumber: DWORD;             // line number in file
    FileName: PWideChar;           // full filename
    Address: DWORD;                // first instruction of line
  end;
  IMAGEHLP_LINEW = _IMAGEHLP_LINEW;
  PIMAGEHLP_LINEW = ^_IMAGEHLP_LINEW;
  TImageHlpLineW = _IMAGEHLP_LINEW;
  PImageHlpLineW = PIMAGEHLP_LINEW;

  TSymInitializeAFunc = function (hProcess: THandle; UserSearchPath: LPSTR; fInvadeProcess: Bool): Bool; stdcall;
  TSymInitializeWFunc = function (hProcess: THandle; UserSearchPath: LPWSTR; fInvadeProcess: Bool): Bool; stdcall;
  TSymGetOptionsFunc = function: DWORD; stdcall;
  TSymSetOptionsFunc = function (SymOptions: DWORD): DWORD; stdcall;
  TSymCleanupFunc = function (hProcess: THandle): Bool; stdcall;
  TSymGetSymFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Symbol: TImagehlpSymbolA): Bool; stdcall;
  TSymGetSymFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Symbol: TImagehlpSymbolW): Bool; stdcall;
  TSymGetModuleInfoAFunc = function (hProcess: THandle; dwAddr: DWORD; var ModuleInfo: TImagehlpModuleA): Bool; stdcall;
  TSymGetModuleInfoWFunc = function (hProcess: THandle; dwAddr: DWORD; var ModuleInfo: TImagehlpModuleW): Bool; stdcall;
  TSymLoadModuleFunc = function (hProcess: THandle; hFile: THandle; ImageName, ModuleName: LPSTR; BaseOfDll, SizeOfDll: DWORD): DWORD; stdcall;
  TSymGetLineFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Line: TImageHlpLineA): Bool; stdcall;
  TSymGetLineFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Line: TImageHlpLineW): Bool; stdcall;

var
  DebugSymbolsInitialized: Boolean = False;
  DebugSymbolsLoadFailed: Boolean = False;
  ImageHlpDllHandle: THandle = 0;
  SymInitializeAFunc: TSymInitializeAFunc = Nil;
  SymInitializeWFunc: TSymInitializeWFunc = Nil;
  SymGetOptionsFunc: TSymGetOptionsFunc = Nil;
  SymSetOptionsFunc: TSymSetOptionsFunc = Nil;
  SymCleanupFunc: TSymCleanupFunc = Nil;
  SymGetSymFromAddrAFunc: TSymGetSymFromAddrAFunc = Nil;
  SymGetSymFromAddrWFunc: TSymGetSymFromAddrWFunc = Nil;
  SymGetModuleInfoAFunc: TSymGetModuleInfoAFunc = Nil;
  SymGetModuleInfoWFunc: TSymGetModuleInfoWFunc = Nil;
  SymLoadModuleFunc: TSymLoadModuleFunc = Nil;
  SymGetLineFromAddrAFunc: TSymGetLineFromAddrAFunc = Nil;
  SymGetLineFromAddrWFunc: TSymGetLineFromAddrWFunc = Nil;

const
  ImageHlpDllName = 'imagehlp.dll';                          // do not localize
  SymInitializeAFuncName = 'SymInitialize';                  // do not localize
  SymInitializeWFuncName = 'SymInitializeW';                 // do not localize
  SymGetOptionsFuncName = 'SymGetOptions';                   // do not localize
  SymSetOptionsFuncName = 'SymSetOptions';                   // do not localize
  SymCleanupFuncName = 'SymCleanup';                         // do not localize
  SymGetSymFromAddrAFuncName = 'SymGetSymFromAddr';          // do not localize
  SymGetSymFromAddrWFuncName = 'SymGetSymFromAddrW';         // do not localize
  SymGetModuleInfoAFuncName = 'SymGetModuleInfo';            // do not localize
  SymGetModuleInfoWFuncName = 'SymGetModuleInfoW';           // do not localize
  SymLoadModuleFuncName = 'SymLoadModule';                   // do not localize
  SymGetLineFromAddrAFuncName = 'SymGetLineFromAddr';        // do not localize
  SymGetLineFromAddrWFuncName = 'SymGetLineFromAddrW';       // do not localize

  DirDelimiter = '\';
  DirSeparator = ';';
  PathDevicePrefix = '\\.\';
  PathUncPrefix    = '\\';

//
// options that are set/returned by SymSetOptions() & SymGetOptions()
// these are used as a mask
//

const
  SYMOPT_CASE_INSENSITIVE       = $00000001;
  {$EXTERNALSYM SYMOPT_CASE_INSENSITIVE}
  SYMOPT_UNDNAME                = $00000002;
  {$EXTERNALSYM SYMOPT_UNDNAME}
  SYMOPT_DEFERRED_LOADS         = $00000004;
  {$EXTERNALSYM SYMOPT_DEFERRED_LOADS}
  SYMOPT_NO_CPP                 = $00000008;
  {$EXTERNALSYM SYMOPT_NO_CPP}
  SYMOPT_LOAD_LINES             = $00000010;
  {$EXTERNALSYM SYMOPT_LOAD_LINES}
  SYMOPT_OMAP_FIND_NEAREST      = $00000020;
  {$EXTERNALSYM SYMOPT_OMAP_FIND_NEAREST}
  SYMOPT_LOAD_ANYTHING          = $00000040;
  {$EXTERNALSYM SYMOPT_LOAD_ANYTHING}
  SYMOPT_IGNORE_CVREC           = $00000080;
  {$EXTERNALSYM SYMOPT_IGNORE_CVREC}
  SYMOPT_NO_UNQUALIFIED_LOADS   = $00000100;
  {$EXTERNALSYM SYMOPT_NO_UNQUALIFIED_LOADS}
  SYMOPT_FAIL_CRITICAL_ERRORS   = $00000200;
  {$EXTERNALSYM SYMOPT_FAIL_CRITICAL_ERRORS}
  SYMOPT_EXACT_SYMBOLS          = $00000400;
  {$EXTERNALSYM SYMOPT_EXACT_SYMBOLS}
  SYMOPT_ALLOW_ABSOLUTE_SYMBOLS = $00000800;
  {$EXTERNALSYM SYMOPT_ALLOW_ABSOLUTE_SYMBOLS}
  SYMOPT_IGNORE_NT_SYMPATH      = $00001000;
  {$EXTERNALSYM SYMOPT_IGNORE_NT_SYMPATH}
  SYMOPT_INCLUDE_32BIT_MODULES  = $00002000;
  {$EXTERNALSYM SYMOPT_INCLUDE_32BIT_MODULES}
  SYMOPT_PUBLICS_ONLY           = $00004000;
  {$EXTERNALSYM SYMOPT_PUBLICS_ONLY}
  SYMOPT_NO_PUBLICS             = $00008000;
  {$EXTERNALSYM SYMOPT_NO_PUBLICS}
  SYMOPT_AUTO_PUBLICS           = $00010000;
  {$EXTERNALSYM SYMOPT_AUTO_PUBLICS}
  SYMOPT_NO_IMAGE_SEARCH        = $00020000;
  {$EXTERNALSYM SYMOPT_NO_IMAGE_SEARCH}
  SYMOPT_SECURE                 = $00040000;
  {$EXTERNALSYM SYMOPT_SECURE}
  SYMOPT_NO_PROMPTS             = $00080000;
  {$EXTERNALSYM SYMOPT_NO_PROMPTS}

  SYMOPT_DEBUG                  = $80000000;
  {$EXTERNALSYM SYMOPT_DEBUG}

//
// UnDecorateSymbolName Flags
//

const
  UNDNAME_COMPLETE               = ($0000); // Enable full undecoration
  {$EXTERNALSYM UNDNAME_COMPLETE}
  UNDNAME_NO_LEADING_UNDERSCORES = ($0001); // Remove leading underscores from MS extended keywords
  {$EXTERNALSYM UNDNAME_NO_LEADING_UNDERSCORES}
  UNDNAME_NO_MS_KEYWORDS         = ($0002); // Disable expansion of MS extended keywords
  {$EXTERNALSYM UNDNAME_NO_MS_KEYWORDS}
  UNDNAME_NO_FUNCTION_RETURNS    = ($0004); // Disable expansion of return type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_FUNCTION_RETURNS}
  UNDNAME_NO_ALLOCATION_MODEL    = ($0008); // Disable expansion of the declaration model
  {$EXTERNALSYM UNDNAME_NO_ALLOCATION_MODEL}
  UNDNAME_NO_ALLOCATION_LANGUAGE = ($0010); // Disable expansion of the declaration language specifier
  {$EXTERNALSYM UNDNAME_NO_ALLOCATION_LANGUAGE}
  UNDNAME_NO_MS_THISTYPE         = ($0020); // NYI Disable expansion of MS keywords on the 'this' type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_MS_THISTYPE}
  UNDNAME_NO_CV_THISTYPE         = ($0040); // NYI Disable expansion of CV modifiers on the 'this' type for primary declaration
  {$EXTERNALSYM UNDNAME_NO_CV_THISTYPE}
  UNDNAME_NO_THISTYPE            = ($0060); // Disable all modifiers on the 'this' type
  {$EXTERNALSYM UNDNAME_NO_THISTYPE}
  UNDNAME_NO_ACCESS_SPECIFIERS   = ($0080); // Disable expansion of access specifiers for members
  {$EXTERNALSYM UNDNAME_NO_ACCESS_SPECIFIERS}
  UNDNAME_NO_THROW_SIGNATURES    = ($0100); // Disable expansion of 'throw-signatures' for functions and pointers to functions
  {$EXTERNALSYM UNDNAME_NO_THROW_SIGNATURES}
  UNDNAME_NO_MEMBER_TYPE         = ($0200); // Disable expansion of 'static' or 'virtual'ness of members
  {$EXTERNALSYM UNDNAME_NO_MEMBER_TYPE}
  UNDNAME_NO_RETURN_UDT_MODEL    = ($0400); // Disable expansion of MS model for UDT returns
  {$EXTERNALSYM UNDNAME_NO_RETURN_UDT_MODEL}
  UNDNAME_32_BIT_DECODE          = ($0800); // Undecorate 32-bit decorated names
  {$EXTERNALSYM UNDNAME_32_BIT_DECODE}
  UNDNAME_NAME_ONLY              = ($1000); // Crack only the name for primary declaration;
  {$EXTERNALSYM UNDNAME_NAME_ONLY}
                                                                                                   //  return just [scope::]name.  Does expand template params
  UNDNAME_NO_ARGUMENTS    = ($2000); // Don't undecorate arguments to function
  {$EXTERNALSYM UNDNAME_NO_ARGUMENTS}
  UNDNAME_NO_SPECIAL_SYMS = ($4000); // Don't undecorate special names (v-table, vcall, vector xxx, metatype, etc)
  {$EXTERNALSYM UNDNAME_NO_SPECIAL_SYMS}

function StrEnsureSuffix(const Suffix, Text: String): String; inline;
var
  SuffixLen: SizeInt;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrEnsureNoSuffix(const Suffix, Text: String): String; inline;
var
  SuffixLen: SizeInt;
  StrLength: SizeInt;
begin
  SuffixLen := Length(Suffix);
  StrLength := Length(Text);
  if Copy(Text, StrLength - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Copy(Text, 1, StrLength - SuffixLen)
  else
    Result := Text;
end;

function GetCurrentFolder: String; inline;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetCurrentDirectory(0, Nil);
  if Required > 0 then begin
    SetLength(Result, Required);
    GetCurrentDirectory(Required, PChar(Result));
    SetLength(Result, Required);
  end;
end;

function ExpandEnvironmentVar(var Value: String): Boolean; inline;
var
  R: Integer;
  Expanded: String;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) > 0;
  if Result then begin
    SetLength(Expanded, R);
    Value := Expanded;
  end;
end;

function GetEnvironmentVar(const Name: String; out Value: String; Expand: Boolean): Boolean; overload; inline;
var
  R: DWORD;
begin
  R := Windows.GetEnvironmentVariable(PChar(Name), Nil, 0);
  SetLength(Value, R);
  R := Windows.GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R > 0;
  if not Result then
    Value := ''
  else begin
    SetLength(Value, R);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;

function GetEnvironmentVar(const Name: String; out Value: String): Boolean; overload; inline;
begin
  Result := GetEnvironmentVar(Name, Value, True);
end;

function StrLeft(const S: String; Count: SizeInt): String; inline;
begin
  Result := Copy(S, 1, Count);
end;

procedure StrToStrings(S, Sep: String; const List: TStrings; const AllowEmptyString: Boolean = True); inline;
var
  I, L: SizeInt;
  Left: String;
begin
  Assert(List <> Nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do begin
      Left := StrLeft(S, I - 1);
      if (Length(Left) > 0) or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if Length(S) > 0 then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

function StringsToStr(const List: TStrings; const Sep: String; const AllowEmptyString: Boolean = True): String; inline;
var
  I, L: SizeInt;
begin
  Result := '';
  for I := 0 to List.Count - 1 do begin
    if (length(List[I]) > 0) or AllowEmptyString then begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count > 0 then begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

function StrRemoveEmptyPaths(const Paths: String): String; inline;
var
  List: TStrings;
  I: Integer;
begin
  List := TStringList.Create;
  try
    StrToStrings(Paths, DirSeparator, List, False);
    for I := 0 to List.Count - 1 do
      if Length(Trim(List[I])) = 0 then
        List[I] := '';
    Result := StringsToStr(List, DirSeparator, False);
  finally
    List.Free;
  end;
end;

type
  TUndecorateSymbolNameA = function (DecoratedName: PAnsiChar; UnDecoratedName: PAnsiChar; UndecoratedLength: DWORD; Flags: DWORD): DWORD; stdcall;
// 'imagehlp.dll' 'UnDecorateSymbolName'

  TUndecorateSymbolNameW = function (DecoratedName: PWideChar; UnDecoratedName: PWideChar; UndecoratedLength: DWORD; Flags: DWORD): DWORD; stdcall;
// 'imagehlp.dll' 'UnDecorateSymbolNameW'

var
  UndecorateSymbolNameA: TUndecorateSymbolNameA = Nil;
  UndecorateSymbolNameAFailed: Boolean = False;
  UndecorateSymbolNameW: TUndecorateSymbolNameW = Nil;
  UndecorateSymbolNameWFailed: Boolean = False;

function UndecorateSymbolName(const DecoratedName: String; out UnMangled: String; Flags: DWORD): Boolean;
const
  ModuleName = 'imagehlp.dll';
  BufferSize = 512;
var
  ModuleHandle: HMODULE;
  WideBuffer: WideString;
  AnsiBuffer: AnsiString;
  Res: DWORD;
begin
  Result := False;
  if ((not Assigned(UndecorateSymbolNameA)) and (not UndecorateSymbolNameAFailed)) or ((not Assigned(UndecorateSymbolNameW)) and (not UndecorateSymbolNameWFailed)) then begin
    ModuleHandle := GetModuleHandle(ModuleName);
    if ModuleHandle = 0 then begin
      ModuleHandle := SafeLoadLibrary(ModuleName);
      if ModuleHandle = 0 then
        Exit;
    end;
    UndecorateSymbolNameA := GetProcAddress(ModuleHandle, 'UnDecorateSymbolName');
    UndecorateSymbolNameAFailed := not Assigned(UndecorateSymbolNameA);
    UndecorateSymbolNameW := GetProcAddress(ModuleHandle, 'UnDecorateSymbolNameW');
    UndecorateSymbolNameWFailed := not Assigned(UndecorateSymbolNameW);
  end;
  if Assigned(UndecorateSymbolNameW) then begin
    SetLength(WideBuffer, BufferSize);
    Res := UnDecorateSymbolNameW(PWideChar(WideString(DecoratedName)), PWideChar(WideBuffer), BufferSize, Flags);
    if Res > 0 then begin
      SetLength(WideBuffer, Res * SizeOf(WideString));
      UnMangled := String(WideBuffer);
      Result := True;
    end;
  end
  else if Assigned(UndecorateSymbolNameA) then begin
    SetLength(AnsiBuffer, BufferSize);
    Res := UnDecorateSymbolNameA(PAnsiChar(AnsiString(DecoratedName)), PAnsiChar(AnsiBuffer), BufferSize, Flags);
    if Res > 0 then begin
      SetLength(AnsiBuffer, Res * SizeOf(AnsiString));
      UnMangled := String(AnsiBuffer);
      Result := True;
    end;
  end;
end;

class function TDebugInfoSymbols.InitializeDebugSymbols: Boolean;
var
  EnvironmentVarValue, SearchPath: String;
  SymOptions: Cardinal;
  ProcessHandle: THandle;
begin
  Result := DebugSymbolsInitialized;
  if not DebugSymbolsLoadFailed then begin
    Result := LoadDebugFunctions;
    DebugSymbolsLoadFailed := not Result;

    if Result then begin
      if Length(DebugInfoSymbolPaths) > 0 then begin
        SearchPath := StrEnsureSuffix(DirSeparator, DebugInfoSymbolPaths);
        SearchPath := StrEnsureNoSuffix(DirSeparator, SearchPath + GetCurrentFolder);

        if GetEnvironmentVar(EnvironmentVarNtSymbolPath, EnvironmentVarValue) and (Length(EnvironmentVarValue) > 0) then
          SearchPath := StrEnsureNoSuffix(DirSeparator, StrEnsureSuffix(DirSeparator, EnvironmentVarValue) + SearchPath);
        if GetEnvironmentVar(EnvironmentVarAlternateNtSymbolPath, EnvironmentVarValue) and (Length(EnvironmentVarValue) > 0) then
          SearchPath := StrEnsureNoSuffix(DirSeparator, StrEnsureSuffix(DirSeparator, EnvironmentVarValue) + SearchPath);

        // DbgHelp.dll crashes when an empty path is specified.
        // This also means that the SearchPath must not end with a DirSeparator. }
        SearchPath := StrRemoveEmptyPaths(SearchPath);
      end
      else
        // Fix crash SymLoadModuleFunc on WinXP SP3 when SearchPath=''
        SearchPath := GetCurrentFolder;

      if IsWinNT then
        // in Windows NT, first argument is a process handle
        ProcessHandle := GetCurrentProcess
      else
        // in Windows 95, 98, ME first argument is a process identifier
        ProcessHandle := GetCurrentProcessId;

      // Debug(WinXPSP3): SymInitializeWFunc==Nil
      if Assigned(SymInitializeWFunc) then
        Result := SymInitializeWFunc(ProcessHandle, PWideChar(WideString(SearchPath)), False)
      else if Assigned(SymInitializeAFunc) then
        Result := SymInitializeAFunc(ProcessHandle, PAnsiChar(AnsiString(SearchPath)), False)
      else
        Result := False;

      if Result then begin
        SymOptions := SymGetOptionsFunc or SYMOPT_DEFERRED_LOADS or SYMOPT_FAIL_CRITICAL_ERRORS or SYMOPT_INCLUDE_32BIT_MODULES or SYMOPT_LOAD_LINES;
        SymOptions := SymOptions and (not (SYMOPT_NO_UNQUALIFIED_LOADS or SYMOPT_UNDNAME));
        SymSetOptionsFunc(SymOptions);
      end;

      DebugSymbolsInitialized := Result;
    end
    else
      UnloadDebugFunctions;
  end;
end;

class function TDebugInfoSymbols.CleanupDebugSymbols: Boolean;
begin
  Result := True;

  if DebugSymbolsInitialized then
    Result := SymCleanupFunc(GetCurrentProcess);

  UnloadDebugFunctions;
end;

function TDebugInfoSymbols.GetLocationInfo(const Addr: Pointer; out Info: TDebugLocationInfo): Boolean;
const
  SymbolNameLength = 1000;
  SymbolSizeA = SizeOf(TImagehlpSymbolA) + SymbolNameLength * SizeOf(AnsiChar);
  SymbolSizeW = SizeOf(TImagehlpSymbolW) + SymbolNameLength * SizeOf(WideChar);
var
  Displacement: DWORD;
  ProcessHandle: THandle;
  SymbolA: PImagehlpSymbolA;
  SymbolW: PImagehlpSymbolW;
  LineA: TImageHlpLineA;
  LineW: TImageHlpLineW;
begin
  ProcessHandle := GetCurrentProcess;

  if Assigned(SymGetSymFromAddrWFunc) then begin
    GetMem(SymbolW, SymbolSizeW);
    try
      ZeroMemory(SymbolW, SymbolSizeW);
      SymbolW^.SizeOfStruct := SizeOf(TImageHlpSymbolW);
      SymbolW^.MaxNameLength := SymbolNameLength;
      Displacement := 0;

      Result := SymGetSymFromAddrWFunc(ProcessHandle, TDebugAddr(Addr), @Displacement, SymbolW^);
      if Result then begin
        Info.DebugInfo := Self;
        Info.Address := Addr;
        Info.BinaryFileName := FileName;
        Info.OffsetFromProcName := Displacement;
        UnDecorateSymbolName(String(WideString(SymbolW^.Name)), Info.ProcedureName, UNDNAME_NAME_ONLY or UNDNAME_NO_ARGUMENTS);
      end;
    finally
      FreeMem(SymbolW);
    end;
  end
  else if Assigned(SymGetSymFromAddrAFunc) then begin
    GetMem(SymbolA, SymbolSizeA);
    try
      ZeroMemory(SymbolA, SymbolSizeA);
      SymbolA^.SizeOfStruct := SizeOf(TImageHlpSymbolA);
      SymbolA^.MaxNameLength := SymbolNameLength;
      Displacement := 0;

      Result := SymGetSymFromAddrAFunc(ProcessHandle, TDebugAddr(Addr), @Displacement, SymbolA^);
      if Result then begin
        Info.DebugInfo := Self;
        Info.Address := Addr;
        Info.BinaryFileName := FileName;
        Info.OffsetFromProcName := Displacement;
        UnDecorateSymbolName(String(AnsiString(SymbolA^.Name)), Info.ProcedureName, UNDNAME_NAME_ONLY or UNDNAME_NO_ARGUMENTS);
      end;
    finally
      FreeMem(SymbolA);
    end;
  end
  else
    Result := False;

  // line number is optional
  if Result and Assigned(SymGetLineFromAddrWFunc) then begin
    ZeroMemory(@LineW, SizeOf(LineW));
    LineW.SizeOfStruct := SizeOf(LineW);
    Displacement := 0;

    if SymGetLineFromAddrWFunc(ProcessHandle, TDebugAddr(Addr), @Displacement, LineW) then begin
      Info.LineNumber := LineW.LineNumber;
      Info.UnitName := String(LineW.FileName);
      Info.OffsetFromLineNumber := Displacement;
    end;
  end
  else if Result and Assigned(SymGetLineFromAddrAFunc) then begin
    ZeroMemory(@LineA, SizeOf(LineA));
    LineA.SizeOfStruct := SizeOf(LineA);
    Displacement := 0;

    if SymGetLineFromAddrAFunc(ProcessHandle, TDebugAddr(Addr), @Displacement, LineA) then begin
      Info.LineNumber := LineA.LineNumber;
      Info.UnitName := String(LineA.FileName);
      Info.OffsetFromLineNumber := Displacement;
    end;
  end;
end;

function TDebugInfoSymbols.InitializeSource: Boolean;
var
  ModuleFileName: TFileName;
  ModuleInfoA: TImagehlpModuleA;
  ModuleInfoW: TImagehlpModuleW;
  ProcessHandle: THandle;
begin
  Result := InitializeDebugSymbols;
  if Result then begin
    if IsWinNT then
      // in Windows NT, first argument is a process handle
      ProcessHandle := GetCurrentProcess
    else
      // in Windows 95, 98, ME, first argument is a process identifier
      ProcessHandle := GetCurrentProcessId;

    if Assigned(SymGetModuleInfoWFunc) then begin
      ZeroMemory(@ModuleInfoW, SizeOf(ModuleInfoW));
      ModuleInfoW.SizeOfStruct := SizeOf(ModuleInfoW);
      Result := SymGetModuleInfoWFunc(ProcessHandle, Module, ModuleInfoW);
      if not Result then begin
        // the symbols for this module are not loaded yet: load the module and query for the symbol again
        ModuleFileName := GetModulePath(Module);
        ZeroMemory(@ModuleInfoW, SizeOf(ModuleInfoW));
        ModuleInfoW.SizeOfStruct := SizeOf(ModuleInfoW);
        // warning: crash on WinXP SP3 when SymInitializeAFunc is called with empty SearchPath
        // OF: possible loss of data
        Result := (SymLoadModuleFunc(ProcessHandle, 0, PAnsiChar(AnsiString(ModuleFileName)), Nil, 0, 0) <> 0) and SymGetModuleInfoWFunc(ProcessHandle, Module, ModuleInfoW);
      end;
      Result := Result and (ModuleInfoW.BaseOfImage <> 0) and not (ModuleInfoW.SymType in [SymNone, SymExport]);
    end
    else if Assigned(SymGetModuleInfoAFunc) then begin
      ZeroMemory(@ModuleInfoA, SizeOf(ModuleInfoA));
      ModuleInfoA.SizeOfStruct := SizeOf(ModuleInfoA);
      Result := SymGetModuleInfoAFunc(ProcessHandle, Module, ModuleInfoA);
      if not Result then begin
        // the symbols for this module are not loaded yet: load the module and query for the symbol again
        ModuleFileName := GetModulePath(Module);
        ZeroMemory(@ModuleInfoA, SizeOf(ModuleInfoA));
        ModuleInfoA.SizeOfStruct := SizeOf(ModuleInfoA);
        // warning: crash on WinXP SP3 when SymInitializeAFunc is called with empty SearchPath
        // OF: possible loss of data
        Result := (SymLoadModuleFunc(ProcessHandle, 0, PAnsiChar(AnsiString(ModuleFileName)), Nil, 0, 0) <> 0) and SymGetModuleInfoAFunc(ProcessHandle, Module, ModuleInfoA);
      end;
      Result := Result and (ModuleInfoA.BaseOfImage <> 0) and not (ModuleInfoA.SymType in [SymNone, SymExport]);
    end
    else
      Result := False;
  end;
end;

class function TDebugInfoSymbols.LoadDebugFunctions: Boolean;
begin
  ImageHlpDllHandle := SafeLoadLibrary(ImageHlpDllName);

  if ImageHlpDllHandle <> 0 then begin
    SymInitializeAFunc := GetProcAddress(ImageHlpDllHandle, SymInitializeAFuncName);
    SymInitializeWFunc := GetProcAddress(ImageHlpDllHandle, SymInitializeWFuncName);
    SymGetOptionsFunc := GetProcAddress(ImageHlpDllHandle, SymGetOptionsFuncName);
    SymSetOptionsFunc := GetProcAddress(ImageHlpDllHandle, SymSetOptionsFuncName);
    SymCleanupFunc := GetProcAddress(ImageHlpDllHandle, SymCleanupFuncName);
    SymGetSymFromAddrAFunc := GetProcAddress(ImageHlpDllHandle, SymGetSymFromAddrAFuncName);
    SymGetSymFromAddrWFunc := GetProcAddress(ImageHlpDllHandle, SymGetSymFromAddrWFuncName);
    SymGetModuleInfoAFunc := GetProcAddress(ImageHlpDllHandle, SymGetModuleInfoAFuncName);
    SymGetModuleInfoWFunc := GetProcAddress(ImageHlpDllHandle, SymGetModuleInfoWFuncName);
    SymLoadModuleFunc := GetProcAddress(ImageHlpDllHandle, SymLoadModuleFuncName);
    SymGetLineFromAddrAFunc := GetProcAddress(ImageHlpDllHandle, SymGetLineFromAddrAFuncName);
    SymGetLineFromAddrWFunc := GetProcAddress(ImageHlpDllHandle, SymGetLineFromAddrWFuncName);
  end;

  // SymGetLineFromAddrFunc is optional
  Result := (ImageHlpDllHandle <> 0) and
    Assigned(SymGetOptionsFunc) and Assigned(SymSetOptionsFunc) and
    Assigned(SymCleanupFunc) and Assigned(SymLoadModuleFunc) and
    (Assigned(SymInitializeAFunc) or Assigned(SymInitializeWFunc)) and
    (Assigned(SymGetSymFromAddrAFunc) or Assigned(SymGetSymFromAddrWFunc)) and
    (Assigned(SymGetModuleInfoAFunc) or Assigned(SymGetModuleInfoWFunc));
end;

class function TDebugInfoSymbols.UnloadDebugFunctions: Boolean;
begin
  Result := ImageHlpDllHandle <> 0;

  if Result then
    FreeLibrary(ImageHlpDllHandle);

  ImageHlpDllHandle := 0;

  SymInitializeAFunc := Nil;
  SymInitializeWFunc := Nil;
  SymGetOptionsFunc := Nil;
  SymSetOptionsFunc := Nil;
  SymCleanupFunc := Nil;
  SymGetSymFromAddrAFunc := Nil;
  SymGetSymFromAddrWFunc := Nil;
  SymGetModuleInfoAFunc := Nil;
  SymGetModuleInfoWFunc := Nil;
  SymLoadModuleFunc := Nil;
  SymGetLineFromAddrAFunc := Nil;
  SymGetLineFromAddrWFunc := Nil;
end;

{ TBinDebugScanner }

function SimpleCryptString(const S: TUTF8String): TUTF8String;
var
  I: Integer;
  C: Byte;
  P: PByte;
begin
  SetLength(Result, Length(S));
  P := PByte(Result);
  for I := 1 to Length(S) do begin
    C := Ord(S[I]);
    if C <> $AA then
      C := C xor $AA;
    P^ := C;
    Inc(P);
  end;
end;

function DecodeNameString(const S: PAnsiChar): String;
var
  I, B: Integer;
  C: Byte;
  P: PByte;
  Buffer: Array [0..255] of AnsiChar;
begin
  Result := '';
  B := 0;
  P := PByte(S);
  case P^ of
    1: begin
      Inc(P);
      Result := UTF8ToString(SimpleCryptString(PAnsiChar(P)));
      Exit;
    end;
    2: begin
      Inc(P);
      Buffer[B] := '@';
      Inc(B);
    end;
  end;
  I := 0;
  C := 0;
  repeat
    case I and $03 of
      0: C := P^ and $3F;
      1: begin
        C := (P^ shr 6) and $03;
        Inc(P);
        Inc(C, (P^ and $0F) shl 2);
      end;
      2: begin
        C := (P^ shr 4) and $0F;
        Inc(P);
        Inc(C, (P^ and $03) shl 4);
      end;
      3: begin
        C := (P^ shr 2) and $3F;
        Inc(P);
      end;
    end;
    case C of
      $00: Break;
      $01..$0A: Inc(C, Ord('0') - $01);
      $0B..$24: Inc(C, Ord('A') - $0B);
      $25..$3E: Inc(C, Ord('a') - $25);
      $3F     : C := Ord('_');
    end;
    Buffer[B] := AnsiChar(C);
    Inc(B);
    Inc(I);
  until B >= SizeOf(Buffer) - 1;
  Buffer[B] := NativeNull;
  Result := UTF8ToString(Buffer);
end;

function PathRemoveExtension(const Path: String): String; inline;
var
  I: Integer;
begin
  I := LastDelimiter(':.' + DirDelimiter, Path);
  if (I > 0) and (Path[I] = '.') then
    Result := Copy(Path, 1, I - 1)
  else
    Result := Path;
end;

function PathExtractFileNameNoExt(const Path: String): String; inline;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

constructor TBinDebugScanner.Create(AStream: TCustomMemoryStream; CacheData: Boolean);
begin
  inherited Create;
  FCacheData := CacheData;
  FStream := AStream;
  CheckFormat;
end;

procedure TBinDebugScanner.CacheLineNumbers;
var
  P: Pointer;
  Value, LineNumber, C, Ln: Integer;
  CurrVA: DWORD;
begin
  if FLineNumbers = Nil then begin
    LineNumber := 0;
    CurrVA := 0;
    C := 0;
    Ln := 0;
    P := MakePtr(PDbgHeader(FStream.Memory)^.LineNumbers);
    Value := 0;
    while ReadValue(P, Value) do begin
      Inc(CurrVA, Value);
      ReadValue(P, Value);
      Inc(LineNumber, Value);
      if C = Ln then begin
        if Ln < 64 then
          Ln := 64
        else
          Ln := Ln + Ln div 4;
        SetLength(FLineNumbers, Ln);
      end;
      FLineNumbers[C].VA := CurrVA;
      FLineNumbers[C].LineNumber := LineNumber;
      Inc(C);
    end;
    SetLength(FLineNumbers, C);
  end;
end;

procedure TBinDebugScanner.CacheProcNames;
var
  P: Pointer;
  Value, FirstWord, SecondWord, C, Ln: Integer;
  CurrAddr: DWORD;
begin
  if FProcNames = Nil then begin
    FirstWord := 0;
    SecondWord := 0;
    CurrAddr := 0;
    C := 0;
    Ln := 0;
    P := MakePtr(PDbgHeader(FStream.Memory)^.Symbols);
    Value := 0;
    while ReadValue(P, Value) do begin
      Inc(CurrAddr, Value);
      ReadValue(P, Value);
      Inc(FirstWord, Value);
      ReadValue(P, Value);
      Inc(SecondWord, Value);
      if C = Ln then begin
        if Ln < 64 then
          Ln := 64
        else
          Ln := Ln + Ln div 4;
        SetLength(FProcNames, Ln);
      end;
      FProcNames[C].Addr := CurrAddr;
      FProcNames[C].FirstWord := FirstWord;
      FProcNames[C].SecondWord := SecondWord;
      Inc(C);
    end;
    SetLength(FProcNames, C);
  end;
end;

{$OVERFLOWCHECKS OFF}
procedure TBinDebugScanner.CheckFormat;
var
  CheckSum: Integer;
  Data, EndData: PAnsiChar;
  Header: PDbgHeader;
begin
  Data := FStream.Memory;
  Header := PDbgHeader(Data);
  FValidFormat := (Data <> Nil) and (FStream.Size > SizeOf(TDbgHeader)) and
                  (FStream.Size mod 4 = 0); // and (Header^.Signature = DbgDataSignature) and (Header^.Version = DbgHeaderVersion);
  if FValidFormat and Header^.CheckSumValid then begin
    CheckSum := -Header^.CheckSum;
    EndData := Data + FStream.Size;
    while Data < EndData do begin
      Inc(CheckSum, PInteger(Data)^);
      Inc(PInteger(Data));
    end;
    CheckSum := (CheckSum shr 8) or (CheckSum shl 24);
    FValidFormat := (CheckSum = Header^.CheckSum);
  end;
end;
{$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

function TBinDebugScanner.DataToStr(A: Integer): String;
var
  P: PAnsiChar;
begin
  if A = 0 then
    Result := ''
  else begin
    P := PAnsiChar(TDebugAddr(FStream.Memory) + TDebugAddr(A) + TDebugAddr(PDbgHeader(FStream.Memory)^.Words) - 1);
    Result := DecodeNameString(P);
  end;
end;

function TBinDebugScanner.GetModuleName: String;
begin
  Result := DataToStr(PDbgHeader(FStream.Memory)^.ModuleName);
end;

function TBinDebugScanner.IsModuleNameValid(const Name: TFileName): Boolean;
begin
  Result := AnsiSameText(ModuleName, PathExtractFileNameNoExt(Name));
end;

function TBinDebugScanner.LineNumberFromAddr(Addr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function TBinDebugScanner.LineNumberFromAddr(Addr: DWORD; out Offset: Integer): Integer;
var
  P: Pointer;
  Value, LineNumber: Integer;
  CurrVA, ModuleStartVA, ItemVA: DWORD;
begin
  ModuleStartVA := ModuleStartFromAddr(Addr);
  LineNumber := 0;
  Offset := 0;
  if FCacheData then begin
    CacheLineNumbers;
    for Value := Length(FLineNumbers) - 1 downto 0 do
      if FLineNumbers[Value].VA <= Addr then begin
        if FLineNumbers[Value].VA >= ModuleStartVA then begin
          LineNumber := FLineNumbers[Value].LineNumber;
          Offset := Addr - FLineNumbers[Value].VA;
        end;
        Break;
      end;
  end
  else begin
    P := MakePtr(PDbgHeader(FStream.Memory)^.LineNumbers);
    CurrVA := 0;
    ItemVA := 0;
    while ReadValue(P, Value) do begin
      Inc(CurrVA, Value);
      if Addr < CurrVA then begin
        if ItemVA < ModuleStartVA then begin
          LineNumber := 0;
          Offset := 0;
        end;
        Break;
      end
      else begin
        ItemVA := CurrVA;
        ReadValue(P, Value);
        Inc(LineNumber, Value);
        Offset := Addr - CurrVA;
      end;
    end;
  end;
  Result := LineNumber;
end;

function TBinDebugScanner.MakePtr(A: Integer): Pointer;
begin
  Result := Pointer(TDebugAddr(FStream.Memory) + TDebugAddr(A));
end;

function TBinDebugScanner.ModuleNameFromAddr(Addr: DWORD): String;
var
  Value, Name: Integer;
  StartAddr: DWORD;
  P: Pointer;
begin
  P := MakePtr(PDbgHeader(FStream.Memory)^.Units);
  Name := 0;
  StartAddr := 0;
  Value := 0;
  while ReadValue(P, Value) do begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else begin
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  Result := DataToStr(Name);
end;

function TBinDebugScanner.ModuleStartFromAddr(Addr: DWORD): DWORD;
var
  Value: Integer;
  StartAddr, ModuleStartAddr: DWORD;
  P: Pointer;
begin
  P := MakePtr(PDbgHeader(FStream.Memory)^.Units);
  StartAddr := 0;
  ModuleStartAddr := DWORD(-1);
  Value := 0;
  while ReadValue(P, Value) do begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then
      Break
    else begin
      ReadValue(P, Value);
      ModuleStartAddr := StartAddr;
    end;
  end;
  Result := ModuleStartAddr;
end;

function TBinDebugScanner.ProcNameFromAddr(Addr: DWORD): String;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function TBinDebugScanner.ProcNameFromAddr(Addr: DWORD; out Offset: Integer): String;
var
  P: Pointer;
  Value, FirstWord, SecondWord: Integer;
  CurrAddr, ModuleStartAddr, ItemAddr: DWORD;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  FirstWord := 0;
  SecondWord := 0;
  Offset := 0;
  if FCacheData then begin
    CacheProcNames;
    for Value := Length(FProcNames) - 1 downto 0 do
      if FProcNames[Value].Addr <= Addr then begin
        if FProcNames[Value].Addr >= ModuleStartAddr then begin
          FirstWord := FProcNames[Value].FirstWord;
          SecondWord := FProcNames[Value].SecondWord;
          Offset := Addr - FProcNames[Value].Addr;
        end;
        Break;
      end;
  end
  else begin
    P := MakePtr(PDbgHeader(FStream.Memory)^.Symbols);
    CurrAddr := 0;
    ItemAddr := 0;
    while ReadValue(P, Value) do begin
      Inc(CurrAddr, Value);
      if Addr < CurrAddr then begin
        if ItemAddr < ModuleStartAddr then begin
          FirstWord := 0;
          SecondWord := 0;
          Offset := 0;
        end;
        Break;
      end
      else begin
        ItemAddr := CurrAddr;
        ReadValue(P, Value);
        Inc(FirstWord, Value);
        ReadValue(P, Value);
        Inc(SecondWord, Value);
        Offset := Addr - CurrAddr;
      end;
    end;
  end;
  if FirstWord <> 0 then begin
    Result := DataToStr(FirstWord);
    if SecondWord <> 0 then
      Result := Result + '.' + DataToStr(SecondWord)
  end
  else
    Result := '';
end;

function TBinDebugScanner.ReadValue(var P: Pointer; var Value: Integer): Boolean;
var
  N: Integer;
  I: Integer;
  B: Byte;
begin
  N := 0;
  I := 0;
  repeat
    B := PByte(P)^;
    Inc(PByte(P));
    Inc(N, (B and $7F) shl I);
    Inc(I, 7);
  until B and $80 = 0;
  Value := N;
  Result := (Value <> MaxInt);
end;

function TBinDebugScanner.SourceNameFromAddr(Addr: DWORD): String;
var
  Value, Name: Integer;
  StartAddr, ModuleStartAddr, ItemAddr: DWORD;
  P: Pointer;
  Found: Boolean;
begin
  ModuleStartAddr := ModuleStartFromAddr(Addr);
  P := MakePtr(PDbgHeader(FStream.Memory)^.SourceNames);
  Name := 0;
  StartAddr := 0;
  ItemAddr := 0;
  Found := False;
  Value := 0;
  while ReadValue(P, Value) do begin
    Inc(StartAddr, Value);
    if Addr < StartAddr then begin
      if ItemAddr < ModuleStartAddr then
        Name := 0
      else
        Found := True;
      Break;
    end
    else begin
      ItemAddr := StartAddr;
      ReadValue(P, Value);
      Inc(Name, Value);
    end;
  end;
  if Found then
    Result := DataToStr(Name)
  else
    Result := '';
end;

{ TExceptFrame }

{$OVERFLOWCHECKS OFF}
function GetJmpDest(Jmp: PJmpInstruction): Pointer;
begin
  // TODO : 64 bit version
  if Jmp^.opCode = $E9 then
    Result := Pointer(TDebugAddr(Jmp) + TDebugAddr(Jmp^.distance) + 5)
  else if Jmp.opCode = $EB then
    Result := Pointer(TDebugAddr(Jmp) + TDebugAddr(ShortInt(Jmp^.distance)) + 2)
  else
    Result := Nil;
  if (Result <> Nil) and (PJmpTable(Result).OPCode = $25FF) then
    if not IsBadReadPtr(PJmpTable(Result).Ptr, SizeOf(Pointer)) then
      Result := Pointer(PDebugAddr(PJmpTable(Result).Ptr)^);
end;
{$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

constructor TExceptFrame.Create(AFrameLocation: Pointer; AExcDesc: PExcDesc);
begin
  inherited Create;
  FFrameKind := efkUnknown;
  FFrameLocation := AFrameLocation;
  FCodeLocation := Nil;
  AnalyseExceptFrame(AExcDesc);
end;

{$RANGECHECKS OFF}
procedure TExceptFrame.AnalyseExceptFrame(AExcDesc: PExcDesc);
var
  Dest: Pointer;
  LocInfo: TDebugLocationInfo;
  FixedProcedureName: String;
  DotPos, I: Integer;
begin
  Dest := GetJmpDest(@AExcDesc^.Jmp);
  if Dest <> Nil then begin
    // get frame kind
    LocInfo := GetLocationInfo(Dest);
    if CompareText(LocInfo.UnitName, 'system') = 0 then begin
      FixedProcedureName := LocInfo.ProcedureName;
      DotPos := Pos('.', FixedProcedureName);
      if DotPos > 0 then
        FixedProcedureName := Copy(FixedProcedureName, DotPos + 1, Length(FixedProcedureName) - DotPos);

      if CompareText(FixedProcedureName, '@HandleAnyException') = 0 then
        FFrameKind := efkAnyException
      else if CompareText(FixedProcedureName, '@HandleOnException') = 0 then
        FFrameKind := efkOnException
      else if CompareText(FixedProcedureName, '@HandleAutoException') = 0 then
        FFrameKind := efkAutoException
      else if CompareText(FixedProcedureName, '@HandleFinally') = 0 then
        FFrameKind := efkFinally;
    end;

    // get location
    if FFrameKind <> efkUnknown then begin
      FCodeLocation := GetJmpDest(PJmpInstruction(TDebugAddr(@AExcDesc^.Instructions)));
      if FCodeLocation = Nil then
        FCodeLocation := @AExcDesc^.Instructions;
    end
    else begin
      FCodeLocation := GetJmpDest(PJmpInstruction(TDebugAddr(AExcDesc)));
      if FCodeLocation = Nil then
        FCodeLocation := AExcDesc;
    end;

    // get on handlers
    if FFrameKind = efkOnException then begin
      SetLength(FExcTab, AExcDesc^.Cnt);
      for I := 0 to AExcDesc^.Cnt - 1 do begin
        if AExcDesc^.ExcTab[I].VTable = Nil then begin
          SetLength(FExcTab, I);
          Break;
        end
        else
          FExcTab[I] := AExcDesc^.ExcTab[I];
      end;
    end;
  end;
end;
{$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ON}

function TExceptFrame.Handles(ExceptObj: TObject): Boolean;
var
  Handler: Pointer;
begin
  Result := HandlerInfo(ExceptObj, Handler);
end;

{$OVERFLOWCHECKS OFF}
function TExceptFrame.HandlerInfo(ExceptObj: TObject; out HandlerAt: Pointer): Boolean;
var
  I: Integer;
  ObjVTable, VTable, ParentVTable: Pointer;
begin
  Result := FrameKind in [efkAnyException, efkAutoException];
  if not Result and (FrameKind = efkOnException) then begin
    HandlerAt := Nil;
    ObjVTable := Pointer(ExceptObj.ClassType);
    for I := Low(FExcTab) to High(FExcTab) do begin
      VTable := ObjVTable;
      Result := FExcTab[I].VTable = Nil;
      while (not Result) and (VTable <> Nil) do begin
        Result := (FExcTab[I].VTable = VTable) or
                  (PShortString(PPointer(PDebugAddr(FExcTab[I].VTable)^ + TDebugAddr(vmtClassName))^)^ = PShortString(PPointer(TDebugAddr(VTable) + TDebugAddr(vmtClassName))^)^);
        if Result then
          HandlerAt := FExcTab[I].Handler
        else begin
          ParentVTable := TClass(VTable).ClassParent;
          if ParentVTable = VTable then
            VTable := Nil
          else
            VTable := ParentVTable;
        end;
      end;
      if Result then
        Break;
    end;
  end
  else if Result then
    HandlerAt := FCodeLocation
  else
    HandlerAt := Nil;
end;
{$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF OVERFLOWCHECKS_ON}

{ TExceptFrameList }

constructor TExceptFrameList.Create(AIgnoreLevels: Integer);
begin
  inherited Create;
  FIgnoreLevels := AIgnoreLevels;
  TraceExceptionFrames;
end;

function TExceptFrameList.AddFrame(AFrame: PExcFrame): TExceptFrame;
begin
  Result := TExceptFrame.Create(AFrame, AFrame^.Desc);
  Add(Result);
end;

function TExceptFrameList.GetItems(Index: Integer): TExceptFrame;
begin
  Result := TExceptFrame(Get(Index));
end;

procedure TExceptFrameList.TraceExceptionFrames;
var
  ExceptionPointer: PExcFrame;
  Level: Integer;
  ModulesList: TDebugModuleInfoList;
begin
  Clear;
  ModulesList := GlobalModulesList.CreateModulesList;
  try
    Level := 0;
    ExceptionPointer := GetExceptionPointer;
    while TDebugAddr(ExceptionPointer) <> High(TDebugAddr) do begin
      if (Level >= IgnoreLevels) and ValidCodeAddr(TDebugAddr(ExceptionPointer^.Desc), ModulesList) then
        AddFrame(ExceptionPointer);
      Inc(Level);
      ExceptionPointer := ExceptionPointer^.next;
    end;
  finally
    GlobalModulesList.FreeModulesList(ModulesList);
  end;
end;

{ TGlobalStackList }

destructor TGlobalStackList.Destroy;
begin
  with LockList do try
    while Count > 0 do
      TObject(Items[0]).Free;
  finally
    UnlockList;
  end;
  inherited Destroy;
end;

procedure TGlobalStackList.AddObject(AObject: TStackBaseList);
var
  ReplacedObj: TObject;
begin
  AObject.FOnDestroy := ItemDestroyed;
  with LockList do try
    ReplacedObj := FindObject(AObject.ThreadID, TStackBaseListClass(AObject.ClassType));
    if ReplacedObj <> Nil then begin
      Remove(ReplacedObj);
      ReplacedObj.Free;
    end;
    Add(AObject);
  finally
    UnlockList;
  end;
end;

procedure TGlobalStackList.Clear;
begin
  with LockList do try
    while Count > 0 do
      TObject(Items[0]).Free;
    { The following call to Clear seems to be useless, but it deallocates memory
      by setting the lists capacity back to zero. For the runtime memory leak check
      within DUnit it is important that the allocated memory before and after the
      test is equal. }
    Clear; // do not remove
  finally
    UnlockList;
  end;
end;

function TGlobalStackList.FindObject(TID: DWORD; AClass: TStackBaseListClass): TStackBaseList;
var
  I: Integer;
  Item: TStackBaseList;
begin
  Result := Nil;
  with LockList do try
    if FTIDLocked and (GetCurrentThreadId = MainThreadID) then
      TID := FLockedTID;
    for I := 0 to Count - 1 do begin
      Item := Items[I];
      if (Item.ThreadID = TID) and (Item is AClass) then begin
        Result := Item;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

function TGlobalStackList.GetExceptStackInfo(TID: DWORD): TStackInfoList;
begin
  Result := TStackInfoList(FindObject(TID, TStackInfoList));
end;

function TGlobalStackList.GetLastExceptFrameList(TID: DWORD): TExceptFrameList;
begin
  Result := TExceptFrameList(FindObject(TID, TExceptFrameList));
end;

procedure TGlobalStackList.ItemDestroyed(Sender: TObject);
begin
  with LockList do try
    Remove(Sender);
  finally
    UnlockList;
  end;
end;

procedure TGlobalStackList.LockThreadID(TID: DWORD);
begin
  with LockList do try
    if GetCurrentThreadId = MainThreadID then begin
      FTIDLocked := True;
      FLockedTID := TID;
    end
    else
      FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

procedure TGlobalStackList.UnlockThreadID;
begin
  with LockList do try
    FTIDLocked := False;
  finally
    UnlockList;
  end;
end;

initialization
  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;
  Notifiers := TThreadList.Create;
  Filters := TThreadList.Create;

finalization
//  FinalizeLibrariesHookExcept;
  FreeThreadObjList(Notifiers);
  FreeThreadObjList(Filters);

end.

