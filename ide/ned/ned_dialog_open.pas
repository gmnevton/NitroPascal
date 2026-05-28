unit ned_dialog_open;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  UCL.Form,
  UCL.ThemeManager,
  UCL.QuickButton,
  UCL.Classes,
  UCL.TitleBar,
  UCL.Edit,
  UCL.Button,
  UCL.Panel,
  UCL.Text,
  SplitEx,
  uFolders,
  ImageList,
  ImgList,
  ShlObj,
  ShellAPI;

type
  TNEDDialogOpen = class(TUForm)
    UTitleBar1: TUTitleBar;
    btnClose: TUQuickButton;
    UPanel1: TUPanel;
    UPanel2: TUPanel;
    UPanel3: TUPanel;
    UPanel4: TUPanel;
    btnCancel: TUButton;
    btnOpen: TUButton;
    edtPath: TUEdit;
    txtPath: TUText;
    btnUpDirectory: TUQuickButton;
    SplitterEx1: TSplitterEx;
    ImageList1: TImageList;
    //
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnUpDirectoryClick(Sender: TObject);
  private
    FCanClose: Boolean;
    FExecutionResult: TModalResult;
    //
    FMainForm: TUForm;
    FOldMainFormResizeEvent: TNotifyEvent;
    //
    FolderView: TEntryView;
    FilesView: TEntryView;
    //
    FPath: String;
    FFileExtension: String;
    //
    procedure MainFormResize(Sender: TObject);
    function GetInterfaceForObj(const IDL: pItemIDList): IUnknown;
    procedure GetDesktopIcons;
    procedure GetFolders;
    procedure ListDirs(const Folders: TEntryItems);
    procedure ListFiles(const Files: TEntryItems);
    procedure GetFilesList;
    procedure GetFolderIcon(const IDL: PItemIDList; Item: TEntryItem);
    procedure GetItemIcon(const IDL: PItemIDList; Item: TEntryItem);
    function GetItemAttributes(const IDL: PItemIDList; var Info: TSHFileInfo): Boolean;
    procedure GetChildren(ShellFolder: IShellFolder; const IDL: PItemIDList; Entry: TEntryItem);
    function GetPathFromPIDL(const IDL: PItemIDList): String;
    function GetPIDLFromPath(const Path: String): PItemIDList;
    function GetDirFromSpecialFolder(const Folder: Integer): String;
    procedure TrySelectMatchingFolder;
    procedure FolderChanged(Sender: TObject);
    procedure FilesViewItemGetType(Sender: TObject; Item: TEntryItem; var ItemType: TEntryItemTypeEnum);
    procedure FilesViewItemSelection(Sender: TObject; Item: TEntryItem; IsSubDirectory: Boolean);
  protected
    function CanDrawBorder: Boolean; override;
    procedure DoDrawBorder; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function Execute(const DefaultPath: String = ''; const DefaultFileExt: String = '.*'): Boolean;
    //
    property FileName: String read FPath;
  end;

implementation

{$R *.dfm}

uses
  Types,
  Graphics,
  ActiveX,
  StrUtils,
  IOUtils,
  UCL.FormOverlay;

//type
//  TWinControlAccess = class(TWinControl);

const
  CLSID_3DObjects                 : TGUID = '{0DB7E03F-FC29-4DC6-9020-FF41B59E513A}';
  CLSID_UserFolder                : TGUID = '{59031A47-3F72-44A7-89C5-5595FE6B30EE}';
  CLSID_OneDrive                  : TGUID = '{018D5C66-4533-4307-9B53-224DE2ED1FE6}';
  CLSID_Libraries                 : TGUID = '{031E4825-7B94-4DC3-B131-E946B44C8DD5}';
  CLSID_ControlPanel              : TGUID = '{21EC2020-3AEA-1069-A2DD-08002B30309D}';
  CLSID_ControlPanel_CategoryView : TGUID = '{26EE0668-A00A-44D7-9371-BEB064C98683}';

function CompareGUID(const g1, g2: TGuid): Boolean;
begin
  Result := CompareMem(@g1, @g2, SizeOf(TGuid));
end;

procedure ReleaseCOM(var ComInterface);
begin
  if Assigned(IUnknown(ComInterface)) then
    IUnknown(ComInterface) := Nil;
end;

procedure FreeItemIDList(var PIDL: PItemIDList);
Var
  ppMalloc: IMalloc;
Begin
  SHGetMalloc(ppMalloc);
  ppMalloc.Free(PIDL);
  PIDL := Nil;
  ppMalloc := Nil;
End;
{
function IsSupportedExtension(const FileName: String): Boolean;
var
  ext: String;
begin
  Result:=False;
  ext:=LowerCase(FileName);
  if (ext <> 'hashgen.exe') and ((ext <> 'hashgenui.exe')) and (ext <> sHashesFileName) then begin
    ext:='';
    ext:=ExtractFileExt(FileName);
    try
      Result:=((ext = '.exe') or
               (ext = '.manifest') or
               (ext = '.bpl') or
               (ext = '.dll') or
               (ext = '.jar') or
               // configs
               (IniFiles and (ext = '.ini')) or
               (ext = '.json') or
               // program data
               (ext = '.xml') or
               (ext = '.xsd') or
               (ext = '.xsl') or
               (ext = '.otf') or
               (ext = '.ttf') or
               // apps
               (ext = '.pdf') or
               (ext = '.doc') or
               (ext = '.docx') or
               (ext = '.xls') or
               (ext = '.xlsx') or
               // certs
               (ext = '.crt') or
               (ext = '.pem'));
    finally
      ext:='';
    end;
  end;
end;
}
procedure SearchForFiles(APath, AExt: String; List: TStringList);
var
  SearchRec: TSearchRec;
  Result: Integer;
begin
  if APath[Length(APath)] <> '\' then
    APath := APath + '\';
  if SysUtils.FindFirst(APath + '*' + AExt, faAnyFile - faDirectory, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory = 0){ and IsSupportedExtension(SearchRec.Name)} then begin
        List.Add(SearchRec.Name);
      end;
      Result:=SysUtils.FindNext(SearchRec);
    until Result <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure SearchForDirectories(APath: String; List: TStringList{; Level: Integer = 0});
var
  SearchRec: TSearchRec;
  Result: Integer;
begin
//    if Level > 1 then
//      Exit;
  if APath[Length(APath)] <> '\' then
    APath := APath + '\';
{$IFDEF DEBUG}
  OutputDebugString(PChar('Found dir: ''' + APath + ''''));
//      Sleep(10);
{$ENDIF}
//  List.Add(APath);
  if SysUtils.FindFirst(APath + '*.*', faDirectory, SearchRec) = 0 then begin
    repeat
      if SearchRec.Attr and faDirectory = faDirectory then begin
        if (Length(SearchRec.Name) > 2) or
           (((Length(SearchRec.Name) = 1) and (SearchRec.Name[1] <> '.')) and
            ((Length(SearchRec.Name) = 2) and (Word(PChar(@SearchRec.Name[1])) <> $2E2E{'..'}))) then begin
          List.Add(SearchRec.Name);
          //ListDirs(APath + SearchRec.Name, List{, Level + 1});
        end;
      end;
      Result:=SysUtils.FindNext(SearchRec);
    until Result <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

{ TNEDDialogOpen }

constructor TNEDDialogOpen.Create(AOwner: TComponent);
begin
  inherited;
  FFormState := FFormState - [fsVisible];
  FCanClose := False;
  FExecutionResult := mrNone;
  //
  FolderView := TEntryView.Create(Self);
  FolderView.Name := 'FolderView';
  FolderView.Parent := UPanel2;
  FolderView.Align := alClient;
  FolderView.ParentColor := True;
  FolderView.BorderStyle := bsNone;
  FolderView.EntryImages := ImageList1;
  //FolderView.ItemImages := ImageList1;
  FolderView.TabStop := True;
  FolderView.OnChange := FolderChanged;
//  FolderView.Show;
  //
  FilesView := TEntryView.Create(Self);
  FilesView.Name := 'FilesView';
  FilesView.Parent := Self;
  FilesView.Align := alClient;
  FilesView.ParentColor := True;
  FilesView.BorderStyle := bsNone;
  FilesView.EntryImages := ImageList1;
  //FilesView.ItemImages := ImageList1;
  FilesView.TabStop := True;
  FilesView.OnItemGetType := FilesViewItemGetType;
  FilesView.OnItemSelection := FilesViewItemSelection;
//  FilesView.Show;

  FPath := '';
  FFileExtension := '';
end;

destructor TNEDDialogOpen.Destroy;
begin
  FPath := '';
  FFileExtension := '';
  FCanClose := False;
  FExecutionResult := mrNone;
  FilesView.Free;
  FolderView.Free;
  inherited;
end;

function TNEDDialogOpen.CanDrawBorder: Boolean;
begin
  Result := True;
end;

procedure TNEDDialogOpen.DoDrawBorder;
begin
  UpdateBorderColor;
  Canvas.Pen.Color := BorderColor;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height - 1);  // left
  Canvas.LineTo(Width - 1, Height - 1); // bottom
  Canvas.LineTo(Width - 1, 0); // right
end;

procedure TNEDDialogOpen.FormCreate(Sender: TObject);
begin
  FFormState := FFormState - [fsVisible];
end;

procedure TNEDDialogOpen.FormResize(Sender: TObject);
begin
  btnCancel.Width := Self.Width div 2;
end;

procedure TNEDDialogOpen.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    Key := 0; // prevent default handling
    FExecutionResult := mrCancel;
    FCanClose := True;
  end;
end;

//var
//  FocusedControl: TWinControl;
//  NextControl: TWinControl;
//begin
//  if Key = VK_TAB then begin
//    FocusedControl := ActiveControl;
//    if FocusedControl <> Nil then begin
//      NextControl := TWinControlAccess(Self).FindNextControl(FocusedControl, not (ssShift in Shift), True, True);
//
//      if Assigned(NextControl) then
//        NextControl.SetFocus;
//
//      Key := 0; // prevent default handling
//    end;
//  end;
//end;

procedure TNEDDialogOpen.MainFormResize(Sender: TObject);
begin
//  Self.Margins.Top := Round(FMainForm.Height * 0.2);
//  Self.Margins.Left := (FMainForm.Width - 600) div 2;
//  Self.Margins.Right := Self.Margins.Left;
//  Self.Margins.Bottom := Self.Margins.Top;
  //
  if Assigned(FOldMainFormResizeEvent) then
    FOldMainFormResizeEvent(Sender);
end;

function GetShellItemName(Folder: IShellFolder; ItemIdList: PItemIDList; Flags: DWord): String;
// Flags:  SHGDN_NORMAL or SHGDN_INFOLDER or SHGDN_FORPARSING
var
  StrResult: TStrRet;
label
  Again;
begin
  Result := '';
Again:
  FillChar(StrResult, SizeOf(StrResult), #0);
  Folder.GetDisplayNameOf(ItemIdList, Flags, StrResult);
  case StrResult.uType of
    STRRET_WSTR: begin
      if Assigned(StrResult.pOleStr) then
        Result := WideCharToString(StrResult.pOleStr)
      else if Flags = SHGDN_NORMAL then begin
        Flags := SHGDN_FORPARSING;
        goto Again;
      end;
    end;
    STRRET_OFFSET: Result := String(PChar(ItemIdList) + StrResult.uOffset);
    STRRET_CSTR: Result := String(StrResult.cStr);
  else
    // If no normal name is available try to get the Parsing-name
    if Flags = SHGDN_NORMAL then begin
      Flags := SHGDN_FORPARSING;
      goto Again;
    end;
  end;
end;

function TNEDDialogOpen.GetInterfaceForObj(const IDL: pItemIDList): IUnknown;
var
  Desktop: IShellFolder;
  DesktopIDL: pItemIDList;
begin
  SHGetDesktopFolder(Desktop);
  try
    // Check if the current pItemIDList is the Desktop (needs special handling)
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, DesktopIDL);
    try
      If Desktop.CompareIDs(0, DesktopIDL, IDL) = 0 then
        Result := Desktop
      else
        Desktop.BindToObject(IDL, Nil, IID_IShellFolder, Result);
    finally
      FreeItemIDList(DesktopIDL);
//      if Assigned(DesktopIDL) then
//        DesktopIDL := Nil;
    end;
  finally
    Desktop := Nil;
  end;
end;

procedure TNEDDialogOpen.GetDesktopIcons;
var
  FI: ShellApi.TSHFileInfo;
  Flags: Windows.UINT;
  TmpHandle: THandle;
begin
  Flags := SHGFI_SYSICONINDEX;
  //Flags := Flags or SHGFI_LARGEICON
  Flags := Flags or SHGFI_SMALLICON;
  ImageList1.Clear;
  TmpHandle := ShellApi.SHGetFileInfo(PChar(''), 0, FI, SizeOf(FI), Flags);
  if TmpHandle <> 0 then begin
    ImageList1.Handle := TmpHandle;
    ImageList1.ShareImages := True;
  end;
end;

procedure TNEDDialogOpen.GetFolders;
var
  Parent: IShellFolder;
  Root: IShellFolder;
  IDL, CloneIDL: PItemIDList;
  Attr: Cardinal;
  Folder: TEntryItem;
  temp: String;
  Child: PItemIDList;
begin
  FolderView.BeginUpdate;
  try
    FolderView.Entries.Clear;
    Root := Nil;
  //  SHGetDesktopFolder(Desktop);
    if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_DRIVES, IDL)) and Succeeded(SHBindToParent(IDL, IID_IShellFolder, Pointer(Parent), Child)) then begin
      Root := GetInterfaceForObj(IDL) as IShellFolder;
      if Succeeded(Parent.GetAttributesOf(1, IDL, Attr)) then begin
        temp := GetShellItemName(Parent, IDL, SHGDN_NORMAL);
        Folder := FolderView.Entries.Add;
        Folder.Caption := temp;
        CloneIDL := ILClone(IDL);
        Folder.Data := CloneIDL;
        GetFolderIcon(IDL, Folder);
        GetChildren(Root, IDL, Folder);
          //
  //        if (HasAttribute(SFGAO_FOLDER)) then
  //          SHGetFileInfo(Pointer(AbsoluteIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
      end;
      FreeItemIDList(IDL);
    end;
  finally
    FolderView.EndUpdate;
    //
    FolderView.ActiveIndex := 0;
    if FolderView.Selected <> Nil then
      FolderView.Selected.Expand;
  end;
end;

procedure TNEDDialogOpen.ListDirs(const Folders: TEntryItems);
var
  List: TStringList;
  Item: TEntryItem;
  path: String;
  IDL, CloneIDL: PItemIDList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    SearchForDirectories(FPath, List);
    for i := 0 to List.Count - 1 do begin
      path := List.Strings[i];
      Item := Folders.Add;
      Item.Caption := path;
      IDL := GetPIDLFromPath(FPath + path);
      CloneIDL := ILClone(IDL);
      Item.Data := CloneIDL;
      GetItemIcon(IDL, Item);
      FreeItemIDList(IDL);
    end;
  finally
    List.Free;
  end;
end;

procedure TNEDDialogOpen.ListFiles(const Files: TEntryItems);
var
  List: TStringList;
  Item: TEntryItem;
  path: String;
  IDL, CloneIDL: PItemIDList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    SearchForFiles(FPath, FFileExtension, List);
    for i := 0 to List.Count - 1 do begin
      path := List.Strings[i];
      Item := Files.Add;
      Item.Caption := path;
      IDL := GetPIDLFromPath(FPath + path);
      CloneIDL := ILClone(IDL);
      Item.Data := CloneIDL;
      GetItemIcon(IDL, Item);
      FreeItemIDList(IDL);
    end;
  finally
    List.Free;
  end;
end;

procedure TNEDDialogOpen.GetFilesList;
begin
  FilesView.BeginUpdate;
  try
    FilesView.Clear;
    ListDirs(FilesView.Entries);
    ListFiles(FilesView.Entries);
  finally
    FilesView.EndUpdate;
    FilesView.ActiveIndex := 0;
  end;
end;

procedure TNEDDialogOpen.GetFolderIcon(const IDL: PItemIDList; Item: TEntryItem);
var
  FileInfo: TSHFileInfo;
begin
  if Assigned(IDL) then begin
    SHGetFileInfo(Pointer(IDL), SFGAO_SHARE, FileInfo, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    Item.ImageIndex := FileInfo.iIcon;
  end
  else
    Item.ImageIndex := -1;
end;

procedure TNEDDialogOpen.GetItemIcon(const IDL: PItemIDList; Item: TEntryItem);
var
  FileInfo: TSHFileInfo;
begin
  if Assigned(IDL) then begin
    SHGetFileInfo(Pointer(IDL), 0{SFGAO_SHARE}, FileInfo, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
    Item.ImageIndex := FileInfo.iIcon;
  end
  else
    Item.ImageIndex := -1;
end;

function TNEDDialogOpen.GetItemAttributes(const IDL: PItemIDList; var Info: TSHFileInfo): Boolean;
begin
  Result := True;
  if Assigned(IDL) then begin
    FillChar(Info, SizeOf(TSHFileInfo), 0);
    Result := SHGetFileInfo(Pointer(IDL), 0{SFGAO_SHARE}, Info, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_ATTRIBUTES) <> 0;
  end
  else
    Result := False;
end;

function TNEDDialogOpen.GetPathFromPIDL(const IDL: PItemIDList): String;
begin
  Result := '';
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IDL, PChar(Result)) then begin
    SetLength(Result, StrLen(PChar(Result)));
    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
    Result := '';
end;

function TNEDDialogOpen.GetPIDLFromPath(const Path: String): PItemIDList;
begin
  Result := ILCreateFromPath(PChar(Path));
end;

function TNEDDialogOpen.GetDirFromSpecialFolder(const Folder: Integer): String;
var
  SpecialDirIDL: pItemIDList;
begin
  Result := '';
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderLocation(0, Folder, SpecialDirIDL);
  SHGetPathFromIDList(SpecialDirIDL, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
  FreeItemIDList(SpecialDirIDL);
end;

procedure TNEDDialogOpen.TrySelectMatchingFolder;
var
  Item: TEntryItem;
  IDL: PItemIDList;
  LPath, LLastPath: String;
  Idx: Integer;
begin
  Idx := -1;
  LPath := '';
  LLastPath := '';
  Item := FolderView.GetFirstEntry;
  while Item <> Nil do begin
    IDL := Item.Data;
    LPath := GetPathFromPIDL(IDL);
    if (Length(LPath) > 0) and StartsText(LPath, FPath) and (Item.TreeIndex > -1) and (Length(LPath) > Length(LLastPath)) then begin
      LLastPath := LPath;
      Idx := Item.TreeIndex;
    end;
    Item := FolderView.GetNextEntry(Item);
  end;
  if Idx > -1 then
    FolderView.ActiveIndex := Idx;
  LPath := '';
  LLastPath := '';
end;

procedure TNEDDialogOpen.FolderChanged(Sender: TObject);
var
  Item: TEntryItem;
  IDL: PItemIDList;
  LPath: String;
begin
  LPath := '';
  Item := FolderView.Selected;
  if Item <> Nil then begin
    IDL := Item.Data;
    LPath := GetPathFromPIDL(IDL);
    if Length(LPath) > 0 then begin
      FPath := LPath;
      txtPath.Caption := FPath;
      GetFilesList;
    end;
  end;
end;

procedure TNEDDialogOpen.FilesViewItemGetType(Sender: TObject; Item: TEntryItem; var ItemType: TEntryItemTypeEnum);
var
  IDL: PItemIDList;
  FileInfo: TSHFileInfo;
begin
  IDL := Item.Data;
  if GetItemAttributes(IDL, FileInfo) then begin
    if FileInfo.dwAttributes and SFGAO_FOLDER = SFGAO_FOLDER then
      ItemType := etSubDirectory
    else
      ItemType := etFile;
  end;
end;

procedure TNEDDialogOpen.FilesViewItemSelection(Sender: TObject; Item: TEntryItem; IsSubDirectory: Boolean);
var
  IDL: PItemIDList;
  LPath: String;
begin
  if IsSubDirectory then begin
    LPath := '';
    IDL := Item.Data;
    LPath := GetPathFromPIDL(IDL);
    if Length(LPath) > 0 then begin
      FPath := LPath;
      txtPath.Caption := FPath;
      GetFilesList;
    end;
  end;
end;

procedure TNEDDialogOpen.GetChildren(ShellFolder: IShellFolder; const IDL: PItemIDList; Entry: TEntryItem);
const
  Item_Type_Attributes =
    0
    or SFGAO_LINK
    or SFGAO_SHARE
    or SFGAO_GHOSTED
    or SFGAO_FILESYSANCESTOR
    or SFGAO_FOLDER
    or SFGAO_FILESYSTEM
    or SFGAO_REMOVABLE
    or SFGAO_COMPRESSED
  ;

var
  Objects: IEnumIdList;
  Flags: DWord;
  AbsIDL, ItemIDL: PItemIDList;
  DummyResult: ULONG;
  attr: UInt;
  descrID: HResult;
  pdid: TSHDescriptionID;
  Item: TEntryItem;
  temp: String;
begin
  Flags := 0;
  Flags := Flags or SHCONTF_FOLDERS;
  Flags := Flags or SHCONTF_NONFOLDERS;
  Flags := Flags or SHCONTF_INCLUDEHIDDEN;
  Flags := Flags or SHCONTF_SHAREABLE;
  Flags := Flags or SHCONTF_STORAGE;
  //
  if Succeeded(ShellFolder.EnumObjects(0, Flags, Objects)) then try
    Objects.Reset;
    DummyResult := 1;
    while (DummyResult <= 1) and (Objects.Next(1, ItemIDL, DummyResult) = NOERROR) do begin
      attr := Item_Type_Attributes;
      if not (ShellFolder.GetAttributesOf(1, ItemIDL, attr) = NOERROR) then
        attr := 0;

      descrID := -1;
      if SHGetDataFromIDListA(ShellFolder, ItemIDL, SHGDFIL_DESCRIPTIONID, @pdid, SizeOf(TSHDescriptionID)) = 0 then
        descrID := pdid.dwDescriptionID;

      if (attr and (SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR)) = 0 then begin
        temp := GetShellItemName(ShellFolder, ItemIDL, SHGDN_FORPARSING);
        if Pos('\\?\', temp) > 0 then // Device
          Continue;
      end;

//      if descrID = SHDID_COMPUTER_NETDRIVE then
//        Continue;

      if CompareGUID(pdid.Id, CLSID_OneDrive) then
        Continue;

      if CompareGUID(pdid.Id, CLSID_3DObjects) or
//         CompareGUID(pdid.Id, CLSID_UserFolder) or
         CompareGUID(pdid.Id, CLSID_Libraries) or
         CompareGUID(pdid.Id, CLSID_ControlPanel) or
         CompareGUID(pdid.Id, CLSID_ControlPanel_CategoryView) then
        Continue;

//      if (attr and SFGAO_FOLDER <> 0) and CompareGUID(pdid.Id, GUID_NULL) then
//        Continue;

      Item := Entry.Items.Add;
      temp := GetShellItemName(ShellFolder, ItemIDL, SHGDN_NORMAL);
      Item.Caption := temp;
      AbsIDL := ILClone(ILCombine(IDL, ItemIDL));
      Item.Data := AbsIDL;
      GetItemIcon(AbsIDL, Item);

//      HC := HasAttribute( TreeNode.fNodeInfo.Attributes, SFGAO_HASSUBFOLDER );
    end;
  finally
    ReleaseCOM(Objects);
  end;
end;

function TNEDDialogOpen.Execute(const DefaultPath: String = ''; const DefaultFileExt: String = '.*'): Boolean;
var
  t, l , w, h: Integer;
begin
  Result := False;
  FMainForm := TUForm(Application.MainForm);

  Self.Hide;
  Application.ProcessMessages;
  FOldMainFormResizeEvent := FMainForm.OnResize;
  try
    // center on main form
    FMainForm.OnResize := MainFormResize;

    FMainForm.DisableAlign;
    try
      Self.Parent := FMainForm;

      t := Round(FMainForm.Height * 0.2);
      l := Round(FMainForm.Width * 0.6) div 2;
      w := FMainForm.Width - Self.Left * 2;
      h := FMainForm.Height - Self.Top * 2;

      Self.SetBounds(l, t, w, h);

      Self.Align := alCustom;
      //
      // @TODO: make TitleBar background color setable by user
      //UTitleBar1.Color := SelectAccentColor(GetCommonThemeManager, $00404040);
      //
      GetDesktopIcons;
      //
      GetFolders;
      //
      if Length(DefaultPath) > 0 then
        FPath := IncludeTrailingPathDelimiter(DefaultPath)
      else
        FPath := GetDirFromSpecialFolder(CSIDL_PERSONAL); // get "My Documents" as default
      if Length(DefaultFileExt) > 0 then
        FFileExtension := DefaultFileExt
      else
        FFileExtension := '.*'; // get all files as default if none was specified
      //
      txtPath.Caption := FPath;
      GetFilesList;
      //
      TrySelectMatchingFolder;
      //
      FMainForm.OverlayType := otTransparent;
      //
      FolderView.ActiveColor := SelectAccentColor(GetCommonThemeManager, FolderView.ActiveColor);
      FolderView.SelectedColor := FolderView.ActiveColor;
      FilesView.ActiveColor := FolderView.ActiveColor;
      FilesView.SelectedColor := FolderView.ActiveColor;
      //FolderView.MultiSelect := True;
      //
      FolderView.Show;
      FilesView.Show;
      Application.ProcessMessages;
    finally
      FMainForm.EnableAlign;
      Application.ProcessMessages;
    end;
    //
    // it would be good to show FolderView first entry as expanded - done
    //
    try
      Self.Show;
      Self.BringToFront;
      Self.Invalidate;
      //Self.SetFocus;
      FilesView.SetFocus;
      //Self.KeyPreview := False;
      //
      // block execution at this point
      FCanClose := False;
      repeat
        Application.HandleMessage;
      until FCanClose or Application.Terminated;
    finally
      FMainForm.OverlayType := otNone;
      Result := FExecutionResult = mrOk;
    end;
  finally
    FMainForm.OnResize := FOldMainFormResizeEvent;
  end;
end;

procedure TNEDDialogOpen.btnCloseClick(Sender: TObject);
begin
  FExecutionResult := mrClose;
  FCanClose := True;
end;

procedure TNEDDialogOpen.btnCancelClick(Sender: TObject);
begin
  FExecutionResult := mrCancel;
  FCanClose := True;
end;


procedure TNEDDialogOpen.btnOpenClick(Sender: TObject);
begin
  FExecutionResult := mrOk;
  FCanClose := True;
end;

procedure TNEDDialogOpen.btnUpDirectoryClick(Sender: TObject);
var
  IDL: PItemIDList;
  LPath: String;
begin
  LPath := IncludeTrailingPathDelimiter(TDirectory.GetParent(ExcludeTrailingPathDelimiter(FPath)));
  if LPath <> FPath then begin
    if Length(LPath) > 0 then begin
      IDL := GetPIDLFromPath(LPath);
      if IDL <> Nil then begin
        FPath := LPath;
        txtPath.Caption := FPath;
        TrySelectMatchingFolder;
        GetFilesList;
      end;
    end;
  end;
end;

end.

