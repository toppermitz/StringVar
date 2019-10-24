{Written by Janez Atmapuri Makovsek, 4th. June 2008, www.dewresearch.com
Stringlist is a replacement for TStringList for Delphi 2006
and after. Usage Example:

procedure TScriptingForm.Button1Click(Sender: TObject);
var strings: StringList;
    astr: string;
begin
    strings.Add('test1');
    strings.Add('test2');
    Caption := string(strings);
    RichEdit.Lines.AddStrings(strings);
end;

It is basically the same as TStringList except that it is a value
class. It does not have to be created, destroyed or put within
try/finally. This is done by the compiler for you. There are
virtually no special performance penalties for these to work.

You can use the same template to make value classes out of
your other objects also. This code is free to use. }

unit StringVar;

interface

uses Classes, SysUtils;

type

    TInterfacedStringList = class(TStringList, IInterface)
    protected
      FRefCount: Integer;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
      class function NewInstance: TObject; override;
      property RefCount: Integer read FRefCount;
    end;

    StringList = record
    private
      FData: TInterfacedStringList;//Interface reference ensures that Data will be freed when record gets out of scope
      InternalData: IInterface;
      function Get(Index: Integer): string;
      function GetCapacity: Integer;
      function GetCaseSensitive: Boolean;
      function GetCommaText: string;
      function GetCount: Integer;
      function GetDelimitedText: string;
      function GetDelimiter: Char;
      function GetDuplicates: TDuplicates;
      function GetLineBreak: string;
      function GetName(Index: Integer): string;
      function GetNameValueSeparator: Char;
      function GetObject(Index: Integer): TObject;
      function GetOnChange: TNotifyEvent;
      function GetOnChanging: TNotifyEvent;
      function GetQuoteChar: Char;
      function GetSorted: Boolean;
      function GetStrictDelimiter: Boolean;
      function GetTextStr: string;
      function GetValue(const Name: string): string;
      function GetValueFromIndex(Index: Integer): string;
      procedure Put(Index: Integer; const Value: string);
      procedure PutObject(Index: Integer; const Value: TObject);
      procedure SetCapacity(const Value: Integer);
      procedure SetCaseSensitive(const Value: Boolean);
      procedure SetCommaText(const Value: string);
      procedure SetDelimitedText(const Value: string);
      procedure SetDelimiter(const Value: Char);
      procedure SetDuplicates(const Value: TDuplicates);
      procedure SetLineBreak(const Value: string);
      procedure SetNameValueSeparator(const Value: Char);
      procedure SetOnChange(const Value: TNotifyEvent);
      procedure SetOnChanging(const Value: TNotifyEvent);
      procedure SetQuoteChar(const Value: Char);
      procedure SetSorted(const Value: Boolean);
      procedure SetStrictDelimiter(const Value: Boolean);
      procedure SetStringsAdapter(const Value: IStringsAdapter);
      procedure SetTextStr(const Value: string);
      procedure SetValue(const Name, Value: string);
      procedure SetValueFromIndex(Index: Integer; const Value: string);
      function Data: TStringList;
      function GetAdapter: IStringsAdapter;
    public
      class operator Equal(const Left, Right: StringList): Boolean;
      class operator Equal(const Left: StringList; Right: TStrings): Boolean;
      class operator Equal(const Left: TStrings; Right: StringList): Boolean;

      class operator notEqual(const Left, Right: StringList): Boolean;
      class operator notEqual(const Left: StringList; Right: TStrings): Boolean;
      class operator notEqual(const Left: TStrings; Right: StringList): Boolean;

//      class operator Add(const Left: string; const Right: StringList): StringList;
//      class operator Add(const Left: StringList; const Right: string): StringList;
      class operator Add(const Left: TStrings; const Right: StringList): StringList;
      class operator Add(const Left: StringList; const Right: TStrings): StringList;

      class operator Explicit(const AValue: StringList): string;
      class operator Implicit(const AValue: StringList): TStrings;
    public
      function Add(const S: string): Integer;
      function AddObject(const S: string; AObject: TObject): Integer;
      procedure Append(const S: string);
      procedure AddStrings(Strings: TStrings);
      procedure Assign(Source: TPersistent);
      procedure BeginUpdate;
      procedure Clear;
      procedure Delete(Index: Integer);
      procedure EndUpdate;
      function Equals(Strings: TStrings): Boolean;
      procedure Exchange(Index1, Index2: Integer);
      function GetEnumerator: TStringsEnumerator;
      function GetText: PChar;
      function IndexOf(const S: string): Integer;
      function IndexOfName(const Name: string): Integer;
      function IndexOfObject(AObject: TObject): Integer;
      procedure Insert(Index: Integer; const S: string);
      procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
      procedure LoadFromFile(const FileName: string);
      procedure LoadFromStream(Stream: TStream);
      procedure Move(CurIndex, NewIndex: Integer);
      procedure SaveToFile(const FileName: string);
      procedure SaveToStream(Stream: TStream);
      procedure SetText(Text: PChar);

      function Find(const S: string; var Index: Integer): Boolean;
      procedure Sort;
      procedure CustomSort(Compare: TStringListSortCompare);

      property Capacity: Integer read GetCapacity write SetCapacity;
      property CommaText: string read GetCommaText write SetCommaText;
      property Count: Integer read GetCount;
      property Delimiter: Char read GetDelimiter write SetDelimiter;
      property DelimitedText: string read GetDelimitedText write SetDelimitedText;
      property LineBreak: string read GetLineBreak write SetLineBreak;
      property Names[Index: Integer]: string read GetName;
      property Objects[Index: Integer]: TObject read GetObject write PutObject;
      property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
      property Values[const Name: string]: string read GetValue write SetValue;
      property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
      property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
      property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
      property Strings[Index: Integer]: string read Get write Put; default;
      property Text: string read GetTextStr write SetTextStr;
      property StringsAdapter: IStringsAdapter read GetAdapter write SetStringsAdapter;

      property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
      property Sorted: Boolean read GetSorted write SetSorted;
      property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
      property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
      property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    end;

implementation

{ StringList }

function StringList.Add(const S: string): Integer;
begin
  Result := Data.Add(S);
end;

function StringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Data.AddObject(S, AObject);
end;

procedure StringList.AddStrings(Strings: TStrings);
begin
  Data.AddStrings(Strings);
end;

procedure StringList.Append(const S: string);
begin
  Data.Append(S);
end;

procedure StringList.Assign(Source: TPersistent);
begin
  Data.Assign(Source);
end;

procedure StringList.BeginUpdate;
begin
  Data.BeginUpdate;
end;

procedure StringList.Clear;
begin
  Data.Clear;
end;

procedure StringList.CustomSort(Compare: TStringListSortCompare);
begin
  Data.CustomSort(Compare)
end;

function StringList.Data: TStringList;
begin
  if not Assigned(InternalData) then
  begin
      FData := TInterfacedStringList.Create;
      InternalData := FData;
  end;

  Result := FData;
end;

procedure StringList.Delete(Index: Integer);
begin
  Data.Delete(Index);
end;

procedure StringList.EndUpdate;
begin
  Data.EndUpdate;
end;

function StringList.Equals(Strings: TStrings): Boolean;
begin
  Result := Data.Equals(Strings);
end;

procedure StringList.Exchange(Index1, Index2: Integer);
begin
  Data.Exchange(Index1,Index2);
end;

function StringList.Find(const S: string; var Index: Integer): Boolean;
begin
  Result := Data.Find(S, Index)
end;

function StringList.Get(Index: Integer): string;
begin
  Result := Data[Index];
end;

function StringList.GetAdapter: IStringsAdapter;
begin
  Result := Data.StringsAdapter;
end;

function StringList.GetCapacity: Integer;
begin
  Result := Data.Capacity;
end;

function StringList.GetCaseSensitive: Boolean;
begin
  result := Data.CaseSensitive;
end;

function StringList.GetCommaText: string;
begin
  Result := Data.CommaText;
end;

function StringList.GetCount: Integer;
begin
  Result := Data.Count;
end;

function StringList.GetDelimitedText: string;
begin
  REsult := Data.DelimitedText;
end;

function StringList.GetDelimiter: Char;
begin
  Result := Data.Delimiter;
end;

function StringList.GetDuplicates: TDuplicates;
begin
    Result := Data.Duplicates;
end;

function StringList.GetEnumerator: TStringsEnumerator;
begin
   Result := Data.GetEnumerator;
end;

function StringList.GetLineBreak: string;
begin
  Result := Data.LineBreak;
end;

function StringList.GetName(Index: Integer): string;
begin
  Result := Data.Names[Index];
end;

function StringList.GetNameValueSeparator: Char;
begin
  Result := Data.NameValueSeparator;
end;

function StringList.GetObject(Index: Integer): TObject;
begin
  Result := Data.Objects[Index];
end;

function StringList.GetOnChange: TNotifyEvent;
begin
  Result := Data.OnChange;
end;

function StringList.GetOnChanging: TNotifyEvent;
begin
  Result := Data.OnChanging;
end;

function StringList.GetQuoteChar: Char;
begin
  Result := Data.QuoteChar;
end;

function StringList.GetSorted: Boolean;
begin
  Result := Data.Sorted;
end;

function StringList.GetStrictDelimiter: Boolean;
begin
  Result := Data.StrictDelimiter;
end;

function StringList.GetText: PChar;
begin
  Result := StrNew(PChar(Data.Text));
end;

function StringList.GetTextStr: string;
begin
  Result := Data.Text;
end;

function StringList.GetValue(const Name: string): string;
begin
  Result := Data.Values['Name'];
end;

function StringList.GetValueFromIndex(Index: Integer): string;
begin
  Result := Data.ValueFromIndex[Index];
end;

function StringList.IndexOf(const S: string): Integer;
begin
  Result := Data.IndexOf(s);
end;

function StringList.IndexOfName(const Name: string): Integer;
begin
  Result := Data.IndexOfName(Name);
end;

function StringList.IndexOfObject(AObject: TObject): Integer;
begin
  Result := Data.IndexOfObject(AObject);
end;

procedure StringList.Insert(Index: Integer; const S: string);
begin
  Data.Insert(Index, s);
end;

procedure StringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Data.Insert(Index, s);
end;

procedure StringList.LoadFromFile(const FileName: string);
begin
  Data.LoadFromFile(FileName);
end;

procedure StringList.LoadFromStream(Stream: TStream);
begin
  Data.LoadFromStream(Stream);
end;

procedure StringList.Move(CurIndex, NewIndex: Integer);
begin
  Data.Move(CurIndex, newIndex);
end;

procedure StringList.Put(Index: Integer; const Value: string);
begin
  Data[Index] := Value;
end;

procedure StringList.PutObject(Index: Integer; const Value: TObject);
begin
  Data.Objects[Index] := Value;
end;

procedure StringList.SaveToFile(const FileName: string);
begin
  Data.SaveToFile(FileName);
end;

procedure StringList.SaveToStream(Stream: TStream);
begin
  Data.SaveToStream(Stream);
end;

procedure StringList.SetCapacity(const Value: Integer);
begin
   Data.Capacity := Value; 
end;

procedure StringList.SetCaseSensitive(const Value: Boolean);
begin
  Data.CaseSensitive := Value;
end;

procedure StringList.SetCommaText(const Value: string);
begin
  Data.CommaText := Value;
end;

procedure StringList.SetDelimitedText(const Value: string);
begin
  Data.DelimitedText := Value;
end;

procedure StringList.SetDelimiter(const Value: Char);
begin
  Data.Delimiter := Value;
end;

procedure StringList.SetDuplicates(const Value: TDuplicates);
begin
  Data.Duplicates := Value;
end;

procedure StringList.SetLineBreak(const Value: string);
begin
  Data.LineBreak := Value;
end;

procedure StringList.SetNameValueSeparator(const Value: Char);
begin
  Data.NameValueSeparator := Value;
end;

procedure StringList.SetOnChange(const Value: TNotifyEvent);
begin
  Data.OnChange := Value;
end;

procedure StringList.SetOnChanging(const Value: TNotifyEvent);
begin
  Data.OnChanging := Value;
end;

procedure StringList.SetQuoteChar(const Value: Char);
begin
  Data.QuoteChar := Value;
end;

procedure StringList.SetSorted(const Value: Boolean);
begin
  Data.Sorted := Value;
end;

procedure StringList.SetStrictDelimiter(const Value: Boolean);
begin
  Data.StrictDelimiter := Value;
end;

procedure StringList.SetStringsAdapter(const Value: IStringsAdapter);
begin
   Data.StringsAdapter := Value;
end;

procedure StringList.SetText(Text: PChar);
begin
  Data.Text := Text;
end;

procedure StringList.SetTextStr(const Value: string);
begin
  Data.Text := Value;
end;

procedure StringList.SetValue(const Name, Value: string);
begin
  Data.Values[Name] := Value;
end;

procedure StringList.SetValueFromIndex(Index: Integer; const Value: string);
begin
  Data.ValueFromIndex[Index] := Value;
end;

procedure StringList.Sort;
begin
  Data.Sort;
end;


class operator StringList.Equal(const Left, Right: StringList): Boolean;
begin
  Result := Left.Equals(Right);
end;

class operator StringList.Equal(const Left: StringList; Right: TStrings): Boolean;
begin
  Result := Left.Equals(Right);
end;

class operator StringList.Equal(const Left: TStrings; Right: StringList): Boolean;
begin
  Result := Left.Equals(Right);
end;

class operator StringList.notEqual(const Left, Right: StringList): Boolean;
begin
  Result := Left <> Right;
end;

class operator StringList.notEqual(const Left: StringList; Right: TStrings): Boolean;
begin
  Result := Left <> Right;
end;

class operator StringList.notEqual(const Left: TStrings; Right: StringList): Boolean;
begin
  Result := Left <> Right;
end;

class operator StringList.Add(const Left: TStrings; const Right: StringList): StringList;
begin
  Result.Data.AddStrings(Left);
  Result.Data.AddStrings(Right);
end;

class operator StringList.Add(const Left: StringList; const Right: TStrings): StringList;
begin
  Result.Data.AddStrings(Left);
  Result.Data.AddStrings(Right);
end;

class operator StringList.Explicit(const AValue: StringList): string;
begin
  Result := AValue.Text;
end;

class operator StringList.Implicit(const AValue: StringList): TStrings;
begin
  Result := AValue.Data;
end;

const kernel = 'kernel32.dll';

function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedIncrement';

function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedDecrement';

{ TInterfacedStringList }

procedure TInterfacedStringList.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

procedure TInterfacedStringList.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise Exception.Create('Invalid pointer operation!');
end;

class function TInterfacedStringList.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedStringList(Result).FRefCount := 1;
end;

function TInterfacedStringList.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedStringList._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedStringList._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.
