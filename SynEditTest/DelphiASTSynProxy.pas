(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSCompressedDiffSynProxy.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2014/02/26  USchuster - new unit
2014/03/01  USchuster - changes for D2009+/UniSynEdit
2014/03/07  USchuster - changes to make it actually work with UniSynEdit

-----------------------------------------------------------------------------*)

unit DelphiASTSynProxy;

{.$I jedi.inc}
{$DEFINE UNISYNEDIT}

interface

uses
  Windows, SysUtils, Classes, SynEditHighlighter, Contnrs, Graphics;

type
  TLineToken = class(TPersistent)
  private
    FText: string;
    FKind: Integer;
    FAttribute: TSynHighlighterAttributes;
    FPos: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Attribute: TSynHighlighterAttributes read FAttribute write FAttribute;
    property Kind: Integer read FKind write FKind;
    property Text: string read FText write FText;
    property Pos: Integer read FPos write FPos;
  end;

  TLineTokens = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TLineToken;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TLineToken;
    procedure Clear;
    procedure Combine;
    procedure HighlightChars(APos, ALength: Integer; AAttribute: TSynHighlighterAttributes);
    procedure SplitByChar;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TLineToken read GetItems; default;
  end;

  THighlightKind = (hkDeleteLine, hkDeleteChar, hkInsertLine, hkInsertChar);

  THighlightChar = class(TObject)
  private
    FCharIndex: Integer;
    FKind: THighlightKind;
  public
    property CharIndex: Integer read FCharIndex write FCharIndex;
    property Kind: THighlightKind read FKind write FKind;
  end;

  THighlightLine = class(TObject)
  private
    FItems: TObjectList;
    FLine: Integer;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): THighlightChar;
  public
    constructor Create(ALine: Integer);
    destructor Destroy; override;
    function Add: THighlightChar;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: THighlightChar read GetItems; default;
    property Line: Integer read FLine;
  end;

  THighlightLines = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): THighlightLine;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(ALine: Integer): THighlightLine;
    function Get(ALine: Integer): THighlightLine;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: THighlightLine read GetItems; default;
  end;

  TBlockRange = class(TObject)
  public
    FromCol: Integer;
    FromLine: Integer;
    ToCol: Integer;
    ToLine: Integer;
    Level: Integer;
  end;

  TBlockLevels = array of Integer;

  TBlockRanges = class(TObject)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TBlockRange;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TBlockRange;
    procedure Clear;
    function GetLineLevels(ALine: Integer): TBlockLevels;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TBlockRange read GetItems; default;
  end;

  THighlightColorScheme = (hcsLevels, hcsSemantic);

  TJVCSCompressedDiffSynProxyHighlighter = class(TSynCustomHighlighter)
  private
    FAttrDeleteLine: TSynHighlighterAttributes;
    FAttrDeleteChar: TSynHighlighterAttributes;
    FAttrInsertLine: TSynHighlighterAttributes;
    FAttrInsertChar: TSynHighlighterAttributes;
    FLevels: array [0..11, 0..2] of TSynHighlighterAttributes;
    FKeyAttr: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FHighlightLines: THighlightLines;
    FIdx: Integer;
    FInternalHighlighter: TSynCustomHighlighter;
    FLineNumber: Integer;
    FLineStr: string;
    FTokens: TLineTokens;
    FBlockRanges: TBlockRanges;
    FColorScheme: THighlightColorScheme;
    procedure AddHighlightChar(ALine, ACharIndex: Integer; AKind: THighlightKind);
    procedure SetColorSchemeColors;
    procedure SetInternalHighlighter(const Value: TSynCustomHighlighter);
    function GetLevelColor(AIndex: Integer): TColor;
    function GetLevelColorCount: Integer;
    procedure SetColorScheme(const Value: THighlightColorScheme);
  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    {$IFDEF UNISYNEDIT}
    function GetExpandedToken: UnicodeString; override;
    function GetExpandedTokenPos: Integer; override;
    {$ENDIF UNISYNEDIT}
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    property InternalHighlighter: TSynCustomHighlighter read FInternalHighlighter write SetInternalHighlighter;
    {$IFDEF UNISYNEDIT}
    procedure DoSetLine(const NewValue: string; LineNumber:Integer); override;
    {$ELSE}
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    {$ENDIF}
    procedure ResetRange; override;
    procedure Next; override;
    //procedure UpdateHighlight(ACompressedDiff: TJVCSCompressedDiff);
    property BlockRanges: TBlockRanges read FBlockRanges;
    property ColorScheme: THighlightColorScheme read FColorScheme write SetColorScheme;
    property LevelColorCount: Integer read GetLevelColorCount;
    property LevelColors[AIndex: Integer]: TColor read GetLevelColor;
  end;

implementation

uses
  SynHighlighterPas;

type
  TSynCustomHighlighterAccess = class(TSynCustomHighlighter);

{ TLineTokens }

constructor TLineTokens.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TLineTokens.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TLineTokens.Add: TLineToken;
begin
  FItems.Add(TLineToken.Create);
  Result := TLineToken(FItems.Last);
end;

procedure TLineTokens.Clear;
begin
  FItems.Clear;
end;

procedure TLineTokens.Combine;
var
  I: Integer;
begin
  for I := Count - 1 downto 1 do
    if (Items[I].Kind = Items[I - 1].Kind) and (Items[I].Attribute = Items[I - 1].Attribute) and
      (Items[I].Pos = Items[I - 1].Pos + Length(Items[I - 1].Text)) then
    begin
      Items[I - 1].Text := Items[I - 1].Text + Items[I].Text;
      FItems.Delete(I);  
    end;     
end;

function TLineTokens.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TLineTokens.GetItems(AIndex: Integer): TLineToken;
begin
  Result := TLineToken(FItems[AIndex]);
end;

procedure TLineTokens.HighlightChars(APos, ALength: Integer; AAttribute: TSynHighlighterAttributes);
var
  I, P2: Integer;
begin
  P2 := APos + ALength - 1;
  for I := 0 to Count - 1 do
    if (Items[I].Pos >= APos) and (Items[I].Pos <= P2) then
    begin
      Items[I].Attribute := AAttribute;
      Items[I].Kind := Items[I].Kind + 100; 
    end;
end;

procedure TLineTokens.SplitByChar;
var
  I, J, L: Integer;
  TempTokens: TLineTokens;
  LineToken: TLineToken;
begin
  TempTokens := TLineTokens.Create;
  try
    for I := 0 to Pred(Count) do
    begin
      LineToken := TempTokens.Add;
      LineToken.Assign(Items[I]);
    end;
    Clear;
    for I := 0 to Pred(TempTokens.Count) do
    begin
      L := Length(TempTokens[I].Text);
      for J := 0 to L - 1 do
      begin
        LineToken := Add;
        LineToken.Assign(TempTokens[I]);
        LineToken.Pos := TempTokens[I].Pos + J;
        LineToken.Text := Copy(TempTokens[I].Text, J + 1, 1);
      end;
    end;
  finally
    TempTokens.Free;
  end;
end;

{ TLineToken }

procedure TLineToken.AssignTo(Dest: TPersistent);
begin
  if Dest is TLineToken then
  begin
    TLineToken(Dest).FText := FText;
    TLineToken(Dest).FKind := FKind;
    TLineToken(Dest).FAttribute := FAttribute;
    TLineToken(Dest).FPos := FPos;
  end
  else
    inherited AssignTo(Dest);
end;

{ THighlightLine }

constructor THighlightLine.Create(ALine: Integer);
begin
  inherited Create;
  FItems := TObjectList.Create;
  FLine := ALine;
end;

destructor THighlightLine.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function THighlightLine.Add: THighlightChar;
begin
  FItems.Add(THighlightChar.Create);
  Result := THighlightChar(FItems.Last);
end;

function THighlightLine.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function THighlightLine.GetItems(AIndex: Integer): THighlightChar;
begin
  Result := THighlightChar(FItems[AIndex]);
end;

{ THighlightLines }

constructor THighlightLines.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor THighlightLines.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function THighlightLines.Find(ALine: Integer): THighlightLine;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Pred(Count) do
    if Items[I].Line = ALine then
    begin
      Result := Items[I];
      Break;
    end;
end;

function THighlightLines.Get(ALine: Integer): THighlightLine;
begin
  Result := Find(ALine);
  if not Assigned(Result) then
  begin
    FItems.Add(THighlightLine.Create(ALine));
    Result := THighlightLine(FItems.Last);
  end;    
end;

function THighlightLines.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function THighlightLines.GetItems(AIndex: Integer): THighlightLine;
begin
  Result := THighlightLine(FItems[AIndex]);
end;

{ TBlockRanges }

constructor TBlockRanges.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TBlockRanges.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TBlockRanges.Add: TBlockRange;
begin
  FItems.Add(TBlockRange.Create);
  Result := TBlockRange(FItems.Last);
end;

procedure TBlockRanges.Clear;
begin
  FItems.Clear;
end;

function TBlockRanges.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBlockRanges.GetItems(AIndex: Integer): TBlockRange;
begin
  Result := TBlockRange(FItems[AIndex]);
end;

function TBlockRanges.GetLineLevels(ALine: Integer): TBlockLevels;
var
  I, J, StartCol, EndCol: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to Pred(Count) do
    if (Items[I].FromLine <= ALine) and (Items[I].ToLine >= ALine) then
    begin
      if Length(Result) < 100 then
        SetLength(Result, 100);
      if Items[I].FromLine = ALine then
        StartCol := Items[I].FromCol
      else
        StartCol := 1;
      if Items[I].ToLine = ALine then
        EndCol := Items[I].ToCol
      else
        EndCol := 100;
      for J := StartCol to EndCol do
        Result[J - 1] := Items[I].Level;
    end;
end;

{ TJVCSCompressedDiffSynProxyHighlighter }

constructor TJVCSCompressedDiffSynProxyHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalHighlighter := nil;
  FAttrDeleteLine := TSynHighlighterAttributes.Create('Delete Line'{$IFDEF UNISYNEDIT}, 'Delete Line'{$ENDIF});
  FAttrDeleteLine.Background := RGB($FF, $E0, $E0);
  FAttrDeleteChar := TSynHighlighterAttributes.Create('Delete Char'{$IFDEF UNISYNEDIT}, 'Delete Char'{$ENDIF});
  FAttrDeleteChar.Background := RGB($FF, $80, $80);
  FAttrInsertLine := TSynHighlighterAttributes.Create('Insert Line'{$IFDEF UNISYNEDIT}, 'Insert Line'{$ENDIF});
  FAttrInsertLine.Background := RGB($E0, $FF, $E0);
  FAttrInsertChar := TSynHighlighterAttributes.Create('Insert Char'{$IFDEF UNISYNEDIT}, 'Insert Char'{$ENDIF});
  FAttrInsertChar.Background := RGB($80, $FF, $80);

  FColorScheme := hcsLevels;
  SetColorSchemeColors;

  FTokens := TLineTokens.Create;
  FHighlightLines := THighlightLines.Create;
  FBlockRanges := TBlockRanges.Create;
end;

destructor TJVCSCompressedDiffSynProxyHighlighter.Destroy;
var
  I, J: Integer;
begin
  for I := Low(FLevels) to High(FLevels) do
    for J := Low(FLevels[I]) to High(FLevels[I]) do
      FLevels[I][J].Free;
  FBlockRanges.Free;
  FAttrDeleteLine.Free;
  FHighlightLines.Free;
  FTokens.Free;
  inherited Destroy;
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.AddHighlightChar(ALine, ACharIndex: Integer; AKind: THighlightKind);
var
  L: THighlightLine;
  H: THighlightChar;
begin
  L := FHighlightLines.Get(ALine);
  H := L.Add;
  H.CharIndex := ACharIndex;
  H.Kind := AKind;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  if Assigned(FInternalHighlighter) then
    Result := TSynCustomHighlighterAccess(FInternalHighlighter).GetDefaultAttribute(Index)
  else
    Result := nil;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetEol: Boolean;
begin
  Result := FIdx >= FTokens.Count;
end;

{$IFDEF UNISYNEDIT}
function TJVCSCompressedDiffSynProxyHighlighter.GetExpandedToken: UnicodeString;
begin
  Result := GetToken;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetExpandedTokenPos: Integer;
begin
  Result := GetTokenPos;
end;
function TJVCSCompressedDiffSynProxyHighlighter.GetLevelColor(AIndex: Integer): TColor;
begin
  if FColorScheme = hcsSemantic then
    Result := FLevels[AIndex][0].Foreground
  else
    Result := FLevels[AIndex][0].Background;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetLevelColorCount: Integer;
begin
  Result := Length(FLevels);
end;

{$ENDIF UNISYNEDIT}

function TJVCSCompressedDiffSynProxyHighlighter.GetToken: string;
begin
  Result := FTokens[FIdx].Text;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FTokens[FIdx].Attribute;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetTokenKind: Integer;
begin
  Result := FTokens[FIdx].Kind;
end;

function TJVCSCompressedDiffSynProxyHighlighter.GetTokenPos: Integer;
begin
  Result := FTokens[FIdx].Pos;
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.Next;
begin
  Inc(FIdx);
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.ResetRange;
begin
  if Assigned(FInternalHighlighter) then
    FInternalHighlighter.ResetRange;
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.SetColorScheme(const Value: THighlightColorScheme);
begin
  if FColorScheme <> Value then
  begin
    FColorScheme := Value;
    SetColorSchemeColors;
  end;
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.SetColorSchemeColors;

  function GetNextSemanticColor(AIndex: Integer): TColor;
  begin
    case (AIndex * 10) mod 67 of
     1: Result := RGB(144, 102, 101);
     2: Result := RGB(192, 135, 133);
     3: Result := RGB(139, 88, 75);
     4: Result := RGB(182, 126, 100);
     5: Result := RGB(169, 114, 52);
     6: Result := RGB(195, 145, 88);
     7: Result := RGB(151, 113, 0);
     8: Result := RGB(179, 146, 45);
     9: Result := RGB(155, 150, 61);
     10: Result := RGB(188, 179, 5);
     11: Result := RGB(130, 142, 50);
     12: Result := RGB(161, 178, 80);
     13: Result := RGB(93, 155, 71);
     14: Result := RGB(126, 188, 107);
     15: Result := RGB(75, 132, 88);
     16: Result := RGB(104, 170, 120);
     17: Result := RGB(44, 135, 117);
     18: Result := RGB(51, 168, 156);
     19: Result := RGB(43, 132, 147);
     20: Result := RGB(89, 163, 180);
     21: Result := RGB(23, 112, 194);
     22: Result := RGB(86, 159, 210);
     23: Result := RGB(51, 97, 176);
     24: Result := RGB(102, 144, 195);
     25: Result := RGB(83, 97, 187);
     26: Result := RGB(119, 149, 227);
     27: Result := RGB(91, 86, 188);
     28: Result := RGB(123, 124, 223);
     29: Result := RGB(119, 94, 157);
     30: Result := RGB(149, 135, 176);
     31: Result := RGB(144, 90, 168);
     32: Result := RGB(169, 133, 185);
     33: Result := RGB(155, 90, 140);
     34: Result := RGB(179, 128, 171);
     35: Result := RGB(155, 90, 140);
     36: Result := RGB(169, 133, 185);
     37: Result := RGB(144, 90, 168);
     38: Result := RGB(149, 135, 176);
     39: Result := RGB(119, 94, 157);
     40: Result := RGB(123, 124, 223);
     41: Result := RGB(91, 86, 188);
     42: Result := RGB(119, 149, 227);
     43: Result := RGB(83, 97, 187);
     44: Result := RGB(102, 144, 195);
     45: Result := RGB(51, 97, 176);
     46: Result := RGB(86, 159, 210);
     47: Result := RGB(23, 112, 194);
     48: Result := RGB(89, 163, 180);
     49: Result := RGB(43, 132, 147);
     50: Result := RGB(51, 168, 156);
     51: Result := RGB(44, 135, 117);
     52: Result := RGB(104, 170, 120);
     53: Result := RGB(75, 132, 88);
     54: Result := RGB(126, 188, 107);
     55: Result := RGB(93, 155, 71);
     56: Result := RGB(161, 178, 80);
     57: Result := RGB(130, 142, 50);
     58: Result := RGB(188, 179, 5);
     59: Result := RGB(155, 150, 61);
     60: Result := RGB(179, 146, 45);
     61: Result := RGB(151, 113, 0);
     62: Result := RGB(195, 145, 88);
     63: Result := RGB(169, 114, 52);
     64: Result := RGB(182, 126, 100);
     65: Result := RGB(139, 88, 75);
     66: Result := RGB(192, 135, 133);
     67: Result := RGB(144, 102, 101);
     else
       Result := RGB(144, 102, 101);
    end;
  end;

var
  I, J: Integer;
  S: string;
begin
  for I := Low(FLevels) to High(FLevels) do
    for J := Low(FLevels[I]) to High(FLevels[I]) do
      if Assigned(FLevels[I][J]) then
      begin
        FLevels[I][J].Free;
        FLevels[I][J] := nil;
      end;
  if FColorScheme = hcsLevels then
  begin
    for I := 0 to 2 do
    begin
      FLevels[0][I] := TSynHighlighterAttributes.Create('Level 0'{$IFDEF UNISYNEDIT}, 'Level 0'{$ENDIF});
      FLevels[0][I].Background := RGB(224, 192, 192);
      FLevels[1][I] := TSynHighlighterAttributes.Create('Level 1'{$IFDEF UNISYNEDIT}, 'Level 1'{$ENDIF});
      FLevels[1][I].Background := RGB(255, 208, 208);
      FLevels[2][I] := TSynHighlighterAttributes.Create('Level 2'{$IFDEF UNISYNEDIT}, 'Level 2'{$ENDIF});
      FLevels[2][I].Background := RGB(192, 224, 192);
      FLevels[3][I] := TSynHighlighterAttributes.Create('Level 3'{$IFDEF UNISYNEDIT}, 'Level 3'{$ENDIF});
      FLevels[3][I].Background := RGB(208, 255, 208);
      FLevels[4][I] := TSynHighlighterAttributes.Create('Level 4'{$IFDEF UNISYNEDIT}, 'Level 4'{$ENDIF});
      FLevels[4][I].Background := RGB(192, 192, 224);
      FLevels[5][I] := TSynHighlighterAttributes.Create('Level 5'{$IFDEF UNISYNEDIT}, 'Level 5'{$ENDIF});
      FLevels[5][I].Background := RGB(208, 208, 255);
      FLevels[6][I] := TSynHighlighterAttributes.Create('Level 6'{$IFDEF UNISYNEDIT}, 'Level 6'{$ENDIF});
      FLevels[6][I].Background := RGB(224, 224, 192);
      FLevels[7][I] := TSynHighlighterAttributes.Create('Level 7'{$IFDEF UNISYNEDIT}, 'Level 7'{$ENDIF});
      FLevels[7][I].Background := RGB(255, 255, 208);
      FLevels[8][I] := TSynHighlighterAttributes.Create('Level 8'{$IFDEF UNISYNEDIT}, 'Level 8'{$ENDIF});
      FLevels[8][I].Background := RGB(224, 192, 224);
      FLevels[9][I] := TSynHighlighterAttributes.Create('Level 9'{$IFDEF UNISYNEDIT}, 'Level 9'{$ENDIF});
      FLevels[9][I].Background := RGB(255, 208, 255);
      FLevels[10][I] := TSynHighlighterAttributes.Create('Level 10'{$IFDEF UNISYNEDIT}, 'Level 10'{$ENDIF});
      FLevels[10][I].Background := RGB(192, 224, 224);
      FLevels[11][I] := TSynHighlighterAttributes.Create('Level 11'{$IFDEF UNISYNEDIT}, 'Level 11'{$ENDIF});
      FLevels[11][I].Background := RGB(208, 255, 255);
    end;
  end
  else
  if FColorScheme = hcsSemantic then
  begin
    for I := 0 to 2 do
    begin
      FLevels[0][I] := TSynHighlighterAttributes.Create('Level 0'{$IFDEF UNISYNEDIT}, 'Level 0'{$ENDIF});
      for J := 1 to 11 do
      begin
        S := Format('Symbol #%d', [J]);
        FLevels[J][I] := TSynHighlighterAttributes.Create(S{$IFDEF UNISYNEDIT}, S{$ENDIF});
        FLevels[J][I].Foreground := GetNextSemanticColor(J);
        FLevels[J][I].Style := [fsBold];
      end;
    end;
  end;
  if Assigned(FInternalHighlighter) then
    SetInternalHighlighter(FInternalHighlighter);
end;

procedure TJVCSCompressedDiffSynProxyHighlighter.SetInternalHighlighter(const Value: TSynCustomHighlighter);
var
  I: Integer;
begin
  FInternalHighlighter := Value;
  FTokens.Clear;
  FKeyAttr := nil;
  FStringAttri := nil;
  if Value is TSynPasSyn then
  begin
    FKeyAttr := TSynPasSyn(Value).KeyAttri;
    FStringAttri := TSynPasSyn(Value).StringAttri;
    for I := Low(FLevels) to High(FLevels) do
    begin
      FLevels[I][1].Foreground := FKeyAttr.Foreground;
      FLevels[I][1].Style := FKeyAttr.Style;
      FLevels[I][2].Foreground := FStringAttri.Foreground;
      FLevels[I][2].Style := FStringAttri.Style;
    end;
  end;
  FIdx := 0;
end;

{$IFDEF UNISYNEDIT}
procedure TJVCSCompressedDiffSynProxyHighlighter.DoSetLine(const NewValue: string; LineNumber: Integer);
{$ELSE}
procedure TJVCSCompressedDiffSynProxyHighlighter.SetLine(NewValue: string; LineNumber: Integer);
{$ENDIF}
var
  I, P, SubIndex: Integer;
  Token: TLineToken;
  Line: THighlightLine;
  Attr: TSynHighlighterAttributes;
  IsDel: Boolean;
  L: TBlockLevels;
begin
  FTokens.Clear;
  {$IFDEF UNISYNEDIT}
  FIdx := -1;
  {$ELSE}
  FIdx := 0;
  {$ENDIF}
  if Assigned(FInternalHighlighter) then
  begin
    {$IFDEF UNISYNEDIT}
    TSynCustomHighlighterAccess(FInternalHighlighter).DoSetLine(NewValue, LineNumber);
    {$ELSE}
    FInternalHighlighter.SetLine(NewValue, LineNumber);
    {$ENDIF}
    while not FInternalHighlighter.GetEol do
    begin
      Token := FTokens.Add;
      Token.Attribute := FInternalHighlighter.GetTokenAttribute;
      Token.Text := FInternalHighlighter.GetToken;
      Token.Pos := FInternalHighlighter.GetTokenPos;
      Token.Kind := FInternalHighlighter.GetTokenKind;
      FInternalHighlighter.Next;
    end;
  end;
  FLineStr := NewValue;
  FLineNumber := LineNumber;

  {
  if (FLineNumber + 1 >= 7) and (FLineNumber + 1 <= 22) then
  begin
    FTokens.SplitByChar;
    for I := 0 to FTokens.Count - 1 do
    begin
      Attr := FLevels[0][0];
      if FTokens[I].Attribute = FKeyAttr then
        Attr := FLevels[0][1];
      FTokens.HighlightChars(I, 1, Attr);
    end;
    FTokens.Combine;
  end;

  if (FLineNumber + 1 = 8)  then
  begin
    FTokens.SplitByChar;
    for I := 0 to FTokens.Count - 1 do
      FTokens.HighlightChars(I, 1, FLevels[1][0]);
    FTokens.Combine;
  end
  else
  if (FLineNumber + 1 = 9)  then
  begin
    FTokens.SplitByChar;
    for I := 0 to FTokens.Count - 1 do
      FTokens.HighlightChars(I, 1, FLevels[1][0]);
    FTokens.Combine;
  end;
  }
  L := FBlockRanges.GetLineLevels(FLineNumber + 1);
  if Length(L) > 0 then
  begin
    FTokens.SplitByChar;
    for I := 0 to FTokens.Count - 1 do
    begin
      SubIndex := 0;
      if FTokens[I].Attribute = FKeyAttr then
        SubIndex := 1
      else
      if FTokens[I].Attribute = FStringAttri then
        SubIndex := 2;
      if Length(L) > I then
      begin
        if L[I] <= High(FLevels) then
        begin
          Attr := FLevels[L[I]][SubIndex];
          FTokens.HighlightChars(I, 1, Attr);
        end;
      end;
    end;
    FTokens.Combine;
  end;
  

{
  Line := FHighlightLines.Find(LineNumber);
  if Assigned(Line) then
  begin
    FTokens.SplitByChar;
    IsDel := False;
    for I := 0 to Line.Count - 1 do
    begin
      case Line[I].Kind of
        hkDeleteLine: Attr := FAttrDeleteLine;
        hkDeleteChar: Attr := FAttrDeleteChar;
        hkInsertLine: Attr := FAttrInsertLine;
        hkInsertChar: Attr := FAttrInsertChar;
        else
          Attr := FAttrDeleteLine;
      end;
      if Line[I].Kind in [hkDeleteLine, hkDeleteChar] then
        IsDel := True;
      FTokens.HighlightChars(Line[I].CharIndex, 1, Attr);
    end;
    if FTokens.Count > 0 then
    begin
      Token := FTokens[Pred(FTokens.Count)];
      P := Token.Pos + Length(Token.Text) - 1;
      if P < 80 then
      begin
        Token := FTokens.Add;
        if IsDel then
          Token.Attribute := FAttrDeleteLine
        else
          Token.Attribute := FAttrInsertLine;
        Token.Text := StringOfChar(' ', 80 - P);
        Token.Pos := P + 1;
        Token.Kind := 100;
      end;
    end;
    FTokens.Combine;
  end;
}
end;

{
procedure TJVCSCompressedDiffSynProxyHighlighter.UpdateHighlight(ACompressedDiff: TJVCSCompressedDiff);
var
  I, J, K: Integer;
  Kind: THighlightKind;
begin
  for I := 0 to ACompressedDiff.Count - 1 do
    if ACompressedDiff[I].Kind in [cdtModifyAdd, cdtModifyDelete] then
    begin
      for J := 0 to Length(ACompressedDiff[I].Text) - 1 do
      begin
        if ACompressedDiff[I].Kind = cdtModifyDelete then
          Kind := hkDeleteLine
        else
          Kind := hkInsertLine;
        AddHighlightChar(I, J, Kind);
      end;
      for J := Low(ACompressedDiff[I].InlineDiffHighlight) to High(ACompressedDiff[I].InlineDiffHighlight) do
        for K := ACompressedDiff[I].InlineDiffHighlight[J].CharIndex to ACompressedDiff[I].InlineDiffHighlight[J].CharIndex + ACompressedDiff[I].InlineDiffHighlight[J].Length - 1 do
        begin
          if ACompressedDiff[I].Kind = cdtModifyDelete then
            Kind := hkDeleteChar
          else
            Kind := hkInsertChar;
          AddHighlightChar(I, K, Kind);
        end;
    end;
end;
}

end.
