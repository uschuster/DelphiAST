unit DelphiASTTempUtils;

interface

uses
  DelphiAST.Classes;

type
  //TODO: rather fix it directly in TSyntaxNode by providing begin and end for every node
  TSyntaxNodeHelper = class helper for TSyntaxNode
    function GetFixedEndCol: Integer;
    function GetFixedEndLine: Integer;
    function GetHasEnd: Boolean;

    property HasEnd: Boolean read GetHasEnd;
    property FixedEndCol: Integer read GetFixedEndCol;
    property FixedEndLine: Integer read GetFixedEndLine;
  end;

implementation

uses
  DelphiAST.Consts;

{ TSyntaxNodeHelper }

function TSyntaxNodeHelper.GetFixedEndCol: Integer;
begin
  if Self is TCompoundSyntaxNode then
    Result := TCompoundSyntaxNode(Self).EndCol
  else
  if (Self is TValuedSyntaxNode) and (Self.Typ = ntName) then
    Result := Self.Col + Length(TValuedSyntaxNode(Self).Value) - 1
  else
  if Self.HasAttribute(anName) then
    Result := Self.Col + Length(Self.GetAttribute(anName)) - 1
  else
    Result := -1;
end;

function TSyntaxNodeHelper.GetFixedEndLine: Integer;
begin
  if Self is TCompoundSyntaxNode then
    Result := TCompoundSyntaxNode(Self).EndLine
  else
  if (Self is TValuedSyntaxNode) and (Self.Typ = ntName) then
    Result := Self.Line
  else
  if Self.HasAttribute(anName) then
    Result := Self.Line
  else
    Result := -1;
end;

function TSyntaxNodeHelper.GetHasEnd: Boolean;
begin
  Result := (Self is TCompoundSyntaxNode) or
    ((Self is TValuedSyntaxNode) and (Self.Typ = ntName)) or
    Self.HasAttribute(anName);
end;

end.
