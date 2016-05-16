unit SymbolResolver;

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, DelphiAST.Classes, DelphiAST.Consts;

type
  TSymbolKind = (skParameter, skReturn, skVariable);

  TSymbolInformation = record
    Kind: TSymbolKind;
    SyntaxNode: TSyntaxNode;
  end;

  TSymbolList = class(TObject)
  private
    FDict: TDictionary<string, TSymbolInformation>;
    function GetHasSymbols: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AName: string; AKind: TSymbolKind; ANode: TSyntaxNode);
    function Exists(const AName: string; out ANode: TSyntaxNode): Boolean;
    property HasSymbols: Boolean read GetHasSymbols;
  end;

procedure ResolveSymbols(ASyntaxNode: TSyntaxNode);

implementation

{ TSymbolList }

constructor TSymbolList.Create;
begin
  inherited Create;
  FDict := TDictionary<string, TSymbolInformation>.Create;
end;

destructor TSymbolList.Destroy;
begin
  FDict.Free;
  inherited Destroy;
end;

procedure TSymbolList.Add(const AName: string; AKind: TSymbolKind; ANode: TSyntaxNode);
var
  Information: TSymbolInformation;
begin
  Information.Kind := AKind;
  Information.SyntaxNode := ANode;
  FDict.Add(AnsiUpperCase(AName), Information);
end;

function TSymbolList.Exists(const AName: string; out ANode: TSyntaxNode): Boolean;
var
  Information: TSymbolInformation;
begin
  Result := FDict.TryGetValue(AnsiUpperCase(AName), Information);
  if Result then
    ANode := Information.SyntaxNode
  else
    ANode := nil;
end;

function TSymbolList.GetHasSymbols: Boolean;
begin
  Result := FDict.Count > 0;
end;

procedure ResolveIdentifiers(ASyntaxNode: TSyntaxNode; ASymbolList: TSymbolList);
var
  I: Integer;
  S: string;
  NameNode: TSyntaxNode;
begin
  if ASyntaxNode.Typ = ntIdentifier then
  begin
    if ASyntaxNode.HasAttribute(anName) then
    begin
      S := ASyntaxNode.GetAttribute(anName);
      if ASymbolList.Exists(S, NameNode) then
        ASyntaxNode.SymbolNode := NameNode;
    end;
  end
  else
  if (ASyntaxNode.Typ <> ntDot) and ASyntaxNode.HasChildren then
    for I := Low(ASyntaxNode.ChildNodes) to High(ASyntaxNode.ChildNodes) do
      ResolveIdentifiers(ASyntaxNode.ChildNodes[I], ASymbolList)
  else
  if (ASyntaxNode.Typ = ntDot) and ASyntaxNode.HasChildren and (ASyntaxNode.ChildNodes[0].Typ = ntIdentifier) then
    ResolveIdentifiers(ASyntaxNode.ChildNodes[0], ASymbolList);
end;

procedure ResolveMethod(ASyntaxNode: TSyntaxNode);
var
  I, J, K, Cnt: Integer;
  SymbolList: TSymbolList;
  TempNode, VariableNode: TSyntaxNode;
  NameNode: TValuedSyntaxNode;
begin
  SymbolList := TSymbolList.Create;
  try
    Cnt := 2;
    for I := Low(ASyntaxNode.ChildNodes) to High(ASyntaxNode.ChildNodes) do
      if ASyntaxNode.ChildNodes[I].Typ = ntParameters then
      begin
        TempNode := ASyntaxNode.ChildNodes[I];
        for J := Low(TempNode.ChildNodes) to High(TempNode.ChildNodes) do
          if TempNode.ChildNodes[J].Typ = ntParameter then
          begin
            VariableNode := TempNode.ChildNodes[J];
            NameNode := nil;
            for K := Low(VariableNode.ChildNodes) to High(VariableNode.ChildNodes) do
              if (VariableNode.ChildNodes[K] is TValuedSyntaxNode) and (VariableNode.ChildNodes[K].Typ = ntName) then
              begin
                NameNode := TValuedSyntaxNode(VariableNode.ChildNodes[K]);
                Break;
              end;
            if Assigned(NameNode) then
            begin
              SymbolList.Add(NameNode.Value, skParameter, VariableNode);
              VariableNode.ColorIndex := Cnt;
              NameNode.ColorIndex := Cnt;
              Inc(Cnt);
            end;
          end;
      end
      else
      if ASyntaxNode.ChildNodes[I].Typ = ntVariables then
      begin
        TempNode := ASyntaxNode.ChildNodes[I];
        for J := Low(TempNode.ChildNodes) to High(TempNode.ChildNodes) do
          if TempNode.ChildNodes[J].Typ = ntVariable then
          begin
            VariableNode := TempNode.ChildNodes[J];
            NameNode := nil;
            for K := Low(VariableNode.ChildNodes) to High(VariableNode.ChildNodes) do
              if (VariableNode.ChildNodes[K] is TValuedSyntaxNode) and (VariableNode.ChildNodes[K].Typ = ntName) then
              begin
                NameNode := TValuedSyntaxNode(VariableNode.ChildNodes[K]);
                Break;
              end;
            if Assigned(NameNode) then
            begin
              SymbolList.Add(NameNode.Value, skVariable, VariableNode);
              VariableNode.ColorIndex := Cnt;
              NameNode.ColorIndex := Cnt;
              Inc(Cnt);
            end;
          end;
      end
      else
      if ASyntaxNode.ChildNodes[I].Typ = ntReturnType then
      begin
        VariableNode := ASyntaxNode.ChildNodes[I];
        SymbolList.Add('Result', skReturn, VariableNode);
        VariableNode.ColorIndex := Cnt;
        Inc(Cnt);
      end;

    if SymbolList.HasSymbols then
      for I := Low(ASyntaxNode.ChildNodes) to High(ASyntaxNode.ChildNodes) do
        if ASyntaxNode.ChildNodes[I].Typ = ntStatements then
          ResolveIdentifiers(ASyntaxNode.ChildNodes[I], SymbolList);
  finally
    SymbolList.Free;
  end;
end;

procedure ResolveSymbols(ASyntaxNode: TSyntaxNode);
var
  I: Integer;
  ImplementationNode, TempNode: TSyntaxNode;
begin
  ImplementationNode := nil;
  for I := 0 to High(ASyntaxNode.ChildNodes) do
    if ASyntaxNode.ChildNodes[I].Typ = ntImplementation then
    begin
      ImplementationNode := ASyntaxNode.ChildNodes[I];
      Break;
    end;
  if Assigned(ImplementationNode) then
  begin
    for I := 0 to High(ImplementationNode.ChildNodes) do
      if ImplementationNode.ChildNodes[I].Typ = ntMethod then
      begin
        TempNode := ImplementationNode.ChildNodes[I];
        ResolveMethod(TempNode);
      end;
  end;
end;

end.
