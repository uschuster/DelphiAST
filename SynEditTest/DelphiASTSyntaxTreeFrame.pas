unit DelphiASTSyntaxTreeFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, DelphiAST.Classes;

type
  TSyntaxTreeGetColorEvent = procedure(Sender: TObject; ANode: TSyntaxNode; ALevel: Integer; var AColor: TColor) of object;
  TSyntaxNodeFocusedEvent = procedure(Sender: TObject; ANode: TSyntaxNode) of object;

  TfrmSyntaxTree = class(TFrame)
    VST: TVirtualStringTree;
    VSTInformation: TVirtualStringTree;
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTInformationGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
  private
    { Private declarations }
    FNode: TSyntaxNode;
    FNodeInformation: TStringList;
    FOnSyntaxNodeFocusedEvent: TSyntaxNodeFocusedEvent;
    FOnSyntaxTreeGetColorEvent: TSyntaxTreeGetColorEvent;
    function DoGetSyntaxNodeColor(ANode: PVirtualNode): TColor;
    procedure DoSyntaxNodeFocused(ANode: TSyntaxNode);
    procedure SetNode(AValue: TSyntaxNode);
    procedure UpdateNodeInformation;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FocusNode(ANode: TSyntaxNode);
    property Node: TSyntaxNode read FNode write SetNode;
    property OnGetColor: TSyntaxTreeGetColorEvent read FOnSyntaxTreeGetColorEvent write FOnSyntaxTreeGetColorEvent;
    property OnSyntaxNodeFocusedEvent: TSyntaxNodeFocusedEvent read FOnSyntaxNodeFocusedEvent write FOnSyntaxNodeFocusedEvent;
  end;

implementation

uses
  TypInfo, DelphiAST.Consts, DelphiASTTempUtils;

{$R *.dfm}

constructor TfrmSyntaxTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodeInformation := TStringList.Create;
end;

destructor TfrmSyntaxTree.Destroy;
begin
  FNodeInformation.Free;
  inherited Destroy;
end;

function TfrmSyntaxTree.DoGetSyntaxNodeColor(ANode: PVirtualNode): TColor;
var
  Data: ^TSyntaxNode;
begin
  Result := clNone;
  if Assigned(FOnSyntaxTreeGetColorEvent) then
  begin
    Data := VST.GetNodeData(ANode);
    if Assigned(Data^) then
      FOnSyntaxTreeGetColorEvent(Self, Data^, VST.GetNodeLevel(ANode), Result);
  end;
end;

procedure TfrmSyntaxTree.DoSyntaxNodeFocused(ANode: TSyntaxNode);
begin
  if Assigned(FOnSyntaxNodeFocusedEvent) then
    FOnSyntaxNodeFocusedEvent(Self, ANode);
end;

procedure TfrmSyntaxTree.FocusNode(ANode: TSyntaxNode);
var
  ItemList: TList;
  InsertNode: TSyntaxNode;
  TestNode: PVirtualNode;
  Found: Boolean;
  Data: ^TSyntaxNode;
begin
  ItemList := TList.Create;
  try
    InsertNode := ANode;
    while Assigned(InsertNode) do
    begin
      ItemList.Add(InsertNode);
      InsertNode := InsertNode.ParentNode;
    end;
    Found := True;
    TestNode := VST.RootNode^.FirstChild;
    while Found and (ItemList.Count > 0) do
    begin
      Found := False;
      while (not Found) and Assigned(TestNode) do
      begin
        Data := VST.GetNodeData(TestNode);
        if Data^ = ItemList.Last then
        begin
          Found := True;
          Break;
        end
        else
          TestNode := TestNode^.NextSibling;
      end;
      if Found then
      begin
        ItemList.Delete(ItemList.Count - 1);
        VST.Expanded[TestNode] := True;
        if ItemList.Count > 0 then
          TestNode := TestNode^.FirstChild;
      end;
    end;
    if Found and (ItemList.Count = 0) then
    begin
      VST.FocusedNode := TestNode;
      VST.Selected[TestNode] := True;
    end;
  finally
    ItemList.Free;
  end;
end;

procedure TfrmSyntaxTree.SetNode(AValue: TSyntaxNode);
begin
  VST.RootNodeCount := 0;
  FNode := AValue;
  if Assigned(FNode) then
    VST.RootNodeCount := 1;
end;

procedure TfrmSyntaxTree.UpdateNodeInformation;
var
  I: Integer;
  S: string;
  Data: ^TSyntaxNode;
  C: TCompoundSyntaxNode;
begin
  VSTInformation.BeginUpdate;
  try
    VSTInformation.RootNodeCount := 0;
    FNodeInformation.Clear;
    if Assigned(VST.FocusedNode) then
    begin
      Data := VST.GetNodeData(VST.FocusedNode);
      FNodeInformation.Add(Format('Node Class=%s', [Data^.ClassName]));
      S := GetEnumName(TypeInfo(TSyntaxNodeType), Ord(Data^.Typ));
      Delete(S, 1, 2);
      FNodeInformation.Add(Format('Type=%s', [S]));
      if Data^ is TValuedSyntaxNode then
        FNodeInformation.Add(Format('Value=%s', [TValuedSyntaxNode(Data^).Value]));
      if Data^.HasAttributes then
        for I := Low(Data^.Attributes) to High(Data^.Attributes) do
          FNodeInformation.Add(Format('Attribute %s=%s', [GetEnumName(TypeInfo(TAttributeName), Ord(Data^.Attributes[I].Key)),Data^.Attributes[I].Value]));
      if Data^ is TCompoundSyntaxNode then
      begin
        C := TCompoundSyntaxNode(Data^);
        S := Format('%d:%d - %d:%d', [C.Line, C.Col, C.EndLine, C.EndCol]);
      end
      else
        S := Format('%d:%d', [Data^.Line, Data^.Col]);
      FNodeInformation.Add(Format('Begin / End=%s', [S]));
      if Data^.HasEnd then
      begin
        S := Format('%d:%d - %d:%d', [Data^.Line, Data^.Col, Data^.FixedEndLine, Data^.FixedEndCol]);
        FNodeInformation.Add(Format('Fixed Begin / End=%s', [S]));
      end;
    end;
    VSTInformation.RootNodeCount := FNodeInformation.Count;
  finally
    VSTInformation.EndUpdate;
  end;
end;

procedure TfrmSyntaxTree.VSTInformationGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  case Column of
    0: CellText := FNodeInformation.Names[Node^.Index];
    1: CellText := FNodeInformation.ValueFromIndex[Node^.Index];
  end;
end;

procedure TfrmSyntaxTree.VSTBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  UserColor: TColor;
begin
  UserColor := DoGetSyntaxNodeColor(Node);
  if UserColor <> clNone then
  begin
    ItemColor := UserColor;
    EraseAction := eaColor;
  end;
end;

procedure TfrmSyntaxTree.VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: ^TSyntaxNode;
  FocusedSyntaxNode: TSyntaxNode;
begin
  inherited;
  FocusedSyntaxNode := nil;
  if Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    FocusedSyntaxNode := Data^;
  end;
  UpdateNodeInformation;
  DoSyntaxNodeFocused(FocusedSyntaxNode);
end;

procedure TfrmSyntaxTree.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Data: ^TSyntaxNode;
  C: TCompoundSyntaxNode;
  S: string;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  S := GetEnumName(TypeInfo(TSyntaxNodeType), Ord(Data^.Typ));
  Delete(S, 1, 2);
  if Data^ is TCompoundSyntaxNode then
  begin
    C := TCompoundSyntaxNode(Data^);
    S := Format('%s [%d:%d - %d:%d]', [S, C.Line, C.Col, C.EndLine, C.EndCol]);
  end
  else
    S := Format('%s [%d:%d]', [S, Data^.Line, Data^.Col]);
  CellText := S;
end;

procedure TfrmSyntaxTree.VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data: ^TSyntaxNode;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  ChildCount := Length(Data^.ChildNodes);
end;

procedure TfrmSyntaxTree.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: ^TSyntaxNode;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(ParentNode) then
    Data^ := FNode
  else
  begin
    ParentData := Sender.GetNodeData(ParentNode);
    Data^ := ParentData^.ChildNodes[Node^.Index];
  end;
  if Length(Data^.ChildNodes) > 0 then
  begin
    //Include(InitialStates, ivsExpanded);
    Include(InitialStates, ivsHasChildren);
  end;
end;

end.
