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
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VSTBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
    { Private declarations }
    FNode: TSyntaxNode;
    FOnSyntaxNodeFocusedEvent: TSyntaxNodeFocusedEvent;
    FOnSyntaxTreeGetColorEvent: TSyntaxTreeGetColorEvent;
    function DoGetSyntaxNodeColor(ANode: PVirtualNode): TColor;
    procedure DoSyntaxNodeFocused(ANode: TSyntaxNode);
    procedure SetNode(AValue: TSyntaxNode);
  public
    { Public declarations }
    property Node: TSyntaxNode read FNode write SetNode;
    property OnGetColor: TSyntaxTreeGetColorEvent read FOnSyntaxTreeGetColorEvent write FOnSyntaxTreeGetColorEvent;
    property OnSyntaxNodeFocusedEvent: TSyntaxNodeFocusedEvent read FOnSyntaxNodeFocusedEvent write FOnSyntaxNodeFocusedEvent;
  end;

implementation

uses
  TypInfo, DelphiAST.Consts;

{$R *.dfm}

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

procedure TfrmSyntaxTree.SetNode(AValue: TSyntaxNode);
begin
  VST.RootNodeCount := 0;
  FNode := AValue;
  if Assigned(FNode) then
    VST.RootNodeCount := 1;
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
