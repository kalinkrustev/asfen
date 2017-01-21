unit dm_Images;

interface

uses
  SysUtils, Classes, ImgList, Controls, ActnMan, ActnColorMaps, Tabs, Forms;

type
  TdmImages = class(TDataModule)
    LargeImages: TImageList;
    Images: TImageList;
    ColorMap: TXPColorMap;
    SmallImages: TImageList;
  private
    procedure FormActivate(Sender: TObject);
    procedure UpdateActionList(F:TCustomForm);
  public
    Tabs:TTabSet;
    function ShowForm(C:TComponentClass;Modal:Boolean;var F;Show:Boolean=True;Owner:TComponent=nil;Event:TNotifyEvent=nil):Integer;
    procedure UpdateTab(F:TCustomForm);
  end;

var
  dmImages: TdmImages;

implementation
uses ActnList;
{$R *.dfm}

var ActionList:TStringList=nil;

{ TdmImages }

procedure TdmImages.FormActivate(Sender: TObject);
begin
  if Sender is TWinControl then TWinControl(Sender).Realign;
end;

function TdmImages.ShowForm(C: TComponentClass; Modal: Boolean; var F;
  Show: Boolean; Owner: TComponent;Event:TNotifyEvent): Integer;
begin
  Result:=-1;
  if TComponent(F)=nil then
  begin
    if Owner=nil then
      Application.CreateForm(C,F)
    else
      TObject(F):=C.Create(Owner);
  end;

  if not Modal and (Tabs<>nil) and (TObject(F) is TCustomForm) then
  begin
    if not TCustomForm(F).Visible then TCustomForm(F).WindowState:=wsMaximized;
    if (TObject(F) is TForm) and (TForm(F).FormStyle<>fsMDIChild) then TForm(F).FormStyle:=fsMDIChild;
    if Tabs.Tabs.IndexOfObject(TObject(F))<0 then
    begin
      Tabs.Tabs.AddObject(TCustomForm(F).Caption,TObject(F));
      if Tabs.Owner<>nil then Tabs.Owner.FreeNotification(TCustomForm(F));
      Tabs.TabIndex:=Tabs.Tabs.Count-1;
      Tabs.Refresh;
    end else
      Tabs.TabIndex:=Tabs.Tabs.IndexOfObject(TObject(F));
  end;

  if (TObject(F) is TCustomForm) then
  begin
    if TCustomForm(F).HelpKeyword='' then TCustomForm(F).HelpKeyword:=TCustomForm(F).ClassName;
    TCustomForm(F).HelpType:=htKeyword;
  end;
  if ActionList<>nil then UpdateActionList(TCustomForm(F));
  if Assigned(Event) then Event(TObject(F));


  if not Show then exit;
  if Modal then
    Result:=TCustomForm(F).ShowModal
  else
  begin
    if not Assigned(TForm(F).OnActivate) then
      TForm(F).OnActivate:=FormActivate;
    TCustomForm(F).Realign;
    TCustomForm(F).Show;
  end;
end;

procedure TdmImages.UpdateActionList(F: TCustomForm);
var
  I: Integer;
begin
  for I := 0 to F.componentcount - 1 do
    if (f.Components[I] is TAction) and
       (ActionList.IndexOfName('Действия^Action.'+f.ClassName+'.'+f.Components[I].Name)<0) and
    not SameText(f.Components[I].Name,'acClose') and
      TAction(f.Components[I]).Visible
    then
       ActionList.Values['Действия^Action.'+f.ClassName+'.'+f.Components[I].Name]:=f.Caption+', '+TAction(f.Components[I]).Caption;
end;

procedure TdmImages.UpdateTab(F: TCustomForm);
var S:String;
begin
  if (Tabs<>nil) and (Tabs.Tabs.IndexOfObject(F)>=0) then
  begin
    S:=F.Hint;
    if S='' then S:=F.Caption;
    Tabs.Tabs[Tabs.Tabs.IndexOfObject(F)]:=S;
  end;
end;

initialization
  if FileExists(ExtractFilePath(ParamStr(0))+'actions.txt') then
  begin
    ActionList:=TStringList.Create;
    ActionList.LoadFromFile(ExtractFilePath(ParamStr(0))+'actions.txt');
  end;
finalization
  if ActionList<>nil then
  begin
    ActionList.Sort;
    ActionList.SaveToFile(ExtractFilePath(ParamStr(0))+'actions.txt');
  end;
  FreeAndNil(ActionList);
end.
