unit frm_Password;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmPassword = class(TForm)
    Password: TEdit;
    Label1: TLabel;
    btnOk: TButton;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    LockPassword:String;
  public
    { Public declarations }
  end;

procedure GetPassword(Password:String);

implementation
uses Util, dm_GUI;
{$R *.dfm}

procedure GetPassword(Password:String);
var F: TfrmPassword;
begin
  F:=nil;
  Application.CreateForm(TfrmPassword,F);
  F.LockPassword:=Password;
  F.ShowModal;
end;

procedure TfrmPassword.btnCloseClick(Sender: TObject);
begin
  if dmGUI.Confirm('Ако имате незаписани промени, те ще бъдат загубени.'#13#10'Наистина ли желаете да затворите приложението?',
                   mtWarning) then
    Application.Terminate;
end;

procedure TfrmPassword.btnOkClick(Sender: TObject);
begin
  if Password.Text=LockPassword then ModalResult:=mrOk
  else raise TMessageException.Create('Грешна парола');
end;

procedure TfrmPassword.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=Application.Terminated or (ModalResult=mrOk);
end;

end.
