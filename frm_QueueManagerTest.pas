unit frm_QueueManagerTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmQueueManagerTest = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBox2: TListBox;
    ListBox3: TListBox;
    ListBox4: TListBox;
    ListBox5: TListBox;
    ListBox6: TListBox;
    ListBox7: TListBox;
    ListBox8: TListBox;
    ListBox9: TListBox;
    ListBox10: TListBox;
    ListBox11: TListBox;
    ListBox12: TListBox;
    ListBox13: TListBox;
    ListBox14: TListBox;
    ListBox15: TListBox;
    ListBox16: TListBox;
    ListBox17: TListBox;
    ListBox18: TListBox;
    ListBox19: TListBox;
    ListBox20: TListBox;
    ListBox21: TListBox;
    ListBox22: TListBox;
    ListBox23: TListBox;
    ListBox24: TListBox;
    ListBox25: TListBox;
    ListBox26: TListBox;
    ListBox27: TListBox;
    ListBox28: TListBox;
    ListBox29: TListBox;
    ListBox30: TListBox;
    ListBox31: TListBox;
    ListBox32: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    Cnt:Integer;
    Threads:array[1..32] of TThread;
  public
    { Public declarations }
  end;

var
  frmQueueManagerTest: TfrmQueueManagerTest;

implementation

uses SIP_QueueManager, SIP_RingList;

{$R *.dfm}

var
  QM:TSIPQueueManager;

type
  MyThread=class(TThread)
  private
    CurrentScript,CurrentAddress:String;
    procedure Show;
  public
    L:TListBox;
    constructor Create(LB:TListBox);
    procedure Execute;override;
  end;

procedure TfrmQueueManagerTest.Button1Click(Sender: TObject);
var RL:TSIPRingList;
    I:Integer;
begin
  RL:=TSIPRingList.Create;
  for I := 1 to 80 do
  begin
    Cnt:=Cnt+1;
    RL.Add(IntToStr(Cnt));
  end;
  QM.Add((Sender as TButton).Caption,RL,1+Random(5));
end;

procedure TfrmQueueManagerTest.Button4Click(Sender: TObject);
begin
  Button1.Click;
//  Sleep(30);
  Button2.Click;
//  Sleep(30);
  Button3.Click;
end;

procedure TfrmQueueManagerTest.FormCreate(Sender: TObject);
var I:Integer;
begin
  QM:=TSIPQueueManager.Create;
  for I := 1 to 32 do
  begin
    Threads[I]:=MyThread.Create(FindComponent('ListBox'+IntToStr(I)) as TListBox);
  end;
end;

{ MyThread }

constructor MyThread.Create(LB:TListBox);
begin
  Inherited Create(True);
  L:=LB;
  Resume;
end;

procedure MyThread.Execute;
begin
  repeat
    Sleep(Random(30));
    QM.Next(CurrentScript,CurrentAddress);
    if (CurrentScript<>'') and (CurrentAddress<>'') then
      Synchronize(Show);
  until Application.Terminated;
end;

procedure MyThread.Show;
begin
  L.Items.Add(CurrentScript+'('+CurrentAddress+')');
end;

end.
