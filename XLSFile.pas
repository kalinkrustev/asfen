{***************************************************************
 XLSFile version 1.0
 (c) 1999 Yudi Wibisono & Masayu Leylia Khodra (DWIDATA)
 e-mail: yudiwbs@bdg.centrin.net.id
 Address: Sarijadi Blok 23 No 20, Bandung, Indonesia (40164)
 Phone: (022) 218101

 XLSfile is free and you can modify it as long as this header
 and its copyright text is intact.
 If you make a modification, please notify me.

 WARNING! THE CODE IS PROVIDED AS IS WITH NO GUARANTEES OF ANY KIND!
 USE THIS AT YOUR OWN RISK - YOU ARE THE ONLY PERSON RESPONSIBLE FOR
 ANY DAMAGE THIS CODE MAY CAUSE - YOU HAVE BEEN WARNED!

 ****************************************************************}

unit XLSfile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
{BOF}
  CBOF      = $0009;
  BIT_BIFF5 = $0800;
  BIT_BIFF4 = $0400;
  BIT_BIFF3 = $0200;
  BOF_BIFF5 = CBOF or BIT_BIFF5;
  BOF_BIFF4 = CBOF or BIT_BIFF4;
  BOF_BIFF3 = CBOF or BIT_BIFF3;
{EOF}
  BIFF_EOF = $000a;
{Document types}
  DOCTYPE_XLS = $0010;
  DOCTYPE_XLC = $0020;
  DOCTYPE_XLM = $0040;
  DOCTYPE_XLW = $0100;
{Dimensions}
  DIMENSIONS = $0000;
  DIMENSIONS_BIFF4 = DIMENSIONS or BIT_BIFF3;
  DIMENSIONS_BIFF3 = DIMENSIONS or BIT_BIFF3;

type

  EReadError = class(Exception);
  EopCodeError = class(Exception);
  EOverUnderError = class(Exception);


  TModeOpen = (moWrite); //,moRead); //read not implemented yet


  TAtributCell = (acHidden,acLocked,acShaded,acBottomBorder,acTopBorder,
                acRightBorder,acLeftBorder,acLeft,acCenter,acRight,acFill);

  TSetOfAtribut = set of TatributCell;


  TMyFiler = class
  public
     Stream:TStream;  //stream yang akan diisi/dibaca
  end;

  TMyReader = class(TMyFiler)
  public
    function readStr:string;
    function readDouble:double;
    function readInt:integer;
    function readByte:byte;
    function readWord:word;
  end;

  TMyWriter = class(TMyFiler)
  public
   procedure WriteSingleStr(s:string);
   //tidak ada informasi length di depan str,
   //digunakan untuk cell string di Excel
   procedure WriteStr(s:string);
   {req: s shouldn't exceed 64KB
    }
   procedure WriteByte(b:byte);
   procedure WriteDouble(d:double);
   procedure WriteInt(i:integer);
   procedure WriteWord(w:word);
  end;

  TMyPersistent = class
  public
     opCode:word; //invarian: opcode<>nil, opcode<>opcodeEOF dan dalam satu aplikasi tidak boleh ada class yang memiliki opcode sama
     procedure Write(W:TMyWriter);virtual;abstract;
     {req: opcode sudah diisikan}
     procedure Read(R:TMyReader);virtual;abstract;
     {req: opcode sudah diisikan}
  end;

  TDispatcher = class
  private
      StrList:TStringList;
      Reader:TMyReader;
      Writer:TMyWriter;
  protected
      FStream:TStream; //stream yang menjadi target
      procedure SetStream(vStream:TStream);
  public
      SLError:TStringList;
      OpcodeEOF:word; //opcode yg menandakan EOF
      procedure Clear;
      procedure RegisterObj(MyPers:TMyPersistent);
      {req: MyPersistent.opCode<>0
       ens: MyPersistent terdaftar}
      procedure Write;
      {ens: semua data obj yang mendaftar masuk dalam stream}
      procedure Read;
      {ens: semua obj yang mendaftar terisi}
      constructor create;
      destructor destroy;override;
      property Stream : TStream read FStream write SetStream;
  end;

  TData = class(TMyPersistent)
  end;

  TBOF = class (TData)      //record awal di file
     procedure read(R:TMyReader);override;
     {req: opcode sudah diisi}
     procedure write(W:TMyWriter);override;
     {req: opcode sudah diisi}
     constructor create;
  end;

  TDimension = class(TData) //record akhir
     MinSaveRecs,MaxSaveRecs,MinSaveCols,MaxSaveCols:word;
     procedure read(R:TMyReader);override;
     {req: opcode sudah diisi}
     procedure write(W:TMyWriter);override;
     {req: opcode sudah diisi}
     constructor create;
  end;

  TCellClass = class of TCell;
  TCell = class(TData)
  protected
     FAtribut:array [0..2] of byte;
     procedure SetAtribut(value:TSetOfAtribut);
     {ens: FAtribut diatur sesuai dengan nilai value}
  public
     Col,Row:word;  //dari 1
     procedure read(R:TMyReader);override;
     procedure write(W:TMyWriter);override;
     property Atribut : TSetOfAtribut write SetAtribut;   //baru bisa nulis
     constructor create;virtual;abstract;
  end;

  TBlankCell = class(TCell)
    procedure read(R:TMyReader);override;
    procedure write(W:TMyWriter);override;
    {req: col, row  dan  atribut sudah ditulis}
    constructor create;override;
  end;

  TDoubleCell = class(TCell)
    Value:double;
    procedure read(R:TMyReader);override;
    procedure write(W:TMyWriter);override;
    {req: col, row  dan  atribut sudah ditulis}
    constructor create;override;
  end;

  TWordCell = class(TCell)
    Value:word;
    procedure read(R:TMyReader);override;
    procedure write(W:TMyWriter);override;
    {req: col, row  dan  atribut sudah ditulis}
    constructor create;override;
  end;

  TStrCell = class(TCell)
     Value:string;
     procedure read(R:TMyReader);override;
     procedure write(W:TMyWriter);override;
     {req: col, row  dan  atribut sudah ditulis}
     constructor create;override;
  end;

  TXLSfile = class(TComponent)
  private
    FFileName:string;
    ModeOpen:TModeOpen;
    Dispatcher:TDispatcher;
    BOF:TBOF;
    Dimension:TDimension;
    function AddCell(vCol,vRow:word;vAtribut:TSetOfAtribut;CellRef:TCellClass):TCell;
    procedure AddData(D:TData);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure AddWordCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:word);
    procedure AddDoubleCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:double);
    procedure AddStrCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:String);
    procedure write;
    procedure clear;
    constructor create(AOwner:TComponent);override;
    destructor destroy;override;
  published
    { Published declarations }
    property FileName :string read FFileName write FFileName;
  end;

procedure Register;

implementation

function TMyReader.readByte:byte;
begin
     Stream.Read(result,1);
end;

function TMyReader.readWord:word;
begin
     Stream.Read(result,2); //panjang string
end;

function TMyReader.readStr:string;
var
   Panjang:Word;
   tempStr:string;
begin
     Stream.Read(Panjang,2); //panjang string
     SetLength(tempStr,panjang);
     Stream.Read(tempStr[1],panjang);
     result:=tempStr;
end;

function TMyReader.readDouble:double;
begin
     Stream.Read(result,8);
end;

function TMyReader.readInt:integer;
begin
     Stream.Read(result,4);
end;

procedure TMyWriter.WriteByte(b:byte);
begin
     Stream.write(b,1);
end;

procedure TMyWriter.WriteWord(w:word);
begin
     Stream.write(w,2);
end;

procedure TMyWriter.WriteSingleStr(s:string);
begin
  if s<>'' then Stream.write(s[1],length(s));
end;

procedure TMyWriter.WriteStr(s:string);
{req: s shouldn't exceed 64KB
}
var
   panjang:integer;
begin
     panjang:=length(s);
     WriteWord(panjang);
     Stream.write(s[1],panjang);
end;

procedure TMyWriter.WriteDouble(d:double);
begin
     Stream.write(d,8); //asumsi double adalah 8 bytes
end;

procedure TMyWriter.WriteInt(i:integer);
begin
     Stream.write(i,4);
end;

procedure TDispatcher.Clear;
var
   i:integer;
begin
     for i:=0 to StrList.count-1 do
     begin
          TMyPersistent(StrList.Objects[i]).Free;
     end;
     StrList.Clear;
     SLError.Clear;
end;

procedure TDispatcher.SetStream(vStream:TStream);
begin
     FStream:=vStream;
     Reader.Stream:=Fstream;
     Writer.stream:=Fstream;
end;

constructor TDispatcher.create;
begin
     OpCodeEOF:=999;
     StrList:=TStringlist.create;
     Reader:=TMyReader.create;
     Writer:=TMyWriter.create;
     SLError:=TStringList.create;
end;

destructor TDispatcher.destroy;
begin
     Clear;
     StrList.free;
     Reader.free;
     Writer.free;
     SLError.free;
     inherited;
end;

procedure TDispatcher.RegisterObj(MyPers:TMyPersistent);
{req: MyPersistent.opCode<>0
ens: MyPersistent terdaftar}
begin
     StrList.AddObject(IntToStr(MyPers.opCode),MyPers);
end;

procedure TDispatcher.Write;
{ens: semua data obj yang mendaftar masuk dalam stream}
var
   i:integer;
   pos,length:longint;
begin
     //index stream, mulai dari 0!
     for i:=0 to StrList.Count-1 do
     begin
          Writer.WriteWord(TMyPersistent(StrList.objects[i]).Opcode);  //opcode
          Writer.WriteWord(0); //untuk tempat length record, nanti diisi lagi
          pos:=Stream.Position;
          TMyPersistent(StrList.Objects[i]).Write(Writer);
          //length-nya jangan lupa
          length:=Stream.Position-pos;
          Stream.Seek(-(length+2),soFromCurrent);  //balikin ke posisi tempat length
          Writer.WriteWord(length);
          Stream.Seek(length,soFromCurrent); //siap menulis lagi
     end;
     //penutup
     Writer.WriteWord(opCodeEOF);
     Writer.WriteWord(0); //panjangnya 0
end;

procedure TDispatcher.Read;
{ req: StrList terurut
  ens: semua obj yang mendaftar terisi}
var
   idx:integer;
   opCode:word;
   panjang,pos:longint;
   stop:boolean;
begin
    stop:=false;
    while not(stop) do
    begin
       opCode:=Reader.ReadWord;
       panjang:=Reader.ReadWord;
       if opCode = opCodeEOF then
          stop:=true
       else
       begin
            pos:=Stream.Position;
            idx:=StrList.IndexOf(IntToStr(opcode));
            if idx <> -1  then
               TMyPersistent(StrList.Objects[idx]).Read(Reader)
            else
            begin //opcode nggak dikenali
               SLError.Add(format('Unknown Opcode %d ',[opCode]));
               Stream.Seek(panjang,soFromCurrent);  //repair
            end;
            //cek apakah kelewatan/kurang ngebacanya
            if Stream.Position <> pos+panjang then
            begin
                begin
                     if Stream.Position<pos+panjang then
                     begin
                        SLError.Add(Format('Opcode %d underrun %d bytes',[opcode,(pos+panjang)-Stream.Position]));
                        Stream.Seek(Stream.Position - (pos+panjang),soFromCurrent);//repair
                     end
                     else
                     begin
                        SLError.Add(Format('Opcode %d overrun %d bytes',[opcode,Stream.Position-(pos+panjang)]));
                        Stream.Seek((pos+panjang)-Stream.Position,soFromCurrent); //repair
                     end;
                end;
            end;
       end; //opcode EOF
    end; //end while
    if SLerror.count>0 then
    begin
         raise EReadError.Create
         ('File format error or file corrupt . Choose File -> Save as to save this file with new format');
    end;
end;

constructor TXLSFile.create(AOwner:TComponent);
begin
     inherited create(AOwner);
     ModeOpen:=moWrite;
     Dispatcher:=TDispatcher.create;
     Dispatcher.opcodeEOF:=BIFF_EOF;
     clear;
end;

destructor TXLSFile.destroy;
begin
     Dispatcher.free;
     inherited;
end;

function TXLSFile.AddCell(vCol,vRow:word;vAtribut:TSetOfAtribut;CellRef:TCellClass):TCell;
//vCol dan Vrow mulai dari 0
//ens: XLSfile yg buat, XLSFile yang bertanggung jawab
var
   C:TCell;
begin
     C:=CellRef.create;
     with C do
     begin
          Col:=vCol;
          Row:=vRow; //yw 23 agt
          Atribut:=vAtribut;
     end;
     AddData(C);
     Result:=C;
end;

procedure TXLSFile.AddWordCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:word);
begin
     with TWordCell(AddCell(vCol,vRow,vAtribut,TWordCell)) do
          value:=aValue;
end;

procedure TXLSFile.AddDoubleCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:double);
begin
     with TDoubleCell(AddCell(vCol,vRow,vAtribut,TDoubleCell)) do
          value:=aValue;
end;

procedure TXLSFile.AddStrCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:String);
begin
     with TStrCell(AddCell(vCol,vRow,vAtribut,TStrCell)) do
          value:=aValue;
end;

procedure TXLSFile.AddData(D:TData);
//req: BOF dan dimension telah ditambahkan lebih dulu
begin
     Dispatcher.RegisterObj(D);
end;

procedure TXLSFile.write;
{req: ListDAta telah diisi}
var
   FileStream:TFIleStream;
begin
     FileStream:=TFileStream.Create(FFileName,fmCreate);
     Dispatcher.Stream:=FileStream;
     Dispatcher.Write;
     FileStream.Free;
end;

procedure TXLSFile.clear;
{req: - objek data yang dibuat secara manual (lewat c:=TWordCell.create dst..) sudah di-free
      - BOF<>nil, Dimension<>nil    }
begin
     Dispatcher.Clear;
     BOF:=TBOF.create;
     Dimension:=TDimension.create;
     Dispatcher.RegisterObj(BOF); //harus pertama
     Dispatcher.RegisterObj(Dimension); //harus kedua
end;

//TBOF  ********************************************************************
constructor TBOF.create;
begin
     opCOde:=BOF_BIFF5;
end;

procedure TBOF.read(R:TMyReader);
begin
end;

procedure TBOF.write(W:TMyWriter);
{req: opcode sudah diisikan}
begin
     with W do
     begin
         writeWord(0); //versi
         writeWord(DOCTYPE_XLS);
         writeWord(0);
     end;
end;
                                         
//TDimension ****************************************************************
procedure TDimension.read(R:TMyReader);
{req: opcode sudah diisi}
begin
end;

procedure TDimension.write(W:TMyWriter);
{req: opcode sudah diisi}
begin
     with w do
     begin
          WriteWord(MinSaveRecs);
          WriteWord(MaxSaveRecs);
          WriteWord(MinSaveCols);
          WriteWord(MaxSaveCols);
     end;
end;

constructor TDimension.create;
begin
     opCode:=DIMENSIONS;
     MinSaveRecs := 0; MaxSaveRecs := 1000;
     MinSaveCols := 0; MaxSaveCols := 100;
end;

//TCell ******************************************************************
procedure TCell.SetAtribut(value:TSetOfAtribut);
{ens: FAtribut diatur sesuai dengan nilai value}
var
   i:integer;
begin
     //reset
     for i:=0 to High(FAtribut) do
         FAtribut[i]:=0;

     {Byte Offset     Bit   Description                     Contents
     0          7     Cell is not hidden              0b
                      Cell is hidden                  1b
                6     Cell is not locked              0b
                      Cell is locked                  1b
                5-0   Reserved, must be 0             000000b
     1          7-6   Font number (4 possible)
                5-0   Cell format code
     2          7     Cell is not shaded              0b
                      Cell is shaded                  1b
                6     Cell has no bottom border       0b
                      Cell has a bottom border        1b
                5     Cell has no top border          0b
                      Cell has a top border           1b
                4     Cell has no right border        0b
                      Cell has a right border         1b
                3     Cell has no left border         0b
                      Cell has a left border          1b
                2-0   Cell alignment code
                           general                    000b
                           left                       001b
                           center                     010b
                           right                      011b
                           fill                       100b
                           Multiplan default align.   111b
     }

     //  bit sequence 76543210

     if  acHidden in value then  //byte 0 bit 7:
         FAtribut[0] := FAtribut[0] + 128;

     if  acLocked in value then //byte 0 bit 6:
         FAtribut[0] := FAtribut[0] + 64 ;

     if  acShaded in value then //byte 2 bit 7:
         FAtribut[2] := FAtribut[2] + 128;

     if  acBottomBorder in value then //byte 2 bit 6
         FAtribut[2] := FAtribut[2] + 64 ;

     if  acTopBorder in value then //byte 2 bit 5
         FAtribut[2] := FAtribut[2] + 32;

     if  acRightBorder in value then //byte 2 bit 4
         FAtribut[2] := FAtribut[2] + 16;

     if  acLeftBorder in value then //byte 2 bit 3
         FAtribut[2] := FAtribut[2] + 8;

     if  acLeft in value then //byte 2 bit 1
         FAtribut[2] := FAtribut[2] + 1
     else
     if  acCenter in value then //byte 2 bit 1
         FAtribut[2] := FAtribut[2] + 2
     else if acRight in value then //byte 2, bit 0 dan bit 1
         FAtribut[2] := FAtribut[2] + 3;
     if acFill in value then //byte 2, bit 0
         FAtribut[2] := FAtribut[2] + 4;
end;

procedure TCell.read(R:TMyReader);
begin
end;

procedure TCell.write(W:TMyWriter);
{req: opcode sudah ditulis}
var
   i:integer;
begin
     with w do
     begin
          WriteWord(Row);
          WriteWord(Col);
          for i:=0 to 2 do
          begin
               writeByte(FAtribut[i]);
          end;
     end;
end;

//TBlankCell  **************************************************************
procedure TBlankCell.read(R:TMyReader);
begin
end;

procedure TBlankCell.write(W:TMyWriter);
{req: col, row  dan  atribut sudah ditulis}
begin
end;

constructor TBlankCell.create;
begin
     opCode:=1;
end;

//TWordCell **************************************************************
procedure TWordCell.read(R:TMyReader);
begin
end;

procedure TWordCell.write(W:TMyWriter);
{req: col, row  dan  atribut sudah ditulis}
begin
     inherited write(W);
     w.WriteWord(value);
end;

constructor TWordCell.create;
begin
     opCode:=2;
end;

//TDoubleCell **************************************************************

procedure TDoubleCell.read(R:TMyReader);
begin
end;

procedure TDoubleCell.write(W:TMyWriter);
{req: col, row  dan  atribut sudah ditulis}
begin
     inherited write(W);
     w.writeDouble(value);
end;

constructor TDoubleCell.create;
begin
     opCode:=3;
end;

//TStrCell ***************************************************************
procedure TStrCell.read(R:TMyReader);
begin
     inherited read(R);
end;

procedure TStrCell.write(W:TMyWriter);
{req: col, row  dan  atribut sudah ditulis}
begin
     inherited Write(W);
     w.WriteByte(length(value));
     w.WriteSIngleStr(value);
end;

constructor TStrCell.create;
begin
     opCode:=4;
end;

procedure Register;
begin                          
  RegisterComponents('AddOn', [TXLSfile]);
end;

end.
