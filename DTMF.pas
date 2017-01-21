unit DTMF;

interface

// translated from
// http://www.answers.com/topic/goertzel-algorithm?cat=technology

const
  Tones: Array[0..3, 0..3] of Char = (
    ('1', '4', '7', '*'),
    ('2', '5', '8', '0'),
    ('3', '6', '9', '#'),
    ('A', 'B', 'C', 'D'));
type
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..16383] of SmallInt;
  TDTMF=class(TObject)
  private
    FSampleRate:Integer;
    FSampleCount:Integer;
    q1,q2,r,coefs:array of Double;
    Last1,Last2:char;
    function Test:char;inline;register;
  public
    function AddSample(S:Smallint):char;inline;register;
    constructor Create(SampleRate:Integer);
  end;
implementation

const
  MAX_BINS = 8;
  GOERTZEL_N = 92;
  FREQS : array [0..7] of Double = (697,  770,  852,  941,  1209,  1336,  1477,  1633);

{ TDTMF }

function TDTMF.AddSample(S: Smallint):char;
var
  I: Integer;
  q0:Double;
begin
  Inc(FSampleCount);
  for I := 0 to MAX_BINS - 1 do
  begin
    q0:=coefs[I]*q1[I]-q2[I]+S;
    q2[I]:=q1[I];
    q1[I]:=q0;
  end;
  if FSampleCount=GOERTZEL_N then
  begin
    for I := 0 to MAX_BINS - 1 do
    begin
      r[I]:=q1[I]*q1[I]+q2[I]*q2[I]-coefs[I]*q1[I]*q2[I];
      q1[I]:=0;
      q2[I]:=0;
    end;
    Result:=Test;
    FSampleCount:=0;
  end else
    Result:=#0;
end;

constructor TDTMF.Create(SampleRate: Integer);
var I:Integer;
begin
  FSampleRate:=SampleRate;

  SetLength(q1,MAX_BINS);
  SetLength(q2,MAX_BINS);
  SetLength(r,MAX_BINS);
  SetLength(coefs,MAX_BINS);

  for I := 0 to MAX_BINS - 1 do
    coefs[I]:=2*cos(2*PI*FREQS[I]/FSampleRate);

end;

function TDTMF.Test;
var
  row, col,peak_count, max_index, i :Integer;
  maxval, t:double;
begin
  row:=0;
  maxval:=0;
  for I := 0 to 3 do
  if r[I]>maxval then
  begin
    maxval:=r[I];
    row:=I;
  end;
  col:=4;
  maxval:=0;
  for I := 4 to 7 do
  if r[I]>maxval then
  begin
    maxval:=r[I];
    col:=I;
  end;

  Result:=#0;
  if r[row]<4e5 then
  else
  if r[col]<4e5 then
  else
  begin
    result:=#1;
    if r[col]>r[row] then
    begin
      max_index:=col;
      if r[row]<r[col]*0.399 then
        result:=#0;
    end else
    begin
      max_index:=row;
      if r[col]<r[row]*0.158 then
        result:=#0;
    end;

    if r[max_index] > 1e9 then
      t:=r[max_index] * 0.158
    else
      t:=r[max_index] * 0.010;

    peak_count := 0;
    for I := 0 to 7 do
    begin
      if r[I]>t then inc(peak_count);
    end;
    if peak_count>2 then
      result:=#0;

    if result=#1 then result:=Tones[col-4][row];
  end;

  if Result<>Last1 then
  begin
    if (Result=Last2) and (Result<>#0) then
    begin
      Last1:=Result;
    end else
    begin
      if (Last1<>Last2) then Last1:=#0;
      Last2:=Result;
      Result:=#0;
    end;
  end else
  begin
    Last2:=Result;
    Result:=#0;
  end;

end;

end.
