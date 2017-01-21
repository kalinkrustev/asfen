unit TranslateFixed;

interface
implementation
uses ComConst,Consts,DBConsts,JConsts,MidConst,
     OleConst,RTLConsts,SysConst, Windows,VDBConsts;

procedure HookResourceString(rs: PResStringRec; newStr: PChar);
var
  oldprotect : DWORD;
begin
  VirtualProtect(rs, SizeOf(rs^), PAGE_EXECUTE_READWRITE, @oldProtect);
  rs^.Identifier := integer(newStr);
  VirtualProtect(rs, SizeOf(rs^), oldProtect, @oldProtect);
end;

procedure Translate;
begin
  HookResourceString(@DBConsts.SInvalidFloatValue,'''%s'' не е валидна числова стойност за полето ''%s''');
  HookResourceString(@DBConsts.SFieldRequired , 'Полето ''%s'' трябва да има стойност');
  HookResourceString(@DBConsts.SFieldValueError , 'Невалидна стойност за полето ''%s''');
  HookResourceString(@SysConst.SInvalidFloat,'''%s'' не е валидна числова стойност');
  HookResourceString(@SysConst.SInvalidInteger,'''%s'' не е валидна целочислена стойност');
  HookResourceString(@SysConst.SInvalidDate , '''%s'' не е валидна дата');
  HookResourceString(@SysConst.SInOutError , 'Входно/изходна грешка %d');
  HookResourceString(@SysConst.SIntOverflow , 'Целочислено препълване');
  HookResourceString(@SysConst.SInvalidOp , 'Невалидна операция с плаваща запетая');
  HookResourceString(@SysConst.SZeroDivide , 'Деление на нула');
  HookResourceString(@SysConst.SOverflow , 'Препълване при работа с плаваща запетая');
  HookResourceString(@Consts.SOKButton , 'OK');
  HookResourceString(@Consts.SCancelButton , 'Отка&з');
  HookResourceString(@Consts.SYesButton , '&Да');
  HookResourceString(@Consts.SNoButton , '&Не');
  HookResourceString(@Consts.SHelpButton , '&Помощ');
  HookResourceString(@Consts.SCloseButton , '&Затвори');
  HookResourceString(@Consts.SIgnoreButton , '&Игнорирай');
  HookResourceString(@Consts.SRetryButton , '&Отново');
  HookResourceString(@Consts.SAbortButton , 'Прекрати');
  HookResourceString(@Consts.SAllButton , '&Всички');
  HookResourceString(@Consts.SMsgDlgWarning , 'Предупреждение');
  HookResourceString(@Consts.SMsgDlgError , 'Грешка');
  HookResourceString(@Consts.SMsgDlgInformation , 'Информация');
  HookResourceString(@Consts.SMsgDlgConfirm , 'Потвърждение');
  HookResourceString(@Consts.SMsgDlgYes , '&Да');
  HookResourceString(@Consts.SMsgDlgNo , '&Не');
  HookResourceString(@Consts.SMsgDlgOK , 'OK');
  HookResourceString(@Consts.SMsgDlgCancel , 'Отка&з');
  HookResourceString(@Consts.SMsgDlgHelp , '&Помощ');
  HookResourceString(@Consts.SMsgDlgHelpNone , 'Няма достъпна помощ');
  HookResourceString(@Consts.SMsgDlgHelpHelp , 'Помощ');
  HookResourceString(@Consts.SMsgDlgAbort , 'Прекрати');
  HookResourceString(@Consts.SMsgDlgRetry , '&Отново');
  HookResourceString(@Consts.SMsgDlgIgnore , '&Игнорирай');
  HookResourceString(@Consts.SMsgDlgAll , '&Всички');
  HookResourceString(@Consts.SMsgDlgNoToAll , 'Н&е на всички');
  HookResourceString(@Consts.SMsgDlgYesToAll , 'Д&а на всички');
  HookResourceString(@VDBConsts.SDeleteRecordQuestion , 'Изтриване на реда?');
  HookResourceString(@VDBConsts.SDeleteMultipleRecordsQuestion , 'Изтриване на избраните редове?');

end;

initialization
  Translate;
finalization
end.

