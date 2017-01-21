{
  @html(<b>)
  Information Objects
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit defines easy-to-use helper classes for handling user-defined
  data structures (native Values, Arrays, Records, DataSets, etc) with RTC components.
}
unit rtcInfo;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows,
  Classes,
  SysUtils,

  {$IFNDEF IDE_1}
  Variants,
  {$ELSE}
  FileCtrl,
  {$ENDIF}

  rtcFastStrings,
  memStrObjList;

var
  // Full Name of the application or extension in which RTC is running.
  AppFileName:string='';

  { Default memory cache size when accepting FormPost data (Param property).
    When cache memory is full, data will start being written to a temporary file,
    to reduce memory usage. This temporary file is created and deleted automaticaly by RTC,
    to keep the usage of the Param property simple. }
  RTC_FORMPOST_CACHESIZE:integer=16000;

{$IFDEF COMPRESS}
  { Minimum data size to be compressed when using automatic compression/decompression.
    Data smaller than this size will not be compressed, to reduce the CPU usage.
    Default value is 246, values smaller than 32 will only increase the CPU usage
    without having any other effect, since very small data chunks can not be compressed
    (there are at least 10 bytes of header information needed in compressed data). }
  RTC_MIN_COMPRESS_SIZE:integer=256;
{$ENDIF}

const
  // Member name used in XML-RPC <struct> to enumerate DataSet Fields
  RTC_XMLRPC_DataSetFieldsName='RTC.DATASET.FIELDS';
  // Member name used in XML-RPC <struct> to enumerate DataSet Rows
  RTC_XMLRPC_DataSetRowsName='RTC.DATASET.ROWS';
  // XML-RPC field name used when sending params to a function as an array rather than a record
  RTC_XMLRPC_ParamsAsArrayName='PARAMS';

type
{$IFDEF COMPRESS}
  TRtcCompressLevel = (// no compression
                       cNone,
                       // fastest compression (low compress rate)
                       cFast,
                       // default compression
                       cDefault,
                       // maximum compression (slow)
                       cMax);
{$ENDIF}

  TRtcCertStoreType = (// Accept any certificate (Ignore certificate errors)
                       certAny,
                       // Do not use any certificates
                       certNone,
                       // Certification Authority certificates
                       certCA,
                       // A certificate store that holds certificates with associated private keys
                       certMY,
                       // Root Certificates
                       certROOT,
                       // Software Publisher Certificates
                       certSPC);

  TRtcFileAccessMode= (// Allow other apps to read and write to the same file
                       rtc_ShareDenyNone,
                       // Allow other apps to read, but not to write to the file
                       rtc_ShareDenyWrite,
                       // Deny all access to the file - exclusive access mode!
                       rtc_ShareExclusive);

  TRtcValueTypes = (// No value assigned
                    rtc_Null,
                    // variable name: Check local or Session variables for a result
                    rtc_Variable,
                    // Function call (Function will be called to get a result)
                    rtc_Function,
                    // Exception message (returned as a Result from a function call which ended in an exception being raised)
                    rtc_Exception,
                    // Array (starting from index 0)
                    rtc_Array,
                    // Record
                    rtc_Record,
                    // DataSet
                    rtc_DataSet,
                    // Text value (String that has to be automaticaly coded and encoded using UTF-8)
                    rtc_Text,
                    // String value (strings up to 2GB)
                    rtc_String,
                    // Wide String value (wide strings up to 2GB)
                    rtc_WideString,
                    // Boolean value
                    rtc_Boolean,
                    // Integer value
                    rtc_Integer,
                    // Large Integer value (int64)
                    rtc_LargeInt,
                    // Floating-point value (double)
                    rtc_Float,
                    // Currency value
                    rtc_Currency,
                    // Date and Time value
                    rtc_DateTime,
                    // Byte Stream
                    rtc_ByteStream,
                    // Any Type
                    rtc_Variant);

  TRtcDataFormat = ( // The best format for communicating between RTC Clients and Servers
                     fmt_RTC,
                     // XML-RPC format: makes it possible to communicate with non-RTC Clients and Servers
                     fmt_XMLRPC);

  TRtcDataFormatSupport = set of TRtcDataFormat;

  TRtcFieldTypes = ( ft_Unknown, ft_String, ft_Smallint, ft_Integer, ft_Word,
                     ft_Boolean, ft_Float, ft_Currency, ft_BCD, ft_Date, ft_Time, ft_DateTime,
                     ft_Bytes, ft_VarBytes, ft_AutoInc, ft_Blob, ft_Memo, ft_Graphic, ft_FmtMemo,
                     ft_ParadoxOle, ft_DBaseOle, ft_TypedBinary, ft_Cursor, ft_FixedChar, ft_WideString,
                     ft_Largeint, ft_ADT, ft_Array, ft_Reference, ft_DataSet, ft_OraBlob, ft_OraClob,
                     ft_Variant, ft_Interface, ft_IDispatch, ft_Guid, ft_TimeStamp, ft_FMTBcd);

  { @abstract(Session lock type) }
  TRtcSessionLockType=({ Allow access to anyone. No client data will be used for identifying clients,
                         which means that any client knowing which Session IDs are open,
                         will have access to those sessions, regardless of its IP address or other header values.
                         @html(<br><br>)
                         This setting is not recommended for Servers available on the Internet. }
                       sesNoLock,
                       { Allow access to an opened Session only to Clients coming from
                         the same IP as the client which has created the session.
                         @html(<br><br>)
                         This setting is not recommended for Web Applications which need to be accessible
                         by anyone, since people behind proxy servers with changing IP addresses will not
                         be able to "stay logged in" (when their IP changes, they will lose access to their Session). }
                       sesIPLock,
                       { This is the default Session Lock setting, which should work for all clients.
                         It will provide maximum security for clients which are NOT behind a proxy server,
                         while still allowing access to clients behind proxy servers with changing IP addresses
                         (the "X-FORWARDED-FOR" header has to be set by the proxy forwarding client requests).
                         @html(<br><br>)
                         If the client opening the Session had the "X-FORWARDED-FOR" header set,
                         any client with the same "X-FORWARDED-FOR" header will be allowed access to his Session
                         (it just has to use the same Session ID as the client which has opened/created the session).
                         If "X-FORWARDER-FOR" header was not set by the client creating the session,
                         Peer IP address will be used for client identification. }
                       sesFwdLock,
                       { Session will always be locked to the Peer IP address,
                         *plus* to the "X-FORWARDED-FOR" request header,
                         if it was set for the client which has opened the session.
                         @html(<br><br>)
                         This setting is not recommended for Web Applications which need to be accessible
                         to a wide public audience, since people behind proxy servers with changing IP addresses
                         will NOT be able to "stay logged in". When their IP address changes,
                         they will lose access to their Session data and need to log in again. }
                       sesIPFwdLock);

const
  // @exclude
  RTC_FIELD2VALUE_TYPES: array[TRtcFieldTypes] of TRtcValueTypes =
                   ( rtc_NULL, // ft_Unknown
                     rtc_String, // ft_String
                     rtc_Integer, // ft_Smallint
                     rtc_Integer, // ft_Integer
                     rtc_Integer, // ft_Word
                     rtc_Boolean, // ft_Boolean
                     rtc_Float, // ft_Float
                     rtc_Currency, // ft_Currency
                     rtc_Currency, // ft_BCD
                     rtc_DateTime, // ft_Date
                     rtc_DateTime, // ft_Time
                     rtc_DateTime, // ft_DateTime
                     rtc_String, // ft_Bytes
                     rtc_String, // ft_VarBytes,
                     rtc_Integer, // ft_AutoInc
                     rtc_String, // ft_Blob
                     rtc_String, // ft_Memo
                     rtc_String, // ft_Graphic
                     rtc_String, // ft_FmtMemo
                     rtc_String, // ft_ParadoxOle
                     rtc_String, // ft_DBaseOle
                     rtc_String, // ft_TypedBinary
                     rtc_NULL, // ft_Cursor
                     rtc_String, // ft_FixedChar
                     rtc_WideString, // ft_WideString
                     rtc_LargeInt, // ft_Largeint
                     rtc_Record, // ft_ADT
                     rtc_Array, // ft_Array
                     rtc_Record, // ft_Reference
                     rtc_DataSet, // ft_DataSet
                     rtc_String, // ft_OraBlob,
                     rtc_String, // ft_OraClob,
                     rtc_String, // ft_Variant
                     rtc_String, // ft_Interface
                     rtc_String, // ft_IDispatch,
                     rtc_String, // ft_Guid,
                     rtc_DateTime, // ft_TimeStamp
                     rtc_Currency); // ft_FMTBcd

  // @exclude
  RTC_TYPE2STR_CONV: array[TRtcValueTypes] of string =
                  ( 'X', // rtc_Null,
                    'V', // rtc_Variable,
                    'FC', // rtc_Function,
                    'E', // rtc_Exception,
                    'AR', // rtc_Array,
                    'RE', // rtc_Record,
                    'DS', // rtc_DataSet,
                    'T', // rtc_Text,
                    'S', // rtc_String,
                    'W', // rtc_WideString,
                    'B', // rtc_Boolean,
                    'I', // rtc_Integer,
                    'L', // rtc_LargeInt,
                    'F', // rtc_Float,
                    'C', // rtc_Currency,
                    'D', // rtc_DateTime,
                    'BS', //rtc_ByteStream
                    '');

  RTC_TYPE2FULLNAME_CONV: array[TRtcValueTypes] of string =
                  ( 'Null', // rtc_Null,
                    'Variable', // rtc_Variable,
                    'FunctionCall', // rtc_Function,
                    'Exception', // rtc_Exception,
                    'Array', // rtc_Array,
                    'Record', // rtc_Record,
                    'DataSet', // rtc_DataSet,
                    'Text', // rtc_Text,
                    'String', // rtc_String,
                    'WideString', // rtc_WideString,
                    'Boolean', // rtc_Boolean,
                    'Integer', // rtc_Integer,
                    'LargeInt', // rtc_LargeInt,
                    'Float', // rtc_Float,
                    'Currency', // rtc_Currency,
                    'DateTime', // rtc_DateTime,
                    'ByteStream', //rtc_ByteStream
                    'Variant');

  // @exclude
  RTC_FIELD2STR_CONV: array[TRtcFieldTypes] of string =
                   ('U', // ft_Unknown: Result:='U';
                    'S', // ft_String: Result:='S';
                    'SI', // ft_Smallint: Result:='SI';
                    'I', // ft_Integer: Result:='I';
                    'WI', // ft_Word: Result:='WI';
                    'B', // ft_Boolean: Result:='B';
                    'F', // ft_Float: Result:='F';
                    'C', // ft_Currency: Result:='C';
                    'BC', // ft_BCD: Result:='BC';
                    'DD', // ft_Date: Result:='DD';
                    'T', // ft_Time: Result:='T';
                    'D', // ft_DateTime: Result:='D';
                    'BY', // ft_Bytes: Result:='BY';
                    'VB', // ft_VarBytes: Result:='VB';
                    'AI', // ft_AutoInc: Result:='AI';
                    'O', // ft_Blob: Result:='O';
                    'M', // ft_Memo: Result:='M';
                    'G', // ft_Graphic: Result:='G';
                    'FM', // ft_FmtMemo: Result:='FM';
                    'PO', // ft_ParadoxOle: Result:='PO';
                    'DO', // ft_DBaseOle: Result:='DO';
                    'TB', // ft_TypedBinary: Result:='TB';
                    'CU', // ft_Cursor: Result:='CU';
                    'FC', // ft_FixedChar: Result:='FC';
                    'W', // ft_WideString: Result:='W';
                    'L', // ft_Largeint: Result:='L';
                    'AD', // ft_ADT: Result:='AD';
                    'AR', // ft_Array: Result:='AR';
                    'RF', // ft_Reference: Result:='RF';
                    'DS', // ft_DataSet: Result:='DS';
                    'OB', // ft_OraBlob: Result:='OB';
                    'OC', // ft_OraClob: Result:='OC';
                    'V', // ft_Variant: Result:='V';
                    'IT', // ft_Interface: Result:='IT';
                    'ID', // ft_IDispatch: Result:='ID';
                    'GU', // ft_Guid: Result:='GU';
                    'DT', // ft_TimeStamp: Result:='DT';
                    'FB'); // ft_FMTBcd: Result:='FB';

  // @exclude
  RTC_VALUE2FIELD_TYPES: array[TRtcValueTypes] of TRtcFieldTypes  =
                   (ft_Unknown, // rtc_Null
                    ft_Unknown, // rtc_Variable
                    ft_Unknown, // rtc_Function
                    ft_String, // rtc_Exception
                    ft_Array, // rtc_Array
                    ft_ADT, // rtc_Record
                    ft_DataSet, // rtc_DataSet
                    ft_String, // rtc_Text
                    ft_String, // rtc_String
                    ft_WideString, // rtc_WideString
                    ft_Boolean, // rtc_Boolean
                    ft_Integer, // rtc_Integer
                    ft_Largeint, // rtc_LargeInt
                    ft_Float, // rtc_Float
                    ft_Currency, // rtc_Currency
                    ft_DateTime, // rtc_DateTime
                    ft_Blob, // rtc_ByteStream
                    ft_Unknown);

type
  { All RTC components use at least one class declared in this unit.

    For Delphi to add this unit to uses clause,
    all RTC components inherit from this class.

    @exclude }
  TRtc_Component = class(TComponent);

  // Type to use to store Floating-point values (default:double; could be changed to "extended" for maximum precision)
  rtcFloat = double;

  // Type to use to store Integer values (default:LongInt; could be changed to "int64" for maximum precision)
  rtcInteger = longint;

  // Type to use to store LargeInt values (default:int64)
  rtcLargeInt = int64;

  // Type to use to store ByteStream data (default:TMemoryStream);
  rtcByteStream = TMemoryStream;

  // @exclude
  rtcClosingTagsType = array of string;

  { @abstract(Basic class for 'storable' objects)

    This is the object class which has to be extended
    with your object information if you want to pass
    objects to RTC components. }
  TRtcObject = class
    { Implement the Kill method so it releases the object from memory.
      Calling 'Free' from anyone else than the class creator (you)
      could result in freeing objects more than once, which is "unhealthy".
      The Kill method will be called on all objects that still
      remain in the list (info list, job queue, etc) when the
      list is being cleared, for whatever reason. }
    procedure Kill; virtual; abstract;
    end;

  TRtcFunctionInfo = class; // forward

  TRtcValue = class; // forward
  TRtcValueObject = class; // forward
  TRtcDataSet = class; // forward
  TRtcRecord = class; // forward
  TRtcArray = class; // forward
  TRtcVariableName = class; // forward
  TRtcExceptionValue = class; // forward
  TRtcByteStream = class; // forward

  { @abstract(All RTC Value Objects extend this class) }
  TRtcValueObject = class(TRtcObject)
  protected
    // @exclude
    function GetTypeStr:String;
    // @exclude
    procedure CopyFrom(Value:TRtcValueObject); virtual; abstract;

    // Object has been extracted from its parent
    procedure Extracted; virtual;

    { Check object type
      @exclude}
    function GetType:TRtcValueTypes; virtual; abstract;

    // @exclude
    class function ObjectFromVariant(const v:Variant):TRtcValueObject;

    // @exclude
    class function ObjectFromType(const typ:TRtcValueTypes):TRtcValueObject;

    {*** RTC format serializing and deserializing functions ***}

    // @exclude
    class function code_toLongString(const typ, s:String):String;
    // @exclude
    class function code_toByteStream(const typ:string; bs:TStream):String;
    // @exclude
    class function code_toShortString(const typ, s:String):String;
    // @exclude
    class function code_toNameString(const s:String):String;
    // @exclude
    class function code_toMidString(const s:String):String;
    // @exclude
    class function code_toEndString(const s:String):String;

    // @exclude
    class function code_checkStrType(const s:String; const at:integer):TRtcValueTypes;
    // @exclude
    class function code_fromLongString(const typ:String; const s:String; var at:integer):String;
    // @exclude
    class procedure code_fromByteStream(const typ:String; const s:String; var at:integer; const bs:TStream);
    // @exclude
    class function code_fromShortString(const typ:String; const s:String; var at:integer):String;
    // @exclude
    class function code_fromNameString(const s: String; var at:integer):String;
    // @exclude
    class function code_fromMidString(const s: String; var at:integer):String;
    // @exclude
    class function code_fromEndString(const s: String; var at:integer):String;

    // @exclude
    class function ObjectFromCode(const s:String; var at:integer):TRtcValueObject; overload;
    // @exclude
    class function ObjectFromCode(const s:String):TRtcValueObject; overload;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); overload; virtual; abstract;
    { Fill object information from String.
      @exclude }
    procedure from_Code(const s:String); overload;

    {*** XML RPC serilizing and deserializing functions ***}

    // @exclude
    class function xmlrpc_FirstCloseTag(var closing_tags:rtcClosingTagsType):string;
    // @exclude
    class procedure xmlrpc_OpenTag(const tag:string; var closing_tags:rtcClosingTagsType);
    // @exclude
    class function xmlrpc_CloseTag(const tag:string; var closing_tags:rtcClosingTagsType):boolean;
    // @exclude
    class function xmlrpc_TagsToXML(var closing:rtcClosingTagsType):string;

    // @exclude
    class function xmlrpc_checkStrType(const s:String; const at:integer):TRtcValueTypes;
    // @exclude
    class procedure xmlrpc_skipWhitespace(const s:String; var at:integer);
    // @exclude
    class function xmlrpc_checkTag(const s:String; at:integer):string;
    // @exclude
    class procedure xmlrpc_skipTag(const s:String; var at:integer; skipWhitespace:boolean=True);
    // @exclude
    class function xmlrpc_readTag(const s:String; var at:integer; const tag_want:string=''; skipWhitespace:boolean=True):string;
    // @exclude
    class function xmlrpc_readValue(const s:String; var at:integer):string;
    // @exclude
    class function xmlrpc_readTrimValue(const s:String; var at:integer):string;
    // @exclude
    class procedure xmlrpc_skipNull(const s: String; var at: integer);

    // @exclude
    class procedure xmlrpc_readByteStream(const s:String; var at:integer; const bs:TStream);
    // @exclude
    class function xmlrpc_writeByteStream(bs:TStream):string;
    // @exclude
    class function xmlrpc_readString(const s:String; var at:integer):string;
    // @exclude
    class function xmlrpc_writeString(const s:String):String;

    // @exclude
    class procedure xmlrpc_skipValueOpen(const tag:string; const s: String; var at: integer; var closing_tags:rtcClosingTagsType);
    // @exclude
    class procedure xmlrpc_skipValueClose(const s:String; var at:integer; var closing_tags:rtcClosingTagsType);

    // @exclude
    class function ObjectFromXMLrpc(const s:String; var at:integer):TRtcValueObject; overload;
    // @exclude
    class function ObjectFromXMLrpc(const s:String):TRtcValueObject; overload;

    { Fill object information from XML RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); overload; virtual; abstract;
    { Fill object information from String.
      @exclude }
    procedure from_XMLrpc(const s:String); overload;

  public
    // @exclude
    procedure Kill; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; virtual; abstract;

    { Create a String containing all object information,
      serialized using a packed RTC-coded format. }
    procedure to_Code(const Result:TRtcHugeString); virtual; abstract;

    { Create a String containing all object information,
      serialized using a packed RTC-coded format. }
    function toCode:String;

    { Create the XML-RPC string containing all object information,
      serialized using the XML RPC standard format. }
    procedure to_XMLRPC(const Result:TRtcHugeString); virtual; abstract;

    { Create the XML-RPC string containing all object information,
      serialized using the XML RPC standard format. }
    function toXMLrpc:String;

    { Create the XML-RPC string containing all object information,
      packed in the XML-RPC request header and footer. }
    function toXMLrpcRequest:string;

    { Create the XML-RPC string containing all object information,
      packed in the XML-RPC response header and footer. }
    function toXMLrpcResponse:string;
    end;

  // @exclude
  TRtcSimpleValue = class(TRtcValueObject)
  protected
    procedure Extracted; override;

  public
    function GetBoolean: boolean; virtual;
    function GetCurrency: Currency; virtual;
    function GetDateTime: TDateTime; virtual;
    function GetException: String; virtual;
    function GetVarName: String; virtual;
    function GetInteger: rtcInteger; virtual;
    function GetLargeInt: rtcLargeInt; virtual;
    function GetFloat: rtcFloat; virtual;
    function GetString: String; virtual;
    function GetWideString: WideString; virtual;
    function GetText: String; virtual;
    function GetByteStream: TStream; virtual;

    procedure SetNull(const Value: boolean); virtual;

    procedure SetBoolean(const Value: boolean); virtual;
    procedure SetCurrency(const Value: Currency); virtual;
    procedure SetDateTime(const Value: TDateTime); virtual;
    procedure SetException(const Value: String); virtual;
    procedure SetVarName(const Value: String); virtual;
    procedure SetInteger(const Value: rtcInteger); virtual;
    procedure SetLargeInt(const Value: rtcLargeInt); virtual;
    procedure SetFloat(const Value: rtcFloat); virtual;
    procedure SetString(const Value: String); virtual;
    procedure SetWideString(const Value: WideString); virtual;
    procedure SetText(const Value: String); virtual;
    procedure SetByteStream(const Value:TStream); virtual;

    function GetVariant: Variant; virtual;
    function SetVariant(const Value: Variant):boolean; virtual;
    end;

  // @exclude
  TRtcByteStream = class(TRtcSimpleValue)
  private
    FValue:TStream;

  protected
    class function NullValue:TStream;
    procedure CopyFrom(Value:TRtcValueObject); override;

    procedure Extracted; override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:TStream); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;

    function GetByteStream: TStream; override;
    procedure SetByteStream(const Value: TStream); override;

    function GetString: String; override;
    function GetVariant: Variant; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcExceptionValue = class(TRtcSimpleValue)
  private
    FValue:String;

  protected
    class function NullValue:String;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:string); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetString: String; override;

    function GetException: String; override;
    procedure SetException(const Value: String); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcVariableName = class(TRtcSimpleValue)
  private
    FValue:String;

  protected
    class function NullValue:String;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:String); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetString: String; override;

    function GetVarName: String; override;
    procedure SetVarName(const Value: String); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcBooleanValue=class(TRtcSimpleValue)
  private
    FValue:boolean;

  protected
    class function NullValue:boolean;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s). }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s). }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:boolean); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetCurrency: Currency; override;
    function GetInteger: rtcInteger; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;

    function GetBoolean: boolean; override;
    procedure SetBoolean(const Value: boolean); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcStringValue=class(TRtcSimpleValue)
  private
    FValue:String;

  protected
    class function NullValue:String;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:string); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText: String; override;

    function GetString: String; override;
    procedure SetString(const Value: String); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcWideStringValue=class(TRtcSimpleValue)
  private
    FValue:WideString;

  protected
    class function NullValue:String;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:WideString); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetText: String; override;

    function GetString: String; override;
    procedure SetString(const Value: String); override;

    function GetWideString: WideString; override;
    procedure SetWideString(const Value: WideString); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcTextValue=class(TRtcSimpleValue)
  private
    FValue:String;

  protected
    class function NullValue:String;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(const Value:String); overload;

    destructor Destroy; override;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;
    procedure SetString(const Value: String); override;

    function GetText: String; override;
    procedure SetText(const Value: String); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcIntegerValue=class(TRtcSimpleValue)
  private
    FValue:rtcInteger;

  protected
    class function NullValue:rtcInteger;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcInteger); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcLargeIntValue=class(TRtcSimpleValue)
  private
    FValue:rtcLargeInt;

  protected
    class function NullValue:rtcInteger;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLRPC, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcLargeInt); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;

    function GetInteger: rtcInteger; override;
    procedure SetInteger(const Value: rtcInteger); override;

    function GetLargeInt: rtcLargeInt; override;
    procedure SetLargeInt(const Value: rtcLargeInt); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcFloatValue=class(TRtcSimpleValue)
  private
    FValue:rtcFloat;

  protected
    class function NullValue:rtcFloat;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:rtcFloat); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;

    function GetString: String; override;

    function GetFloat: rtcFloat; override;
    procedure SetFloat(const Value: rtcFloat); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcCurrencyValue=class(TRtcSimpleValue)
  private
    FValue:Currency;

  protected
    class function NullValue:Currency;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:Currency); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetDateTime: TDateTime; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;

    function GetCurrency: Currency; override;
    procedure SetCurrency(const Value: Currency); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  // @exclude
  TRtcDateTimeValue=class(TRtcSimpleValue)
  private
    FValue:TDateTime;

  protected
    class function NullValue:TDateTime;
    procedure CopyFrom(Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    constructor Create; overload;
    constructor Create(Value:TDateTime); overload;

    procedure SetNull(const Value: boolean); override;

    function GetType:TRtcValueTypes; override;
    function GetBoolean: boolean; override;
    function GetCurrency: Currency; override;
    function GetInteger: rtcInteger; override;
    function GetLargeInt: rtcLargeInt; override;
    function GetFloat: rtcFloat; override;

    function GetString: String; override;

    function GetDateTime: TDateTime; override;
    procedure SetDateTime(const Value: TDateTime); override;

    function GetVariant: Variant; override;
    function SetVariant(const Value: Variant):boolean; override;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  { @abstract(Abstract Value Object)
    There are two ways to work with TRtcAbsValue objects.
    One is to let the stored object be auto-created on first read access
    to the asStream/asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewStream/NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsValue = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    procedure SetObject(const Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure SetAsObject(const Value:TRtcValueObject);
    // @exclude
    function GetObject:TRtcValueObject; virtual; abstract;

    // @exclude
    function GetArray: TRtcArray;
    // @exclude
    function GetRecord: TRtcRecord;
    // @exclude
    function GetDataSet: TRtcDataSet;
    // @exclude
    function GetFunctionInfo: TRtcFunctionInfo;

    // @exclude
    function GetBoolean: boolean;
    // @exclude
    function GetCurrency: Currency;
    // @exclude
    function GetDateTime: TDateTime;
    // @exclude
    function GetException: String;
    // @exclude
    function GetVarName: String;
    // @exclude
    function GetInteger: rtcInteger;
    // @exclude
    function GetLargeInt: rtcLargeInt;
    // @exclude
    function GetNull: boolean;
    // @exclude
    function GetFloat: rtcFloat;
    // @exclude
    function GetString: String;
    // @exclude
    function GetWideString: WideString;
    // @exclude
    function GetText: String;
    // @exclude
    function GetByteStream: TStream;

    // @exclude
    function GetVariant: Variant;
    // @exclude
    procedure SetVariant(const Value: Variant);

    // @exclude
    procedure SetArray(const Value: TRtcArray);
    // @exclude
    procedure SetRecord(const Value: TRtcRecord);
    // @exclude
    procedure SetDataSet(const Value: TRtcDataSet);
    // @exclude
    procedure SetFunctionInfo(const Value: TRtcFunctionInfo);

    // @exclude
    procedure SetBoolean(const Value: boolean);
    // @exclude
    procedure SetCurrency(const Value: Currency);
    // @exclude
    procedure SetDateTime(const Value: TDateTime);
    // @exclude
    procedure SetException(const Value: String);
    // @exclude
    procedure SetVarName(const Value: String);
    // @exclude
    procedure SetInteger(const Value: rtcInteger);
    // @exclude
    procedure SetLargeInt(const Value: rtcLargeInt);
    // @exclude
    procedure SetNull(const Value: boolean);
    // @exclude
    procedure SetFloat(const Value: rtcFloat);
    // @exclude
    procedure SetString(const Value: String);
    // @exclude
    procedure SetWideString(const Value: WideString);
    // @exclude
    procedure SetText(const Value: String);
    // @exclude
    procedure SetByteStream(const Value: TStream);

    // @exclude
    function GetCode: String;
    // @exclude
    procedure SetCode(const Value: String);

    // @exclude
    function GetXMLrpc: String;
    // @exclude
    procedure SetXMLrpc(const Value: String);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array.
      To turn this Value object into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray:= (assigns a copy of the array) or
      asObject:= (assigns existing array instance). }
    function NewArray: TRtcArray;
    { Assign a new, fresh Record.
      To turn this Value object into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord:= (assigns a copy of the record) or
      asObject:= (assigns existing record instance). }
    function NewRecord: TRtcRecord;
    { Assign a new, fresh DataSet.
      To turn this Value object into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
     using asDataSet:= (assigns a copy of the dataset) or
      asObject:= (assigns existing dataset instance). }
    function NewDataSet: TRtcDataSet;
    { Assign a new, fresh FunctionInfo.
      To turn this Value object into a function call,
      you have to call this "newFunction" method, or: @html(<br><br>)
      assign already created TRtcFunctionInfo using asFunction:= (assigns a copy of the Function object) or @html(<br><br>)
      asObject:= (assigns existing TRtcFunctionInfo instance). }
    function NewFunction(const func_name:String=''): TRtcFunctionInfo; overload;

    { Assign a new, fresh Byte Stream.
      To turn this Value object into a Byte Stream,
      you have to call this "newByteStream" method, or: @html(<br><br>)
      assign already created TStream using asByteStream:= (assigns a copy of the Stream data). }
    function NewByteStream: TStream;

    { Assign a new, fresh Boolean value.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean: boolean;
    { Assign a new, fresh Currency value.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency: Currency;
    { Assign a new, fresh DateTime value.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime: TDateTime;
    { Assign a new, fresh Exception value.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException: String;
    { Assign a new, fresh VariableName value.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName value. }
    function NewVariable: String;
    { Assign a new, fresh Integer value.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger: rtcInteger;
    { Assign a new, fresh Large Integer value.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt: rtcLargeInt;
    { Assign a new, fresh Floating-point value.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat: rtcFloat;
    { Assign a new, fresh String value.
      Only sets the type to String.
      You DO NOT have to use this function before accessing as String value. }
    function NewString: String;
    { Assign a new, fresh WideString value.
      Only sets the type to WideString.
      You DO NOT have to use this function before accessing as WideString value. }
    function NewWideString: WideString;
    { Assign a new, fresh Text value.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText: String;

    { read: Is the Value NULL (not assigned) ? /
      write: TRUE = destroy any object stored here and set value to null. }
    property isNull:boolean read GetNull write SetNull;
    { Check Value type. NULL values always return rtc_NULL }
    property isType:TRtcValueTypes read GetType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,String. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      ByteStream, Record, Array, DataSet or FunctionInfo. }
    property Value:Variant read GetVariant write SetVariant;
    { alias for @Link(TRtcAbsValue.Value) }
    property asValue:Variant read GetVariant write SetVariant;

    { Read/Write as Boolean value }
    property asBoolean:boolean read GetBoolean write SetBoolean;
    { Read/Write as Integer value }
    property asInteger:rtcInteger read GetInteger write SetInteger;
    { Read/Write as Large Integer value }
    property asLargeInt:rtcLargeInt read GetLargeInt write SetLargeInt;
    { Read/Write as Float value }
    property asFloat:rtcFloat read GetFloat write SetFloat;
    { Read/Write as Currency value }
    property asCurrency:Currency read GetCurrency write SetCurrency;
    { Read/Write as DataTime value }
    property asDateTime:TDateTime read GetDateTime write SetDateTime;
    { Read/Write as Exception value (String) }
    property asException:String read GetException write SetException;
    { Read/Write as VariableName (String) }
    property asVarName:String read GetVarName write SetVarName;
    { Read/Write as String value (access as raw String) }
    property asString:String read GetString write SetString;
    { Read/Write as WideString value (access as raw WideString) }
    property asWideString:WideString read GetWideString write SetWideString;
    { Read/Write as Text value (uses UTF-8 encode to write and UTF-8 decode to read String) }
    property asText:String read GetText write SetText;
    { Read: return this object coded as a String, from which the object can be reconstructed anytime. /
      Write: reconstruct object from assigned String. }
    property asCode:String read GetCode write SetCode;
    { Read: return this object coded as a XML-RPC String,
            from which the object can be reconstructed. /
      Write: reconstruct object from assigned XML-RPC String. }
    property asXMLrpc:String read GetXMLrpc write SetXMLrpc;

    { read: Access stored ByteStream / write: assign a copy of the source Stream }
    property asByteStream:TStream read GetByteStream write SetByteStream;

    { read: Access stored array / write: assign a copy of the source TRtcArray }
    property asArray:TRtcArray read GetArray write SetArray;
    { read: Access stored record / write: assign a copy of the source TRtcRecord }
    property asRecord:TRtcRecord read GetRecord write SetRecord;
    { read: Access stored DataSet / write: assign a copy of the source TRtcDataset }
    property asDataSet:TRtcDataSet read GetDataSet write SetDataSet;
    { read: Access stored FunctionInfo / write: assign a copy of the source TRtcFunctionInfo }
    property asFunction:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;

    { Read: Access stored information as TRtcValueObject / @html(<br>)
      Write: NIL = remove object without destroying it / @html(<br>)
      Write: object = Assign object to this structure. It will be maintained and destroyed
      with this structure, as long as it isn't removed by calling asObject:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject:TRtcValueObject read GetObject write SetAsObject;

    { There are two ways to work with TRtcValue objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Abstract Record Object)
    There are two ways to work with TRtcAbsRecord objects. @html(<br>)
    One is to let the stored object be auto-created on first read access
    to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsRecord = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    function GetObject(const index: String): TRtcValueObject; virtual; abstract;
    // @exclude
    procedure SetObject(const index: String; Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure SetAsObject(const index: String; Value:TRtcValueObject);

    // @exclude
    function GetValueType(const index: String): TRtcValueTypes;

    // @exclude
    function GetArray(const index: String): TRtcArray;
    // @exclude
    function GetRecord(const index: String): TRtcRecord;
    // @exclude
    function GetDataSet(const index: String): TRtcDataSet;
    // @exclude
    function GetFunctionInfo(const index: String): TRtcFunctionInfo;
    // @exclude
    function GetByteStream(const index: String): TStream;

    // @exclude
    function GetNull(const index: String): boolean;
    // @exclude
    function GetBoolean(const index: String): boolean;
    // @exclude
    function GetCurrency(const index: String): Currency;
    // @exclude
    function GetDateTime(const index: String): TDateTime;
    // @exclude
    function GetException(const index: String): String;
    // @exclude
    function GetVarName(const index: String): String;
    // @exclude
    function GetInteger(const index: String): rtcInteger;
    // @exclude
    function GetLargeInt(const index: String): rtcLargeInt;
    // @exclude
    function GetFloat(const index: String): rtcFloat;
    // @exclude
    function GetString(const index: String): String;
    // @exclude
    function GetWideString(const index: String): WideString;
    // @exclude
    function GetText(const index: String): String;

    // @exclude
    function GetVariant(const index: String): Variant;
    // @exclude
    procedure SetVariant(const index: String; const Value: Variant);

    // @exclude
    procedure SetArray(const index: String; const Value: TRtcArray);
    // @exclude
    procedure SetRecord(const index: String; const Value: TRtcRecord);
    // @exclude
    procedure SetDataSet(const index: String; const Value: TRtcDataSet);
    // @exclude
    procedure SetFunctionInfo(const index: String; const Value: TRtcFunctionInfo);
    // @exclude
    procedure SetByteStream(const index: String; const Value: TStream);

    // @exclude
    procedure SetNull(const index: String; const Value: boolean);
    // @exclude
    procedure SetBoolean(const index: String; const Value: boolean);
    // @exclude
    procedure SetCurrency(const index: String; const Value: Currency);
    // @exclude
    procedure SetDateTime(const index: String; const Value: TDateTime);
    // @exclude
    procedure SetException(const index: String; const Value: String);
    // @exclude
    procedure SetVarName(const index: String; const Value: String);
    // @exclude
    procedure SetInteger(const index: String; const Value: rtcInteger);
    // @exclude
    procedure SetLargeInt(const index: String; const Value: rtcLargeInt);
    // @exclude
    procedure SetFloat(const index: String; const Value: rtcFloat);
    // @exclude
    procedure SetString(const index: String; const Value: String);
    // @exclude
    procedure SetWideString(const index: String; const Value: WideString);
    // @exclude
    procedure SetText(const index: String; const Value: String);

    // @exclude
    function GetCode(const index: String): String;
    // @exclude
    procedure SetCode(const index: String; const Value: String);

    // @exclude
    function GetXMLrpc(const index: String): String;
    // @exclude
    procedure SetXMLrpc(const index: String; const Value: String);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array to the 'index' field.
      To turn the 'index' field into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray[]:= (assigns a copy of the array) or
      asObject[]:= (assigns existing array instance). }
    function NewArray(const index: String): TRtcArray;
    { Assign a new, fresh Record to the 'index' field.
      To turn the 'index' field into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord[]:= (assigns a copy of the record) or
      asObject[]:= (assigns existing record instance). }
    function NewRecord(const index: String): TRtcRecord;
    { Assign a new, fresh DataSet to the 'index' field.
      To turn the 'index' field into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
      using asDataSet[]:= (assigns a copy of the dataset) or
      asObject[]:= (assigns existing dataset instance). }
    function NewDataSet(const index: String): TRtcDataSet;
    { Assign a new, fresh FunctionInfo to the 'index' field.
      To turn the 'index' field into a function call,
      you have to call "newFunction" or assign already created TRtcFunctionInfo
      using asFunction[]:= (assigns a copy of the FunctionInfo object) or
      asObject[]:= (assigns existing FunctionInfo instance). }
    function NewFunction(const index: String; const func_name:String=''): TRtcFunctionInfo;

    { Assign a new, fresh Byte Stream to the 'index' field.
      To turn the 'index' field into a Byte Stream,
      you have to call "newByteStream" or assign already created TStream
      using asByteStream[]:= (assigns a copy of the Stream data). }
    function NewByteStream(const index: String): TStream;

    { Assign a new, fresh Boolean value to the 'index' field.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean(const index: String): boolean;
    { Assign a new, fresh Currency value to the 'index' field.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency(const index: String): Currency;
    { Assign a new, fresh DateTime value to the 'index' field.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime(const index: String): TDateTime;
    { Assign a new, fresh Exception value to the 'index' field.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException(const index: String): String;
    { Assign a new, fresh VariableName value to the 'index' field.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName. }
    function NewVariable(const index: String): String;
    { Assign a new, fresh Integer value to the 'index' field.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger(const index: String): rtcInteger;
    { Assign a new, fresh Large Integer value to the 'index' field.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt(const index: String): rtcLargeInt;
    { Assign a new, fresh Floating-point value to the 'index' field.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat(const index: String): rtcFloat;
    { Assign a new, fresh String value to the 'index' field.
      Only sets the type to String.
      You DO NOT have to use this function before accessing as String value. }
    function NewString(const index: String): String;
    { Assign a new, fresh WideString value to the 'index' field.
      Only sets the type to WideString.
      You DO NOT have to use this function before accessing as WideString value. }
    function NewWideString(const index: String): WideString;
    { Assign a new, fresh Text value to the 'index' field.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText(const index: String): String;

    { read: Is the 'index' field value NULL (not assigned) ? /
      write: TRUE = set 'index' field value to null (this will destroy any object stored there) }
    property isNull[const index:String]:boolean read GetNull write SetNull;
    { Check 'index' field Value type. NULL values always return rtc_NULL }
    property isType[const index:String]:TRtcValueTypes read GetValueType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,String. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      Record, Array, DataSet or FunctionInfo. }
    property Value[const index:String]:Variant read GetVariant write SetVariant; default;
    { alias for @Link(TRtcAbsValue.Value) }
    property asValue[const index:String]:Variant read GetVariant write SetVariant;

    { Read/Write 'index' field as Boolean value }
    property asBoolean[const index:String]:boolean read GetBoolean write SetBoolean;
    { Read/Write 'index' field as Integer value }
    property asInteger[const index:String]:rtcInteger read GetInteger write SetInteger;
    { Read/Write 'index' field as Large Integer value }
    property asLargeInt[const index:String]:rtcLargeInt read GetLargeInt write SetLargeInt;
    { Read/Write 'index' field as Float value }
    property asFloat[const index:String]:rtcFloat read GetFloat write SetFloat;
    { Read/Write 'index' field as Currency value }
    property asCurrency[const index:String]:Currency read GetCurrency write SetCurrency;
    { Read/Write 'index' field as DataTime value }
    property asDateTime[const index:String]:TDateTime read GetDateTime write SetDateTime;
    { Read/Write 'index' field as Exception value (String) }
    property asException[const index:String]:String read GetException write SetException;
    { Read/Write 'index' field as VariableName (String) }
    property asVarName[const index:String]:String read GetVarName write SetVarName;

    { Read/Write 'index' field as String (raw String access) value }
    property asString[const index:String]:String read GetString write SetString;
    { Read/Write 'index' field as WideString (raw WideString access) value }
    property asWideString[const index:String]:WideString read GetWideString write SetWideString;
    { Read/Write 'index' field as Text value (UTF-8 encode on write, decode on read) }
    property asText[const index:String]:String read GetText write SetText;
    { Read: return 'index' field coded as a String,
      from which the object can be reconstructed anytime. /
      Write: reconstruct object from String and assign to 'index' field. }
    property asCode[const index:String]:String read GetCode write SetCode;
    { Read: return 'index' field coded as a XML-RPC String,
            from which the object can be reconstructed. /
      Write: reconstruct object from String and assign to 'index' field. }
    property asXMLrpc[const index:String]:String read GetXMLrpc write SetXMLrpc;

    { read: Access Stream stored in the 'index' field /
      write: assign a copy of the source Stream to the 'index' field }
    property asByteStream[const index:String]:TStream read GetByteStream write SetByteStream;

    { read: Access array stored in the 'index' field /
      write: assign a copy of the source TRtcArray to the 'index' field }
    property asArray[const index:String]:TRtcArray read GetArray write SetArray;
    { read: Access record stored in the 'index' field /
      write: assign a copy of the source TRtcRecord to the 'index' field }
    property asRecord[const index:String]:TRtcRecord read GetRecord write SetRecord;
    { read: Access DataSet stored in the 'index' field /
      write: assign a copy of the source TRtcDataset to the 'index' field }
    property asDataSet[const index:String]:TRtcDataSet read GetDataSet write SetDataSet;
    { read: Access FunctionInfo stored in the 'index' field /
      write: assign a copy of the source TRtcFunctionInfo to the 'index' field }
    property asFunction[const index:String]:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;

    { Read: Access information in the 'index' field as TRtcValueObject / @html(<br>)
      Write: NIL = remove object from the 'index' field without destroying it / @html(<br>)
      Write: object = Assign object to the 'index' field in this structure.
      It will be maintained and destroyed with this structure,
      as long as it isn't removed by calling asObject[index]:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject[const index:String]:TRtcValueObject read GetObject write SetAsObject;

    { There are two ways to work with TRtcAbsRecord objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Abstract Array Object)
    There are two ways to work with TRtcAbsArray objects.
    One is to let the stored object be auto-created on first read access
    to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
    the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
    to create objects before first usage (AutoCreate=FALSE). You control the behavior
    by setting the AutoCreate property to TRUE or FALSE. Any child object created
    using the New... function will inherit this property state. You can change this
    property at any time, on any object. Default AutoCreate state for objects
    created by using the Create and FromCode methods is always FALSE. }
  TRtcAbsArray = class(TRtcValueObject)
  protected
    // @exclude
    FAutoCreate:boolean;

    // @exclude
    function GetObject(index: integer): TRtcValueObject; virtual; abstract;
    // @exclude
    procedure SetObject(index: integer; Value:TRtcValueObject; asCopy:boolean=False); virtual; abstract;
    // @exclude
    procedure SetAsObject(index: integer; Value:TRtcValueObject);

    // @exclude
    function GetValueType(index: integer): TRtcValueTypes;

    // @exclude
    function GetArray(index: integer): TRtcArray;
    // @exclude
    function GetRecord(index: integer): TRtcRecord;
    // @exclude
    function GetDataSet(index: integer): TRtcDataSet;
    // @exclude
    function GetFunctionInfo(index: integer): TRtcFunctionInfo;

    // @exclude
    function GetByteStream(index: integer): TStream;

    // @exclude
    function GetNull(index: integer): boolean;
    // @exclude
    function GetBoolean(index: integer): boolean;
    // @exclude
    function GetCurrency(index: integer): Currency;
    // @exclude
    function GetDateTime(index: integer): TDateTime;
    // @exclude
    function GetException(index: integer): String;
    // @exclude
    function GetVarName(index: integer): String;
    // @exclude
    function GetInteger(index: integer): rtcInteger;
    // @exclude
    function GetLargeInt(index: integer): rtcLargeInt;
    // @exclude
    function GetFloat(index: integer): rtcFloat;
    // @exclude
    function GetString(index: integer): String;
    // @exclude
    function GetWideString(index: integer): WideString;
    // @exclude
    function GetText(index: integer): String;

    // @exclude
    function GetVariant(index: integer): Variant;
    // @exclude
    procedure SetVariant(index: integer; const Value: Variant);

    // @exclude
    procedure SetArray(index: integer; const Value: TRtcArray);
    // @exclude
    procedure SetRecord(index: integer; const Value: TRtcRecord);
    // @exclude
    procedure SetDataSet(index: integer; const Value: TRtcDataSet);
    // @exclude
    procedure SetFunctionInfo(index: integer; const Value: TRtcFunctionInfo);

    // @exclude
    procedure SetByteStream(index: integer; const Value: TStream);

    // @exclude
    procedure SetNull(index: integer; const Value: boolean);
    // @exclude
    procedure SetBoolean(index: integer; const Value: boolean);
    // @exclude
    procedure SetCurrency(index: integer; const Value: Currency);
    // @exclude
    procedure SetDateTime(index: integer; const Value: TDateTime);
    // @exclude
    procedure SetException(index: integer; const Value: String);
    // @exclude
    procedure SetVarName(index: integer; const Value: String);
    // @exclude
    procedure SetInteger(index: integer; const Value: rtcInteger);
    // @exclude
    procedure SetLargeInt(index: integer; const Value: rtcLargeInt);
    // @exclude
    procedure SetFloat(index: integer; const Value: rtcFloat);
    // @exclude
    procedure SetString(index: integer; const Value: String);
    // @exclude
    procedure SetWideString(index: integer; const Value: WideString);
    // @exclude
    procedure SetText(index: integer; const Value: String);

    // @exclude
    function GetCode(index: integer): String;
    // @exclude
    procedure SetCode(index: integer; const Value: String);

    // @exclude
    function GetXMLrpc(index: integer): String;
    // @exclude
    procedure SetXMLrpc(index: integer; const Value: String);

  public
    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; virtual; abstract;

    { Assign a new, fresh Array to the 'index' field.
      To turn the 'index' field into an array of values,
      you have to call "newArray" or assign already created TRtcArray
      using asArray[]:= (assigns a copy of the array) or
      asObject[]:= (assigns existing array instance). }
    function NewArray(index: integer): TRtcArray;
    { Assign a new, fresh Record to the 'index' field.
      To turn the 'index' field into a record of values,
      you have to call "newRecord" or assign already created TRtcRecord
      using asRecord[]:= (assigns a copy of the record) or
      asObject[]:= (assigns existing record instance). }
    function NewRecord(index: integer): TRtcRecord;
    { Assign a new, fresh DataSet to the 'index' field.
      To turn the 'index' field into a dataset of values,
      you have to call "newDataSet" or assign already created TRtcDataSet
      using asDataSet[]:= (assigns a copy of the dataset) or
      asObject[]:= (assigns existing dataset instance). }
    function NewDataSet(index: integer): TRtcDataSet;
    { Assign a new, fresh FunctionInfo to the 'index' field.
      To turn the 'index' field into a function call,
      you have to call "newFunction" or assign already created TRtcFunctionInfo
      using asFunction[]:= (assigns a copy of the FunctionInfo object) or
      asObject[]:= (assigns existing FunctionInfo instance). }
    function NewFunction(index: integer; const func_name:String=''): TRtcFunctionInfo;

    { Assign a new, fresh Byte Stream to the 'index' field.
      To turn the 'index' field into a Byte Stream,
      you have to call "newByteStream" or assign already created TStream
      using asByteStream[]:= (assigns a copy of the Stream data). }
    function NewByteStream(index: integer): TStream;

    { Assign a new, fresh Boolean value to the 'index' field.
      Only sets the type to Boolean.
      You DO NOT have to use this function before accessing as Boolean value. }
    function NewBoolean(index: integer): boolean;
    { Assign a new, fresh Currency value to the 'index' field.
      Only sets the type to Currency.
      You DO NOT have to use this function before accessing as Currency value. }
    function NewCurrency(index: integer): Currency;
    { Assign a new, fresh DateTime value to the 'index' field.
      Only sets the type to DateTime.
      You DO NOT have to use this function before accessing as DateTime value. }
    function NewDateTime(index: integer): TDateTime;
    { Assign a new, fresh Exception value to the 'index' field.
      Only sets the type to Exception.
      You DO NOT have to use this function before accessing as Exception value. }
    function NewException(index: integer): String;
    { Assign a new, fresh VariableName value to the 'index' field.
      Only sets the type to VariableName.
      You DO NOT have to use this function before accessing as VariableName. }
    function NewVariable(index: integer): String;
    { Assign a new, fresh Integer value to the 'index' field.
      Only sets the type to Integer.
      You DO NOT have to use this function before accessing as Integer value. }
    function NewInteger(index: integer): rtcInteger;
    { Assign a new, fresh Large Integer value to the 'index' field.
      Only sets the type to Large Integer.
      You DO NOT have to use this function before accessing as Large Integer value. }
    function NewLargeInt(index: integer): rtcLargeInt;
    { Assign a new, fresh Floating-point value to the 'index' field.
      Only sets the type to Floating-point.
      You DO NOT have to use this function before accessing as Floating-point value. }
    function NewFloat(index: integer): rtcFloat;
    { Assign a new, fresh String value to the 'index' field.
      Only sets the type to String.
      You DO NOT have to use this function before accessing as String value. }
    function NewString(index: integer): String;
    { Assign a new, fresh WideString value to the 'index' field.
      Only sets the type to WideString.
      You DO NOT have to use this function before accessing as WideString value. }
    function NewWideString(index: integer): WideString;
    { Assign a new, fresh Text value to the 'index' field.
      Only sets the type to Text.
      You DO NOT have to use this function before accessing as Text value. }
    function NewText(index: integer): String;

    { read: Is the 'index' field value NULL (not assigned) ? /
      write: TRUE = set 'index' field value to null (this will destroy any object stored there) }
    property isNull[index:integer]:boolean read GetNull write SetNull;
    { Check 'index' field Value type. NULL values always return rtc_NULL }
    property isType[index:integer]:TRtcValueTypes read GetValueType;

    { Read/Write as native value (Variant). @html(<br>)
      You can use this property to get/set all native types:
      Boolean,Integer,Float,Currency,DateTime,String. @html(<br>)
      You CAN NOT use this property to get or set complex structures like
      Record, Array, DataSet or FunctionInfo. }
    property Value[index:integer]:Variant read GetVariant write SetVariant; default;
    { alias for @Link(TRtcAbsValue.Value) }
    property asValue[index:integer]:Variant read GetVariant write SetVariant;

    { Read/Write 'index' field as Boolean value }
    property asBoolean[index:integer]:boolean read GetBoolean write SetBoolean;
    { Read/Write 'index' field as Integer value }
    property asInteger[index:integer]:rtcInteger read GetInteger write SetInteger;
    { Read/Write 'index' field as Large Integer value }
    property asLargeInt[index:integer]:rtcLargeInt read GetLargeInt write SetLargeInt;
    { Read/Write 'index' field as Float value }
    property asFloat[index:integer]:rtcFloat read GetFloat write SetFloat;
    { Read/Write 'index' field as Currency value }
    property asCurrency[index:integer]:Currency read GetCurrency write SetCurrency;
    { Read/Write 'index' field as DataTime value }
    property asDateTime[index:integer]:TDateTime read GetDateTime write SetDateTime;
    { Read/Write 'index' field as Exception value (integer) }
    property asException[index:integer]:String read GetException write SetException;
    { Read/Write 'index' field as VariableName (integer) }
    property asVarName[index:integer]:String read GetVarName write SetVarName;

    { Read/Write 'index' field as String value (raw access) }
    property asString[index:integer]:String read GetString write SetString;
    { Read/Write 'index' field as WideString value (raw access) }
    property asWideString[index:integer]:WideString read GetWideString write SetWideString;
    { Read/Write 'index' field as Text value (UTF-8 encode on write, decode on read) }
    property asText[index:integer]:String read GetText write SetText;
    { Read: return 'index' field coded as a String,
      from which the object can be reconstructed anytime. /
      Write: reconstruct object from String and assign to 'index' field. }
    property asCode[index:integer]:String read GetCode write SetCode;
    { Read: return 'index' field coded as an XML-RPC String,
      from which the object can be reconstructed. /
      Write: reconstruct object from XML-RPC String and assign to 'index' field. }
    property asXMLrpc[index:integer]:String read GetXMLrpc write SetXMLrpc;

    { read: Access Stream stored in the 'index' field /
      write: assign a copy of the source Stream to the 'index' field }
    property asByteStream[index:integer]:TStream read GetByteStream write SetByteStream;

    { read: Access array stored in the 'index' field /
      write: assign a copy of the source TRtcArray to the 'index' field }
    property asArray[index:integer]:TRtcArray read GetArray write SetArray;
    { read: Access record stored in the 'index' field /
      write: assign a copy of the source TRtcRecord to the 'index' field }
    property asRecord[index:integer]:TRtcRecord read GetRecord write SetRecord;
    { read: Access DataSet stored in the 'index' field /
      write: assign a copy of the source TRtcDataset to the 'index' field }
    property asDataSet[index:integer]:TRtcDataSet read GetDataSet write SetDataSet;
    { read: Access FunctionInfo stored in the 'index' field /
      write: assign a copy of the source TRtcFunctionInfo to the 'index' field }
    property asFunction[index:integer]:TRtcFunctionInfo read GetFunctionInfo write SetFunctionInfo;

    { Read: Access information in the 'index' field as TRtcValueObject / @html(<br>)
      Write: NIL = remove object from the 'index' field without destroying it / @html(<br>)
      Write: object = Assign object to the 'index' field in this structure.
      It will be maintained and destroyed with this structure,
      as long as it isn't removed by calling asObject[index]:=nil. @html(<br><br>)

      WARNING: NEVER! use asObject to assign an object which you did not create yourself,
      or to assign an object to more than one TRtcValueObject structure.
      If the same object instance is assigned to more than one structure,
      you WILL end up with Access Violation Errors when those objects are
      released by the connection component. To assign a copy of an object,
      allways use the object.CopyOf function or assign the object using
      specific 'as...' property (which doesn't assign the object itself,
      but rather creates a copy of the object). }
    property asObject[index:integer]:TRtcValueObject read GetObject write SetAsObject;

    { There are two ways to work with TRtcAbsArray objects.
      One is to let the stored object be auto-created on first read access
      to the asArray/asRecord/asDataSet/asFunction properties (AutoCreate=TRUE),
      the other is to use NewArray/NewRecord/NewFunction/NewDataSet functions
      to create objects before first usage (AutoCreate=FALSE). You control the behavior
      by setting the AutoCreate property to TRUE or FALSE. Any child object created
      using the New... function will inherit this property state. You can change this
      property at any time, on any object. Default AutoCreate state for objects
      created by using the Create and FromCode methods is always FALSE. }
    property AutoCreate:boolean read FAutoCreate write FAutoCreate;
    end;

  { @abstract(Object to store and transport a single value of any kind)
    This value can be anything from a static value, to an array,
    a named-list or a function call. }
  TRtcValue = class(TRtcAbsValue)
  protected
    // @exclude
    FValue:TRtcValueObject;

    procedure Extracted; override;

    // @exclude
    function GetType:TRtcValueTypes; override;

    // @exclude
    procedure SetObject(const _Value:TRtcValueObject; asCopy:boolean=False); override;
    // @exclude
    function GetObject:TRtcValueObject; override;

    // @exclude
    procedure CopyFrom(_Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    // @exclude
    constructor Create;

    // @exclude
    destructor Destroy; override;

    { Clear this object (initialize: Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract;

    { Create TRtcValue object from 'data' String, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      Since TRtcValue encapsulates one entity of any kind, you can use
      TRtcValue to reconstruct objects where you do not know which object type
      is stored after the 'at' chacacter inside your 'data' String. }
    class function FromCode(const data:String; var at:integer):TRtcValue; overload;

    { Create TRtcValue object from 'data' String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the String.
      @html(<br><br>)
      Since TRtcValue encapsulates one entity of any kind, you can use
      TRtcValue to reconstruct objects where you do not know which object type
      is stored after the 'at' chacacter inside your 'data' String. }
    class function FromCode(const data:String):TRtcValue; overload;

    { Create TRtcValue object from 'data' XML-RPC String, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:String; var at:integer):TRtcValue; overload;

    { Create TRtcValue object from 'data' XML-RPC String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the XML-RPC String. }
    class function FromXMLrpc(const data:String):TRtcValue; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  { @abstract(Object to store and transport data in record) }
  TRtcRecord = class(TRtcAbsRecord)
  protected
    // @exclude
    FValues:tRtcFastStrObjList;

    // @exclude
    class function NullValue:TRtcRecord;
    // @exclude
    function GetType:TRtcValueTypes; override;

    // @exclude
    function GetObject(const index: String): TRtcValueObject; override;
    // @exclude
    procedure SetObject(const index: String; _Value:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetFieldCount: integer;
    // @exclude
    function GetFieldName(index: integer): String;
    // @exclude
    procedure SetFieldName(index: integer; const _Value: String);

    // @exclude
    procedure CopyFrom(_Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index:String);

    { Create TRtcRecord object from 'data' String, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String after 'at'
      position was prepared using the TRtcRecord.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String; var at:integer):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the String.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data'
      was prepared using the TRtcRecord.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as XML-RPC String, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:String; var at:integer):TRtcRecord; overload;

    { Create TRtcRecord object from 'data' as XML-RPC String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the String. }
    class function FromXMLrpc(const data:String):TRtcRecord; overload;

    { Get all Record elements as one continuous string }
    function GetAsString: String;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;

    // Count Fields in Record (same as FieldCount)
    function Count:integer; virtual;

    // Count Fields in Record
    property FieldCount:integer read GetFieldCount default 0;
    // Get/Set Name of the field at position 'index'
    property FieldName[index:integer]:String read GetFieldName write SetFieldName;
    end;

  { @abstract(Object to store and transport data in an array) }
  TRtcArray = class(TRtcAbsArray)
  protected
    // @exclude
    FValues:TList;
    // @exclude
    class function NullValue:TRtcArray;
    // @exclude
    function GetType:TRtcValueTypes; override;

    // @exclude
    function GetObject(index: integer): TRtcValueObject; override;
    // @exclude
    procedure SetObject(index: integer; _Value:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetFieldCount: integer;

    // @exclude
    procedure CopyFrom(_Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index: integer);

    { Create TRtcArray object from 'data' String, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String after 'at'
      position was prepared using the TRtcArray.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String; var at:integer):TRtcArray; overload;

    { Create TRtcArray object from 'data' String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the String.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String
      was prepared using the TRtcArray.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String):TRtcArray; overload;

    { Create TRtcArray object from 'data' as XML-RPC String, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:String; var at:integer):TRtcArray; overload;

    { Create TRtcArray object from 'data' as XML-RPC String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the String. }
    class function FromXMLrpc(const data:String):TRtcArray; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;

    { Combine all array elements into one continuous string }
    function GetAsString: String;

    // Count fields in the Array (same as FieldCount)
    function Count:integer; virtual;

    // Count fields in the Array
    property FieldCount:integer read GetFieldCount default 0;
    end;

  { @abstract(Object to store and transport DataSets) }
  TRtcDataSet = class(TRtcAbsRecord)
  protected
    // @exclude
    FNames:tRtcFastStrObjList;

    // @exclude
    FTypes:array of TRtcFieldTypes;
    // @exclude
    FSizes:array of integer;
    // @exclude
    FRequired:array of boolean;
    // @exclude
    FData:TList;
    // @exclude
    FRow:integer;
    // @exclude
    class function NullValue:TRtcDataSet;
    // @exclude
    function GetType:TRtcValueTypes; override;

    // @exclude
    function GetObject(const index: String): TRtcValueObject; override;
    // @exclude
    procedure SetObject(const index: String; _Value:TRtcValueObject; asCopy:boolean=False); override;

    // @exclude
    function GetRowData: TRtcArray;
    // @exclude
    procedure SetRowData(const _Value: TRtcArray);
    // @exclude
    function GetFieldIndex(const index: string): integer;

    // @exclude
    function GetFieldCount: integer;
    // @exclude
    function GetFieldName(index: integer): String;
    // @exclude
    procedure SetFieldName(index: integer; const _Value: String);
    // @exclude
    function GetFieldSize(const index: string): Integer;
    // @exclude
    procedure SetFieldSize(const index: string; const _Value: Integer);
    // @exclude
    function GetFieldType(const index: string): TRtcFieldTypes;
    // @exclude
    procedure SetFieldType(const index: string; const _Value: TRtcFieldTypes);
    // @exclude
    function GetFieldRequired(const index: string): boolean;
    // @exclude
    procedure SetFieldRequired(const index: string; const _Value: boolean);

    // @exclude
    function GetRowCount: integer;
    // @exclude
    function GetRow: integer;
    // @exclude
    procedure SetRow(const _Value: integer);

    // @exclude
    procedure CopyFrom(_Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    // @exclude
    constructor Create; virtual;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { You have used one of the typed "as..." properties (asByteStream, asRecord,
      asArray, asDataSet, asFunction - all except "asObject") to get field data
      and want to continue using the data read after this object was destroyed?
      Then you need to use the Extract() procedure to remove the pointer
      to that data from this container object, so it won't be destroyed with the container.

      NOTE that Extract() should ONLY be used in combination with typed "as" properties.
      It should NOT be used in combination with the "asObject" property to get value objects,
      since that will give you direct access to data containers, but containers holding up simple
      data types will be destroyed by the Extract() method and you will get AVs.

       When using "asObject" to get a pointer to the data, if you want to extract the
       object from this parent object, you need to use "asObject:=nil" instead of Extract() }
    procedure Extract(const index:String);

    { Create TRtcDataSet object from 'data' String, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String after 'at'
      position was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String; var at:integer):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the String.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String
      was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as XML-RPC String, starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:String; var at:integer):TRtcDataSet; overload;

    { Create TRtcDataSet object from 'data' as XML-RPC String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the String. }
    class function FromXMLrpc(const data:String):TRtcDataSet; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;

    // Jump to first row
    procedure First;
    // Jump to last row
    procedure Last;
    // Jump to a prior row
    procedure Prior;
    { Jump to the next row. "Next" will move you *behind* the last Row
      if you are on the last, but it wil not move further than that.
      EOF will become TRUE if you were on the last row before calling Next. }
    procedure Next;
    // Insert a row here
    procedure Insert;
    // Append a row after last row
    procedure Append;
    // Delete current Row
    procedure Delete;
    // Behind the last row (Row>=RowCount)?
    function EOF:boolean;
    // Before the first row (Row<0)?
    function BOF:boolean;
    // No rows in dataset?
    function Empty:boolean;

    { Combine all DatSet elements as one continuous string }
    function GetAsString: String;

    // You can use this method to create a new field and set all its properties in one call
    procedure SetField(const FldName:string; FldType:TRtcFieldTypes; FldSize:integer=0; FldRequired:boolean=False);

    { This function was added only to make field access similar to TDataSet.FieldByName() possible.
      FieldByName will always return a TRtcValue object (never NIL), which you can use to read data
      inside this specific field in this specific row. If nothing was assigned to that row and field,
      a new NULL value will automatically be created and assigned on first access.

      When using FieldByName, a container object will be created for each field and row,
      which takes more memory and makes field access slower. Unless you absolutely
      NEED to use FieldByName to access field data, it would be much better to use
      asString[], asInteger[], asDateTime[] and other "as..." properties, which work
      faster and require less memory, since the TRtcValue container is not needed. }
    function FieldByName(const index:string):TRtcValue;

    // Count Rows in Dataset
    property RowCount:integer read GetRowCount;
    // Check current Row position / Set row position
    property Row:integer read GetRow write SetRow;
    { Get/Set data for the complete current row. @html(<br>)
      Once a Row is assigned, it can not be removed without being destroyed.
      By assigning NIL to RowData, existing row will be destroyed. }
    property RowData:TRtcArray read GetRowData write SetRowData;

    // Count Fields in the record (all records in a dataset have the same fields)
    property FieldCount:integer read GetFieldCount;
    // Get/Set name for the field at position 'index'
    property FieldName[index:integer]:String read GetFieldName write SetFieldName;

    // Get index (position) for the Field with name 'index'
    property FieldIndex[const index:string]:integer read GetFieldIndex;
    { Get/Set "FieldType" property for the field with name 'index'.
      Actual data type can varry from row to row.
      Value Type of data stored in the field in a specific row
      can be obtained using the isType[] property. }
    property FieldType[const index:string]:TRtcFieldTypes read GetFieldType write SetFieldType;
    // Get/Set "DataSize" property for the field with name 'index'
    property FieldSize[const index:string]:Integer read GetFieldSize write SetFieldSize;
    // Get/Set "required" property for the field with name 'index'
    property FieldRequired[const index:string]:boolean read GetFieldRequired write SetFieldRequired;

    end;

  { @abstract(Object to store and transport information for a remote function call) }
  TRtcFunctionInfo = class(TRtcRecord)
  protected
    // @exclude
    class function NullValue:TRtcFunctionInfo;
    // @exclude
    function GetType:TRtcValueTypes; override;

    // @exclude
    procedure CopyFrom(_Value:TRtcValueObject); override;

    { Fill object information from String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_Code, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_Code(const s:String; var at:integer); override;

    { Fill object information from XML-RPC String, starting after character
      at position 'at' and move 'at' accordingly.
      Before the first call to from_XMLrpc, 'at' has to be 0 (zero).
      When the whole String was processed, 'at' will equal length(s).
      @exclude }
    procedure from_XMLrpc(const s:String; var at:integer); override;

  public
    // Name of the Function
    FunctionName:String;

    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { Clear this object (Free all values and objects assigned) }
    procedure Clear; override;

    { Create TRtcFunctionInfo object from 'data' String, starting after character at position 'at'.
      Before the first call to FromCode, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromCode,
      'at' should equal to length(data).
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String after 'at'
      position was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String; var at:integer):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromCode with 'at' parameter
      if there are more objects stored in the String.
      @html(<br><br>)
      This operation can succeed ONLY if content in 'data' String
      was prepared using the TRtcDataSet.toCode function.
      FromCode will raise an exception otherwise. }
    class function FromCode(const data:String):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as XML-RPC String,
      starting after character at position 'at'.
      Before the first call to FromXMLrpc, 'at' has to be 0 (zero).
      On completion, 'at' will be positioned at the last character read from String
      while recreating the object. After the last call to FromXMLrpc,
      'at' should equal to length(data). }
    class function FromXMLrpc(const data:String; var at:integer):TRtcFunctionInfo; overload;

    { Create TRtcFunctionInfo object from 'data' as XML-RPC String.
      If there is more or less data than needed to create this object,
      an exception will be raised. Use FromXMLrpc with 'at' parameter
      if there are more objects stored in the String. }
    class function FromXMLrpc(const data:String):TRtcFunctionInfo; overload;

    { Returns exact copy of the object. @html(<br>)
      When assigning the object to structures other than those extending TRtcValueObject,
      if you want to keep one object instance for yourself,
      you can use 'copyOf' to assign a copy of this object instead of the original. }
    function copyOf:TRtcValueObject; override;

    { Create a String containing all object information. }
    procedure to_Code(const Result:TRtcHugeString); override;
    { Create a XML-RPC String containing all object information. }
    procedure to_XMLRPC(const Result:TRtcHugeString); override;
    end;

  { @abstract(Advanced Information Object, used for storing multiple objects and values for local usage)

    TRtcInfo extends TRtcRecord with Obj and Child properties,
    allowing you to store pointers to local TObject instances,
    which will NOT intervene with any other field or property
    values defined by TRtcRecord. @html(<br><br>)

    Objects stored using Obj and/or Child properties are kept in a separate list,
    so they can NOT be accessed by TRtcRecord and therefor will NOT be used by TRtcRecord. }
  TRtcInfo = class(TRtcRecord)
  private
    ObjList:tStrObjList;

    function Get_Object(const index:String):TObject;
    procedure Set_Object(const index:String; obj:TObject);
    function Get_ChildInfo(const index: String): TRtcInfo;
    procedure Set_ChildInfo(const index: String; const _Value: TRtcInfo);

  public
    { @exclude }
    constructor Create; override;

    { Number of properties assigned to this TRtcInfo,
      including objects assigned to Obj and/or Child properties. }
    function Count:integer; override;

    { Number of objects assigned to Obj and/or Child properties. }
    function ObjCount:integer;

    { Clear this info: this will also 'Kill' all TRtcObjects assigned to Obj. }
    procedure Clear; override;

    { Create a new Child-Info object and return its pointer }
    function NewChild(const index:String):TRtcInfo;

    { Get and set an Object (TRtcObject or TObject) for a given name (String, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      without calling the 'Kill' method for TRtcObject. }
    property Obj[const index:String]:TObject read Get_Object write Set_Object;

    { Get and set child-info Object for a given name (String, NOT case-sensitive).
      Seting it to NIL will remove the object from list,
      without calling the 'Kill' method for TRtcInfo.
      If AutoCreate is TRUE, read access to Child will auto-create a new instance of TRtcInfo. }
    property Child[const index:String]:TRtcInfo read Get_ChildInfo write Set_ChildInfo;

    // Kill Object or child-info object with name "index" and set this pointer to NIL
    procedure SetNil(const index:string);
    end;

  { @abstract(Basic Session Information object)
    TRtcSession extends TRtcInfo with a few Session propertis,
    which will NOT be used by TRtcRecord's toCode and FromCode functions.
    This means that you can pack all the session variables into
    a transportable object, without the information which is not
    part of your session data (ID, Created, PeerAddr, etc). }
  TRtcSession=class(TRtcInfo)
  protected
    // @exclude
    FLockType:TRtcSessionLockType;
    // @exclude
    FForwardedFor:String;
    // @exclude
    FID,FPeerAddr:string;
    // @exclude
    FCreated:TDateTime;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    // Session ID
    property ID:String read FID;
    // Session Create Time
    property Created:TDateTime read FCreated;
    // Session Peer Address
    property PeerAddr:String read FPeerAddr;
    // Session "X-FORWARDER-FOR" value
    property ForwardedFor:String read FForwardedFor;
    // Session Lock Type
    property LockType:TRtcSessionLockType read FLockType;
    end;

  { @abstract(HTTP Values: Reading and writing HTTP Values) }
  TRtcHttpValues = class
  private
    FValChange,
    FTxtChange:boolean;

    FValues:tRtcFastStrObjList;

    FDelimiter:String;
    FOrigQuery:String;
    FCacheSize:integer;
    FTempFileSize:int64;
    FTempFileName:string;

    procedure PrepareValues;
    procedure PrepareText;

    function GetItemCount: integer;
    function GetItemName(index: integer): String;
    function GetItemValue(index: integer): String;
    procedure SetItemName(index: integer; const Value: String);
    procedure SetItemValue(index: integer; const Value: String);

    function GetValue(const index: String): String;
    procedure SetValue(const index: String; const Value: String);

    function GetDelimiter: String;
    procedure SetDelimiter(const Value: String);

  protected
    { @exclude }
    function GetAsText:String; virtual;
    { @exclude }
    procedure SetText(const _Value: String); virtual;

  public
    { @exclude }
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    { Clear this parameters object (prepare for new parameters information) }
    procedure Clear; virtual;

    // Add more text (used with "Read" to append more data)
    procedure AddText(const s:string);

    { Return TRUE if a File was uploaded using <input type="file">.
      To retieve File content, use the GetFile method. }
    function IsFile(const index:string):boolean;

    { Get file uploaded using <input type="file"> and store its content in a "Stream".
      Return TRUE if successful, FALSE if file not found or variable name not recognized. }
    function GetFile(const index:string; stream:TStream):boolean; overload;

    { Get file uploaded using <input type="file"> and store its content in a local file "LocalFileName".
      Return TRUE if successful, FALSE if file not found or variable name not recognized. }
    function GetFile(const index:string; const LocalFileName:string):boolean; overload;

    { Maximum size of memory cache which may be used for storing data.
      If this cache size is filled, AddText() and Text will write all data to
      a temporary file and continue storing data in that temp file
      instead of storing all data in memory. This makes this object
      suitable for accepting file uploads. @html(<br><br>)
      -1 = "store everything in memory", @html(<br>)
      0 = use default value (RTC_FORMPOST_CACHESIZE) @html(<br>) }
    property CacheSize:integer read FCacheSize write FCacheSize;

    // Number of Items
    property ItemCount:integer read GetItemCount default 0;
    // Name of the 'index'-th item (attribute), starting from 0
    property ItemName[index:integer]:String read GetItemName write SetItemName;
    // Value of the 'index'-th item (attribute), starting from 0
    property ItemValue[index:integer]:String read GetItemValue write SetItemValue;

    // Value of the attribute with name 'index'
    property Value[const index:String]:String read GetValue write SetValue; default;
    // same as "Value[]"
    property asString[const index:String]:String read GetValue write SetValue;

    { Items delimiter, default = '&' @html(<br>)
      On the server-side, delimiter will in most cases be recognized automaticaly,
      so you just have to assign complete Content Body to the Text property
      and you can access all variables by name using Value[]. }
    property Delimiter:String read GetDelimiter write SetDelimiter;

    { Use this property if you need to set the exact Text as
      unmodified String or read exact Text as one String. }
    property Text:String read GetAsText write SetText;
    end;

  { @abstract(HTTP Header: Basic Request/Response wrapper) }
  TRtcHttpHeader = class
  private
    FValues:tRtcFastStrObjList;

    FCookie:TRtcHttpValues;

    function GetHeaderCount: integer;
    function GetIHeader(index: integer): String;
    procedure SetIHeader(index: integer; const Value: String);
    function GetHeader(const index: String): String;
    procedure SetHeader(const index: String; const Value: String);
    function GetIHeaderName(index: integer): String;
    procedure SetIHeaderName(index: integer; const Value: String);

    function GetContentLength: int64;
    function GetContentType: String;
    procedure SetContentLength(const _Value: int64);
    procedure SetContentType(const _Value: String);

    function GetCookie: TRtcHttpValues;

  protected
    { @exclude }
    function GetHeaderText: String; virtual;
    { @exclude }
    procedure SetHeaderText(const _Value: String); virtual;
    { @exclude }
    function GetCookieName:String; virtual; abstract;
    { @exclude }
    function isCookieName(const Value:String):boolean;

  public
    { @exclude }
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    { Clear this request object (prepare for new request information) }
    procedure Clear; virtual;

    { Cookie: same as TRtcRequest.Value['COKIE'] or TRtcResponse.Value['SET-COOKIE'].
      You can use this property to access Cookie values by name. }
    property Cookie:TRtcHttpValues read GetCookie;

    // Number of Items in the HTTP Response Header
    property ItemCount:integer read GetHeaderCount default 0;
    // Name of the 'index'-th item (attribute) for the HTTP Response Header (starting from 0)
    property ItemName[index:integer]:String read GetIHeaderName write SetIHeaderName;
    // Value of the 'index'-th item (attribute) for the HTTP Response Header (starting from 0)
    property ItemValue[index:integer]:String read GetIHeader write SetIHeader;

    // Value of the attribute with name 'index' from the HTTP Response Header.
    property Value[const index:String]:String read GetHeader write SetHeader; default;
    // same as "Value[]"
    property asString[const index:String]:String read GetHeader write SetHeader;

    { Content-Type value. If not set, will be left blank.
      @html(<br>)
      This property maps to Value['CONTENT-TYPE'] }
    property ContentType:String read GetContentType write SetContentType;
    { Content Length to be sent out after Header.
      If you are generating one small response, which doesn't need to
      be split in multiple packages, content length will be automatically
      set after your event handler finishes execution. Otherwise,
      you need to set this to the exact number of characters (bytes) you
      want to send out for this request, before your first call to Write().
      @html(<br>)
      This property maps to Value['CONTENT-LENGTH'] }
    property ContentLength:int64 read GetContentLength write SetContentLength;
    // Synonim for ContentLength
    property DataSize:int64 read GetContentLength write SetContentLength;

    { You can use this property to set multiple header values using HTTP formatting,
      or get a preformated HTTP header, with all values set for this response. }
    property HeaderText:String read GetHeaderText write SetHeaderText;
    end;

  { @abstract(Basic Http Request wrapper) }
  TRtcRequest=class(TRtcHttpHeader)
  private
    FInfo:TRtcInfo;
    FQuery:TRtcHttpValues;
    FParams:TRtcHttpValues;

    FMethod:String;
    FFileName:String;
    FFullName:String;
    FClose:boolean;
    procedure SetURI(const _Value: String);
    function GetMethod: String;
    procedure SetMethod(const _Value: String);
    function GetParams: TRtcHttpValues;
    function GetQuery: TRtcHttpValues;
    function GetInfo: TRtcInfo;
    procedure SetFileName(const _Value: String);
    function GetForwardedFor: String;
    procedure SetForwardedFor(const _Value: String);

  protected
    { @exclude }
    // function GetHeaderText: String; override;

    { @exclude }
    procedure SetHeaderText(const _Value: String); override;

    { @exclude }
    function GetCookieName:string; override;

    { @exclude }
    function GetURI: String;
    { @exclude }
    function GetURL: String;
    { @exclude }
    function GetRequestAgent: String;
    { @exclude }
    function GetRequestHost: String;
    { @exclude }
    function GetRequestReferer: String;
    { @exclude }
    procedure SetRequestAgent(const _Value: String);
    { @exclude }
    procedure SetRequestHost(const _Value: String);
    { @exclude }
    procedure SetRequestReferer(const _Value: String);

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;
    // @exclude
    procedure Clear; override;

    // Request method (GET, POST, ...)
    property Method:String read GetMethod write SetMethod;
    // Request File Name (as written in the URL, without the part after '?')
    property FileName:String read FFileName write SetFileName;
    { Request Query (the part after '?' in the URL).
      Use this to prepare the query on the client-side before sending
      and to read the query on the server-side (when received). }
    property Query:TRtcHttpValues read GetQuery;

    { Params gives you an easy-to-use way to prepare FORM parameters. @html(<br>)
      This property supports "application/x-www-form-urlencoded" and
      "multipart/form-data" decoding and encoding of Form data. @html(<br>)
      Delimiter (one of Params properties) determines what kind of formatting
      will be used when accessing Values and Text in Params. If Delmiter is
      undefined or contains a single character, simple "URLencoding" is used
      (same as the Query property), and if Delimiter contains 2 or more characters,
      "multipart/form-data" encoding is used. @html(<br><br>)

      This property is *NOT* used by the Client, nor the Server automatically.
      Server ONLY sets the Delimiter property, in case Content-Type is
      "multipart/form-data", so you don't have to extract the delimiter manualy
      (simply call "Params.AddText(Read)" on every "OnDataReceived" event). @html(<br><br>)

      This property has a built-in cache limit, which will be used to create a temporary file
      on disk and store data to that temporary file when cache size is exceeded, which makes
      it capable of accepting large file uploads from a Web Browser using the
      "multipart/form-data" format with input type "file". When a file was uploaded to
      your server and you used Params.AddText(Read) to store all data received,
      you can use the Params.Value[myFormVariable] property to access the name of the
      file uploaded from the Browser and GetFile(myFormVariable,MyLocalFileName) to write the
      file into a specific file on your local drive. @html(<br><br>)

      Client can also use this property to prepare the parameters and then use the "Text"
      property to call Write(Params.Text), so it doesn't have to manualy prepare form data.
      But, this property is *NOT* suited for the Client to upload files to a Web Server.
      Only server-side code for accepting uploaded files from a Web Browser is now supported. }
    property Params:TRtcHttpValues read GetParams;

    // Will the connection be closed after result has been sent out?
    property Close:boolean read FClose write FClose default false;

    // 'Host' value from the HTTP Header (mirrored for easier access)
    property Host:String read GetRequestHost write SetRequestHost;
    // 'User-agent' value from HTTP Header (mirrored for easier access)
    property Agent:String read GetRequestAgent write SetRequestAgent;
    // 'Referer' value from HTTP Header (mirrored for easier access)
    property Referer:String read GetRequestReferer write SetRequestReferer;
    // 'X-FORWARDED-FOR' vakze from HTTP Header (mirrored for easier access)
    property ForwardedFor:String read GetForwardedFor write SetForwardedFor;

    { URI = fully qualified URI.
      in stand-alone EXE, this is same as FileName + Query.Text,
      in a ISAPI DLL extension, this is a fully qualified URI, including DLL name and path }
    property URI:String read GetURI write SetURI;
    // URL = Host + URI
    property URL:String read GetURL;

    { User-defined additional request info. }
    property Info:TRtcInfo read GetInfo;
    end;

  { @abstract(Basic Http Response wrapper) }
  TRtcResponse=class(TRtcHttpHeader)
  private
    FStatusCode:integer;
    FStatusText:String;

  protected
    { @exclude }
    // function GetHeaderText: String; override;
    { @exclude }
    procedure SetHeaderText(const _Value: String); override;

    { @exclude }
    function GetCookieName:string; override;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    // Status code to be sent back (default = 200)
    property StatusCode:integer read FStatusCode write FStatusCode;
    // Status text to be sent back (default = OK)
    property StatusText:String read FStatusText write FStatusText;
    end;

  { @abstract(RTC File Stream implementation) }
  TRtcFileStream = class(TRtcObject)
    private
      f:integer;
      l:int64;

    public
      destructor Destroy; override;
      procedure Kill; override;

      procedure Open(const fname:string);
      procedure Seek(loc:int64);
      function Read(size:int64):string;
      procedure Close;
    end;

  { TRtcCustomEvent is the event handler type for custom-defined events. }
  TRtcCustomEvent = procedure(Sender:TObject; Obj:TObject) of object;

  { TRtcCustomDataEvent is the event handler type for custom-defined data events. }
  TRtcCustomDataEvent = procedure(Sender:TObject; Obj:TObject; Data:TRtcValue) of object;

// Does file with name "fname" exist?
function File_Exists(const fname:string):boolean;
// Size of the file with name "fname"
function File_Size(const fname:string):int64;
// Read "Size" bytes of file "fname", starting at "Loc" (0=begining), using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:string; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):string; overload;
// Read complete file "fname", using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:string; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):string; overload;
{ Scan up to "Size" bytes of file "fname" for string "search_string",
  starting at "Loc" (0=beginning), using "AccessMode" (default = rtc_ShareDenyNone)
  and up to "BufferSize" memory for scanning the file.
  Larger buffer size will increase scanning speed, but use more memory.
  Recommended are "BufferSize" values of 16000 or more bytes. @html(<br><br>)
  If "search_string" is found, its location in file is returned (0=beginning).
  If "search_string" is not found, this function returns -1. }
function Scan_File(const fname, search_string:string; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64;
// Delete file "fname"
procedure Delete_File(const fname:string);
// Rename file "old_name" to "new_name"
procedure Rename_File(const old_name,new_name:string);
// Write "Data" to file "fname" starting at "Loc" (0=begining, -1 = end), using "AccessMode" (default = rtc_ShareExclusive)
function Write_File(const fname:string; const Data:string; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
// Write "Data" to file "fname", overwriting old file, using "AccessMode" (default = rtc_ShareExclusive)
function Write_File(const fname:string; const Data:string; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
// File date and time
function File_Age(const fname:string):TDateTime;
// Get Temporary Directory path
function GetTempDirectory: String;
// Get Temporary File Name inside a Temporary Directory
function GetTempFile:string;

// Encode string to be used as Http URL
function URL_Encode(const AStr:string):string;
// Decode Http URL string to be used in Delphi code
function URL_Decode(const AStr:string):string;

// encode a binary string to Mime (Base-64)
function Mime_Encode(const s: AnsiString): AnsiString;
// decode a Mime (Base-64) string to a binary string
function Mime_Decode(const s: AnsiString): AnsiString;

// Convert WideString to a normal UTF-8 encoded string (used internaly by RTC)
function Utf8Encode(const WS: WideString): String;
// Convert a normal UTF-8 encoded string to a WideString (used internaly by RTC)
function Utf8Decode(const S: String): WideString;

// Convert Ansi String to UTF-8 string (used internaly by RTC)
function AnsiToUtf8(const S: String): String;
// Convert UTF-8 String to Ansi string (used internaly by RTC)
function Utf8ToAnsi(const S: String): String;

// Convert a Currency value to a "RTC Currency String" (used internaly by RTC)
function Curr2Str(v:Currency):String;
// Convert a "RTC Currency String" to a Currency value (used internaly by RTC)
function Str2Curr(const s:String):Currency;

// Convert a Floating-point value to a "RTC Floating-point String" (used internaly by RTC)
function Str2Float(const s:String):rtcFloat;
// Convert a "RTC Floating-point String" to a Floating-point Value (used internaly by RTC)
function Float2Str(v:rtcFloat):String;

// Convert a DateTime value to a "RTC DateTime String" (used internaly by RTC)
function DateTime2Str(v:TDateTime):String;
// Convert a "RTC DateTime String" to a DateTime value (used internaly by RTC)
function Str2DateTime(s:String):TDateTime;

{ Check if Type is a simple value type (not a FunctionInfo, Record, DataSet or Array)
  @exclude }
function isSimpleValueType(typ:TRtcValueTypes):boolean;

{ Check if object contains a simple value type (not a FunctionInfo, Record, DataSet or Array)
  @exclude }
function isSimpleValue(obj:TRtcValueObject):boolean;

{ Check if 's' appears to be an XML string. }
function isXMLString(const s:string):boolean;

{ Combine two RTC types and return the best type to hold values from type1 and type2.
  Combining anything  with a NIL type will return the not-NIL  type.
  Combining anything with a String type will always return a string type. }
function rtcCombineTypes(const type1,type2:TRtcValueTypes):TRtcValueTypes;

// Return Type Name for rtc type
function rtcTypeName(const type1:TRtcValueTypes):string;

implementation

type
  TRtcTypeObject = class
    asType:TRtcValueTypes;
    end;

const
  MARK_NAME:char=':'; // char after NAME
  MARK_TYPE:char='='; // char after TYPE
  MARK_LEN_START:char='"'; // char to mark Long String start
  MARK_LEN_END:char='"'; // char to mark Long String end
  MARK_MID:char=','; // char after mid string
  MARK_END:char=';'; // char to mark END-OF-LINE

const
  // set of simple value types
  TRtcSimpleValueTypes:set of TRtcValueTypes = [
                               rtc_Null,
                               rtc_Variable,
                               rtc_Exception,
                               rtc_Text,rtc_String,rtc_WideString,
                               rtc_Boolean,
                               rtc_Integer,rtc_LargeInt,
                               rtc_Float,rtc_Currency,rtc_DateTime,
                               rtc_ByteStream ];

function rtcTypeName(const type1:TRtcValueTypes):string;
  begin
  Result:=RTC_TYPE2FULLNAME_CONV[type1];
  end;

{ Combine two RTC types and return the best type to hold values from type1 and type2 }
function rtcCombineTypes(const type1,type2:TRtcValueTypes):TRtcValueTypes;
  begin
  if type1=type2 then
    Result:=type1
  else if type1=rtc_Null then
    Result:=type2
  else if type2=rtc_Null then
    Result:=type1
  else if (type1=rtc_WideString) or (type2=rtc_WideString) then
    Result:=rtc_WideString
  else if (type1=rtc_Text) or (type2=rtc_Text) then
    Result:=rtc_Text
  else if (type1=rtc_String) or (type2=rtc_String) then
    Result:=rtc_String
  else if (type1 in [rtc_Integer..rtc_DateTime,rtc_Variant,rtc_Boolean]) and
          (type2 in [rtc_Integer..rtc_DateTime,rtc_Variant,rtc_Boolean]) then
    begin
    if type1>type2 then
      Result:=type1
    else
      Result:=type2;
    end
  else if (type1=rtc_DataSet) or (type2=rtc_DataSet) then
    Result:=rtc_DataSet
  else if (type1=rtc_Record) or (type2=rtc_Record) then
    Result:=rtc_Record
  else if (type1=rtc_Array) or (type2=rtc_Array) then
    Result:=rtc_Array
  else if (type1=rtc_Variant) or (type2=rtc_Variant) then
    Result:=rtc_Variant
  else
    Result:=type1;
  end;

function TrimCopy(const S: string; I, L:integer): string;
  begin
  L:=L+I-1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
    begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
    end;
  end;

{ Check if 's' appears to be an XML string. }
function isXMLString(const s:string):boolean;
  var
    a:integer;
  begin
  Result:=False;
  if s<>'' then
    begin
    for a:=1 to length(s) do
      if s[a]='<' then
        begin
        if a=length(s) then
          Break
        else if s[a+1] in ['!','?','a'..'z','A'..'Z','-','.','/'] then
          Result:=True;
        end
      else  if not (s[a] in [#9,#10,#13,#32]) then
        Break;
    end;
  end;

function isSimpleValueType(typ:TRtcValueTypes):boolean;
  begin
  Result:=typ in TRtcSimpleValueTypes;
  end;

function isSimpleValue(obj:TRtcValueObject):boolean;
  begin
  if not assigned(obj) then
    Result:=True
  else
    Result:=obj.GetType in TRtcSimpleValueTypes;
  end;

function GetTempDirectory: String;
  var
    tempFolder: array[0..MAX_PATH] of Char;
  begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
  end;

function GetTempFile:string;
{$IFDEF FPC}
  var
    tempFile,tempFolder: String;
  begin
  tempFolder:=GetTempDirectory;
  tempFile:=GetTempFileName(tempFolder, 'RTC');
  end;
{$ELSE}
  var
    tempFile: array[0..MAX_PATH] of Char;
    tempFolder: array[0..MAX_PATH] of Char;
  begin
  GetTempPath(MAX_PATH, @tempFolder);
  if GetTempFileName(@tempFolder, 'RTC', 0, @tempFile)<>0 then
    result := StrPas(tempFile)
  else
    result := '';
  end;
{$ENDIF}

function URL_Encode(const AStr: String): String;
  const
    ToHex:array[$0..$F] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  var
    sb: byte;
    a,b:integer;
  begin
  SetLength(Result, Length(AStr) * 3);
  b:=0;
  for a:=1 to length(AStr) do
    case AStr[a] of
      '0'..'9',
      'A'..'Z',
      'a'..'z',
      '*','@','.','_','-','/','(',')','$':
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      ' ':
        begin
        Inc(b);
        Result[b]:='+';
        end;
      else
        begin
        sb:=Ord(AStr[a]);
        Inc(b);
        Result[b]:='%';
        Result[b+1]:=ToHex[sb shr 4];
        Result[b+2]:=ToHex[sb and $F];
        Inc(b,2);
        end;
      end;
  SetLength(Result, b);
  end;

function URL_Decode(const AStr: String): String;
  const
    fromHex:array['0'..'9'] of byte = (0,1,2,3,4,5,6,7,8,9);
    fromHexA:array['A'..'F'] of byte = ($A,$B,$C,$D,$E,$F);
    fromHexB:array['a'..'f'] of byte = ($A,$B,$C,$D,$E,$F);
  var
    sb: byte;
    a,b:integer;
  begin
  SetLength(Result, Length(AStr));
  b:=0; a:=0;
  while a<length(AStr) do
    begin
    Inc(a);
    case AStr[a] of
      '%':
        begin
        Inc(a);
        case AStr[a] of
          '0'..'9': sb:=fromHex[AStr[a]] shl 4;
          'A'..'F': sb:=fromHexA[AStr[a]] shl 4;
          'a'..'f': sb:=fromHexB[AStr[a]] shl 4;
          else
            raise EConvertError.Create('Error decoding URL: '+AStr);
          end;
        Inc(a);
        case AStr[a] of
          '0'..'9': Inc(sb,fromHex[AStr[a]]);
          'A'..'F': Inc(sb,fromHexA[AStr[a]]);
          'a'..'f': Inc(sb,fromHexB[AStr[a]]);
          else
            raise EConvertError.Create('Error decoding URL: '+AStr);
          end;
        Inc(b);
        Result[b]:=Char(sb);
        end;
      '+':
        begin
        Inc(b);
        Result[b]:=' ';
        end;
      else
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      end;
    end;
  SetLength(Result, b);
  end;

{ TRtcFileStream }

destructor TRtcFileStream.Destroy;
  begin
  Close;
  inherited;
  end;

procedure TRtcFileStream.Kill;
  begin
  Free;
  end;

procedure TRtcFileStream.Open(const fname: string);
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  l:=0;
  if f<0 then
    raise Exception.Create('Unable to open file for read access.');
  end;

procedure TRtcFileStream.Close;
  begin
  if f>=0 then
    begin
    FileClose(f);
    f:=-1;
    end;
  end;

function TRtcFileStream.Read(Size: int64): string;
  var
    sRead:int64;
  begin
  if f<0 then
    raise Exception.Create('File not open.')
  else
    begin
    if Size<0 then
      Size:=FileSeek(f,int64(0),2)-L;
    SetLength(Result,Size);
    sRead:=FileRead(f,Result[1],Size);
    Inc(L,sRead);
    if sRead<Size then
      SetLength(Result,sRead);
    end;
  end;

procedure TRtcFileStream.Seek(Loc: int64);
  begin
  if f<0 then
    raise Exception.Create('File not open.')
  else
    begin
    if Loc<0 then Loc:=0;
    l:=FileSeek(f,Loc,0);
    if l<>LOC then raise Exception.Create('Error seeking through file.');
    end;
  end;

function File_Exists(const fname:string):boolean;
  var
    f:integer;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f<0 then
    Result:=False
  else
    begin
    FileClose(f);
    Result:=True;
    end;
  end;

function File_Size(const fname:string):int64;
  var
    f:integer;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f<0 then
    Result:=-1
  else
    begin
    Result:=FileSeek(f,int64(0),2);
    FileClose(f);
    end;
  end;

function File_Age(const fname:string):TDateTime;
  var
    f:integer;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f<0 then
    Result:=-1
  else
    begin
    Result:=FileDateToDateTime(FileGetDate(f));
    FileClose(f);
    end;
  end;

function Read_File(const fname:string; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):string; overload;
  var
    f:integer;
    sRead:int64;
  begin
  Result:='';
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if f<0 then
    Exit
  else
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      SetLength(Result,Size);
      sRead:=FileRead(f,Result[1],Size);
      if sRead<Size then
        SetLength(Result,sRead);
    finally
      FileClose(f);
      end;
    end;
  end;

function Read_File(const fname:string; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):string; overload;
  begin
  Result:=Read_File(fname,0,-1,AccessMode);
  end;

function Scan_File(const fname, search_string:string; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64;
  var
    f:integer;
    test:string;
    have_size,
    want_size,
    old_size, mypos, len:int64;
  begin
  Result:=-1;
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if (f>=0) and (Size>0) and (length(search_string)>0) then
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      old_size:=0;
      len:=length(search_string);
      BufferSize:=BufferSize+len;
      if BufferSize<Size then
        SetLength(test,BufferSize)
      else
        SetLength(test,Size);
      repeat
        // Do not read more than requested
        if Size>BufferSize-old_size then
          want_size:=BufferSize-old_size // Do not oveflow our memory buffer
        else
          want_size:=Size;

        // Read next block behind last one
        have_size := FileRead(f, test[old_size+1], want_size);
        if have_size<=0 then // no more data to read!
          Break
        else if length(test)>old_size+have_size then // less data read than memory reserved
          SetLength(test, old_size+have_size);

        mypos:=Pos(search_string, test);
        if mypos>0 then
          begin
          // Loc = last reading location
          // mypos = substring location
          // rd_size = bytes left-over from last read
          Result:=Loc+mypos-1-old_size;
          Break;
          end
        else if (have_size=want_size) and (Size>0) then // expecting more data
          begin
          // Copy last "len" bytes to the beginning of our test string
          Move(test[old_size+have_size-len+1],test[1],len);
          old_size:=len;

          Dec(Size,have_size);
          Inc(Loc,have_size);
          end
        else // this was last block read
          Break;
        until False;
    finally
      FileClose(f);
      end;
    end
  else if (f>=0) then
    FileClose(f);
  end;

procedure Delete_File(const fname:string);
  begin
  if File_Exists(fname) then
    DeleteFile(fname);
  end;

procedure Rename_File(const old_name,new_name:string);
  begin
  SysUtils.RenameFile(old_name,new_name);
  end;

function Write_File(const fname:string; const Data:string; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  var
    f:integer;
  begin
  Result:=False;
  case AccessMode of
    rtc_ShareDenyNone: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyWrite);
    else f:=FileOpen(fname,fmOpenReadWrite+fmShareExclusive);
    end;
  if f<0 then
    f:=FileCreate(fname);
  if f>=0 then
    begin
    // if Loc<0 then Loc:=0;
    try
      if length(Data)>0 then
        begin
        if Loc<0 then
          begin
          FileSeek(f,0,2);
          if FileWrite(f,data[1],length(data))=length(data) then
            Result:=True;
          end
        else
          begin
          if FileSeek(f,Loc,0)=Loc then
            if FileWrite(f,data[1],length(data))=length(data) then
              Result:=True;
          end;
        end;
    finally
      FileClose(f);
      end;
    end;
  end;

// Write "Data" to file "fname", overwriting old file.
function Write_File(const fname:string; const Data:string; AccessMode:TRtcFileAccessMode=rtc_ShareExclusive):boolean; overload;
  begin
  DeleteFile(fname);
  Result:=Write_File(fname, data, 0, AccessMode);
  end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function Utf8Encode(const WS: WideString): String;
var
  L: Integer;
  Temp: String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: String): String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: String): String;
begin
  Result := Utf8Decode(S);
end;

//=====================================================
//  Mime functions
//=====================================================

const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;

const
  MIME_ENCODE_TABLE: array[0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072,
    073, 074, 075, 076, 077, 078, 079, 080,
    081, 082, 083, 084, 085, 086, 087, 088,
    089, 090, 097, 098, 099, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110,
    111, 112, 113, 114, 115, 116, 117, 118,
    119, 120, 121, 122, 048, 049, 050, 051,
    052, 053, 054, 055, 056, 057, 043, 047);

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array[Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059,
    060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006,
    007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022,
    023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032,
    033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048,
    049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

type
  PByte4 = ^TByte4;
  TByte4 = packed record
    b1, b2, b3, b4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    b1, b2, b3: Byte;
  end;

  PCardinal = ^Cardinal;

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
  var
    b, InnerLimit, OuterLimit: Cardinal;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount < MIME_DECODED_LINE_BREAK then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := Cardinal(InPtr);
  Inc(OuterLimit, InputByteCount);

  repeat

    repeat
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 8;
      b := b or InPtr^.b3;
      Inc(InPtr);

      OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[b];
      Inc(OutPtr);
      until Cardinal(InPtr) >= InnerLimit;

    OutPtr^.b1 := 13;
    OutPtr^.b2 := 10;
    Inc(Cardinal(OutPtr), 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
    until InnerLimit > OuterLimit;
  end;

procedure MimeEncodeNoCRLF(const InputBuffer;
                           const InputByteCount: Cardinal;
                           out OutputBuffer);
  var
    b, InnerLimit, OuterLimit: Cardinal;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  while Cardinal(InPtr) < InnerLimit do
    begin
    b := InPtr^.b1;
    b := b shl 8;
    b := b or InPtr^.b2;
    b := b shl 8;
    b := b or InPtr^.b3;
    Inc(InPtr);

    OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b1 := MIME_ENCODE_TABLE[b];
    Inc(OutPtr);
    end;

  case InputByteCount - OuterLimit of
    1:begin
      b := InPtr^.b1;
      b := b shl 4;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b3 := MIME_PAD_CHAR;
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    2:begin
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 2;
      OutPtr.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    end;
  end;

function Mime_Encode(const s: AnsiString): AnsiString;
  var
    l: Cardinal;
    aSize, iDelta, ODelta: Cardinal;
  begin
  if s<>'' then
    begin
    l := length(s);
    if l > 0 then
      aSize := (l + 2) div 3 * 4 + (l - 1) div MIME_DECODED_LINE_BREAK * 2
    else
      aSize:=l;
    SetLength(Result, aSize);
    MimeEncodeFullLines(s[1], l, Result[1]);
    iDelta := l div MIME_DECODED_LINE_BREAK;
    ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
    iDelta := iDelta * MIME_DECODED_LINE_BREAK;
    MimeEncodeNoCRLF(s[1+iDelta],
                     l - iDelta,
                     Result[1+ODelta]);
    end
  else
    Result := '';
  end;

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
  var
    lByteBuffer, lByteBufferSpace, c: Cardinal;
    InPtr, OuterLimit: ^Byte;
    OutPtr: PByte3;
  begin
  if InputBytesCount > 0 then
    begin
    InPtr := @InputBuffer;
    Cardinal(OuterLimit) := Cardinal(InPtr) + InputBytesCount;
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
      begin
      c := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if c = $FF then Continue;
      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or c;
      Dec(lByteBufferSpace);

      if lByteBufferSpace <> 0 then Continue;

      OutPtr^.b3 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
      end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
    end
  else
    Result := 0;
  end;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
  var
    lByteBuffer: Cardinal;
  begin
  case ByteBufferSpace of
    1:begin
      lByteBuffer := ByteBuffer shr 2;
      PByte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
      Result := 2;
      end;
    2:begin
      lByteBuffer := ByteBuffer shr 4;
      PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
      Result := 1;
      end;
    else
      Result := 0;
    end;
  end;

function Mime_Decode(const s: AnsiString): AnsiString;
  var
    ByteBuffer, ByteBufferSpace: Cardinal;
    aSize,l: Cardinal;
  begin
  if Pointer(s) <> nil then
    begin
    l := length(s);
    aSize:= (l + 3) div 4 * 3;
    SetLength(Result, aSize);
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    l := MimeDecodePartial(s[1], l, Result[1], ByteBuffer, ByteBufferSpace);
    Inc(l, MimeDecodePartialEnd(Result[1+l], ByteBuffer, ByteBufferSpace));
    SetLength(Result, l);
    end
  else
    Result := '';
  end;

function TypeToStr(typ:TRtcValueTypes):String;
  begin
  Result:=RTC_TYPE2STR_CONV[typ];
  {case typ of
    rtc_Null:Result:='X';
    rtc_Function:Result:='FC';
    rtc_DataSet:Result:='DS';
    rtc_Array:Result:='AR';
    rtc_Record:Result:='RE';
    rtc_Exception:Result:='E';
    rtc_Variable:Result:='V';
    rtc_String:Result:='S';
    rtc_WideString:Result:='W';
    rtc_Text:Result:='T';
    rtc_Boolean:Result:='B';
    rtc_Integer:Result:='I';
    rtc_LargeInt:Result:='L';
    rtc_Float:Result:='F';
    rtc_Currency:Result:='C';
    rtc_DateTime:Result:='D';
    rtc_ByteStream:Result:='BS';
    end;}
  end;

function StrToType(typ:String):TRtcValueTypes;
  begin
  typ:=UpperCase(typ);
  if length(typ)=1 then
    begin
    case typ[1] of
      'X': Result:=rtc_Null;
      'E': Result:=rtc_Exception;
      'V': Result:=rtc_Variable;
      'S': Result:=rtc_String;
      'W': Result:=rtc_WideString;
      'T': Result:=rtc_Text;
      'B': Result:=rtc_Boolean;
      'I': Result:=rtc_Integer;
      'L': Result:=rtc_LargeInt;
      'F': Result:=rtc_Float;
      'C': Result:=rtc_Currency;
      'D': Result:=rtc_DateTime;
      else raise Exception.Create('Unknown object type specifier "'+typ+'".');
      end;
    end
  else if typ='FC' then Result:=rtc_Function
  else if typ='DS' then Result:=rtc_DataSet
  else if typ='AR' then Result:=rtc_Array
  else if typ='RE' then Result:=rtc_Record
  else if typ='BS' then Result:=rtc_ByteStream
  else raise Exception.Create('Unknown object type specifier "'+typ+'".');
  end;

function FieldTypeToStr(typ:TRtcFieldTypes):String;
  begin
  Result:=RTC_FIELD2STR_CONV[typ];
  {case typ of
    ft_Unknown: Result:='U';
    ft_String: Result:='S';
    ft_Smallint: Result:='SI';
    ft_Integer: Result:='I';
    ft_Word: Result:='WI';
    ft_Boolean: Result:='B';
    ft_Float: Result:='F';
    ft_Currency: Result:='C';
    ft_BCD: Result:='BC';
    ft_Date: Result:='DD';
    ft_Time: Result:='T';
    ft_DateTime: Result:='D';
    ft_Bytes: Result:='BY';
    ft_VarBytes: Result:='VB';
    ft_AutoInc: Result:='AI';
    ft_Blob: Result:='O';
    ft_Memo: Result:='M';
    ft_Graphic: Result:='G';
    ft_FmtMemo: Result:='FM';
    ft_ParadoxOle: Result:='PO';
    ft_DBaseOle: Result:='DO';
    ft_TypedBinary: Result:='TB';
    ft_Cursor: Result:='CU';
    ft_FixedChar: Result:='FC';
    ft_WideString: Result:='W';
    ft_Largeint: Result:='L';
    ft_ADT: Result:='AD';
    ft_Array: Result:='AR';
    ft_Reference: Result:='RF';
    ft_DataSet: Result:='DS';
    ft_OraBlob: Result:='OB';
    ft_OraClob: Result:='OC';
    ft_Variant: Result:='V';
    ft_Interface: Result:='IT';
    ft_IDispatch: Result:='ID';
    ft_Guid: Result:='GU';
    ft_TimeStamp: Result:='DT';
    ft_FMTBcd: Result:='FB';
    else raise Exception.Create('Missing conversion for FieldType.');
    end;}
  end;

function StrToFieldType(typ:String):TRtcFieldTypes;
  begin
  typ:=UpperCase(typ);
  if length(typ)=1 then
    begin
    case typ[1] of
      'U': Result:=ft_Unknown;
      'S': Result:=ft_String;
      'I': Result:=ft_Integer;
      'B': Result:=ft_Boolean;
      'F': Result:=ft_Float;
      'C': Result:=ft_Currency;
      'T': Result:=ft_Time;
      'D': Result:=ft_DateTime;
      'O': Result:=ft_Blob;
      'M': Result:=ft_Memo;
      'G': Result:=ft_Graphic;
      'W': Result:=ft_WideString;
      'L': Result:=ft_Largeint;
      'V': Result:=ft_Variant;
      else raise Exception.Create('Unknown Field Type specifier "'+typ+'".');
      end;
    end
  else if typ='SI' then Result:=ft_Smallint
  else if typ='WI' then Result:=ft_Word
  else if typ='BC' then Result:=ft_BCD
  else if typ='DD' then Result:=ft_Date
  else if typ='BY' then Result:=ft_Bytes
  else if typ='VB' then Result:=ft_VarBytes
  else if typ='AI' then Result:=ft_AutoInc
  else if typ='FM' then Result:=ft_FmtMemo
  else if typ='PO' then Result:=ft_ParadoxOle
  else if typ='DO' then Result:=ft_DBaseOle
  else if typ='TB' then Result:=ft_TypedBinary
  else if typ='CU' then Result:=ft_Cursor
  else if typ='FC' then Result:=ft_FixedChar
  else if typ='AD' then Result:=ft_ADT
  else if typ='AR' then Result:=ft_Array
  else if typ='RF' then Result:=ft_Reference
  else if typ='DS' then Result:=ft_DataSet
  else if typ='OB' then Result:=ft_OraBlob
  else if typ='OC' then Result:=ft_OraClob
  else if typ='IT' then Result:=ft_Interface
  else if typ='ID' then Result:=ft_IDispatch
  else if typ='GU' then Result:=ft_Guid
  else if typ='DT' then Result:=ft_TimeStamp
  else if typ='FB' then Result:=ft_FMTBcd
  else raise Exception.Create('Unknown Field Type specifier "'+typ+'".');
  end;

function nullValueCode:String;
  begin
  Result:=TRtcValueObject.code_toShortString(TypeToStr(rtc_Null),'');
  end;

function nullValueXMLrpc:String;
  begin
  Result:='<value><nil/></value>';
  end;

function PosEx(c:char; const s:String; at:integer):integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=at to length(s) do
    if s[a]=c then
      begin
      Result:=a;
      Break;
      end;
  end;

function PosEx2(const c,s:String; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function MakeDecimal(const s:string):string;
  var
    p:longint;
  begin
  Result:=s;
  p:=Pos('.',s);
  if p>0 then Result[p]:=DecimalSeparator;
  end;

function Str2Curr(const s:String):Currency;
  begin
  if DecimalSeparator<>'.' then
    Result:=StrToCurr(MakeDecimal(s))
  else
    Result:=StrToCurr(s);
  end;

function FillZero(const s:string;len:integer):string;
  begin
  Result:=s;
  while length(Result)<len do
    Result:='0'+Result;
  end;

function Curr2Str(v:Currency):String;
  var
    p:longint;
  begin
  Result:=CurrToStr(v);
  if DecimalSeparator<>'.' then
    begin
    p:=Pos(DecimalSeparator,Result);
    if p>0 then Result[p]:='.';
    end;
  end;

function Str2Float(const s:String):rtcFloat;
  begin
  if DecimalSeparator<>'.' then
    Result:=StrToFloat(MakeDecimal(s))
  else
    Result:=StrToFloat(s);
  end;

function Float2Str(v:rtcFloat):String;
  var
    p:longint;
  begin
  Result:=FloatToStr(v);
  if DecimalSeparator<>'.' then
    begin
    p:=Pos(DecimalSeparator,Result);
    if p>0 then Result[p]:='.';
    end;
  end;

function Str2DateTime(s:String):TDateTime;
  var
    y,m,d,
    hh,mm,ss,ms:word;
    a:integer;
  function GetNum(const sep:String):word;
    begin
    if s='' then
      Result:=0
    else if sep='' then
      begin
      Result:=StrToInt(s);
      s:='';
      end
    else
      begin
      a:=Pos(sep,s);
      if a<=0 then a:=length(s)+1;
      try
        Result:=StrToInt(Copy(s,1,a-1));
      except
        raise EConvertError.Create('Invalid DateTime format.');
        end;
      Delete(s,1,a);
      s:=Trim(s);
      end;
    end;
  begin
  try
    Result:=0;
    if Pos('-',s)>0 then
      begin
      y:=GetNum('-');
      m:=GetNum('-');
      d:=GetNum(' ');
      Result:=EncodeDate(y,m,d);
      end;
    if Pos(':',s)>0 then
      begin
      hh:=GetNum(':');
      mm:=GetNum(':');
      ss:=GetNum('.');
      ms:=GetNum('');
      if Result>0 then
        Result:=Result+EncodeTime(hh,mm,ss,ms)
      else
        Result:=Result-EncodeTime(hh,mm,ss,ms);
      end;
  except
    on E:Exception do
      raise EConvertError.Create(E.Message+#13#10'Invalid date format.');
    end;
  end;

function DateTime2Str(v:TDateTime):String;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:=''
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:=IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss)+'.'+IntToStr(ms);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    Result:=IntToStr(y)+'-'+IntToStr(m)+'-'+IntToStr(d);
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    Result:=IntToStr(y)+'-'+IntToStr(m)+'-'+IntToStr(d)+' '+
            IntToStr(hh)+':'+IntToStr(mm)+':'+IntToStr(ss)+'.'+IntToStr(ms);
    end;
  end;

function DateTime2ISOStr(v:TDateTime):String;
  var
    y,m,d:word;
    hh,mm,ss,ms:word;
  begin
  if v=0 then
    Result:='00000000T00:00:00'
  else if trunc(v)=0 then // Time only
    begin
    DecodeTime(v, hh,mm,ss,ms);
    Result:='00000000T'+
            FillZero(IntToStr(hh),2)+':'+
            FillZero(IntToStr(mm),2)+':'+
            FillZero(IntToStr(ss),2);
    end
  else if frac(v)=0 then // Date only
    begin
    DecodeDate(v, y,m,d);
    Result:=FillZero(IntToStr(y),4)+
            FillZero(IntToStr(m),2)+
            FillZero(IntToStr(d),2)+
            'T00:00:00';
    end
  else // Date and Time
    begin
    DecodeDate(v, y,m,d);
    DecodeTime(v, hh,mm,ss,ms);
    Result:=FillZero(IntToStr(y),4)+
            FillZero(IntToStr(m),2)+
            FillZero(IntToStr(d),2)+'T'+
            FillZero(IntToStr(hh),2)+':'+
            FillZero(IntToStr(mm),2)+':'+
            FillZero(IntToStr(ss),2);
    end;
  end;

function ISOStr2DateTime(s:String):TDateTime;
  var
    y,m,d,
    hh,mm,ss,ms:word;
    s2:string;
    a:integer;
  function GetNum(const sep:String):word;
    begin
    if s='' then
      Result:=0
    else if sep='' then
      begin
      Result:=StrToInt(s);
      s:='';
      end
    else
      begin
      a:=Pos(sep,s);
      if a<=0 then a:=length(s)+1;
      try
        Result:=StrToInt(Copy(s,1,a-1));
      except
        raise EConvertError.Create('Invalid DateTime format.');
        end;
      Delete(s,1,a);
      s:=Trim(s);
      end;
    end;
  begin
  try
    Result:=0;
    a:=Pos('T',s);
    if (a>0) or (Pos(':',s)<=0) then // date included or time not included
      begin
      if Pos('-',s)>0 then
        begin
        y:=GetNum('-');
        m:=GetNum('-');
        d:=GetNum('T');
        end
      else
        begin
        if a>0 then
          begin
          s2:=Copy(s,1,a-1);
          Delete(s,1,a);
          end
        else
          begin
          s2:=s;
          s:='';
          end;
        if length(s2)>=4 then
          begin
          y:=StrToInt(Copy(s2,1,4));
          Delete(s2,1,4);
          end
        else
          y:=0;
        if length(s2)>=2 then
          begin
          m:=StrToInt(Copy(s2,1,2));
          Delete(s2,1,2);
          end
        else
          m:=0;
        if length(s2)>=2 then
          begin
          d:=StrToInt(Copy(s2,1,2));
          Delete(s2,1,2);
          end
        else
          d:=0;
        end;

      if (y>0) or (m>0) or (d>0) then
        Result:=EncodeDate(y,m,d);

      if length(s2)>0 then
        raise EConvertError.Create('Date Part too long.');
      end;

    if length(s)>0 then // time included
      begin
      hh:=GetNum(':');
      mm:=GetNum(':');
      ss:=GetNum('.');
      ms:=GetNum('+');
      if (hh>0) or (mm>0) or (ss>0) or (ms>0) then
        if Result>0 then
          Result:=Result+EncodeTime(hh,mm,ss,ms)
        else
          Result:=Result-EncodeTime(hh,mm,ss,ms);
      end;
  except
    on E:Exception do
      raise EConvertError.Create(E.Message+#13#10'Invalid DateTime format.');
    end;
  end;

{ TRtcValueObject }

procedure TRtcValueObject.Kill;
  begin
  Free;
  end;

function TRtcValueObject.GetTypeStr:String;
  begin
  Result:=TypeToStr(GetType);
  end;

class function TRtcValueObject.code_fromLongString(const typ,s:String; var at:integer):String;
  var
    loc1,loc,len:integer;
    val:String;
  begin
  if at<0 then
    raise Exception.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise Exception.Create('No more data. Starting position beyond end-of-String.');

  if CompareText(Copy(s,at+1,length(typ)+1),typ+MARK_TYPE)<>0 then
    raise Exception.Create('Expected object type specifier not found.');

  loc1:=at+length(typ)+1; // position of MARK_TYPE

  if Copy(s,loc1+1,1)=MARK_END then
    begin
    Result:='';
    at:=loc1+1;
    end
  else
    begin
    loc:=PosEx(MARK_LEN_START, s, loc1+1); // position of MARK_LEN
    if (loc<=0) then
      raise Exception.Create('String START-MARK missing.');

    try
      val:=Copy(s, loc1+1, loc-loc1-1);
      if val='' then
        len:=0
      else
        len:=StrToInt(val);
    except
      on E:Exception do
        raise Exception.Create('Length value missing.');
      end;

    if length(s)<loc+len+2 then
      raise Exception.Create('Not enough data.')
    else if Copy(s, loc+len+1, 1)<>MARK_LEN_END then
      raise Exception.Create('String END-MARK missing.')
    else if Copy(s, loc+len+2, 1)<>MARK_END then
      raise Exception.Create('COMMAND END-MARK missing.');

    Result:=Copy(s,loc+1,len); // get String after LENGTH-MARK
    at:=loc+len+2; // update pointer
    end;
  end;

class procedure TRtcValueObject.code_fromByteStream(const typ, s: String; var at: integer; const bs: TStream);
  var
    loc1,loc,len:integer;
    val:String;
  begin
  if at<0 then
    raise Exception.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise Exception.Create('No more data. Starting position beyond end-of-String.');

  if CompareText(Copy(s,at+1,length(typ)+1),typ+MARK_TYPE)<>0 then
    raise Exception.Create('Expected object type specifier not found.');

  loc1:=at+length(typ)+1; // position of MARK_TYPE

  if Copy(s,loc1+1,1)=MARK_END then
    begin
    bs.Size:=0;
    at:=loc1+1;
    end
  else
    begin
    loc:=PosEx(MARK_LEN_START, s, loc1+1); // position of MARK_LEN
    if (loc<=0) then
      raise Exception.Create('String START-MARK missing.');

    try
      val:=Copy(s, loc1+1, loc-loc1-1);
      if val='' then
        len:=0
      else
        len:=StrToInt(val);
    except
      on E:Exception do
        raise Exception.Create('Length value missing.');
      end;

    if length(s)<loc+len+2 then
      raise Exception.Create('Not enough data.')
    else if Copy(s, loc+len+1, 1)<>MARK_LEN_END then
      raise Exception.Create('String END-MARK missing.')
    else if Copy(s, loc+len+2, 1)<>MARK_END then
      raise Exception.Create('COMMAND END-MARK missing.');

    bs.Size:=0;
    bs.Write(s[loc+1],len);

    bs.Position:=0; // set to starting position

    at:=loc+len+2; // update pointer
    end;
  end;

class function TRtcValueObject.code_fromShortString(const typ,s:String; var at:integer):String;
  var
    loc,len:integer;
  begin
  if at<0 then
    raise Exception.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise Exception.Create('No more data. Starting position beyond end-of-String.');

  if CompareText(Copy(s,at+1,length(typ)+length(MARK_TYPE)),typ+MARK_TYPE)<>0 then
    raise Exception.Create('Expected object type specifier not found.');

  loc:=at+length(typ)+1; // position of MARK_TYP

  len:=PosEx(MARK_END, s, loc+1); // position of MARK_END
  if len<=0 then
    raise Exception.Create('END-MARK missing.');

  Result:=Copy(s,loc+1,len-loc-1); // get String between MARK_TYP and MARK_END
  at:=len; // update pointner
  end;

class function TRtcValueObject.code_fromNameString(const s: String; var at:integer):String;
  var
    len:integer;
  begin
  len:=PosEx(MARK_NAME, s, at+1); // position of MARK_END
  if len<=0 then
    raise Exception.Create('END-MARK missing.');

  Result:=Copy(s, at+1, len-at-1); // get String up to MARK_NAME
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_fromMidString(const s: String; var at:integer):String;
  var
    len:integer;
  begin
  len:=PosEx(MARK_MID, s, at+1); // position of MARK_MID
  if len<=0 then
    raise Exception.Create('MID-MARK missing.');

  Result:=Copy(s, at+1, len-at-1); // get String up to MARK_MID
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_fromEndString(const s: String; var at:integer):String;
  var
    len:integer;
  begin
  len:=PosEx(MARK_END, s, at+1); // position of MARK_END
  if len<=0 then
    raise Exception.Create('END-MARK missing.');

  Result:=Copy(s, at+1, len-at-1); // get String up to MARK_END
  at:=len; // update pointer
  end;

class function TRtcValueObject.code_checkStrType(const s: String; const at:integer): TRtcValueTypes;
  var
    loc:integer;
    typ:String;
  begin
  loc:=PosEx(MARK_TYPE, s, at+1);
  if loc<=0 then
    raise Exception.Create('Object type specifier not found.');

  typ:=Copy(s,at+1,loc-at-1);
  Result:=StrToType(typ);
  end;

class function TRtcValueObject.code_toByteStream(const typ: string; bs: TStream): String;
  var
    len,loc:integer;
    s:string;
  begin
  if assigned(bs) and (bs.Size>0) then
    begin
    // Value Header
    s:=typ +MARK_TYPE+IntToStr(bs.Size)+MARK_LEN_START;
    len:=length(s)+ bs.Size;

    SetLength(Result, len+length(MARK_LEN_END)+length(MARK_END));
    Move(s[1],Result[1],length(s));

    // Data
    loc:=bs.Position;
    try
      bs.Position:=0;
      bs.Read(Result[length(s)+1],bs.Size);
    finally
      bs.Position:=loc;
      end;

    // Footer
    s:=MARK_LEN_END+MARK_END;
    Move(s[1],Result[len+1],length(s));
    end
  else
    Result:=typ +MARK_TYPE+MARK_END;
  end;

class function TRtcValueObject.code_toLongString(const typ,s: String): String;
  var
    l1,l2,l3,p:longint;
    s2:string;
  begin
  {This code ...

  if length(s)>0 then
    Result:=typ +MARK_TYPE+ IntToStr(length(s)) +MARK_LEN_START+ s +MARK_LEN_END+MARK_END
  else
    Result:=typ +MARK_TYPE+MARK_END;

  was replaced by this code ... }
  l1:=length(typ);
  l3:=length(s);
  if l3=0 then
    begin
    SetLength(Result,l1+2);
    Move(typ[1],Result[1],l1); p:=l1+1;
    Result[p]:=MARK_TYPE; Inc(p);
    Result[p]:=MARK_END;
    end
  else
    begin
    s2:=IntToStr(l3); l2:=length(s2);
    SetLength(Result,l1+l2+l3+4);

    Move(typ[1],Result[1],l1); p:=l1+1;
    Result[p]:=MARK_TYPE; Inc(p);

    Move(s2[1],Result[p],l2); Inc(p,l2);

    Result[p]:=MARK_LEN_START; Inc(p);
    Move(s[1],Result[p],l3); Inc(p,l3);
    Result[p]:=MARK_LEN_END; Inc(p);

    Result[p]:=MARK_END;
    end;
  end;

class function TRtcValueObject.code_toShortString(const typ,s: String): String;
  var
    l1,l3,p:longint;
  begin
  {This code ....

  if length(s)>0 then
    Result:=typ +MARK_TYPE+ s +MARK_END
  else
    Result:=typ +MARK_TYPE+MARK_END;

  was replaced by this code ... }

  l1:=length(typ);
  l3:=length(s);
  SetLength(Result,l1+2+l3);

  Move(typ[1],Result[1],l1); p:=l1+1;
  Result[p]:=MARK_TYPE; Inc(p);
  if l3>0 then
    begin
    Move(s[1],Result[p],l3); Inc(p,l3);
    Result[p]:=MARK_END;
    end
  else
    Result[p]:=MARK_END;
  end;

class function TRtcValueObject.code_toNameString(const s: String): String;
  var
    l:longint;
  begin
  { This code ...

    Result:=s+MARK_NAME;

  changed to ... }

  l:=length(s); SetLength(Result,l+1);
  Move(s[1],Result[1],l);
  Result[l+1]:=MARK_NAME;
  end;

class function TRtcValueObject.code_toMidString(const s: String): String;
  var
    l:longint;
  begin
  {This code ...

    Result:=s + MARK_MID;

  changed to ... }

  l:=length(s); SetLength(Result,l+1);
  Move(s[1],Result[1],l);
  Result[l+1]:=MARK_MID;
  end;

class function TRtcValueObject.code_toEndString(const s: String): String;
  var
    l:longint;
  begin
  {This code ...

    Result:=s + MARK_END;

  changed to ... }

  l:=length(s); SetLength(Result,l+1);
  Move(s[1],Result[1],l);
  Result[l+1]:=MARK_END;
  end;

function EncodeXMLrpc(const s:string):string;
  var
    a,b:integer;
  begin
  b:=length(s);
  for a:=1 to length(s) do
    case s[a] of
      '<':Inc(b,3);
      '&':Inc(b,4);
      end;
  SetLength(Result,b);
  b:=1;
  for a:=1 to length(s) do
    case s[a] of
      '<':begin
          Result[b]:='&';
          Result[b+1]:='l';
          Result[b+2]:='t';
          Result[b+3]:=';';
          Inc(b,4);
          end;
      '&':begin
          Result[b]:='&';
          Result[b+1]:='a';
          Result[b+2]:='m';
          Result[b+3]:='p';
          Result[b+4]:=';';
          Inc(b,5);
          end;
      else
          begin
          Result[b]:=s[a];
          Inc(b);
          end;
      end;
  end;

function DecodeXMLrpc(const s:string):string;
  var
    a,b,c,i,len:integer;
    st:string;
  begin
  len:=length(s);
  SetLength(Result,length(s));
  a:=1; b:=0;
  while a<=len do
    begin
    if s[a]='&' then
      begin
      c:=a;
      i:=len;
      if i>c+7 then i:=c+7;

      while (c<i) and (s[c]<>';') do Inc(c);
      if s[c]<>';' then
        begin
        Inc(b);
        Result[b]:=s[a];
        Inc(a);
        end
      else
        begin
        if (c>a) and (s[a+1]='#') then
          st:=Copy(s,a+2,c-a-2)
        else
          st:=UpperCase(Copy(s,a+1,c-a-1));
        Inc(b);
        if s[a+1]='#' then
          begin
          i:=StrToIntDef(st,-1);
          if (i>=0) and (i<=255) then
            Result[b]:=Char(i)
          else
            begin
            Result[b]:=s[a];
            c:=a;
            end;
          end
        else if st='LT' then
          Result[b]:='<'
        else if st='GT' then
          Result[b]:='>'
        else if st='AMP' then
          Result[b]:='&'
        else if st='QUOT' then
          Result[b]:='"'
        else if st='APOS' then
          Result[b]:=#39
        else if st='NBSP' then
          Result[b]:=' '
        else
          begin
          Result[b]:=s[a];
          c:=a;
          end;
        a:=c+1;
        end;
      end
    else
      begin
      Inc(b);
      Result[b]:=s[a];
      Inc(a);
      end;
    end;
  SetLength(Result,b);
  end;

class function TRtcValueObject.xmlrpc_readString(const s: String; var at: integer): string;
  begin
  Result:=DecodeXMLrpc(xmlrpc_readValue(s,at));
  end;

class function TRtcValueObject.xmlrpc_writeString(const s: String): String;
  begin
  Result:=EncodeXMLrpc(s);
  end;

class procedure TRtcValueObject.xmlrpc_readByteStream(const s: String; var at: integer; const bs: TStream);
  var
    val,val2:String;
  begin
  if at<0 then
    raise Exception.Create('Starting position < 0 !?')
  else if at>=length(s) then
    raise Exception.Create('No more data. Starting position beyond end-of-String.');

  bs.Size:=0;
  val:=xmlrpc_readValue(s,at);
  if val<>'' then
    begin
    val2:=Mime_Decode(val);
    val:='';
    if val2<>'' then
      begin
      bs.Write(val2[1],length(val2));
      bs.Position:=0; // move pointer back to the beginning of the stream
      val2:='';
      end;
    end;
  end;

class function TRtcValueObject.xmlrpc_writeByteStream(bs: TStream):string;
  var
    val:string;
    loc:integer;
  begin
  if assigned(bs) and (bs.Size>0) then
    begin
    SetLength(val, bs.size);
    // Copy Data from ByteStream to temp string
    loc:=bs.Position;
    try
      bs.Position:=0;
      bs.Read(val[1],bs.Size);
    finally
      bs.Position:=loc;
      end;

    Result:=Mime_Encode(val);
    val:='';
    end
  else
    Result:='';
  end;

class procedure TRtcValueObject.xmlrpc_OpenTag(const tag:string; var closing_tags:rtcClosingTagsType);
  begin
  SetLength(closing_tags, length(closing_tags)+1);
  closing_tags[length(closing_tags)-1]:='/'+tag;
  end;

class function TRtcValueObject.xmlrpc_CloseTag(const tag:string; var closing_tags:rtcClosingTagsType):boolean;
  begin
  if tag='' then // not a TAG
    Result:=False
  else if (length(closing_tags)>0) and (closing_tags[length(closing_tags)-1]=tag) then
    begin
    SetLength(closing_tags,length(closing_tags)-1);
    Result:=True;
    end
  else
    Result:=False;
  end;

class function TRtcValueObject.xmlrpc_FirstCloseTag(var closing_tags:rtcClosingTagsType):string;
  begin
  if length(closing_tags)>0 then
    Result:=closing_tags[length(closing_tags)-1]
  else
    Result:='';
  end;

class function TRtcValueObject.xmlrpc_TagsToXML(var closing:rtcClosingTagsType):string;
  var
    a:integer;
  begin
  Result:='';
  for a:=length(closing)-1 downto 0 do
    Result:=Result+'<'+closing[a]+'>';
  end;

class procedure TRtcValueObject.xmlrpc_skipWhitespace(const s: String; var at: integer);
  var
    len:integer;
  begin
  len:=length(s);
  while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
  end;

class function TRtcValueObject.xmlrpc_readTag(const s: String; var at: integer; const tag_want:string=''; skipWhitespace:boolean=True): string;
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    Result:=''
  else if s[at+1]<>'<' then
    raise Exception.Create('XML-RPC Error: Tag opening "<" expected, but "'+s[at]+'" found.')
  else
    begin
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx2('-->',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx2('?>',s,at+1);
            if at2<0 then
              raise Exception.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // return text inside '< >'
    Result:=TrimCopy(s,at+2,at2-at-2);
    // Position behind '>'
    at:=at2;

    if skipWhiteSpace then
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);

    if Result<>'' then
      if Result[length(Result)]='/' then
        Result:=TrimCopy(Result,1,length(Result)-1)+'/'
      else if Result[1]='/' then
        Result:='/'+TrimCopy(Result,2,length(Result)-1);
    end;
  if (tag_want<>'') then
    begin
    if Result='' then
      raise Exception.Create('XML-RPC Error: Tag <'+tag_want+'> expected, but missing.')
    else if CompareText(Result,tag_want)<>0 then
      raise Exception.Create('XML-RPC Error: Tag <'+tag_want+'> expected, but <'+Result+'> found.');
    end;
  end;

class function TRtcValueObject.xmlrpc_checkTag(const s: String; at: integer): string;
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    Result:=''
  else if s[at+1]<>'<' then
    Result:=''
  else
    begin
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx2('-->',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx2('?>',s,at+1);
            if at2<0 then
              raise Exception.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // return text inside '< >'
    Result:=TrimCopy(s,at+2,at2-at-2);

    if Result<>'' then
      if Result[length(Result)]='/' then
        Result:=TrimCopy(Result,1,length(Result)-1)+'/'
      else if Result[1]='/' then
        Result:='/'+TrimCopy(Result,2,length(Result)-1);
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipTag(const s: String; var at: integer; skipWhiteSpace:boolean=True);
  var
    len,at2:integer;
  begin
  len:=length(s);
  if at>=len then
    begin
    end
  else if s[at+1]<>'<' then
    raise Exception.Create('XML-RPC Error: Tag opening "<" expected, but "'+s[at]+'" found.')
  else
    begin
    while (at+3<len) and (s[at+1]='<') and (s[at+2] in ['!','?']) do // skip <?xml?>, <!----> and <!DOC> - not part of XML-RPC
      begin
      case s[at+2] of
        '!':begin
            if s[at+3]='-' then
              begin
              at2:=PosEx2('-->',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing "-->" expected, but missing.');
              at:=at2+2;
              end
            else
              begin
              at2:=PosEx('>',s,at+1);
              if at2<0 then
                raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
              at:=at2;
              end;
            end;
        '?':begin
            at2:=PosEx2('?>',s,at+1);
            if at2<0 then
              raise Exception.Create('XML-RPC Error: Tag closing "?>" expected, but missing.');
            at:=at2+1;
            end;
        end;
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
      end;

    at2:=PosEx('>',s,at+1);
    if at2<0 then
      raise Exception.Create('XML-RPC Error: Tag closing ">" expected, but missing.');
    // Position behind '>'
    at:=at2;
    if skipWhiteSpace then
      while (at<len) and (s[at+1] in [#32,#9,#13,#10]) do Inc(at);
    end;
  end;

class function TRtcValueObject.xmlrpc_readValue(const s: String; var at: integer): string;
  var
    at2:integer;
  begin
  if at>=length(s) then
    Result:=''
  else if s[at+1]='<' then
    Result:=''
  else
    begin
    at2:=PosEx('<',s,at+1);
    if at2<0 then
      raise Exception.Create('XML-RPC Error: Tag opening "<" expected, but missing.');
    // return text before '<'
    Result:=Copy(s,at+1,at2-at-1);
    // Position at '<'
    at:=at2-1;
    end;
  end;

class function TRtcValueObject.xmlrpc_readTrimValue(const s: String; var at: integer): string;
  var
    at2:integer;
  begin
  if at>=length(s) then
    Result:=''
  else if s[at+1]='<' then
    Result:=''
  else
    begin
    at2:=PosEx('<',s,at+1);
    if at2<0 then
      raise Exception.Create('XML-RPC Error: Tag opening "<" expected, but missing.');
    // return text before '<'
    Result:=TrimCopy(s,at+1,at2-at-1);
    // Position at '<'
    at:=at2-1;
    end;
  end;

class function TRtcValueObject.xmlrpc_checkStrType(const s: String; const at: integer): TRtcValueTypes;
  var
    xtag:string;
    at2:integer;
  begin
  at2:=at;
  xmlrpc_skipWhitespace(s, at2);
  xtag:=UpperCase(xmlrpc_checkTag(s,at2));

  // Skip ?XML header, if present
  if xtag='' then
    raise Exception.Create('XML-RPC Error: Type identifier expected.');

  // Check if this is a complex structure
  if (xtag='METHODRESPONSE') or (xtag='PARAMS') or (xtag='PARAM') then
    begin
    xmlrpc_skipTag(s,at2); // <methodResponse>
    xtag:=UpperCase(xmlrpc_checkTag(s,at2));
    Result:=rtc_Variant;
    Exit;
    end;

  // Anything can be inside a 'VALUE' tag
  if xtag='VALUE' then
    begin
    repeat
      xmlrpc_skipTag(s,at2); // <value>
      xtag:=UpperCase(xmlrpc_checkTag(s,at2));
      while xtag='DATA' do
        begin
        xmlrpc_skipTag(s,at2); // <data>
        xtag:=UpperCase(xmlrpc_checkTag(s,at2));
        end;
      until xtag<>'VALUE';

    if xtag='' then // not followed by a tag, containing a string
      xtag:='STRING'
    else if xtag='/VALUE' then // empty <value></value> set means an empty string
      xtag:='STRING';
    end;

  // Now check the type
  if xtag='FAULT' then
    Result:=rtc_Exception
  else if (xtag='METHODCALL') or (xtag='CALL') then
    Result:=rtc_Function
  else if (xtag='I4') or (xtag='INT') then
    Result:=rtc_LargeInt
  else if xtag='BOOLEAN' then
    Result:=rtc_Boolean
  else if xtag='STRING' then
    Result:=rtc_String
  else if xtag='DOUBLE' then
    Result:=rtc_Float
  else if (xtag='DATETIME.ISO8601') or (xtag='DATETIME') or (xtag='TIMESTAMP') then
    Result:=rtc_DateTime
  else if (xtag='BASE64') or (xtag='BASE64BINARY') or (xtag='BINARY') then
    Result:=rtc_ByteStream
  else if (xtag='NAME') then
    begin
    xmlrpc_skipTag(s,at2); // <name>

    xtag:=UpperCase(xmlrpc_readTrimValue(s,at2));
    if (xtag=RTC_XMLRPC_DataSetFieldsName) or
       (xtag=RTC_XMLRPC_DataSetRowsName) then
      Result:=rtc_DataSet
    else if (xtag='FAULTCODE') or (xtag='FAULTSTRING') then
      Result:=rtc_Exception
    else
      Result:=rtc_Record;
    end
  else if (xtag='STRUCT') or (xtag='METHODFAULT') or (xtag='MEMBER') then
    begin
    xmlrpc_skipTag(s,at2); // <struct>

    xtag:=UpperCase(xmlrpc_checkTag(s,at2));
    if xtag='MEMBER' then
      begin
      xmlrpc_skipTag(s,at2); // <member>
      xtag:=UpperCase(xmlrpc_checkTag(s,at2));
      end;

    if xtag='NAME' then
      begin
      xmlrpc_skipTag(s,at2); // <name>

      xtag:=UpperCase(xmlrpc_readTrimValue(s,at2));
      if (xtag=RTC_XMLRPC_DataSetFieldsName) or
         (xtag=RTC_XMLRPC_DataSetRowsName) then
        Result:=rtc_DataSet
      else if (xtag='FAULTCODE') or (xtag='FAULTSTRING') then
        Result:=rtc_Exception
      else
        Result:=rtc_Record;
      end
    else if xtag<>'' then
      Result:=rtc_Record
    else
      raise Exception.Create('XML-RPC Error: <Struct><Member> Tags followed by plain data (<Name> expected).');
    end
  else if xtag='ARRAY' then
    Result:=rtc_Array
  else if (xtag<>'') and (xtag[length(xtag)]='/') then
    Result:=rtc_Null
  else
    raise Exception.Create('XML-RPC Error: Type identifier expected, <'+xtag+'> found.');
  end;

class procedure TRtcValueObject.xmlrpc_skipValueOpen(const tag:string; const s: String; var at: integer; var closing_tags:rtcClosingTagsType);
  var
    at3:integer;
    xtag:string;
  begin
  xmlrpc_skipWhitespace(s, at);
  xtag:=UpperCase(xmlrpc_checkTag(s,at));

  // Skip ?XML header, if present
  if xtag='' then
    raise Exception.Create('XML-RPC Error: Type identifier expected.');

  // Skip 'METHODRESPONSE', if present.
  if (xtag='METHODRESPONSE') or (xtag='PARAMS') or (xtag='PARAM') then
    begin
    if tag<>'METHODRESPONSE' then
      raise Exception.Create('XML-RPC Error: Tag <'+xtag+'> found, while <'+tag+'> was expected.');

    if xtag='METHODRESPONSE' then
      begin
      xmlrpc_OpenTag(xtag,closing_tags);
      xmlrpc_skipTag(s,at); // <methodResponse>
      end;
    Exit;
    end;

  // skip 'FAULT', if present
  if xtag='FAULT' then
    begin
    xmlrpc_OpenTag(xtag, closing_tags);

    xmlrpc_skipTag(s,at); // <fault>

    xtag:=UpperCase(xmlrpc_checkTag(s,at));
    end;

  // Anything can be inside a 'VALUE' tag
  if xtag='VALUE' then
    begin
    repeat
      xmlrpc_OpenTag(xtag, closing_tags);
      xmlrpc_skipTag(s,at,False); // <value> - don't skip whitespace

      at3:=at;
      xmlrpc_skipWhitespace(s,at);
      xtag:=UpperCase(xmlrpc_checkTag(s,at));

      while xtag='DATA' do // skip all <DATA> tags
        begin
        xmlrpc_OpenTag(xtag, closing_tags);
        xmlrpc_skipTag(s,at,False); // <data>

        at3:=at;
        xmlrpc_skipWhitespace(s,at);
        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        end;
      until xtag<>'VALUE';

    if (xtag='') or (xtag='/VALUE') then // empty string or <value></value>
      begin
      if tag<>'STRING' then
        raise Exception.Create('XML-RPC Error: Tag <'+xtag+'> found, but <'+tag+'> expected.');
      at:=at3;
      Exit;
      end
    else if xtag[1]='/' then // closing tag, but not </value>
      raise Exception.Create('XML-RPC Error: Type identifier or </value> expected (<'+xtag+'> found).');
    end;

  if xtag='' then
    raise Exception.Create('XML-RPC Error: Type identifier <'+tag+'> expected, but missing.')
  else if (xtag='NAME') or (xtag='MEMBER') then
    begin
    if tag<>'STRUCT' then
      raise Exception.Create('XML-RPC Error: Tag <'+xtag+'> found, while <'+tag+'> was expected.');
    end
  else
    begin
    xmlrpc_OpenTag(xtag, closing_tags);
    xmlrpc_skipTag(s,at,False); // <int>/<i4>/<string>/<base64>/... - don't skip whitespace
    if xtag='I4' then xtag:='INT';
    if (xtag='DATETIME') or (xtag='TIMESTAMP') then xtag:='DATETIME.ISO8601';
    if (xtag='BASE64BINARY') or (xtag='BINARY') then xtag:='BASE64';
    if xtag='METHODFAULT' then xtag:='STRUCT';
    if xtag='CALL' then xtag:='METHODCALL';
    if tag<>xtag then
      raise Exception.Create('XML-RPC Error: Tag <'+xtag+'> found, while <'+tag+'> was expected.');
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipValueClose(const s: String; var at: integer; var closing_tags:rtcClosingTagsType);
  var
    xtag:string;
  begin
  xmlrpc_skipWhitespace(s, at);
  while length(closing_tags)>0 do
    begin
    xtag:=UpperCase(xmlrpc_checkTag(s,at));

    if xmlrpc_CloseTag(xtag,closing_tags) then
      xmlrpc_skipTag(s,at) // <"xtag">
    else
      raise Exception.Create('XML-RPC Error: Closing Tag(s) missing: '+xmlrpc_TagsToXML(closing_tags));
    end;
  end;

class procedure TRtcValueObject.xmlrpc_skipNull(const s: String; var at: integer);
  var
    xtag:string;
    closing_tags:rtcClosingTagsType;
  begin
  SetLength(closing_tags,0);
  try
    xmlrpc_skipWhitespace(s, at);
    xtag:=UpperCase(xmlrpc_checkTag(s,at));
    // Skip ?XML header, if present
    if xtag='' then
      raise Exception.Create('XML-RPC Error: Type identifier expected.');

    // Skip 'FAULT', if present
    if xtag='FAULT' then
      begin
      xmlrpc_OpenTag(xtag, closing_tags);

      xmlrpc_skipTag(s,at); // <fault>

      xtag:=UpperCase(xmlrpc_checkTag(s,at));
      end;
    // Anything can be inside a 'VALUE' tag
    if xtag='VALUE' then
      begin
      repeat
        xmlrpc_OpenTag(xtag, closing_tags);
        xmlrpc_skipTag(s,at); // <value> including whitespace

        xtag:=UpperCase(xmlrpc_checkTag(s,at));

        while xtag='DATA' do
          begin
          xmlrpc_OpenTag(xtag, closing_tags);
          xmlrpc_skipTag(s,at); // <value> including whitespace

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          end;
        until xtag<>'VALUE';
      end;

    if (xtag<>'') and (xtag[length(xtag)]='/') then
      begin
      xmlrpc_skipTag(s,at); // <nil/>

      while length(closing_tags)>0 do
        begin
        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        if xmlrpc_CloseTag(xtag,closing_tags) then
          xmlrpc_skipTag(s,at)
        else
          raise Exception.Create('XML-RPC Error: Closing Tag(s) missing: '+xmlrpc_TagsToXML(closing_tags));
        end;
      end
    else
      raise Exception.Create('XML-RPC Error: <NIL/> expected, <'+xtag+'> found.');
  finally
    SetLength(closing_tags,0);
    end;
  end;

class function TRtcValueObject.ObjectFromType(const typ: TRtcValueTypes): TRtcValueObject;
  begin
  case typ of
    rtc_Null:         Result:=nil;
    rtc_Function:     Result:=TRtcFunctionInfo.Create;
    rtc_Exception:    Result:=TRtcExceptionValue.Create;
    rtc_Variable:     Result:=TRtcVariableName.Create;
    rtc_Array:        Result:=TRtcArray.Create;
    rtc_Record:       Result:=TRtcRecord.Create;
    rtc_DataSet:      Result:=TRtcDataSet.Create;
    rtc_String:       Result:=TRtcStringValue.Create;
    rtc_WideString:   Result:=TRtcWideStringValue.Create;
    rtc_Text:         Result:=TRtcTextValue.Create;
    rtc_Boolean:      Result:=TRtcBooleanValue.Create;
    rtc_Integer:      Result:=TRtcIntegerValue.Create;
    rtc_LargeInt:     Result:=TRtcLargeIntValue.Create;
    rtc_Float:        Result:=TRtcFloatValue.Create;
    rtc_Currency:     Result:=TRtcCurrencyValue.Create;
    rtc_DateTime:     Result:=TRtcDateTimeValue.Create;
    rtc_ByteStream:   Result:=TRtcByteStream.Create;
    rtc_Variant:      Result:=TRtcValue.Create;
    else
      raise Exception.Create('Unsupported object type in function ObjectFromType.');
    end;
  end;

class function TRtcValueObject.ObjectFromXMLRPC(const s: String; var at: integer): TRtcValueObject;
  var
    typ:TRtcValueTypes;
  begin
  typ:=xmlrpc_checkStrType(s,at);
  if typ=rtc_Null then
    begin
    xmlrpc_skipNull(s,at);
    Result:=nil;
    end
  else
    begin
    Result:=ObjectFromType(typ);
    Result.from_XMLRPC(s,at);
    end;
  end;

class function TRtcValueObject.ObjectFromCode(const s: String; var at:integer): TRtcValueObject;
  var
    typ:TRtcValueTypes;
    data:String;
  begin
  typ:=code_checkStrType(s,at);
  if typ=rtc_Null then
    begin
    data:=code_fromShortString(TypeToStr(rtc_Null),s,at);
    Result:=nil;
    end
  else
    begin
    Result:=ObjectFromType(typ);
    Result.from_Code(s,at);
    end;
  end;

class function TRtcValueObject.ObjectFromVariant(const v: Variant): TRtcValueObject;
  begin
  case TVarData(V).VType of
    varEmpty,varNull:
      begin
      Result:=nil;
      end;
    varBoolean:
      begin
      Result:=TRtcBooleanValue.Create;
      TRtcBooleanValue(Result).SetBoolean(v);
      end;
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64:
      begin
      Result:=TRtcLargeIntValue.Create;
      TRtcLargeIntValue(Result).SetLargeInt(v);
      end;
    {$ENDIF}

    {$IFNDEF IDE_1}
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      Result:=TRtcIntegerValue.Create;
      {$IFDEF IDE_0}
        TRtcIntegerValue(Result).SetInteger(LongInt(V));
      {$ELSE}
        TRtcIntegerValue(Result).SetInteger(v);
      {$ENDIF}
      end;
    varSingle,
    varDouble:
      begin
      Result:=TRtcFloatValue.Create;
      TRtcFloatValue(Result).SetFloat(v);
      end;
    varCurrency:
      begin
      Result:=TRtcCurrencyValue.Create;
      TRtcCurrencyValue(Result).SetCurrency(v);
      end;
    varDate:
      begin
      Result:=TRtcDateTimeValue.Create;
      TRtcDateTimeValue(Result).SetDateTime(v);
      end;
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varString:
      begin
      Result:=TRtcStringValue.Create;
      TRtcStringValue(Result).SetString(v);
      end;
    varOleStr:
      begin
      Result:=TRtcWideStringValue.Create;
      TRtcWideStringValue(Result).SetWideString(v);
      end;
    else
      raise EConvertError.Create('Unsupported Variant type.');
    end;
  end;

class function TRtcValueObject.ObjectFromCode(const s: String): TRtcValueObject;
  var
    at:integer;
  begin
  at:=0;
  Result:=ObjectFromCode(s,at);
  if at<>length(s) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValueObject.ObjectFromXMLRPC(const s: String): TRtcValueObject;
  var
    at:integer;
  begin
  at:=0;
  Result:=ObjectFromXMLRPC(s,at);
  if at<>length(s) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcValueObject.from_Code(const s: String);
  var
    at:integer;
  begin
  from_Code(s,at);
  if at<>length(s) then
    raise Exception.Create('String contains more data than expected.');
  end;

procedure TRtcValueObject.from_XMLRPC(const s: String);
  var
    at:integer;
  begin
  from_XMLRPC(s,at);
  if at<>length(s) then
    raise Exception.Create('String contains more data than expected.');
  end;

function TRtcValueObject.toXMLrpcRequest: string;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    s.Add('<?xml version="1.0"?>'#13#10);
    to_XMLRPC(s);

    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpcResponse: string;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    if GetType=rtc_Exception then
      begin
      s.Add('<?xml version="1.0"?>'#13#10'<methodResponse><fault>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</fault></methodResponse>');
      end
    else
      begin
      s.Add('<?xml version="1.0"?>'#13#10'<methodResponse><params><param>'#13#10);
      to_XMLRPC(s);
      s.Add(#13#10'</param></params></methodResponse>');
      end;

    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toCode: String;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_Code(s);
    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

function TRtcValueObject.toXMLrpc: String;
  var
    s:TRtcHugeString;
  begin
  s:=TRtcHugeString.Create;
  try
    to_XMLRPC(s);
    Result:=s.Get;
  finally
    s.Free;
    end;
  end;

procedure TRtcValueObject.Extracted;
  begin
  // standard implementation - do nothing
  end;

{ TRtcSimpleValue }

function TRtcSimpleValue.GetBoolean: boolean;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Boolean.');
  end;

function TRtcSimpleValue.GetCurrency: Currency;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Currency.');
  end;

function TRtcSimpleValue.GetDateTime: TDateTime;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TDateTime.');
  end;

function TRtcSimpleValue.GetException: String;
  begin
  Result:='';
  raise EConvertError.Create('Can not convert '+ClassName+' to TRtcExceptionValue.');
  end;

function TRtcSimpleValue.GetVarName: String;
  begin
  Result:='';
  raise EConvertError.Create('Can not convert '+ClassName+' to TRtcVariableName.');
  end;

function TRtcSimpleValue.GetInteger: rtcInteger;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Integer.');
  end;

function TRtcSimpleValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=GetInteger;
  end;

function TRtcSimpleValue.GetFloat: rtcFloat;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Float.');
  end;

function TRtcSimpleValue.GetString: String;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to String.');
  end;

function TRtcSimpleValue.GetWideString: WideString;
  begin
  Result:=GetString;
  end;

function TRtcSimpleValue.GetText: String;
  begin
  Result:=GetString;
  end;

procedure TRtcSimpleValue.SetBoolean(const Value: boolean);
  begin
  raise EConvertError.Create('Can not convert Boolean to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetCurrency(const Value: Currency);
  begin
  raise EConvertError.Create('Can not convert Currency to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetDateTime(const Value: TDateTime);
  begin
  raise EConvertError.Create('Can not convert TDateTime to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetException(const Value: String);
  begin
  raise EConvertError.Create('Can not convert TRtcExceptionValue to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetVarName(const Value: String);
  begin
  raise EConvertError.Create('Can not convert TRtcVariableName to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetInteger(const Value: rtcInteger);
  begin
  raise EConvertError.Create('Can not convert Integer to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetLargeInt(const Value: rtcLargeInt);
  begin
  SetInteger(Value);
  end;

procedure TRtcSimpleValue.SetNull(const Value: boolean);
  begin
  raise Exception.Create('Can not re-set '+ClassName+' to NULL.');
  end;

procedure TRtcSimpleValue.SetFloat(const Value: rtcFloat);
  begin
  raise EConvertError.Create('Can not convert Float to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetString(const Value: String);
  begin
  raise EConvertError.Create('Can not convert String to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.SetWideString(const Value: WideString);
  begin
  SetString(Value);
  end;

procedure TRtcSimpleValue.SetText(const Value: String);
  begin
  SetString(Value);
  end;

function TRtcSimpleValue.GetVariant: Variant;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to Variant.');
  end;

function TRtcSimpleValue.SetVariant(const Value: Variant): boolean;
  begin
  raise EConvertError.Create('Can not convert Variant to '+ClassName+'.');
  end;

function TRtcSimpleValue.GetByteStream: TStream;
  begin
  raise EConvertError.Create('Can not convert '+ClassName+' to TStream.');
  end;

procedure TRtcSimpleValue.SetByteStream(const Value: TStream);
  begin
  raise EConvertError.Create('Can not convert TStream to '+ClassName+'.');
  end;

procedure TRtcSimpleValue.Extracted;
  begin
  // simple value objects - destroy the container
  Free;
  end;

{ TRtcExceptionValue }

constructor TRtcExceptionValue.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcExceptionValue.Destroy;
  begin
  FValue:='';
  inherited;
  end;

function TRtcExceptionValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Exception;
  end;

function TRtcExceptionValue.GetException: String;
  begin
  Result:=FValue;
  end;

function TRtcExceptionValue.GetString: String;
  begin
  Result:=FValue;
  end;

class function TRtcExceptionValue.NullValue: String;
  begin
  Result:='';
  end;

procedure TRtcExceptionValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcExceptionValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcExceptionValue(Value).FValue;
  end;

function TRtcExceptionValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcExceptionValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcExceptionValue.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toLongString(GetTypeStr, FValue) );
  end;

procedure TRtcExceptionValue.to_XMLRPC(const Result:TRtcHugeString);
  var
    fcode,fstring:string;
    a:integer;
  begin
  if Copy(FValue,1,1)='#' then
    begin
    a:=Pos(':',FValue);
    if a<=0 then // only the code, no string.
      begin
      fcode:=Copy(FValue,2,length(FValue)-1);
      if fcode='' then
        fstring:=FValue
      else
        fstring:='';
      end
    else if a>2 then // code and string
      begin
      fcode:=Copy(FValue,2,a-2);
      fstring:=Copy(FValue,a+1,length(FValue)-a);
      end
    else // string starts with an empty '#:' - no code
      begin
      fcode:='';
      fstring:=FValue;
      end;
    for a:=1 to length(fcode) do
      if not (fcode[a] in ['-','+','0'..'9']) then
        begin
        fcode:='';
        fstring:=FValue;
        Break;
        end;
    end
  else
    begin
    fcode:='';
    fstring:=FValue;
    end;

  if fcode='' then fcode:='0';

  Result.Add('<value><struct>'#13#10'<member><name>faultCode</name><value><int>');
  Result.Add(fcode);
  Result.Add('</int></value></member>'#13#10'<member><name>faultString</name><value><string>');
  Result.Add( xmlrpc_writeString(fstring) );
  Result.Add('</string></value></member>'#13#10'</struct></value>');
  end;

procedure TRtcExceptionValue.from_Code(const s: String; var at:integer);
  begin
  FValue:=code_fromLongString(GetTypeStr, s, at);
  end;

procedure TRtcExceptionValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    s1:string;
    xtag, fcode,fstring:string;
    xval:TRtcValueObject;
    c_tag:string;
  begin
  fcode:='';
  fstring:='';
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xtag:=UpperCase(xmlrpc_checkTag(s,at));
    if xtag='MEMBER' then
      begin
      repeat
        xmlrpc_skipTag(s,at); // <member>

        xmlrpc_readTag(s,at,'NAME');
        s1:=UpperCase(xmlrpc_readTrimValue(s,at));
        xmlrpc_readTag(s,at,'/NAME');

        if s1='FAULTCODE' then
          begin
          s1:=UpperCase(xmlrpc_checkTag(s,at));
          if s1<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fcode:=TRtcSimpleValue(xval).GetString
                else
                  raise Exception.Create('XML-RPC Error parsing "fault": faultCode value expected, complex object found.');
              finally
                xval.Free;
                end;
              end;
            end;
          end
        else if s1='FAULTSTRING' then
          begin
          s1:=UpperCase(xmlrpc_checkTag(s,at));
          if s1<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fstring:=TRtcSimpleValue(xval).GetString
                else
                  raise Exception.Create('XML-RPC Error parsing "fault": faultString value expected, complex object found.');
              finally
                xval.Free;
                end;
              end;
            end;
          end
        else
          raise Exception.Create('XML-RPC Error: "faultCode" or "faultString" expected, but "'+s1+'" found.');
        xmlrpc_readTag(s,at,'/MEMBER');

        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        until xtag<>'MEMBER';
      end
    else if xtag='NAME' then
      begin
      repeat
        xmlrpc_readTag(s,at,'NAME');
        s1:=UpperCase(xmlrpc_readTrimValue(s,at));
        xmlrpc_readTag(s,at,'/NAME');

        if s1='FAULTCODE' then
          begin
          s1:=UpperCase(xmlrpc_checkTag(s,at));
          if (s1<>c_tag) and (s1<>'NAME') then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fcode:=TRtcSimpleValue(xval).GetString
                else
                  raise Exception.Create('XML-RPC Error parsing "fault": faultCode value expected, complex object found.');
              finally
                xval.Free;
                end;
              end;
            end;
          end
        else if s1='FAULTSTRING' then
          begin
          s1:=UpperCase(xmlrpc_checkTag(s,at));
          if (s1<>c_tag) and (s1<>'NAME') then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            if assigned(xval) then
              begin
              try
                if isSimpleValue(xval) then
                  fstring:=TRtcSimpleValue(xval).GetString
                else
                  raise Exception.Create('XML-RPC Error parsing "fault": faultString value expected, complex object found.');
              finally
                xval.Free;
                end;
              end;
            end;
          end
        else
          raise Exception.Create('XML-RPC Error: "faultCode" or "faultString" expected, but "'+s1+'" found.');

        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        until xtag<>'NAME';
      end;

    if (fcode<>'') and (fcode<>'0') then
      FValue:='#'+fcode+':'+fstring
    else
      FValue:=fstring;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcExceptionValue.SetException(const Value: String);
  begin
  FValue:=Value;
  end;

function TRtcExceptionValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcExceptionValue.SetVariant(const Value: Variant): boolean;
  begin
  Result:=False;
  end;

constructor TRtcExceptionValue.Create(const Value: string);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcByteStream }

constructor TRtcByteStream.Create;
  begin
  inherited;
  FValue:=nil;
  end;

destructor TRtcByteStream.Destroy;
  begin
  if assigned(FValue) then
    begin
    FValue.Free;
    FValue:=nil;
    end;
  inherited;
  end;

class function TRtcByteStream.NullValue: TStream;
  begin
  Result:=nil;
  end;

function TRtcByteStream.GetType: TRtcValueTypes;
  begin
  Result:=rtc_ByteStream;
  end;

procedure TRtcByteStream.CopyFrom(Value: TRtcValueObject);
  var
    bs:TStream;
    loc:int64;
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;

  bs:=TRtcByteStream(Value).GetByteStream;
  loc:=bs.Position;
  try
    bs.Position:=0;
    FValue.CopyFrom(bs,bs.Size);
    FValue.Position:=loc;
  finally
    bs.Position:=loc;
    end;
  end;

function TRtcByteStream.copyOf: TRtcValueObject;
  begin
  Result:=TRtcByteStream.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcByteStream.from_Code(const s: String; var at: integer);
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;
  code_fromByteStream(GetTypeStr,s,at,FValue);
  end;

procedure TRtcByteStream.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  if assigned(FValue) then
    FValue.Size:=0
  else
    FValue:=rtcByteStream.Create;

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('BASE64',s,at,tags);

    xmlrpc_readByteStream(s,at,FValue);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcByteStream.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toByteStream(GetTypeStr, FValue) );
  end;

procedure TRtcByteStream.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><base64>');
  Result.Add(xmlrpc_writeByteStream(FValue));
  Result.Add('</base64></value>');
  end;

function TRtcByteStream.GetByteStream: TStream;
  begin
  if not assigned(FValue) then
    FValue:=rtcByteStream.Create;
  Result:=FValue;
  end;

function TRtcByteStream.GetString: String;
  var
    loc:int64;
  begin
  if not assigned(FValue) then
    Result:=''
  else
    begin
    SetLength(Result, FValue.Size);
    loc:=FValue.Position;
    try
      FValue.Position:=0;
      FValue.Read(Result[1],length(Result));
    finally
      FValue.Position:=loc;
      end;
    end;
  end;

function TRtcByteStream.GetVariant: Variant;
  begin
  Result:=GetString;
  end;

procedure TRtcByteStream.SetByteStream(const Value: TStream);
  var
    loc:int64;
  begin
  if Value=NullValue then
    begin
    if assigned(FValue) then
      begin
      FValue.Free;
      FValue:=nil;
      end;
    end
  else if (Value<>FValue) then
    begin
    if assigned(FValue) then
      FValue.Size:=0
    else
      FValue:=rtcByteStream.Create;

    loc:=Value.Position;
    Value.Position:=0;
    try
      FValue.CopyFrom(Value,Value.Size);
      FValue.Position:=loc;
    finally
      Value.Position:=loc;
      end;
    end;
  end;

procedure TRtcByteStream.SetNull(const Value: boolean);
  begin
  if Value then
    if assigned(FValue) then
      begin
      FValue.Free;
      FValue:=nil;
      end;
  end;

constructor TRtcByteStream.Create(const Value: TStream);
  begin
  inherited Create;
  FValue:=Value;
  end;

procedure TRtcByteStream.Extracted;
  begin
  // remove pointer to the Byte Stream, so it isn't destroyed with the container
  FValue:=nil;
  // before it is destroyed
  inherited;
  end;

{ TRtcVariableName }

constructor TRtcVariableName.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcVariableName.Destroy;
  begin
  FValue:='';
  inherited;
  end;

function TRtcVariableName.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Variable;
  end;

function TRtcVariableName.GetVarName: String;
  begin
  Result:=FValue;
  end;

function TRtcVariableName.GetString: String;
  begin
  Result:=FValue;
  end;

class function TRtcVariableName.NullValue: String;
  begin
  Result:='';
  end;

procedure TRtcVariableName.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcVariableName.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcVariableName(Value).FValue;
  end;

function TRtcVariableName.copyOf: TRtcValueObject;
  begin
  Result:=TRtcVariableName.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcVariableName.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toLongString(GetTypeStr, FValue) );
  end;

procedure TRtcVariableName.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value>');
  Result.Add(xmlrpc_writeString(FValue));
  Result.Add('</value>');
  end;

procedure TRtcVariableName.from_Code(const s: String; var at:integer);
  begin
  FValue:=code_fromLongString(GetTypeStr, s, at);
  end;

procedure TRtcVariableName.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=xmlrpc_readString(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcVariableName.SetVarName(const Value: String);
  begin
  FValue:=Value;
  end;

function TRtcVariableName.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcVariableName.SetVariant(const Value: Variant): boolean;
  begin
  Result:=False;
  end;

constructor TRtcVariableName.Create(const Value: String);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcBooleanValue }

function TRtcBooleanValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Boolean;
  end;

function TRtcBooleanValue.GetBoolean: boolean;
  begin
  Result:=FValue;
  end;

function TRtcBooleanValue.GetCurrency: Currency;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetInteger: rtcInteger;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetFloat: rtcFloat;
  begin
  if FValue then
    Result:=1
  else
    Result:=0;
  end;

function TRtcBooleanValue.GetString: String;
  begin
  if FValue then
    Result:='T'
  else
    Result:='F';
  end;

class function TRtcBooleanValue.NullValue: boolean;
  begin
  Result:=False;
  end;

procedure TRtcBooleanValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcBooleanValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcBooleanValue(Value).FValue;
  end;

function TRtcBooleanValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcBooleanValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcBooleanValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue then
    Result.Add( code_toShortString(GetTypeStr, 'T') )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcBooleanValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue then
    Result.Add('<value><boolean>1</boolean></value>')
  else
    Result.Add('<value><boolean>0</boolean></value>');
  end;

procedure TRtcBooleanValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=UpperCase(code_fromShortString(GetTypeStr, s, at));
  if (data='') or (data='0') or
     (data='F') or (data='N') or
     (data='FALSE') or (data='NO') then
    FValue:=False
  else
    FValue:=True;
  end;

procedure TRtcBooleanValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('BOOLEAN',s,at,tags);

    data:=UpperCase(xmlrpc_readTrimValue(s,at));
    if (data='') or (data='0') or
       (data='F') or (data='N') or
       (data='FALSE') or (data='NO') then
      FValue:=False
    else
      FValue:=True;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcBooleanValue.SetBoolean(const Value: boolean);
  begin
  FValue:=Value;
  end;

function TRtcBooleanValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcBooleanValue.SetVariant(const Value: Variant): boolean;
  begin
  if TVarData(Value).VType=varBoolean then
    begin
    FValue:=Value;
    Result:=True;
    end
  else
    Result:=False;
  end;

constructor TRtcBooleanValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcBooleanValue.Create(Value: boolean);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcStringValue }

constructor TRtcStringValue.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcStringValue.Destroy;
  begin
  FValue:='';
  inherited;
  end;

function TRtcStringValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_String;
  end;

function TRtcStringValue.GetBoolean: boolean;
  var
    v:string;
  begin
  v:=UpperCase(FValue);
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcStringValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(FValue);
  end;

function TRtcStringValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(FValue);
  end;

function TRtcStringValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt(FValue);
  end;

function TRtcStringValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(FValue);
  end;

function TRtcStringValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(FValue);
  end;

function TRtcStringValue.GetString: String;
  begin
  Result:=FValue;
  end;

function TRtcStringValue.GetText: String;
  begin
  Result:=Utf8ToAnsi(FValue);
  end;

class function TRtcStringValue.NullValue: String;
  begin
  Result:='';
  end;

procedure TRtcStringValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcStringValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcStringValue(Value).FValue;
  end;

function TRtcStringValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcStringValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcStringValue.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toLongString(GetTypeStr, FValue) );
  end;

procedure TRtcStringValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value>');
  Result.Add(xmlrpc_writeString(FValue));
  Result.Add('</value>');
  end;

procedure TRtcStringValue.from_Code(const s: String; var at:integer);
  begin
  FValue:=code_fromLongString(GetTypeStr, s, at);
  end;

procedure TRtcStringValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=xmlrpc_readString(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcStringValue.SetString(const Value: String);
  begin
  FValue:=Value;
  end;

function TRtcStringValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcStringValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString:
      begin
      FValue:=Value;
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

constructor TRtcStringValue.Create(const Value: string);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcWideStringValue }

constructor TRtcWideStringValue.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcWideStringValue.Destroy;
  begin
  FValue:='';
  inherited;
  end;

function TRtcWideStringValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_WideString;
  end;

function TRtcWideStringValue.GetBoolean: boolean;
  var
    v:string;
  begin
  v:=UpperCase(FValue);
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcWideStringValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(FValue);
  end;

function TRtcWideStringValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(FValue);
  end;

function TRtcWideStringValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt(FValue);
  end;

function TRtcWideStringValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(FValue);
  end;

function TRtcWideStringValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(FValue);
  end;

function TRtcWideStringValue.GetString: String;
  begin
  Result:=FValue;
  end;

function TRtcWideStringValue.GetWideString: WideString;
  begin
  Result:=FValue;
  end;

function TRtcWideStringValue.GetText: String;
  begin
  Result:=Utf8ToAnsi(FValue);
  end;

class function TRtcWideStringValue.NullValue: String;
  begin
  Result:='';
  end;

procedure TRtcWideStringValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcWideStringValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcWideStringValue(Value).FValue;
  end;

function TRtcWideStringValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcWideStringValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcWideStringValue.to_Code(const Result:TRtcHugeString);
  var
    str:string;
  begin
  SetLength(str,length(FValue)*SizeOf(WideChar));
  if length(str)>0 then
    Move(FValue[1],str[1],length(str));
  Result.Add( code_toLongString(GetTypeStr, str) );
  end;

procedure TRtcWideStringValue.from_Code(const s: String; var at:integer);
  var
    str:string;
  begin
  str:=code_fromLongString(GetTypeStr, s, at);
  SetLength(FValue,length(str) div SizeOf(WideChar));
  if length(str)>0 then
    Move(str[1],FValue[1],length(str));
  end;

procedure TRtcWideStringValue.to_XMLRPC(const Result:TRtcHugeString);
  var
    str:string;
  begin
  str:=Utf8Encode(FValue);
  Result.Add('<value>');
  Result.Add(xmlrpc_writeString(str));
  Result.Add('</value>');
  str:='';
  end;

procedure TRtcWideStringValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    str:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    str:=xmlrpc_readString(s,at);
    FValue:=Utf8Decode(str);
    str:='';

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcWideStringValue.SetWideString(const Value: WideString);
  begin
  FValue:=Value;
  end;

procedure TRtcWideStringValue.SetString(const Value: String);
  begin
  FValue:=Value;
  end;

function TRtcWideStringValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcWideStringValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString:
      begin
      FValue:=Value;
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

constructor TRtcWideStringValue.Create(const Value: WideString);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcTextValue }

constructor TRtcTextValue.Create;
  begin
  inherited;
  FValue:='';
  end;

destructor TRtcTextValue.Destroy;
  begin
  FValue:='';
  inherited;
  end;

function TRtcTextValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Text;
  end;

function TRtcTextValue.GetBoolean: boolean;
  var
    v:string;
  begin
  v:=UpperCase(FValue);
  if (v='') or (v='0') or
     (v='F') or (v='N') or
     (v='FALSE') or (v='NO') then
    Result:=False
  else
    Result:=True;
  end;

function TRtcTextValue.GetCurrency: Currency;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Curr(GetText);
  end;

function TRtcTextValue.GetDateTime: TDateTime;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2DateTime(GetText);
  end;

function TRtcTextValue.GetInteger: rtcInteger;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt(GetText);
  end;

function TRtcTextValue.GetLargeInt: rtcLargeInt;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=StrToInt64(GetText);
  end;

function TRtcTextValue.GetFloat: rtcFloat;
  begin
  if FValue='' then
    Result:=0
  else
    Result:=Str2Float(GetText);
  end;

function TRtcTextValue.GetString: String;
  begin
  Result:=FValue;
  end;

function TRtcTextValue.GetText: String;
  begin
  Result:=Utf8ToAnsi(FValue);
  end;

procedure TRtcTextValue.SetString(const Value: String);
  begin
  FValue:=Value;
  end;

procedure TRtcTextValue.SetText(const Value: String);
  begin
  FValue:=AnsiToUtf8(Value);
  end;

class function TRtcTextValue.NullValue: String;
  begin
  Result:='';
  end;

procedure TRtcTextValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcTextValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcTextValue(Value).FValue;
  end;

function TRtcTextValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcTextValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcTextValue.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toLongString(GetTypeStr, FValue) );
  end;

procedure TRtcTextValue.from_Code(const s: String; var at:integer);
  begin
  FValue:=code_fromLongString(GetTypeStr, s, at);
  end;

procedure TRtcTextValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRING',s,at,tags);

    FValue:=xmlrpc_readString(s,at);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcTextValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value>');
  Result.Add(xmlrpc_writeString(FValue));
  Result.Add('</value>');
  end;

function TRtcTextValue.GetVariant: Variant;
  begin
  Result:=GetText;
  end;

function TRtcTextValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_0}
    varStrArg,
    {$ENDIF}
    varOleStr,varString:
      begin
      SetText(Value);
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

constructor TRtcTextValue.Create(const Value: String);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcIntegerValue }

function TRtcIntegerValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Integer;
  end;

function TRtcIntegerValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcIntegerValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcIntegerValue.GetString: String;
  begin
  Result:=IntToStr(FValue);
  end;

class function TRtcIntegerValue.NullValue: rtcInteger;
  begin
  Result:=0;
  end;

procedure TRtcIntegerValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcIntegerValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcIntegerValue(Value).FValue;
  end;

function TRtcIntegerValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcIntegerValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcIntegerValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    Result.Add( code_toShortString(GetTypeStr, IntToStr(FValue)) )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcIntegerValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=code_fromShortString(GetTypeStr, s, at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt(data);
  end;

procedure TRtcIntegerValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(IntToStr(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcIntegerValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=StrToInt(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcIntegerValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

function TRtcIntegerValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_0}
  Result:=LongInt(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcIntegerValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64,
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      {$IFDEF IDE_0}
      FValue:=LongInt(Value);
      {$ELSE}
      FValue:=Value;
      {$ENDIF}
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcIntegerValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcIntegerValue.Create(Value: rtcInteger);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcLargeIntValue }

function TRtcLargeIntValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_LargeInt;
  end;

function TRtcLargeIntValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcLargeIntValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetInteger: rtcInteger;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcLargeIntValue.GetString: String;
  begin
  Result:=IntToStr(FValue);
  end;

class function TRtcLargeIntValue.NullValue: rtcInteger;
  begin
  Result:=0;
  end;

procedure TRtcLargeIntValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcLargeIntValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcLargeIntValue(Value).FValue;
  end;

function TRtcLargeIntValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcLargeIntValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcLargeIntValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    Result.Add( code_toShortString(GetTypeStr, IntToStr(FValue)) )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcLargeIntValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=code_fromShortString(GetTypeStr, s, at);
  if data='' then
    FValue:=0
  else
    FValue:=StrToInt64(data);
  end;

procedure TRtcLargeIntValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('INT',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=StrToInt64(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcLargeIntValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><i4>');
  Result.Add(IntToStr(FValue));
  Result.Add('</i4></value>');
  end;

procedure TRtcLargeIntValue.SetInteger(const Value: rtcInteger);
  begin
  FValue:=Value;
  end;

procedure TRtcLargeIntValue.SetLargeInt(const Value: rtcLargeInt);
  begin
  FValue:=Value;
  end;

function TRtcLargeIntValue.GetVariant: Variant;
  begin
  {$IFDEF IDE_1}
  Result:=LongInt(FValue);
  {$ELSE}
  Result:=FValue;
  {$ENDIF}
  end;

function TRtcLargeIntValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    {$IFNDEF IDE_1}
    varLongWord,
    varInt64:
      begin
      FValue:=Value;
      Result:=True;
      end;
    {$ENDIF}

    {$IFNDEF IDE_1}
    varShortInt,
    varWord,
    {$ENDIF}
    varByte,
    varSmallint,
    varInteger:
      begin
      {$IFDEF IDE_1}
      FValue:=LongInt(Value);
      {$ELSE}
      FValue:=Value;
      {$ENDIF}
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcLargeIntValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcLargeIntValue.Create(Value: rtcLargeInt);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcFloatValue }

function TRtcFloatValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Float;
  end;

function TRtcFloatValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcFloatValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetInteger: rtcInteger;
  begin
  Result:=round(FValue);
  end;

function TRtcFloatValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=round(FValue);
  end;

function TRtcFloatValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.GetString: String;
  begin
  Result:=Float2Str(FValue);
  end;

class function TRtcFloatValue.NullValue: rtcFloat;
  begin
  Result:=0;
  end;

procedure TRtcFloatValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcFloatValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcFloatValue(Value).FValue;
  end;

function TRtcFloatValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcFloatValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcFloatValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    Result.Add( code_toShortString(GetTypeStr, Float2Str(FValue)) )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcFloatValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=code_fromShortString(GetTypeStr, s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Float(data);
  end;

procedure TRtcFloatValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    begin
    Result.Add('<value><double>');
    Result.Add(Float2Str(FValue));
    Result.Add('</double></value>');
    end
  else
    Result.Add('<value><double>0</double></value>');
  end;

procedure TRtcFloatValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DOUBLE',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Float(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcFloatValue.SetFloat(const Value: rtcFloat);
  begin
  FValue:=Value;
  end;

function TRtcFloatValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcFloatValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varSingle,
    varDouble:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcFloatValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcFloatValue.Create(Value: rtcFloat);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcCurrencyValue }

function TRtcCurrencyValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Currency;
  end;

function TRtcCurrencyValue.GetBoolean: boolean;
  begin
  Result:= FValue<>0;
  end;

function TRtcCurrencyValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetInteger: rtcInteger;
  begin
  Result:=round(FValue);
  end;

function TRtcCurrencyValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=round(FValue);
  end;

function TRtcCurrencyValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.GetString: String;
  begin
  Result:=Curr2Str(FValue);
  end;

class function TRtcCurrencyValue.NullValue: Currency;
  begin
  Result:=0;
  end;

procedure TRtcCurrencyValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcCurrencyValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcCurrencyValue(Value).FValue;
  end;

function TRtcCurrencyValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcCurrencyValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcCurrencyValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    Result.Add( code_toShortString(GetTypeStr, Curr2Str(FValue)) )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcCurrencyValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=code_fromShortString(GetTypeStr, s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2Curr(data);
  end;

procedure TRtcCurrencyValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    begin
    Result.Add('<value><double>');
    Result.Add(Curr2Str(FValue));
    Result.Add('</double></value>');
    end
  else
    Result.Add('<value><double>0</double></value>');
  end;

procedure TRtcCurrencyValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    data:string;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DOUBLE',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    if data='' then
      FValue:=0
    else
      FValue:=Str2Curr(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcCurrencyValue.SetCurrency(const Value: Currency);
  begin
  FValue:=Value;
  end;

function TRtcCurrencyValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcCurrencyValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varCurrency:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcCurrencyValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcCurrencyValue.Create(Value: Currency);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcDateTimeValue }

function TRtcDateTimeValue.GetType: TRtcValueTypes;
  begin
  Result:=rtc_DateTime;
  end;

function TRtcDateTimeValue.GetBoolean: boolean;
  begin
  Result:=FValue<>0;
  end;

function TRtcDateTimeValue.GetCurrency: Currency;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetDateTime: TDateTime;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetInteger: rtcInteger;
  begin
  Result:=trunc(FValue);
  end;

function TRtcDateTimeValue.GetLargeInt: rtcLargeInt;
  begin
  Result:=trunc(FValue);
  end;

function TRtcDateTimeValue.GetFloat: rtcFloat;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.GetString: String;
  begin
  Result:=DateTime2Str(FValue);
  end;

class function TRtcDateTimeValue.NullValue: TDateTime;
  begin
  Result:=0;
  end;

procedure TRtcDateTimeValue.SetNull(const Value: boolean);
  begin
  if Value then
    FValue:=NullValue;
  end;

procedure TRtcDateTimeValue.CopyFrom(Value: TRtcValueObject);
  begin
  FValue:=TRtcDateTimeValue(Value).FValue;
  end;

function TRtcDateTimeValue.copyOf: TRtcValueObject;
  begin
  Result:=TRtcDateTimeValue.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcDateTimeValue.to_Code(const Result:TRtcHugeString);
  begin
  if FValue<>0 then
    Result.Add( code_toShortString(GetTypeStr, DateTime2Str(FValue)) )
  else
    Result.Add( code_toShortString(GetTypeStr, '') );
  end;

procedure TRtcDateTimeValue.from_Code(const s: String; var at:integer);
  var
    data:String;
  begin
  data:=code_fromShortString(GetTypeStr, s, at);
  if data='' then
    FValue:=0
  else
    FValue:=Str2DateTime(data);
  end;

procedure TRtcDateTimeValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  Result.Add('<value><datetime.iso8601>');
  Result.Add(DateTime2ISOStr(FValue));
  Result.Add('</datetime.iso8601></value>');
  end;

procedure TRtcDateTimeValue.from_XMLrpc(const s: String; var at: integer);
  var
    data:string;
    tags:rtcClosingTagsType;
  begin
  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('DATETIME.ISO8601',s,at,tags);

    data:=xmlrpc_readTrimValue(s,at);
    FValue:=ISOStr2DateTime(data);

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcDateTimeValue.SetDateTime(const Value: TDateTime);
  begin
  FValue:=Value;
  end;

function TRtcDateTimeValue.GetVariant: Variant;
  begin
  Result:=FValue;
  end;

function TRtcDateTimeValue.SetVariant(const Value: Variant): boolean;
  begin
  case TVarData(Value).VType of
    varDate,
    varSingle,
    varDouble:
      begin
      FValue:=Value;
      Result:=True;
      end;
    else
      Result:=False;
    end;
  end;

constructor TRtcDateTimeValue.Create;
  begin
  inherited Create;
  end;

constructor TRtcDateTimeValue.Create(Value: TDateTime);
  begin
  inherited Create;
  FValue:=Value;
  end;

{ TRtcAbsValue }

function TRtcAbsValue.GetNull: boolean;
  begin
  Result:= asObject=nil;
  end;

function TRtcAbsValue.GetBoolean: boolean;
  begin
  if not assigned(asObject) then
    Result:=TRtcBooleanValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetBoolean
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetBoolean
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to Boolean.');
  end;

function TRtcAbsValue.GetCurrency: Currency;
  begin
  if not assigned(asObject) then
    Result:=TRtcCurrencyValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetCurrency
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetCurrency
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to Currency.');
  end;

function TRtcAbsValue.GetDateTime: TDateTime;
  begin
  if not assigned(asObject) then
    Result:=TRtcDateTimeValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetDateTime
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetDateTime
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TDateTime.');
  end;

function TRtcAbsValue.GetException: String;
  begin
  if not assigned(asObject) then
    Result:=TRtcExceptionValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetException
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetException
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsValue.GetVarName: String;
  begin
  if not assigned(asObject) then
    Result:=TRtcVariableName.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetVarName
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetVarName
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsValue.GetInteger: rtcInteger;
  begin
  if not assigned(asObject) then
    Result:=TRtcIntegerValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetInteger
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetInteger
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to Integer.');
  end;

function TRtcAbsValue.GetLargeInt: rtcLargeInt;
  begin
  if not assigned(asObject) then
    Result:=TRtcLargeIntValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetLargeInt
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetLargeInt
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to LargeInt.');
  end;

function TRtcAbsValue.GetFloat: rtcFloat;
  begin
  if not assigned(asObject) then
    Result:=TRtcFloatValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetFloat
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetFloat
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to Float.');
  end;

function TRtcAbsValue.GetString: String;
  begin
  if not assigned(asObject) then
    Result:=TRtcStringValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetString
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetString
  else if asObject is TRtcArray then
    Result:=TRtcArray(asObject).GetAsString
  else if asObject is TRtcRecord then
    Result:=TRtcRecord(asObject).GetAsString
  else if asObject is TRtcDataSet then
    Result:=TRtcDataSet(asObject).GetAsString
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to String.');
  end;

function TRtcAbsValue.GetWideString: WideString;
  begin
  if not assigned(asObject) then
    Result:=TRtcWideStringValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetWideString
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetWideString
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to WideString.');
  end;

function TRtcAbsValue.GetText: String;
  begin
  if not assigned(asObject) then
    Result:=TRtcTextValue.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetText
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetText
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to Text.');
  end;

function TRtcAbsValue.GetByteStream: TStream;
  begin
  if not assigned(asObject) then
    Result:=TRtcByteStream.NullValue
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetByteStream
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetByteStream
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to ByteStream.');
  end;

function TRtcAbsValue.GetArray: TRtcArray;
  begin
  if not assigned(asObject) then
    begin
    if AutoCreate then
      Result:=NewArray
    else
      Result:=TRtcArray.NullValue;
    end
  else if asObject is TRtcArray then
    Result:=TRtcArray(asObject)
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetArray
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcArray.');
  end;

function TRtcAbsValue.GetRecord: TRtcRecord;
  begin
  if not assigned(asObject) then
    begin
    if AutoCreate then
      Result:=NewRecord
    else
      Result:=TRtcRecord.NullValue
    end
  else if asObject is TRtcRecord then
    Result:=TRtcRecord(asObject)
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetRecord
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsValue.GetDataSet: TRtcDataSet;
  begin
  if not assigned(asObject) then
    begin
    if AutoCreate then
      Result:=NewDataSet
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if asObject is TRtcDataSet then
    Result:=TRtcDataSet(asObject)
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetDataSet
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsValue.GetFunctionInfo: TRtcFunctionInfo;
  begin
  if not assigned(asObject) then
    begin
    if AutoCreate then
      Result:=NewFunction
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if asObject is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(asObject)
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetFunctionInfo
  else
    raise EConvertError.Create('Unable to convert '+asObject.ClassName+' to TRtcFunctionInfo.');
  end;

procedure TRtcAbsValue.SetNull(const Value: boolean);
  begin
  if Value then
    SetObject(nil,True);
  end;

procedure TRtcAbsValue.SetArray(const Value: TRtcArray);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetFunctionInfo(const Value: TRtcFunctionInfo);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetDataSet(const Value: TRtcDataSet);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetRecord(const Value: TRtcRecord);
  begin
  SetObject(Value, True);
  end;

procedure TRtcAbsValue.SetException(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetException(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcExceptionValue) then
      begin
      obj:=TRtcExceptionValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcExceptionValue(obj).SetException(Value);
    end;
  end;

procedure TRtcAbsValue.SetVarName(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetVarName(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcVariableName) then
      begin
      obj:=TRtcVariableName.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcVariableName(obj).SetVarName(Value);
    end;
  end;

procedure TRtcAbsValue.SetBoolean(const Value: boolean);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetBoolean(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcBooleanValue) then
      begin
      obj:=TRtcBooleanValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcBooleanValue(obj).SetBoolean(Value);
    end;
  end;

procedure TRtcAbsValue.SetCurrency(const Value: Currency);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetCurrency(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcCurrencyValue) then
      begin
      obj:=TRtcCurrencyValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcCurrencyValue(obj).SetCurrency(Value);
    end;
  end;

procedure TRtcAbsValue.SetDateTime(const Value: TDateTime);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetDateTime(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcDateTimeValue) then
      begin
      obj:=TRtcDateTimeValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcDateTimeValue(obj).SetDateTime(Value);
    end;
  end;

procedure TRtcAbsValue.SetInteger(const Value: rtcInteger);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetInteger(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcIntegerValue) then
      begin
      obj:=TRtcIntegerValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcIntegerValue(obj).SetInteger(Value);
    end;
  end;

procedure TRtcAbsValue.SetLargeInt(const Value: rtcLargeInt);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetLargeInt(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcLargeIntValue) then
      begin
      obj:=TRtcLargeIntValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcLargeIntValue(obj).SetLargeInt(Value);
    end;
  end;

procedure TRtcAbsValue.SetFloat(const Value: rtcFloat);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetFloat(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcFloatValue) then
      begin
      obj:=TRtcFloatValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcFloatValue(obj).SetFloat(Value);
    end;
  end;

procedure TRtcAbsValue.SetString(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetString(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcStringValue) then
      begin
      obj:=TRtcStringValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcStringValue(obj).SetString(Value);
    end;
  end;

procedure TRtcAbsValue.SetWideString(const Value: WideString);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetWideString(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcWideStringValue) then
      begin
      obj:=TRtcWideStringValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcWideStringValue(obj).SetWideString(Value);
    end;
  end;

procedure TRtcAbsValue.SetText(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetText(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcTextValue) then
      begin
      obj:=TRtcTextValue.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcTextValue(obj).SetText(Value);
    end;
  end;

procedure TRtcAbsValue.SetVariant(const Value: Variant);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetVariant(Value)
  else
    begin
    if assigned(obj) then
      if not (obj is TRtcSimpleValue) then
        raise Exception.Create('Value already assigned. Set to NULL before assigning another value.')
      else if TRtcSimpleValue(obj).SetVariant(Value) then
        Exit; // value changed

    obj:=TRtcValueObject.ObjectFromVariant(Value);
    try
      if obj is TRtcAbsArray then
        TRtcAbsArray(obj).AutoCreate:=AutoCreate
      else if obj is TRtcAbsRecord then
        TRtcAbsRecord(obj).AutoCreate:=AutoCreate
      else if obj is TRtcAbsValue then
        TRtcAbsValue(obj).AutoCreate:=AutoCreate;
      asObject:=obj;
    except
      obj.Free;
      raise;
      end;
    end;
  end;

procedure TRtcAbsValue.SetByteStream(const Value: TStream);
  var
    obj:TRtcValueObject;
  begin
  obj:=asObject;
  if assigned(obj) and (obj is TRtcValue) then
    TRtcValue(obj).SetByteStream(Value)
  else
    begin
    if not assigned(obj) or not (obj is TRtcByteStream) then
      begin
      obj:=TRtcByteStream.Create;
      try
        SetObject(obj);
      except
        obj.Free;
        raise;
        end;
      end;
    TRtcByteStream(obj).SetByteStream(Value);
    end;
  end;

procedure TRtcAbsValue.SetAsObject(const Value: TRtcValueObject);
  begin
  SetObject(Value);
  end;

function TRtcAbsValue.GetVariant: Variant;
  begin
  if not assigned(asObject) then
    Result:=Null
  else if asObject is TRtcSimpleValue then
    Result:=TRtcSimpleValue(asObject).GetVariant
  else if asObject is TRtcValue then
    Result:=TRtcValue(asObject).GetVariant
  else
    raise EConvertError.Create('Can not convert '+asObject.ClassName+' to Variant.');
  end;

function TRtcAbsValue.NewArray: TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsValue.NewRecord: TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsValue.NewDataSet: TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsValue.NewFunction(const func_name:String=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsValue.NewBoolean: boolean;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcBooleanValue) then
    begin
    obj:=TRtcBooleanValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcBooleanValue(asObject).SetNull(True);
  Result:=TRtcBooleanValue(asObject).GetBoolean;
  end;

function TRtcAbsValue.NewCurrency: Currency;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcCurrencyValue) then
    begin
    obj:=TRtcCurrencyValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcCurrencyValue(asObject).SetNull(True);
  Result:=TRtcCurrencyValue(asObject).GetCurrency;
  end;

function TRtcAbsValue.NewDateTime: TDateTime;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcDateTimeValue) then
    begin
    obj:=TRtcDateTimeValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcDateTimeValue(asObject).SetNull(True);
  Result:=TRtcDateTimeValue(asObject).GetDateTime;
  end;

function TRtcAbsValue.NewException: String;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcExceptionValue) then
    begin
    obj:=TRtcExceptionValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcExceptionValue(asObject).SetNull(True);
  Result:=TRtcExceptionValue(asObject).GetException;
  end;

function TRtcAbsValue.NewVariable: String;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcVariableName) then
    begin
    obj:=TRtcVariableName.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcVariableName(asObject).SetNull(True);
  Result:=TRtcVariableName(asObject).GetVarName;
  end;

function TRtcAbsValue.NewInteger: rtcInteger;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcIntegerValue) then
    begin
    obj:=TRtcIntegerValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcIntegerValue(asObject).SetNull(True);
  Result:=TRtcIntegerValue(asObject).GetInteger;
  end;

function TRtcAbsValue.NewLargeInt: rtcLargeInt;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcLargeIntValue) then
    begin
    obj:=TRtcLargeIntValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcLargeIntValue(asObject).SetNull(True);
  Result:=TRtcLargeIntValue(asObject).GetLargeInt;
  end;

function TRtcAbsValue.NewFloat: rtcFloat;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcFloatValue) then
    begin
    obj:=TRtcFloatValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcFloatValue(asObject).SetNull(True);
  Result:=TRtcFloatValue(asObject).GetFloat;
  end;

function TRtcAbsValue.NewString: String;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcStringValue) then
    begin
    obj:=TRtcStringValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcStringValue(asObject).SetNull(True);
  Result:=TRtcStringValue(asObject).GetString;
  end;

function TRtcAbsValue.NewWideString: WideString;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcWideStringValue) then
    begin
    obj:=TRtcWideStringValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcWideStringValue(asObject).SetNull(True);
  Result:=TRtcWideStringValue(asObject).GetWideString;
  end;

function TRtcAbsValue.NewText: String;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcTextValue) then
    begin
    obj:=TRtcTextValue.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcTextValue(asObject).SetNull(True);
  Result:=TRtcTextValue(asObject).GetText;
  end;

function TRtcAbsValue.NewByteStream: TStream;
  var
    obj:TRtcValueObject;
  begin
  if not assigned(asObject) or not (asObject is TRtcByteStream) then
    begin
    obj:=TRtcByteStream.Create;
    try
      SetObject(obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcByteStream(asObject).SetNull(True);
  Result:=TRtcByteStream(asObject).GetByteStream;
  end;

procedure TRtcAbsValue.SetCode(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject) and not (asObject is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned. Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

function TRtcAbsValue.GetCode: String;
  begin
  if not assigned(asObject) then
    Result:=nullValueCode
  else
    Result:=asObject.toCode;
  end;

procedure TRtcAbsValue.SetXMLrpc(const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject) and not (asObject is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned. Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

function TRtcAbsValue.GetXMLrpc: String;
  begin
  if not assigned(asObject) then
    Result:=nullValueXMLrpc
  else
    Result:=asObject.toXMLrpc;
  end;

{ TRtcValue }

constructor TRtcValue.Create;
  begin
  inherited;
  FValue:=nil;
  end;

destructor TRtcValue.Destroy;
  begin
  Clear;
  inherited;
  end;

procedure TRtcValue.Clear;
  begin
  SetNull(True);
  end;

function TRtcValue.GetType: TRtcValueTypes;
  begin
  if not assigned(FValue) then
    Result:=rtc_Null
  else
    Result:=FValue.GetType;
  end;

function TRtcValue.GetObject:TRtcValueObject;
  begin
  Result:=FValue;
  end;

procedure TRtcValue.SetObject(const _Value: TRtcValueObject; asCopy:boolean=False);
  var
    old:TRtcValueObject;
  begin
  if FValue<>_Value then
    begin
    if assigned(FValue) then
      begin
      if _Value<>nil then
        begin
        if FValue is TRtcSimpleValue then
          begin
          old:=FValue;
          if asCopy then
            FValue:=_Value.CopyOf
          else
            FValue:=_Value;
          old.Free;
          end
        else
          raise Exception.Create('Value of type '+FValue.ClassName+' allready assigned.'#13#10+
                                 'Set to NULL before assigning a different object.');
        end
      else
        begin
        {if FValue is TRtcSimpleValue then
          FValue.Free
        else} if asCopy then
          FValue.Free;
        FValue:=nil;
        end;
      end
    else
      begin
      if asCopy then
        FValue:=_Value.copyOf
      else
        FValue:=_Value;
      end;
    end;
  end;

procedure TRtcValue.CopyFrom(_Value: TRtcValueObject);
  begin
  if not isNull then
    raise Exception.Create('Can not merge objects. This Value Object already has data.');
  SetObject(TRtcValue(_Value).asObject, True);
  end;

function TRtcValue.copyOf: TRtcValueObject;
  begin
  if assigned(FValue) then
    Result:=FValue.copyOf
  else
    Result:=nil;
  end;

procedure TRtcValue.to_Code(const Result:TRtcHugeString);
  begin
  if not assigned(FValue) then
    Result.Add(nullValueCode)
  else
    FValue.to_Code(Result);
  end;

procedure TRtcValue.from_Code(const s: String; var at:integer);
  begin
  if assigned(FValue) then
    raise Exception.Create('Can not merge data. TRtcValue object is already in use.');

  FValue:=ObjectFromCode(s,at);
  end;

procedure TRtcValue.to_XMLRPC(const Result:TRtcHugeString);
  begin
  if not assigned(FValue) then
    Result.Add(nullValueXMLrpc)
  else
    FValue.to_XMLRPC(Result);
  end;

procedure TRtcValue.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    xname, xtag:string;
    xval:TRtcValueObject;
    xrec:TRtcRecord;
    xarr:TRtcArray;
    idx:integer;
    have_params,
    have_name,
    inside_param,
    have_param:boolean;
  begin
  if assigned(FValue) then
    raise Exception.Create('Can not merge data. TRtcValue object is already in use.');

  if xmlrpc_checkStrType(s,at)=rtc_Variant then
    begin
    xval:=nil;

    SetLength(tags,0);
    try
      xmlrpc_skipValueOpen('METHODRESPONSE',s,at,tags);
      xmlrpc_skipWhitespace(s,at);

      xtag:=UpperCase(xmlrpc_checkTag(s,at));
      if (xtag='PARAMS/') then
        begin
        xmlrpc_skipTag(s,at); // <PARAMS>
        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        end
      else if (xtag='PARAMS') or (xtag='PARAM') then // we could have parameters
        begin
        if xtag='PARAMS' then
          begin
          have_params:=True;
          xmlrpc_skipTag(s,at); // <PARAMS>
          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          end
        else
          have_params:=False;

        if xtag='/PARAMS' then // no parameters (empty array)
          begin
          // newArray(RTC_XMLRPC_ParamsAsArrayName)
          end
        else if (xtag='PARAM') or (xtag='NAME') then // we have a parameter!
          begin
          if xtag='PARAM' then
            begin
            xmlrpc_skipTag(s,at); // <PARAM>
            inside_param:=True;
            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            end
          else
            inside_param:=False;

          if xtag='/PARAM' then
            begin
            xval:=nil;
            xmlrpc_skipTag(s,at); // </PARAM>
            end
          else if xtag='NAME' then // receiving named parameter list
            begin
            have_param:=True;
            have_name:=False;

            xrec:=TRtcRecord.Create;
            repeat
              xmlrpc_readTag(s,at,'NAME');
              xname:=xmlrpc_readTrimValue(s,at);
              xmlrpc_readTag(s,at,'/NAME');

              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              if xtag<>'/PARAM' then
                begin
                xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
                xrec.asObject[xname]:=xval;
                xval:=nil;
                end;

              if inside_param then
                begin
                xtag:=UpperCase(xmlrpc_checkTag(s,at));
                if (xtag='/PARAM') then
                  begin
                  inside_param:=False;
                  have_param:=False;
                  xmlrpc_skipTag(s,at);
                  end;
                end;

              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              if xtag='NAME' then
                begin
                if have_param then
                  have_name:=True
                else
                  Break;
                end
              else if xtag='PARAM' then
                begin
                if not have_param and not have_name then
                  begin
                  inside_param:=True;
                  have_param:=True;
                  xmlrpc_skipTag(s,at);
                  end
                else
                  Break;
                end
              else
                Break;
              until false;
            xval:=xrec;
            end
          else
            begin
            // read all data stored in this parameter
            xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
            if inside_param then xmlrpc_readTag(s,at,'/PARAM');
            end;

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          if (xtag='/PARAMS') or (xtag='/METHODRESPONSE') then // A single <PARAM>
            begin
            // a single parameter
            FValue:=xval;
            xval:=nil;
            end
          else if xtag='PARAM' then // More than one <PARAM> - it's an array!
            begin
            xarr:=TRtcArray.Create;
            if assigned(xval) then
              begin
              xarr.asObject[0]:=xval;
              xval:=nil;
              end;

            xval:=xarr;

            idx:=1;
            repeat
              xmlrpc_skipTag(s,at); // <PARAM>

              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              if xtag<>'/PARAM' then
                begin
                // read all data stored in this parameter
                xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
                xmlrpc_readTag(s,at,'/PARAM');
                end
              else
                begin
                xval:=nil;
                xmlrpc_skipTag(s,at); // </PARAM>
                end;

              Inc(idx);
              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              until xtag<>'PARAM';

            FValue:=xval;
            xval:=nil;
            end;
          end
        else if have_params then
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          xtag:=UpperCase(xmlrpc_checkTag(s,at));

          if xtag='/PARAMS' then // A single <PARAM>
            begin
            // Our data is in xval
            FValue:=xval;
            xval:=nil;
            end
          else // More than one parameter - it's an array!
            begin
            xarr:=TRtcArray.Create;
            if assigned(xval) then
              begin
              xarr.asObject[0]:=xval;
              xval:=nil;
              end;
            xval:=xarr;

            idx:=1;
            repeat
              // read all data stored in this parameter
              xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
              Inc(idx);
              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              until xtag='/PARAMS';

            FValue:=xval;
            xval:=nil;
            end;
          end;

        if have_params then
          xmlrpc_readTag(s,at,'/PARAMS');
        end
      else if xtag<>'/METHODRESPONSE' then // receiving data without <PARAMS><PARAM>
        begin
        if xtag='NAME' then // receiving named parameter list
          begin
          xrec:=TRtcRecord.Create;
          xval:=xrec;
          repeat
            xmlrpc_readTag(s,at,'NAME');
            xname:=xmlrpc_readTrimValue(s,at);
            xmlrpc_readTag(s,at,'/NAME');

            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            if xtag<>'/METHODRESPONSE' then
              begin
              try
                xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
                xrec.asObject[xname]:=xval;
              finally
                xval:=xrec;
                end;
              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              if xtag='/METHODRESPONSE' then
                Break;
              end
            else
              Break;
            until false;
          end
        else
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          end;

        if xtag='/METHODRESPONSE' then // A single <PARAM>
          begin
          // Our data is in xval
          FValue:=xval;
          xval:=nil;
          end
        else // More than one parameter - it's an array!
          begin
          xarr:=TRtcArray.Create;
          if assigned(xval) then
            begin
            xarr.asObject[0]:=xval;
            xval:=nil;
            end;
          xval:=xarr;

          idx:=1;
          repeat
            // read all data stored in this parameter
            xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
            Inc(idx);
            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            until xtag='/METHODRESPONSE';

          FValue:=xval;
          xval:=nil;
          end;
        end;

      xmlrpc_skipValueClose(s,at,tags);
    finally
      SetLength(tags,0);
      if assigned(xval) then xval.Free;
      end;
    end
  else
    FValue:=ObjectFromXMLrpc(s,at);
  end;

class function TRtcValue.FromCode(const data: String; var at:integer): TRtcValue;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcValue.Create;
  try
    Result.from_Code(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcValue.FromCode(const data: String): TRtcValue;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcValue.FromXMLrpc(const data: String; var at: integer): TRtcValue;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcValue.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcValue.FromXMLrpc(const data: String): TRtcValue;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcValue.Extracted;
  begin
  if assigned(FValue) then
    begin
    FValue.Extracted;
    FValue:=nil;
    end;
  Free;
  end;

procedure TRtcValue.Extract;
  begin
  if assigned(FValue) then
    begin
    FValue.Extracted;
    FValue:=nil;
    end;
  end;

{ TRtcAbsRecord }

function TRtcAbsRecord.GetNull(const index: String): boolean;
  begin
  Result:= GetObject(index)=nil;
  end;

function TRtcAbsRecord.GetArray(const index: String): TRtcArray;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewArray(index)
    else
      Result:=TRtcArray.NullValue;
    end
  else if obj is TRtcArray then
    Result:=TRtcArray(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetArray
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcArray.');
  end;

function TRtcAbsRecord.GetRecord(const index: String): TRtcRecord;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewRecord(index)
    else
      Result:=TRtcRecord.NullValue;
    end
  else if obj is TRtcRecord then
    Result:=TRtcRecord(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetRecord
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsRecord.GetDataSet(const index: String): TRtcDataSet;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewDataSet(index)
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if obj is TRtcDataSet then
    Result:=TRtcDataSet(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetDataSet
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsRecord.GetFunctionInfo(const index: String): TRtcFunctionInfo;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewFunction(index)
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if obj is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetFunctionInfo
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcFunctionInfo.');
  end;

function TRtcAbsRecord.GetBoolean(const index: String): boolean;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcBooleanValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetBoolean
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetBoolean
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Boolean.');
  end;

function TRtcAbsRecord.GetCurrency(const index: String): Currency;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcCurrencyValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetCurrency
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetCurrency
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Currency.');
  end;

function TRtcAbsRecord.GetDateTime(const index: String): TDateTime;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcDateTimeValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetDateTime
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetDateTime
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TDateTime.');
  end;

function TRtcAbsRecord.GetException(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcExceptionValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetException
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetException
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsRecord.GetVarName(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcVariableName.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetVarName
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetVarName
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsRecord.GetInteger(const index: String): rtcInteger;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcIntegerValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetInteger
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetInteger
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Integer.');
  end;

function TRtcAbsRecord.GetLargeInt(const index: String): rtcLargeInt;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcLargeIntValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetLargeInt
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetLargeInt
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to LargeInt.');
  end;

function TRtcAbsRecord.GetFloat(const index: String): rtcFloat;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcFloatValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetFloat
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetFloat
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Float.');
  end;

function TRtcAbsRecord.GetString(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcStringValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetString
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetString
  else if obj is TRtcArray then
    Result:=TRtcArray(obj).GetAsString
  else if obj is TRtcRecord then
    Result:=TRtcRecord(obj).GetAsString
  else if obj is TRtcDataSet then
    Result:=TRtcDataSet(obj).GetAsString
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to String.');
  end;

function TRtcAbsRecord.GetWideString(const index: String): WideString;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcWideStringValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetWideString
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetWideString
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to WideString.');
  end;

function TRtcAbsRecord.GetText(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcTextValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetText
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetText
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Text.');
  end;

function TRtcAbsRecord.GetVariant(const index: String): Variant;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=Null
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetVariant
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetVariant
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Variant.');
  end;

function TRtcAbsRecord.GetByteStream(const index: String): TStream;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcByteStream.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetByteStream
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetByteStream
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to ByteStream.');
  end;

function TRtcAbsRecord.GetValueType(const index: String): TRtcValueTypes;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=rtc_Null
  else
    Result:=obj.GetType;
  end;

procedure TRtcAbsRecord.SetNull(const index: String; const Value: boolean);
  begin
  if Value then
    SetObject(index, nil, True);
  end;

procedure TRtcAbsRecord.SetArray(const index: String; const Value: TRtcArray);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetRecord(const index: String; const Value: TRtcRecord);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetDataSet(const index: String; const Value: TRtcDataSet);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetFunctionInfo(const index: String; const Value: TRtcFunctionInfo);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsRecord.SetBoolean(const index: String; const Value: boolean);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcBooleanValue) then
    begin
    obj:=TRtcBooleanValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcBooleanValue(obj).SetBoolean(Value);
  end;

procedure TRtcAbsRecord.SetCurrency(const index: String; const Value: Currency);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcCurrencyValue) then
    begin
    obj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcCurrencyValue(obj).SetCurrency(Value);
  end;

procedure TRtcAbsRecord.SetDateTime(const index: String; const Value: TDateTime);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcDateTimeValue) then
    begin
    obj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcDateTimeValue(obj).SetDateTime(Value);
  end;

procedure TRtcAbsRecord.SetException(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcExceptionValue) then
    begin
    obj:=TRtcExceptionValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcExceptionValue(obj).SetException(Value);
  end;

procedure TRtcAbsRecord.SetVarName(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcVariableName) then
    begin
    obj:=TRtcVariableName.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcVariableName(obj).SetVarName(Value);
  end;

procedure TRtcAbsRecord.SetInteger(const index: String; const Value: rtcInteger);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcIntegerValue) then
    begin
    obj:=TRtcIntegerValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcIntegerValue(obj).SetInteger(Value);
  end;

procedure TRtcAbsRecord.SetLargeInt(const index: String; const Value: rtcLargeInt);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcLargeIntValue) then
    begin
    obj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcLargeIntValue(obj).SetLargeInt(Value);
  end;

procedure TRtcAbsRecord.SetFloat(const index: String; const Value: rtcFloat);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcFloatValue) then
    begin
    obj:=TRtcFloatValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcFloatValue(obj).SetFloat(Value);
  end;

procedure TRtcAbsRecord.SetString(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcStringValue) then
    begin
    obj:=TRtcStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcStringValue(obj).SetString(Value);
  end;

procedure TRtcAbsRecord.SetWideString(const index: String; const Value: WideString);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcWideStringValue) then
    begin
    obj:=TRtcWideStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcWideStringValue(obj).SetWideString(Value);
  end;

procedure TRtcAbsRecord.SetText(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcTextValue) then
    begin
    obj:=TRtcTextValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcTextValue(obj).SetText(Value);
  end;

procedure TRtcAbsRecord.SetByteStream(const index: String; const Value: TStream);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcByteStream) then
    begin
    obj:=TRtcByteStream.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcByteStream(obj).SetByteStream(Value);
  end;

procedure TRtcAbsRecord.SetVariant(const index: String; const Value: Variant);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if assigned(obj) then
    if not (obj is TRtcSimpleValue) then
      raise Exception.Create('Value already assigned. Set to NULL before assigning another value.')
    else if TRtcSimpleValue(obj).SetVariant(Value) then
      Exit; // value changed

  obj:=TRtcValueObject.ObjectFromVariant(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    SetObject(index,obj);
  except
    obj.Free;
    raise;
    end;
  end;

procedure TRtcAbsRecord.SetAsObject(const index: String; Value: TRtcValueObject);
  begin
  SetObject(index, Value);
  end;

function TRtcAbsRecord.NewArray(const index: String): TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsRecord.NewRecord(const index: String): TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsRecord.NewDataSet(const index: String): TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsRecord.NewFunction(const index: String; const func_name:String=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsRecord.NewBoolean(const index: String): boolean;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcBooleanValue) then
    begin
    obj:=TRtcBooleanValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcBooleanValue(obj).SetNull(True);
  Result:=TRtcBooleanValue(obj).GetBoolean;
  end;

function TRtcAbsRecord.NewCurrency(const index: String): Currency;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcCurrencyValue) then
    begin
    obj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcCurrencyValue(obj).SetNull(True);
  Result:=TRtcCurrencyValue(obj).GetCurrency;
  end;

function TRtcAbsRecord.NewDateTime(const index: String): TDateTime;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcDateTimeValue) then
    begin
    obj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcDateTimeValue(obj).SetNull(True);
  Result:=TRtcDateTimeValue(obj).GetDateTime;
  end;

function TRtcAbsRecord.NewException(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcExceptionValue) then
    begin
    obj:=TRtcExceptionValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcExceptionValue(obj).SetNull(True);
  Result:=TRtcExceptionValue(obj).GetException;
  end;

function TRtcAbsRecord.NewVariable(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcVariableName) then
    begin
    obj:=TRtcVariableName.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcVariableName(obj).SetNull(True);
  Result:=TRtcVariableName(obj).GetVarName;
  end;

function TRtcAbsRecord.NewInteger(const index: String): rtcInteger;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcIntegerValue) then
    begin
    obj:=TRtcIntegerValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcIntegerValue(obj).SetNull(True);
  Result:=TRtcIntegerValue(obj).GetInteger;
  end;

function TRtcAbsRecord.NewLargeInt(const index: String): rtcLargeInt;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcLargeIntValue) then
    begin
    obj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcLargeIntValue(obj).SetNull(True);
  Result:=TRtcLargeIntValue(obj).GetLargeInt;
  end;

function TRtcAbsRecord.NewFloat(const index: String): rtcFloat;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcFloatValue) then
    begin
    obj:=TRtcFloatValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcFloatValue(obj).SetNull(True);
  Result:=TRtcFloatValue(obj).GetFloat;
  end;

function TRtcAbsRecord.NewString(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcStringValue) then
    begin
    obj:=TRtcStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcStringValue(obj).SetNull(True);
  Result:=TRtcStringValue(obj).GetString;
  end;

function TRtcAbsRecord.NewWideString(const index: String): WideString;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcWideStringValue) then
    begin
    obj:=TRtcWideStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcWideStringValue(obj).SetNull(True);
  Result:=TRtcWideStringValue(obj).GetWideString;
  end;

function TRtcAbsRecord.NewText(const index: String): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcTextValue) then
    begin
    obj:=TRtcTextValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcTextValue(obj).SetNull(True);
  Result:=TRtcTextValue(obj).GetText;
  end;

function TRtcAbsRecord.NewByteStream(const index: String): TStream;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcByteStream) then
    begin
    obj:=TRtcByteStream.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcByteStream(obj).SetNull(True);
  Result:=TRtcByteStream(obj).GetByteStream;
  end;

function TRtcAbsRecord.GetCode(const index: String): String;
  begin
  if not assigned(asObject[index]) then
    Result:=nullValueCode
  else
    Result:=asObject[index].toCode;
  end;

procedure TRtcAbsRecord.SetCode(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject[index]) and not (asObject[index] is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned to field "'+index+'". Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject[index]:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

function TRtcAbsRecord.GetXMLrpc(const index: String): String;
  begin
  if not assigned(asObject[index]) then
    Result:=nullValueXMLrpc
  else
    Result:=asObject[index].toXMLrpc;
  end;

procedure TRtcAbsRecord.SetXMLrpc(const index: String; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject[index]) and not (asObject[index] is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned to field "'+index+'". Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject[index]:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

{ TRtcRecord }

constructor TRtcRecord.Create;
  begin
  inherited;
  FValues:=nil;
  end;

destructor TRtcRecord.Destroy;
  begin
  Clear;
  inherited;
  end;

procedure TRtcRecord.Clear;
  begin
  if assigned(FValues) then
    begin
    FValues.DestroyObjects;
    FValues.Free;
    FValues:=nil;
    end;
  end;

function TRtcRecord.GetFieldCount: integer;
  begin
  if assigned(FValues) then
    Result:=FValues.Count
  else
    Result:=0;
  end;

function TRtcRecord.Count: integer;
  begin
  Result:=GetFieldCount;
  end;

function TRtcRecord.GetFieldName(index: integer): String;
  begin
  if not assigned(FValues) then
    Result:=''
  else if (index>=0) and (index<FValues.Count) then
    Result:=FValues.Strings[index]
  else
    Result:='';
  end;

procedure TRtcRecord.SetFieldName(index: integer; const _Value: String);
  var
    idx:integer;
  begin
  if not assigned(FValues) then
    FValues:=tRtcFastStrObjList.Create;

  idx:=FValues.Find(_Value);
  if (idx>=0) then // already found.
    begin
    if (idx<>index) then
      raise Exception.Create('Field with name "'+_Value+'" already exists.');
    end
  else if (index>=0) and (index<FValues.Count) then
    FValues.Strings[index]:=_Value
  else
    FValues.Add(_Value);
  end;

function TRtcRecord.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Record;
  end;

function TRtcRecord.GetObject(const index: String): TRtcValueObject;
  var
    idx:integer;
  begin
  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      Result:=TRtcValueObject(FValues.Objects[idx])
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcRecord.SetObject(const index: String; _Value: TRtcValueObject; asCopy:boolean=False);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  if index='' then
    raise Exception.Create('TRtcRecord.SetObject: Fields without a name not allowed.');

  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      begin
      obj:=TRtcValueObject(FValues.Objects[idx]);
      if obj<>_Value then
        begin
        if _Value<>nil then
          begin
          if not assigned(obj) or (obj is TRtcSimpleValue) then
            begin
            if asCopy then
              FValues.Objects[idx]:=_Value.copyOf
            else
              FValues.Objects[idx]:=_Value;
            if assigned(obj) then
              obj.Free;
            end
          else if obj is TRtcValue then
            TRtcValue(obj).SetObject(_Value, asCopy)
          else
            raise Exception.Create('Value of type '+obj.ClassName+' allready assigned to "'+index+'".'#13#10+
                                   'Set ['+index+'] to NULL before assigning a different object.')
          end
        else
          begin
          if assigned(obj) and asCopy then
            obj.Free;
          FValues.Objects[idx]:=nil; // do not call Delete(idx), or other fields Index positions would change
          end;
        end;
      end
    else if _Value<>nil then
      begin
      if asCopy then
        FValues.Add(index, _Value.copyOf)
      else
        FValues.Add(index, _Value);
      end;
    end
  else if _Value<>nil then
    begin
    FValues:=tRtcFastStrObjList.Create;
    if asCopy then
      FValues.Add(index, _Value.copyOf)
    else
      FValues.Add(index, _Value);
    end;
  end;

class function TRtcRecord.NullValue: TRtcRecord;
  begin
  Result:=nil;
  end;

procedure TRtcRecord.CopyFrom(_Value: TRtcValueObject);
  var
    idx:integer;
    mylist:tRtcFastStrObjList;
    obj:TRtcValueObject;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge objects. This Record already has data.');

  if assigned(TRtcRecord(_Value).FValues) then
    begin
    mylist:=tRtcFastStrObjList.Create;
    try
      with TRtcRecord(_Value).FValues do
        for idx:=0 to Count-1 do
          begin
          obj:=TRtcValueObject(Objects[idx]);
          if assigned(obj) then
            obj:=obj.copyOf;
          mylist.Add(Strings[idx], obj);
          end;
    except
      mylist.Free;
      raise;
      end;
    end
  else
    mylist:=nil;

  FValues:=mylist;
  end;

function TRtcRecord.copyOf: TRtcValueObject;
  begin
  Result:=TRtcRecord.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcRecord.from_Code(const s: String; var at:integer);
  var
    fname,
    data:String;
    idx,cnt:integer;
    obj:TObject;
    val:tRtcFastStrObjList;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Record data. TRtcRecord object is already in use.');

  data:=code_fromShortString(TypeToStr(rtc_Record),s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=StrToInt(data);
  except
    raise Exception.Create('TRtcRecord.from_Code: Field Count missing.');
    end;

  val:=tRtcFastStrObjList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      fname:=code_fromNameString(s,at);
      obj:=ObjectFromCode(s,at);
      val.Add(fname, obj);
      end;
  except
    val.DestroyObjects;
    val.Free;
    raise;
    end;

  if assigned(FValues) then
    begin
    FValues.DestroyObjects;
    FValues.Free;
    end;

  FValues:=val;
  end;

procedure TRtcRecord.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  Result.Add( code_toShortString(TypeToStr(rtc_Record),IntToStr(GetFieldCount)) );
  for idx:=0 to GetFieldCount-1 do
    begin
    {$IFDEF RtcUpperCaseFieldNames}
      Result.Add(code_toNameString(UpperCase(FieldName[idx])));
    {$ELSE}
      Result.Add(code_toNameString(FieldName[idx]));
    {$ENDIF}
    obj:=TRtcValueObject(FValues.Objects[idx]);
    if assigned(obj) then
      obj.to_Code(Result)
    else
      Result.Add(nullValueCode);
    end;
  end;

class function TRtcRecord.FromCode(const data: String; var at:integer): TRtcRecord;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcRecord.Create;
  try
    Result.from_Code(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcRecord.FromCode(const data: String): TRtcRecord;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcRecord.FromXMLrpc(const data: String; var at: integer): TRtcRecord;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcRecord.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcRecord.FromXMLrpc(const data: String): TRtcRecord;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLRPC(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcRecord.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  Result.Add('<value><struct>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    Result.Add('<member><name>');
    Result.Add(FieldName[idx]);
    Result.Add('</name>');
    obj:=TRtcValueObject(FValues.Objects[idx]);
    if assigned(obj) then
      obj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add('</member>'#13#10);
    end;
  Result.Add('</struct></value>');
  end;

function TRtcRecord.GetAsString: String;
  var
    res:TRtcHugeString;
    idx:integer;
    b:boolean;
    fname:string;
  begin
  res:=TRtcHugeString.Create;
  try
    b:=False;
    Res.Add('(');
    for idx:=0 to Count-1 do
      begin
      fname:=FieldName[idx];
      if not isNull[fname] then
        begin
        if b then Res.Add('; ') else b:=True;
        Res.Add(fname+'='+asString[fname]);
        end;
      end;
    Res.Add(')');
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

procedure TRtcRecord.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    xname,xtag:string;
    xval:TRtcValueObject;
    val:tRtcFastStrObjList;
    c_tag:string;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Record data. TRtcRecord object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    val:=tRtcFastStrObjList.Create;
    try
      xtag:=UpperCase(xmlrpc_checkTag(s,at));
      if xtag='MEMBER' then // <member><name>..</name><value>...</value></member>
        begin
        repeat
          xmlrpc_skipTag(s,at); // <member>

          xmlrpc_readTag(s,at,'NAME');
          xname:=xmlrpc_readTrimValue(s,at);
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          if xtag<>'/MEMBER' then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            val.Add(xname, xval);
            end;
          xmlrpc_readTag(s,at,'/MEMBER');

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          until xtag<>'MEMBER';
        end
      else if xtag='NAME' then // <name>..</name><value>...</value>
        begin
        repeat
          xmlrpc_readTag(s,at,'NAME');
          xname:=xmlrpc_readTrimValue(s,at);
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          if (xtag<>'NAME') and (xtag<>c_tag) then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            val.Add(xname,xval);
            end;
          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          until xtag<>'NAME';
        end;
    except
      val.DestroyObjects;
      val.Free;
      raise;
      end;

    FValues:=val;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcRecord.Extract(const index: String);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  if index='' then
    raise Exception.Create('TRtcRecord.Extract: Fields without a name not allowed.');

  if assigned(FValues) then
    begin
    idx:=FValues.Find(index);
    if idx>=0 then
      begin
      obj:=TRtcValueObject(FValues.Objects[idx]);
      if assigned(obj) then
        begin
        obj.Extracted;
        FValues.Objects[idx]:=nil; // do not call Delete(idx), or other fields Index positions would change
        end;
      end;
    end;
  end;

{ TRtcFunctionInfo }

constructor TRtcFunctionInfo.Create;
  begin
  inherited;
  FunctionName:='';
  end;

destructor TRtcFunctionInfo.Destroy;
  begin
  Clear;
  inherited;
  end;

procedure TRtcFunctionInfo.Clear;
  begin
  inherited;
  FunctionName:='';
  end;

function TRtcFunctionInfo.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Function;
  end;

class function TRtcFunctionInfo.NullValue: TRtcFunctionInfo;
  begin
  Result:=nil;
  end;

procedure TRtcFunctionInfo.CopyFrom(_Value: TRtcValueObject);
  begin
  inherited CopyFrom(_Value);
  FunctionName:=TRtcFunctionInfo(_Value).FunctionName;
  end;

function TRtcFunctionInfo.copyOf: TRtcValueObject;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcFunctionInfo.to_Code(const Result:TRtcHugeString);
  begin
  Result.Add( code_toShortString(TypeToStr(rtc_Function), FunctionName));
  inherited to_Code(Result);
  end;

procedure TRtcFunctionInfo.from_Code(const s: String; var at:integer);
  var
    fname:String;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Function Calls. TRtcFunctionInfo is already in use.');

  fname:=code_fromShortString(TypeToStr(rtc_Function), s, at);
  inherited from_Code(s,at);
  FunctionName:=fname;
  end;

class function TRtcFunctionInfo.FromCode(const data: String; var at:integer): TRtcFunctionInfo;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcFunctionInfo.Create;
  try
    Result.from_Code(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcFunctionInfo.FromCode(const data: String): TRtcFunctionInfo;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcFunctionInfo.FromXMLrpc(const data: String; var at: integer): TRtcFunctionInfo;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcFunctionInfo.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcFunctionInfo.FromXMLrpc(const data: String): TRtcFunctionInfo;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcFunctionInfo.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    arr:TRtcArray;
    obj:TRtcValueObject;
  begin
  if GetFieldCount=0 then // no parameters
    begin
    Result.Add('<methodCall><methodName>');
    Result.Add(xmlrpc_writeString(FunctionName));
    Result.Add('</methodName></methodCall>');
    end
  else if (GetFieldCount=1) and (isType[RTC_XMLRPC_ParamsAsArrayName]=rtc_Array) then // params as array
    begin
    arr:=asArray[RTC_XMLRPC_ParamsAsArrayName];
    if arr.FieldCount=0 then
      begin
      Result.Add('<methodCall><methodName>');
      Result.Add(xmlrpc_writeString(FunctionName));
      Result.Add('</methodName><params></params></methodCall>');
      end
    else
      begin
      Result.Add('<methodCall><methodName>');
      Result.Add(xmlrpc_writeString(FunctionName));
      Result.Add('</methodName><params>'#13#10);
      for idx:=0 to arr.FieldCount-1 do
        begin
        Result.Add('<param>');
        obj:=TRtcValueObject(arr.asObject[idx]);
        if assigned(obj) then
          obj.to_XMLRPC(Result)
        else
          Result.Add(nullValueXMLrpc);
        Result.Add('</param>'#13#10);
        end;
      Result.Add('</params></methodCall>');
      end;
    end
  else
    begin
    Result.Add('<methodCall><methodName>');
    Result.Add(xmlrpc_writeString(FunctionName));
    Result.Add('</methodName><params>'#13#10'<param>');
    inherited to_XMLRPC(Result);
    Result.Add('</param>'#13#10'</params></methodCall>');
    end;
  end;

procedure TRtcFunctionInfo.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    tmp, c_tag, xname, xtag:string;
    xval:TRtcValueObject;
    xrec:TRtcRecord;
    xarr:TRtcArray;
    idx:integer;
    have_params,
    have_name,
    inside_param,
    have_param:boolean;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Record data. TRtcRecord object is already in use.');

  xval:=nil;

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('METHODCALL',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xmlrpc_readTag(s,at,'METHODNAME');
    FunctionName:=xmlrpc_readTrimValue(s,at);
    xmlrpc_readTag(s,at,'/METHODNAME');

    xtag:=UpperCase(xmlrpc_checkTag(s,at));
    if (xtag='PARAMS/') then
      begin
      xmlrpc_skipTag(s,at); // <PARAMS>
      xtag:=UpperCase(xmlrpc_checkTag(s,at));
      FValues:=tRtcFastStrObjList.Create; // no parameters
      end
    else if (xtag='PARAMS') or (xtag='PARAM') then // we could have parameters
      begin
      if xtag='PARAMS' then
        begin
        have_params:=True;
        xmlrpc_skipTag(s,at); // <PARAMS>
        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        end
      else
        have_params:=False;

      if xtag='/PARAMS' then // no parameters (empty array)
        newArray(RTC_XMLRPC_ParamsAsArrayName)
      else if (xtag='PARAM') or (xtag='NAME') then // we have a parameter!
        begin
        if xtag='PARAM' then
          begin
          xmlrpc_skipTag(s,at); // <PARAM>
          inside_param:=True;
          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          end
        else
          inside_param:=False;

        if xtag='/PARAM' then
          begin
          xval:=nil;
          xmlrpc_skipTag(s,at); // </PARAM>
          end
        else if xtag='NAME' then // receiving named parameter list
          begin
          have_param:=True;
          have_name:=False;

          xrec:=TRtcRecord.Create;
          repeat
            xmlrpc_readTag(s,at,'NAME');
            xname:=xmlrpc_readTrimValue(s,at);
            xmlrpc_readTag(s,at,'/NAME');

            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            if xtag<>'/PARAM' then
              begin
              xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
              xrec.asObject[xname]:=xval;
              xval:=nil;
              end;

            if inside_param then
              begin
              xtag:=UpperCase(xmlrpc_checkTag(s,at));
              if (xtag='/PARAM') then
                begin
                inside_param:=False;
                have_param:=False;
                xmlrpc_skipTag(s,at);
                end;
              end;

            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            if xtag='NAME' then
              begin
              if have_param then
                have_name:=True
              else
                Break;
              end
            else if xtag='PARAM' then
              begin
              if not have_param and not have_name then
                begin
                inside_param:=True;
                have_param:=True;
                xmlrpc_skipTag(s,at);
                end
              else
                Break;
              end
            else
              Break;
            until false;
          xval:=xrec;
          end
        else
          begin
          // read all data stored in this parameter
          xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          if inside_param then xmlrpc_readTag(s,at,'/PARAM');
          end;

        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        if (xtag='/PARAMS') or (xtag=c_tag) then // A single <PARAM>
          begin
          if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
            begin
            FValues:=tRtcFastStrObjList.Create;
            try
              // Move all data to our parameter list and destroy the original
              xrec:=TRtcRecord(xval);
              try
                for idx:=0 to xrec.FValues.Count-1 do
                  begin
                  tmp:=xrec.FValues.Strings[idx];
                  FValues.Add(tmp, xrec.FValues.Objects[idx]);
                  xrec.FValues.Objects[idx]:=nil;
                  end;
              finally
                xrec.Free;
                xval:=nil;
                end;
            except
              FValues.DestroyObjects;
              FValues.Free;
              FValues:=nil;
              raise;
              end;
            end
          else // 1-item array
            begin
            xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
            if assigned(xval) then
              begin
              xarr.asObject[0]:=xval;
              xval:=nil;
              end;
            end;
          end
        else if xtag='PARAM' then // More than one <PARAM> - it's an array!
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.asObject[0]:=xval;
            xval:=nil;
            end;

          idx:=1;
          repeat
            xmlrpc_skipTag(s,at); // <PARAM>

            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            if xtag<>'/PARAM' then
              begin
              // read all data stored in this parameter
              xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
              xmlrpc_readTag(s,at,'/PARAM');
              end
            else
              begin
              xval:=nil;
              xmlrpc_skipTag(s,at); // </PARAM>
              end;

            Inc(idx);
            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            until xtag<>'PARAM';
          end;
        end
      else if have_params then
        begin
        // read all data stored in this parameter
        xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);

        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        if xtag='/PARAMS' then // A single value inside <PARAMS>
          begin
          if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
            begin
            FValues:=tRtcFastStrObjList.Create;
            try
              // Move all data to our parameter list and destroy the original
              xrec:=TRtcRecord(xval);
              try
                for idx:=0 to xrec.FValues.Count-1 do
                  begin
                  tmp:=xrec.FValues.Strings[idx];
                  FValues.Add(tmp, xrec.FValues.Objects[idx]);
                  xrec.FValues.Objects[idx]:=nil;
                  end;
              finally
                xrec.Free;
                xval:=nil;
                end;
            except
              FValues.DestroyObjects;
              FValues.Free;
              FValues:=nil;
              raise;
              end;
            end
          else // 1-item array
            begin
            xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
            if assigned(xval) then
              begin
              xarr.asObject[0]:=xval;
              xval:=nil;
              end;
            end;
          end
        else // More than one value - it's an array!
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.asObject[0]:=xval;
            xval:=nil;
            end;

          idx:=1;
          repeat
            // read all data stored in this parameter
            xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            Inc(idx);
            until xtag='/PARAMS';
          end;
        end;

      if have_params then
        xmlrpc_readTag(s,at,'/PARAMS');
      end
    else if xtag<>c_tag then // receiving data without <PARAMS><PARAM>
      begin
      if xtag='NAME' then // receiving named parameter list
        begin
        xrec:=TRtcRecord.Create;
        repeat
          xmlrpc_readTag(s,at,'NAME');
          xname:=xmlrpc_readTrimValue(s,at);
          xmlrpc_readTag(s,at,'/NAME');

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          if xtag<>c_tag then
            begin
            xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
            xrec.asObject[xname]:=xval;
            xval:=nil;
            xtag:=UpperCase(xmlrpc_checkTag(s,at));
            if xtag=c_tag then
              Break;
            end
          else
            Break;
          until false;
        xval:=xrec;
        end
      else
        begin
        // read all data stored in this parameter
        xval:=TRtcValueObject.ObjectFromXMLrpc(s,at);
        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        end;

      if xtag=c_tag then // A single <PARAM>
        begin
        if assigned(xval) and (xval.GetType=rtc_Record) then // standard structure with named parameters
          begin
          FValues:=tRtcFastStrObjList.Create;
          try
            // Move all data to our parameter list and destroy the original
            xrec:=TRtcRecord(xval);
            try
              for idx:=0 to xrec.FValues.Count-1 do
                begin
                tmp:=xrec.FValues.Strings[idx];
                FValues.Add(tmp, xrec.FValues.Objects[idx]);
                xrec.FValues.Objects[idx]:=nil;
                end;
            finally
              xrec.Free;
              xval:=nil;
              end;
          except
            FValues.DestroyObjects;
            FValues.Free;
            FValues:=nil;
            raise;
            end;
          end
        else // 1-item array
          begin
          xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
          if assigned(xval) then
            begin
            xarr.asObject[0]:=xval;
            xval:=nil;
            end;
          end;
        end
      else // More than one parameter - it's an array!
        begin
        xarr:=newArray(RTC_XMLRPC_ParamsAsArrayName);
        if assigned(xval) then
          begin
          xarr.asObject[0]:=xval;
          xval:=nil;
          end;

        idx:=1;
        repeat
          // read all data stored in this parameter
          xarr.asObject[idx]:=TRtcValueObject.ObjectFromXMLrpc(s,at);
          Inc(idx);
          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          until xtag=c_tag;
        end;
      end
    else
      FValues:=tRtcFastStrObjList.Create; // no parameters

    xmlrpc_skipValueClose(s,at,tags);
  finally
    if assigned(xval) then xval.Free;
    SetLength(tags,0);
    end;
  end;

{ TRtcAbsArray }

function TRtcAbsArray.GetNull(index: integer): boolean;
  begin
  Result:= GetObject(index)=nil;
  end;

function TRtcAbsArray.GetArray(index: integer): TRtcArray;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewArray(index)
    else
      Result:=TRtcArray.NullValue;
    end
  else if obj is TRtcArray then
    Result:=TRtcArray(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetArray
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcArray.');
  end;

function TRtcAbsArray.GetDataSet(index: integer): TRtcDataSet;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewDataSet(index)
    else
      Result:=TRtcDataSet.NullValue;
    end
  else if obj is TRtcDataSet then
    Result:=TRtcDataSet(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetDataSet
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcDataSet.');
  end;

function TRtcAbsArray.GetRecord(index: integer): TRtcRecord;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewRecord(index)
    else
      Result:=TRtcRecord.NullValue;
    end
  else if obj is TRtcRecord then
    Result:=TRtcRecord(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetRecord
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcRecord.');
  end;

function TRtcAbsArray.GetFunctionInfo(index: integer): TRtcFunctionInfo;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    begin
    if AutoCreate then
      Result:=NewFunction(index)
    else
      Result:=TRtcFunctionInfo.NullValue;
    end
  else if obj is TRtcFunctionInfo then
    Result:=TRtcFunctionInfo(obj)
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetFunctionInfo
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcFunctionInfo.');
  end;

function TRtcAbsArray.GetBoolean(index: integer): boolean;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcBooleanValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetBoolean
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetBoolean
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Boolean.');
  end;

function TRtcAbsArray.GetCurrency(index: integer): Currency;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcCurrencyValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetCurrency
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetCurrency
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Currency.');
  end;

function TRtcAbsArray.GetDateTime(index: integer): TDateTime;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcDateTimeValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetDateTime
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetDateTime
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TDateTime.');
  end;

function TRtcAbsArray.GetException(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcExceptionValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetException
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetException
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcExceptionValue.');
  end;

function TRtcAbsArray.GetVarName(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcVariableName.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetVarName
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetVarName
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to TRtcVariableName.');
  end;

function TRtcAbsArray.GetInteger(index: integer): rtcInteger;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcIntegerValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetInteger
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetInteger
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Integer.');
  end;

function TRtcAbsArray.GetLargeInt(index: integer): rtcLargeInt;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcLargeIntValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetLargeInt
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetLargeInt
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to LargeInt.');
  end;

function TRtcAbsArray.GetFloat(index: integer): rtcFloat;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcFloatValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetFloat
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetFloat
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Float.');
  end;

function TRtcAbsArray.GetString(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcStringValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetString
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetString
  else if obj is TRtcArray then
    Result:=TRtcArray(obj).GetAsString
  else if obj is TRtcRecord then
    Result:=TRtcRecord(obj).GetAsString
  else if obj is TRtcDataSet then
    Result:=TRtcDataSet(obj).GetAsString
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to String.');
  end;

function TRtcAbsArray.GetWideString(index: integer): WideString;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcWideStringValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetWideString
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetWideString
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to WideString.');
  end;

function TRtcAbsArray.GetText(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcTextValue.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetText
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetText
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Text.');
  end;

function TRtcAbsArray.GetByteStream(index: integer): TStream;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=TRtcByteStream.NullValue
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetByteStream
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetByteStream
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to ByteStream.');
  end;

function TRtcAbsArray.GetVariant(index: integer): Variant;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=Null
  else if obj is TRtcSimpleValue then
    Result:=TRtcSimpleValue(obj).GetVariant
  else if obj is TRtcValue then
    Result:=TRtcValue(obj).GetVariant
  else
    raise EConvertError.Create('Can not convert '+obj.ClassName+' to Variant.');
  end;

function TRtcAbsArray.GetValueType(index: integer): TRtcValueTypes;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) then
    Result:=rtc_Null
  else
    Result:=obj.GetType;
  end;

procedure TRtcAbsArray.SetNull(index: integer; const Value: boolean);
  begin
  if Value then
    SetObject(index, nil, True);
  end;

procedure TRtcAbsArray.SetArray(index: integer; const Value: TRtcArray);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetRecord(index: integer; const Value: TRtcRecord);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetDataSet(index: integer; const Value: TRtcDataSet);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetFunctionInfo(index: integer; const Value: TRtcFunctionInfo);
  begin
  SetObject(index, Value, True);
  end;

procedure TRtcAbsArray.SetBoolean(index: integer; const Value: boolean);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcBooleanValue) then
    begin
    obj:=TRtcBooleanValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcBooleanValue(obj).SetBoolean(Value);
  end;

procedure TRtcAbsArray.SetCurrency(index: integer; const Value: Currency);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcCurrencyValue) then
    begin
    obj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcCurrencyValue(obj).SetCurrency(Value);
  end;

procedure TRtcAbsArray.SetDateTime(index: integer; const Value: TDateTime);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcDateTimeValue) then
    begin
    obj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcDateTimeValue(obj).SetDateTime(Value);
  end;

procedure TRtcAbsArray.SetException(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcExceptionValue) then
    begin
    obj:=TRtcExceptionValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcExceptionValue(obj).SetException(Value);
  end;

procedure TRtcAbsArray.SetVarName(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcVariableName) then
    begin
    obj:=TRtcVariableName.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcVariableName(obj).SetVarName(Value);
  end;

procedure TRtcAbsArray.SetInteger(index: integer; const Value: rtcInteger);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcIntegerValue) then
    begin
    obj:=TRtcIntegerValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcIntegerValue(obj).SetInteger(Value);
  end;

procedure TRtcAbsArray.SetLargeInt(index: integer; const Value: rtcLargeInt);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcLargeIntValue) then
    begin
    obj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcLargeIntValue(obj).SetLargeInt(Value);
  end;

procedure TRtcAbsArray.SetFloat(index: integer; const Value: rtcFloat);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcFloatValue) then
    begin
    obj:=TRtcFloatValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcFloatValue(obj).SetFloat(Value);
  end;

procedure TRtcAbsArray.SetString(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcStringValue) then
    begin
    obj:=TRtcStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcStringValue(obj).SetString(Value);
  end;

procedure TRtcAbsArray.SetWideString(index: integer; const Value: WideString);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcWideStringValue) then
    begin
    obj:=TRtcWideStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcWideStringValue(obj).SetWideString(Value);
  end;

procedure TRtcAbsArray.SetText(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcTextValue) then
    begin
    obj:=TRtcTextValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcTextValue(obj).SetText(Value);
  end;

procedure TRtcAbsArray.SetByteStream(index: integer; const Value: TStream);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcByteStream) then
    begin
    obj:=TRtcByteStream.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end;
  TRtcByteStream(obj).SetByteStream(Value);
  end;

procedure TRtcAbsArray.SetVariant(index: integer; const Value: Variant);
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if assigned(obj) then
    if not (obj is TRtcSimpleValue) then
      raise Exception.Create('Value already assigned. Set to NULL before assigning another value.')
    else if TRtcSimpleValue(obj).SetVariant(Value) then
      Exit; // value changed

  obj:=TRtcValueObject.ObjectFromVariant(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    SetObject(index,obj);
  except
    obj.Free;
    raise;
    end;
  end;

procedure TRtcAbsArray.SetAsObject(index: integer; Value: TRtcValueObject);
  begin
  SetObject(index, Value);
  end;

function TRtcAbsArray.NewArray(index: integer): TRtcArray;
  begin
  Result:=TRtcArray.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsArray.NewRecord(index: integer): TRtcRecord;
  begin
  Result:=TRtcRecord.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsArray.NewDataSet(index: integer): TRtcDataSet;
  begin
  Result:=TRtcDataSet.Create;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsArray.NewFunction(index: integer; const func_name:String=''): TRtcFunctionInfo;
  begin
  Result:=TRtcFunctionInfo.Create;
  Result.FunctionName:=func_name;
  Result.AutoCreate:=AutoCreate;
  try
    SetObject(index, Result);
  except
    Result.Free;
    raise;
    end;
  end;

function TRtcAbsArray.NewBoolean(index: integer): boolean;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcBooleanValue) then
    begin
    obj:=TRtcBooleanValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcBooleanValue(obj).SetNull(True);
  Result:=TRtcBooleanValue(obj).GetBoolean;
  end;

function TRtcAbsArray.NewCurrency(index: integer): Currency;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcCurrencyValue) then
    begin
    obj:=TRtcCurrencyValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcCurrencyValue(obj).SetNull(True);
  Result:=TRtcCurrencyValue(obj).GetCurrency;
  end;

function TRtcAbsArray.NewDateTime(index: integer): TDateTime;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcDateTimeValue) then
    begin
    obj:=TRtcDateTimeValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcDateTimeValue(obj).SetNull(True);
  Result:=TRtcDateTimeValue(obj).GetDateTime;
  end;

function TRtcAbsArray.NewException(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcExceptionValue) then
    begin
    obj:=TRtcExceptionValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcExceptionValue(obj).SetNull(True);
  Result:=TRtcExceptionValue(obj).GetException;
  end;

function TRtcAbsArray.NewVariable(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcVariableName) then
    begin
    obj:=TRtcVariableName.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcVariableName(obj).SetNull(True);
  Result:=TRtcVariableName(obj).GetVarName;
  end;

function TRtcAbsArray.NewInteger(index: integer): rtcInteger;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcIntegerValue) then
    begin
    obj:=TRtcIntegerValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcIntegerValue(obj).SetNull(True);
  Result:=TRtcIntegerValue(obj).GetInteger;
  end;

function TRtcAbsArray.NewLargeInt(index: integer): rtcLargeInt;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcLargeIntValue) then
    begin
    obj:=TRtcLargeIntValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcLargeIntValue(obj).SetNull(True);
  Result:=TRtcLargeIntValue(obj).GetLargeInt;
  end;

function TRtcAbsArray.NewFloat(index: integer): rtcFloat;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcFloatValue) then
    begin
    obj:=TRtcFloatValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcFloatValue(obj).SetNull(True);
  Result:=TRtcFloatValue(obj).GetFloat;
  end;

function TRtcAbsArray.NewString(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcStringValue) then
    begin
    obj:=TRtcStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcStringValue(obj).SetNull(True);
  Result:=TRtcStringValue(obj).GetString;
  end;

function TRtcAbsArray.NewWideString(index: integer): WideString;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcWideStringValue) then
    begin
    obj:=TRtcWideStringValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcWideStringValue(obj).SetNull(True);
  Result:=TRtcWideStringValue(obj).GetWideString;
  end;

function TRtcAbsArray.NewText(index: integer): String;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcTextValue) then
    begin
    obj:=TRtcTextValue.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcTextValue(obj).SetNull(True);
  Result:=TRtcTextValue(obj).GetText;
  end;

function TRtcAbsArray.NewByteStream(index: integer): TStream;
  var
    obj:TRtcValueObject;
  begin
  obj:=GetObject(index);
  if not assigned(obj) or not (obj is TRtcByteStream) then
    begin
    obj:=TRtcByteStream.Create;
    try
      SetObject(index, obj);
    except
      obj.Free;
      raise;
      end;
    end
  else
    TRtcByteStream(obj).SetNull(True);
  Result:=TRtcByteStream(obj).GetByteStream;
  end;

function TRtcAbsArray.GetCode(index: integer): String;
  begin
  if not assigned(asObject[index]) then
    Result:=nullValueCode
  else
    Result:=asObject[index].toCode;
  end;

procedure TRtcAbsArray.SetCode(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject[index]) and not (asObject[index] is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned to field ['+IntToStr(index)+']. Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromCode(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject[index]:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

function TRtcAbsArray.GetXMLrpc(index: integer): String;
  begin
  if not assigned(asObject[index]) then
    Result:=nullValueXMLrpc
  else
    Result:=asObject[index].toXMLrpc;
  end;

procedure TRtcAbsArray.SetXMLrpc(index: integer; const Value: String);
  var
    obj:TRtcValueObject;
  begin
  if assigned(asObject[index]) and not (asObject[index] is TRtcSimpleValue) then
    raise Exception.Create('Value already assigned to field ['+IntToStr(index)+']. Set to NULL before assigning another value.');

  obj:=TRtcValueObject.ObjectFromXMLrpc(Value);
  try
    if obj is TRtcAbsArray then
      TRtcAbsArray(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsRecord then
      TRtcAbsRecord(obj).AutoCreate:=AutoCreate
    else if obj is TRtcAbsValue then
      TRtcAbsValue(obj).AutoCreate:=AutoCreate;
    asObject[index]:=obj;
  except
    obj.Free;
    raise;
    end;
  end;

{ TRtcArray }

constructor TRtcArray.Create;
  begin
  inherited;
  FValues:=nil;
  end;

destructor TRtcArray.Destroy;
  begin
  Clear;
  inherited;
  end;

procedure TRtcArray.Clear;
  var
    idx:integer;
    obj:TObject;
  begin
  if assigned(FValues) then
    begin
    for idx:=0 to FValues.Count-1 do
      begin
      obj:=TObject(FValues.Items[idx]);
      if assigned(obj) then obj.Free;
      end;
    FValues.Free;
    FValues:=nil;
    end;
  end;

function TRtcArray.GetType: TRtcValueTypes;
  begin
  Result:=rtc_Array;
  end;

function TRtcArray.GetFieldCount: integer;
  begin
  if assigned(FValues) then
    Result:=FValues.Count
  else
    Result:=0;
  end;

function TRtcArray.Count: integer;
  begin
  Result:=GetFieldCount;
  end;

function TRtcArray.GetAsString: String;
  var
    res:TRtcHugeString;
    i:integer;
  begin
  res:=TRtcHugeString.Create;
  try
    for i:=0 to Count-1 do
      res.Add(asString[i]);
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

function TRtcArray.GetObject(index: integer): TRtcValueObject;
  begin
  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      Result:=TRtcValueObject(FValues.Items[index])
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcArray.SetObject(index: integer; _Value: TRtcValueObject; asCopy:boolean=False);
  var
    obj:TRtcValueObject;
  begin
  if index<0 then
    raise Exception.Create('TRtcArray.SetObject: index lower than 0 (zero) not allowed.');

  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      begin
      obj:=TRtcValueObject(FValues.Items[index]);
      if obj<>_Value then
        begin
        if _Value<>nil then
          begin
          if not assigned(obj) or (obj is TRtcSimpleValue) then
            begin
            if asCopy then
              FValues.Items[index]:=_Value.copyOf
            else
              FValues.Items[index]:=_Value;
            if assigned(obj) then obj.Free;
            end
          else if obj is TRtcValue then
            TRtcValue(obj).SetObject(_Value,asCopy)
          else
            raise Exception.Create('Value of type '+obj.ClassName+' allready assigned at index '+IntToStr(index)+'.'#13#10+
                                   'Set ['+IntToStr(index)+'] to NULL before assigning a different object.')
          end
        else
          begin
          if assigned(obj) and asCopy then
            obj.Free;
          FValues.Items[index]:=nil;
          if (index=FValues.Count-1) then
            FValues.Delete(index);
          end;
        end;
      end
    else if _Value<>nil then
      begin
      while FValues.Count<index+1 do
        FValues.Add(nil);
      if asCopy then
        FValues.Items[index]:=_Value.copyOf
      else
        FValues.Items[index]:=_Value;
      end;
    end
  else if _Value<>nil then
    begin
    FValues:=TList.Create;
    while FValues.Count<index+1 do
      FValues.Add(nil);
    if asCopy then
      FValues.Items[index]:=_Value.copyOf
    else
      FValues.Items[index]:=_Value;
    end;
  end;

class function TRtcArray.NullValue: TRtcArray;
  begin
  Result:=nil;
  end;

procedure TRtcArray.CopyFrom(_Value: TRtcValueObject);
  var
    idx:integer;
    mylist:TList;
    obj:TRtcValueObject;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Arrays. Data already assigned to this Array.');
  if assigned(TRtcArray(_Value).FValues) then
    begin
    mylist:=TList.Create;
    with TRtcArray(_Value).FValues do
      for idx:=0 to Count-1 do
        begin
        obj:=TRtcValueObject(Items[idx]);
        if assigned(obj) then
          obj:=obj.copyOf;
        mylist.Add(obj);
        end;
    end
  else
    mylist:=nil;
  FValues:=mylist;
  end;

function TRtcArray.CopyOf: TRtcValueObject;
  begin
  Result:=TRtcArray.Create;
  Result.CopyFrom(self);
  end;

procedure TRtcArray.from_Code(const s: String; var at:integer);
  var
    data:String;
    idx,cnt:integer;
    obj:TObject;
    val:TList;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Arrays. TRtcArray is already in use.');

  data:=code_fromShortString(TypeToStr(rtc_Array),s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=StrToInt(data);
  except
    raise Exception.Create('TRtcArray.from_Code: Field Count missing.');
    end;

  val:=TList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      obj:=ObjectFromCode(s,at);
      val.Add(obj);
      end;
  except
    for idx:=0 to val.Count-1 do
      begin
      obj:=TObject(val.Items[idx]);
      if assigned(obj) then obj.Free;
      end;
    val.Free;
    raise;
    end;

  FValues:=val;
  end;

procedure TRtcArray.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  Result.Add(code_toShortString(TypeToStr(rtc_Array),IntToStr(GetFieldCount)));
  for idx:=0 to GetFieldCount-1 do
    begin
    obj:=TRtcValueObject(FValues.Items[idx]);
    if assigned(obj) then
      obj.to_Code(Result)
    else
      Result.Add(nullValueCode);
    end;
  end;

class function TRtcArray.FromCode(const data: String; var at:integer): TRtcArray;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcArray.Create;
  try
    Result.from_Code(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcArray.FromCode(const data: String): TRtcArray;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcArray.FromXMLrpc(const data: String; var at: integer): TRtcArray;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcArray.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcArray.FromXMLrpc(const data: String): TRtcArray;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcArray.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
  begin
  Result.Add('<value><array><data>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    obj:=TRtcValueObject(FValues.Items[idx]);
    if assigned(obj) then
      obj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add(#13#10);
    end;
  Result.Add('</data></array></value>');
  end;

procedure TRtcArray.from_XMLrpc(const s: String; var at: integer);
  var
    tags:rtcClosingTagsType;
    c_tag, xtag:string;
    xval:TRtcValueObject;
    val:TList;
    idx:integer;
  begin
  if assigned(FValues) then
    raise Exception.Create('Can not merge Array data. TRtcArray object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('ARRAY',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    c_tag:=xmlrpc_FirstCloseTag(tags);

    xtag:=UpperCase(xmlrpc_checkTag(s,at));

    val:=TList.Create;
    try
      if xtag='DATA' then
        begin
        xmlrpc_skipTag(s,at); // <data>

        xtag:=UpperCase(xmlrpc_checkTag(s,at));
        while xtag<>'/DATA' do
          begin
          xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
          val.Add(xval);

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          end;

        xmlrpc_readTag(s,at,'/DATA');
        end
      else if xtag<>c_tag then
        begin
        repeat
          xval:=TRtcValueObject.ObjectFromXMLRPC(s,at);
          val.Add(xval);

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          until xtag=c_tag;
        end;
    except
      for idx:=0 to val.Count-1 do
        begin
        xval:=TRtcValueObject(val.Items[idx]);
        if assigned(xval) then xval.Free;
        end;
      val.Free;
      raise;
      end;

    FValues:=val;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcArray.Extract(const index: integer);
  var
    obj:TRtcValueObject;
  begin
  if index<0 then
    raise Exception.Create('TRtcArray.SetObject: index lower than 0 (zero) not allowed.');

  if assigned(FValues) then
    begin
    if (index>=0) and (index<FValues.Count) then
      begin
      obj:=TRtcValueObject(FValues.Items[index]);
      if assigned(obj) then
        begin
        obj.Extracted;
        FValues.Items[index]:=nil;
        if (index=FValues.Count-1) then
          FValues.Delete(index);
        end;
      end;
    end;
  end;

{ TRtcDataSet }

constructor TRtcDataSet.Create;
  begin
  inherited;
  FNames:=nil;
  FData:=nil;
  SetLength(FTypes,0);
  SetLength(FSizes,0);
  SetLength(FRequired,0);
  FRow:=0;
  end;

destructor TRtcDataSet.Destroy;
  begin
  Clear;
  inherited;
  end;

procedure TRtcDataSet.Clear;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    FNames.Free;
    FNames:=nil;
    end;
  if assigned(FData) then
    begin
    for idx:=0 to FData.Count-1 do
      if assigned(FData.Items[idx]) then
        TObject(FData.Items[idx]).Free;
    FData.Free;
    FData:=nil;
    end;
  SetLength(FTypes,0);
  SetLength(FSizes,0);
  SetLength(FRequired,0);
  end;

function TRtcDataSet.GetType: TRtcValueTypes;
  begin
  Result:=rtc_DataSet;
  end;

function TRtcDataSet.GetFieldCount: integer;
  begin
  if assigned(FNames) then
    Result:=FNames.Count
  else
    Result:=0;
  end;

function TRtcDataSet.GetFieldName(index: integer): String;
  begin
  if assigned(FNames) and (index>=0) and (index<FNames.Count) then
    Result:=FNames.Strings[index]
  else
    Result:='';
  end;

procedure TRtcDataSet.SetFieldName(index: integer; const _Value: String);
  var
    idx:integer;
  begin
  if _Value='' then
    raise Exception.Create('Can not set field name to an empty String.');
  if assigned(FNames) and (index>=0) and (index<FNames.Count) then
    begin
    idx:=FNames.Find(_Value);

    if (idx>=0) and (idx<>index) then
      raise Exception.Create('Another field with name "'+_Value+'" already exists.');

    FNames.Strings[index]:=_Value;
    end
  else
    raise Exception.Create('No Field at index '+IntToStr(index)+'.');
  end;

function TRtcDataSet.GetFieldType(const index: string): TRtcFieldTypes;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FTypes) then
        Result:=FTypes[idx]
      else
        Result:=ft_Unknown;
      end
    else
      Result:=ft_Unknown;
    end
  else
    Result:=ft_Unknown;
  end;

procedure TRtcDataSet.SetFieldType(const index: String; const _Value: TRtcFieldTypes);
  var
    idx:integer;
  begin
  if index='' then
    raise Exception.Create('Can not set field type without field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStrObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FTypes) do
    begin
    SetLength(FTypes,length(FTypes)+1);
    FTypes[length(FTypes)-1]:=ft_Unknown;
    end;
  FTypes[idx]:=_Value;
  end;

function TRtcDataSet.GetFieldRequired(const index: string): boolean;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FRequired) then
        Result:=FRequired[idx]
      else
        Result:=False;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

procedure TRtcDataSet.SetFieldRequired(const index: string; const _Value: boolean);
  var
    idx:integer;
  begin
  if index='' then
    raise Exception.Create('Can not set required property for field without a field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStrObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FRequired) do
    begin
    SetLength(FRequired,length(FRequired)+1);
    FRequired[length(FRequired)-1]:=False;
    end;
  FRequired[idx]:=_Value;
  end;

function TRtcDataSet.GetFieldSize(const index: string): Integer;
  var
    idx:integer;
  begin
  if assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      if idx<length(FSizes) then
        Result:=FSizes[idx]
      else
        Result:=0;
      end
    else
      Result:=0;
    end
  else
    Result:=0;
  end;

procedure TRtcDataSet.SetFieldSize(const index: string; const _Value: Integer);
  var
    idx:integer;
  begin
  if index='' then
    raise Exception.Create('Can not set field size without field name.');

  if not assigned(FNames) then
    FNames:=tRtcFastStrObjList.Create;

  idx:=FNames.Find(index);
  if idx<0 then
    idx:=FNames.Add(index);

  while idx>=length(FSizes) do
    begin
    SetLength(FSizes,length(FSizes)+1);
    FSizes[length(FSizes)-1]:=0;
    end;
  FSizes[idx]:=_Value;
  end;

function TRtcDataSet.GetObject(const index: String): TRtcValueObject;
  var
    idx:integer;
  begin
  if assigned(FData) and assigned(FNames) and
     (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      Result:=TRtcArray(FData.Items[FRow]).asObject[idx]
    else
      Result:=nil;
    end
  else
    Result:=nil;
  end;

function TRtcDataSet.GetFieldIndex(const index: string): integer;
  begin
  Result:=FNames.Find(index);
  end;

function TRtcDataSet.GetRowData: TRtcArray;
  begin
  if assigned(FData) and (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    Result:=TRtcArray(FData.Items[FRow])
  else
    Result:=nil;
  end;

procedure TRtcDataSet.SetRowData(const _Value: TRtcArray);
  begin
  if assigned(FData) and (FRow>=0) and (FRow<FData.Count) then
    begin
    if TRtcArray(FData.Items[FRow])<>_Value then
      begin
      if assigned(FData.Items[FRow]) then
        TRtcArray(FData.Items[FRow]).Free;
      FData.Items[FRow]:=_Value;
      end;
    end
  else
    raise Exception.Create('Can not assign RowData to non-existing Row.');
  end;

procedure TRtcDataSet.SetObject(const index: String; _Value: TRtcValueObject; asCopy:boolean=False);
  var
    idx:integer;
    myrow:TRtcArray;
  begin
  if FRow<0 then
    raise Exception.Create('TRtcDataSet.SetObject: Row index beyond BOF.')
  else if index='' then
    raise Exception.Create('TRtcDataSet.SetObject: Fields without a name not allowed. Set Field Data by using a Field Name.');

  if assigned(FData) and assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then // name found
      begin
      if (FRow>=0) and (FRow<FData.Count) then // row number exists
        begin
        myrow:=TRtcArray(FData.Items[FRow]);
        if assigned(myrow) then
          begin
          if myrow.asObject[idx]<>_Value then
            myrow.SetObject(idx,_Value,asCopy);
          end
        else if _Value<>nil then
          begin
          myrow:=TRtcArray.Create;
          try
            myrow.SetObject(idx,_Value,asCopy);
          except
            myrow.Free;
            raise;
            end;
          FData.Items[FRow]:=myrow;
          end;
        end
      else if _Value<>nil then
        begin
        while FData.Count<FRow+1 do
          FData.Add(nil);
        myrow:=TRtcArray.Create;
        try
          myrow.SetObject(idx,_Value,asCopy);
        except
          myrow.Free;
          raise;
          end;
        FData.Items[FRow]:=myrow;
        end;
      end
    else if _Value<>nil then // name not found
      begin
      idx:=FNames.Add(index); // add name

      while FData.Count<FRow+1 do
        FData.Add(nil);
      myrow:=TRtcArray(FData.Items[FRow]);
      if not assigned(myrow) then
        myrow:=TRtcArray.Create;
      try
        myrow.SetObject(idx,_Value,asCopy);
      except
        if not assigned(FData.Items[FRow]) then
          myrow.Free;
        raise;
        end;
      FData.Items[FRow]:=myrow;
      end;
    end
  else if _Value<>nil then // no data yet
    begin
    if not assigned(FNames) then
      FNames:=tRtcFastStrObjList.Create;

    if not assigned(FData) then
      FData:=TList.Create;

    idx:=FNames.Find(index);
    if idx<0 then // name found
      idx:=FNames.Add(index); // add name

    while FData.Count<FRow+1 do
      FData.Add(nil);
    myrow:=TRtcArray.Create;
    try
      myrow.SetObject(idx,_Value,asCopy);
    except
      myrow.Free;
      raise;
      end;
    FData.Items[FRow]:=myrow;
    end;
  // Set Field type, if not defined.
  if assigned(_Value) and (FieldType[index]=ft_Unknown) then
    FieldType[index]:=RTC_VALUE2FIELD_TYPES[_Value.GetType];
  end;

function TRtcDataSet.Empty: boolean;
  begin
  Result:=RowCount=0;
  end;

function TRtcDataSet.GetRowCount: integer;
  begin
  if assigned(FData) then
    Result:=FData.Count
  else
    Result:=0;
  end;

function TRtcDataSet.GetRow: integer;
  begin
  Result:=FRow;
  end;

procedure TRtcDataSet.SetRow(const _Value: integer);
  begin
  if _Value<-1 then
    raise Exception.Create('Can not move to a Row beyond BOF, index out of bounds.')
  else
    FRow:=_Value;
  end;

function TRtcDataSet.BOF: boolean;
  begin
  Result:=(RowCount=0) or (FRow<0);
  end;

function TRtcDataSet.EOF: boolean;
  begin
  Result:=(RowCount=0) or (FRow>=RowCount);
  end;

procedure TRtcDataSet.First;
  begin
  FRow:=0;
  end;

procedure TRtcDataSet.Last;
  begin
  if RowCount>0 then
    FRow:=RowCount-1
  else
    FRow:=0;
  end;

procedure TRtcDataSet.Append;
  begin
  FRow:=RowCount;
  Insert;
  end;

procedure TRtcDataSet.Insert;
  begin
  if FRow<0 then FRow:=0;
  if not assigned(FData) then FData:=TList.Create;
  if not assigned(FNames) then
    FNames:=tRtcFastStrObjList.Create;

  if FRow<FData.Count then
    FData.Insert(FRow,nil)
  else
    begin
    while FRow>FData.Count-1 do
      FData.Add(nil);
    end;
  end;

procedure TRtcDataSet.Delete;
  begin
  if (Row<0) then
    raise Exception.Create('Can not delete a row before first row.')
  else if (Row>=RowCount) then
    raise Exception.Create('Can not delete a row after last row.');
  if assigned(FData.Items[FRow]) then
    TRtcArray(FData.Items[FRow]).Free;
  FData.Delete(FRow);
  if (FRow>=RowCount) and (FRow>0) then
    Dec(FRow);
  end;

procedure TRtcDataSet.Next;
  begin
  if FRow<RowCount then
    Inc(FRow);
  end;

procedure TRtcDataSet.Prior;
  begin
  if FRow>=0 then
    Dec(FRow);
  end;

class function TRtcDataSet.NullValue: TRtcDataSet;
  begin
  Result:=nil;
  end;

procedure TRtcDataSet.to_Code(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
    fname:string;
  const
    BoolToStr:array[boolean] of string =('F','T');
  begin
  Result.Add(code_toShortString(TypeToStr(rtc_DataSet),IntToStr(GetFieldCount)));
  for idx:=0 to GetFieldCount-1 do
    begin
    fname:=FieldName[idx];
    {$IFDEF RtcUpperCaseFieldNames}
      Result.Add(code_toNameString(UpperCase(fname)));
    {$ELSE}
      Result.Add(code_toNameString(fname));
    {$ENDIF}
    Result.Add(code_toMidString(FieldTypeToStr(FieldType[fname])));
    Result.Add(code_toMidString(IntToStr(FieldSize[fname])));
    Result.Add(code_toEndString(BoolToStr[FieldRequired[fname]]));
    end;
  Result.Add(code_toShortString('ROWS',IntToStr(GetRowCount)));
  for idx:=0 to GetRowCount-1 do
    begin
    obj:=TRtcValueObject(FData.Items[idx]);
    if assigned(obj) then
      obj.to_Code(Result)
    else
      Result.Add(nullValueCode);
    end;
  end;

procedure TRtcDataSet.from_Code(const s: String; var at:integer);
  var
    fname,
    ftype,
    fsize,
    freq,
    data:String;
    idx,cnt:integer;
    obj:TObject;
  begin
  if assigned(FData) or assigned(FNames) then
    raise Exception.Create('Can not merge DataSet data. TRtcDataSet object is already in use.');

  data:=code_fromShortString(TypeToStr(rtc_DataSet),s,at);
  try
    if data='' then
      cnt:=0
    else
      cnt:=StrToInt(data);
  except
    raise Exception.Create('TRtcDataSet.from_Code: Field Count missing.');
    end;

  FNames:=tRtcFastStrObjList.Create;
  try
    for idx:=0 to cnt-1 do
      begin
      fname:=code_fromNameString(s,at);
      ftype:=code_fromMidString(s,at);
      fsize:=code_fromMidString(s,at);
      freq:=code_fromEndString(s,at);
      FieldType[fname]:=StrToFieldType(ftype);
      FieldSize[fname]:=StrToInt(fsize);
      if UpperCase(freq)='T' then
        FieldRequired[fname]:=True
      else
        FieldRequired[fname]:=False;
      end;

    data:=code_fromShortString('ROWS',s,at);
    try
      if data='' then
        cnt:=0
      else
        cnt:=StrToInt(data);
    except
      raise Exception.Create('TRtcDataSet.from_Code: Row Count missing.');
      end;

    FData:=TList.Create;
    try
      for idx:=0 to cnt-1 do
        begin
        obj:=ObjectFromCode(s,at);
        FData.Add(obj);
        end;
    except
      for idx:=0 to FData.Count-1 do
        begin
        obj:=TObject(FData.Items[idx]);
        if assigned(obj) then obj.Free;
        end;
      FData.Free;
      FData:=nil;
      raise;
      end;
  except
    FNames.DestroyObjects;
    FNames.Free;
    FNames:=nil;
    raise;
    end;
  end;

procedure TRtcDataSet.CopyFrom(_Value: TRtcValueObject);
  var
    idx:integer;
    fname:string;
  begin
  if assigned(FData) or assigned(FNames) then
    raise Exception.Create('Can not merge DataSets. This DataSet already has data assigned.');

  for idx:=0 to TRtcDataSet(_Value).FieldCount-1 do
    begin
    fname:=TRtcDataSet(_Value).FieldName[idx];
    FieldType[fname]:=TRtcDataSet(_Value).FieldType[fname];
    FieldSize[fname] := TRtcDataSet(_Value).FieldSize[fname];
    FieldRequired[fname] := TRtcDataSet(_Value).FieldRequired[fname];
    end;

  FData:=TList.Create;

  for idx:=0 to TRtcDataSet(_Value).RowCount-1 do
    begin
    if assigned(TRtcDataSet(_Value).FData.Items[idx]) then
      FData.Add(TRtcArray(TRtcDataSet(_Value).FData.Items[idx]).copyOf)
    else
      FData.Add(nil);
    end;
  end;

function TRtcDataSet.copyOf: TRtcValueObject;
  begin
  Result:=TRtcDataSet.Create;
  Result.CopyFrom(self);
  end;

class function TRtcDataSet.FromCode(const data: String; var at:integer): TRtcDataSet;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcDataSet.Create;
  try
    Result.from_Code(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcDataSet.FromCode(const data: String): TRtcDataSet;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromCode(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

class function TRtcDataSet.FromXMLrpc(const data: String; var at: integer): TRtcDataSet;
  var
    oldat:integer;
  begin
  oldat:=at;
  Result:=TRtcDataSet.Create;
  try
    Result.from_XMLrpc(data,at);
  except
    at:=oldat;
    Result.Free;
    raise;
    end;
  end;

class function TRtcDataSet.FromXMLrpc(const data: String): TRtcDataSet;
  var
    at:integer;
  begin
  at:=0;
  Result:=FromXMLrpc(data,at);
  if at<>length(data) then
    begin
    Result.Free;
    raise Exception.Create('String contains more data than expected.');
    end;
  end;

procedure TRtcDataSet.to_XMLRPC(const Result:TRtcHugeString);
  var
    idx:integer;
    obj:TRtcValueObject;
    fname:string;
  const
    BoolToStr:array[boolean] of string =('0','1');
  begin
  Result.Add('<value><struct><member><name>');
  Result.Add(RTC_XMLRPC_DataSetFieldsName);
  Result.Add('</name>'#13#10'<value><array><data>'#13#10);
  for idx:=0 to GetFieldCount-1 do
    begin
    fname:=FieldName[idx];
    Result.Add('<value><struct>'#13#10'<member><name>Name</name><value>');
    Result.Add(xmlrpc_WriteString(fname));
    Result.Add('</value></member>'#13#10'<member><name>Type</name><value>');
    Result.Add(FieldTypeToStr(FieldType[fname]));
    Result.Add('</value></member>'#13#10);
    if FieldSize[fname]<>0 then
      begin
      Result.Add('<member><name>Size</name><value><int>');
      Result.Add(IntToStr(FieldSize[fname]));
      Result.Add('</int></value></member>'#13#10);
      end;
    if FieldRequired[fname] then
      begin
      Result.Add('<member><name>Required</name><value><boolean>');
      Result.Add(BoolToStr[FieldRequired[fname]]);
      Result.Add('</boolean></value></member>'#13#10);
      end;
    Result.Add('</struct></value>'#13#10);
    end;
  Result.Add('</data></array></value></member>'#13#10'<member><name>'+
              RTC_XMLRPC_DataSetRowsName+
              '</name>'#13#10'<value><array><data>'#13#10);
  for idx:=0 to GetRowCount-1 do
    begin
    obj:=TRtcValueObject(FData.Items[idx]);
    if assigned(obj) then
      obj.to_XMLRPC(Result)
    else
      Result.Add(nullValueXMLrpc);
    Result.Add(#13#10);
    end;
  Result.Add('</data></array></value></member></struct></value>');
  end;

function TRtcDataSet.GetAsString: String;
  var
    res:TRtcHugeString;
    oldrow,idx,cnt:integer;
    b:boolean;
    fname:string;
  begin
  res:=TRtcHugeString.Create;
  try
    oldrow:=Row;
    try
      First;
      cnt:=GetFieldCount;
      while not EOF do
        begin
        b:=False;
        Res.Add('(');
        for idx:=0 to cnt-1 do
          begin
          fname:=FieldName[idx];
          if not isNull[fname] then
            begin
            if b then Res.Add('; ') else b:=True;
            Res.Add(fname+'='+asString[fname]);
            end;
          end;
        Res.Add(')');
        if not EOF then
          Res.Add(#13#10);
        Next;
        end;
    finally
      Row:=oldrow;
      end;
    Result:=res.Get;
  finally
    res.Free;
    end;
  end;

procedure TRtcDataSet.from_XMLrpc(const s: String; var at: integer);
  var
    fname,s1:string;
    a,idx:integer;
    obj:TRtcValueObject;
    xarr:TRtcArray;
    xrec:TRtcRecord;
    tags:rtcClosingTagsType;
    xtag:string;
    o_memb:boolean;
  begin
  if assigned(FData) or assigned(FNames) then
    raise Exception.Create('Can not merge DataSet data. TRtcDataSet object is already in use.');

  SetLength(tags,0);
  try
    xmlrpc_skipValueOpen('STRUCT',s,at,tags);
    xmlrpc_skipWhitespace(s,at);

    try
      xtag:=UpperCase(xmlrpc_checkTag(s,at));
      if (xtag='MEMBER') or (xtag='NAME') then
        begin
        o_memb:=xtag='MEMBER';
        repeat
          if o_memb then xmlrpc_skipTag(s,at); // <member>

          xmlrpc_readTag(s,at,'NAME');
          s1:=UpperCase(xmlrpc_readTrimValue(s,at));
          xmlrpc_readTag(s,at,'/NAME');

          if s1=RTC_XMLRPC_DataSetFieldsName then
            begin
            if assigned(FNames) then
              raise Exception.Create('XML-RPC Error parsing DataSet: <member><name>'+RTC_XMLRPC_DataSetFieldsName+'</name> already parsed, second Fields definition found.');

            FNames:=tRtcFastStrObjList.Create;

            obj:=ObjectFromXMLrpc(s,at);
            try
              if assigned(obj) and (obj.GetType<>rtc_Array) then
                raise Exception.Create('XML-RPC Error parsing Dataset: DataSet Fields definitions inside <array> expected, different data type found.');
              xarr:=TRtcArray(obj);
              for a:=0 to xarr.Count-1 do
                begin
                if xarr.isType[a]<>rtc_Record then
                  raise Exception.Create('XML-RPC Error parsing Dataset: Field definition inside <struc> expected, different data type found.');

                xrec:=xarr.asRecord[a];
                fname:=xrec.asString['Name'];
                FieldType[fname]:=StrToFieldType(xrec.asString['Type']);
                FieldSize[fname]:=xrec.asInteger['Size'];
                FieldRequired[fname]:=xrec.asBoolean['Required'];
                end;
            finally
              obj.Free;
              end;
            end
          else if s1=RTC_XMLRPC_DataSetRowsName then
            begin
            if assigned(FData) then
              raise Exception.Create('XML-RPC Error parsing DataSet: <member><name>'+RTC_XMLRPC_DataSetRowsName+'</name> already parsed, second Rows definition found.');

            obj:=ObjectFromXMLrpc(s,at);
            try
              if assigned(obj) and (obj.GetType<>rtc_Array) then
                raise Exception.Create('XML-RPC Error parsing Dataset: DataSet Fields definitions inside <array> expected, different data type found.');

              xarr:=TRtcArray(obj);
              FData:=xarr.FValues;
              xarr.FValues:=nil;
            finally
              obj.Free;
              end;
            end
          else
            raise Exception.Create('XML-RPC Error parsing DataSet: "'+RTC_XMLRPC_DataSetFieldsName+'" or "'+
                                   RTC_XMLRPC_DataSetRowsName+'" in <name> expected, but "'+s1+'" found.');

          if o_memb then
            xmlrpc_readTag(s,at,'/MEMBER');

          xtag:=UpperCase(xmlrpc_checkTag(s,at));
          o_memb:=xtag='MEMBER';

          until (xtag<>'MEMBER') and (xtag<>'NAME');
        end;

      if not assigned(FNames) then
        begin
        if not assigned(FData) then
          begin
          FData:=TList.Create;
          FNames:=tRtcFastStrObjList.Create;
          end
        else
          raise Exception.Create('XML-RPC Error parsing DataSet: Field definitions missing, tags <member><name>'+RTC_XMLRPC_DataSetRowsName+'</name> not found.');
        end
      else if not assigned(FData) then
        FData:=TList.Create;

    except
      if assigned(FData) then
        begin
        for idx:=0 to FData.Count-1 do
          begin
          obj:=TRtcValueObject(FData.Items[idx]);
          if assigned(obj) then obj.Free;
          end;
        FData.Free;
        FData:=nil;
        end;
      if assigned(FNames) then
        begin
        FNames.DestroyObjects;
        FNames.Free;
        FNames:=nil;
        end;
      raise;
      end;

    xmlrpc_skipValueClose(s,at,tags);
  finally
    SetLength(tags,0);
    end;
  end;

procedure TRtcDataSet.SetField(const FldName: string; FldType: TRtcFieldTypes; FldSize: integer; FldRequired: boolean);
  begin
  FieldType[FldName]:=FldType;
  FieldSize[FldName]:=FldSize;
  FieldRequired[FldName]:=FldRequired;
  end;

function TRtcDataSet.FieldByName(const index: string): TRtcValue;
  var
    idx:integer;
    myrow:TRtcArray;
    obj:TRtcValueObject;
  begin
  if assigned(FData) and assigned(FNames) and
     (FRow>=0) and (FRow<FData.Count) and
     assigned(FData.Items[FRow]) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then
      begin
      myrow:=TRtcArray(FData.Items[FRow]);
      obj:=myrow.asObject[idx];
      if obj=nil then
        begin
        obj:=TRtcValue.Create;
        myrow.asObject[idx]:=obj;
        end
      else if not (obj is TRtcValue) then
        begin
        obj:=TRtcValue.Create;
        TRtcValue(obj).asObject:=myrow.asObject[idx]; // store old reference
        myrow.asObject[idx]:=nil; // remove old reference
        myrow.asObject[idx]:=obj; // assign new reference
        end;
      Result:=TRtcValue(obj);
      end
    else
      begin
      obj:=TRtcValue.Create;
      asObject[index]:=obj;
      Result:=TRtcValue(obj);
      end;
    end
  else
    begin
    obj:=TRtcValue.Create;
    asObject[index]:=obj;
    Result:=TRtcValue(obj);
    end;
  end;

procedure TRtcDataSet.Extract(const index: String);
  var
    idx:integer;
    myrow:TRtcArray;
  begin
  if FRow<0 then
    raise Exception.Create('TRtcDataSet.SetObject: Row index beyond BOF.')
  else if index='' then
    raise Exception.Create('TRtcDataSet.SetObject: Fields without a name not allowed. Set Field Data by using a Field Name.');

  if assigned(FData) and assigned(FNames) then
    begin
    idx:=FNames.Find(index);
    if idx>=0 then // name found
      begin
      if (FRow>=0) and (FRow<FData.Count) then // row number exists
        begin
        myrow:=TRtcArray(FData.Items[FRow]);
        if assigned(myrow) then
          myrow.Extract(idx);
        end;
      end;
    end;
  end;

{ TRtcInfo }

constructor TRtcInfo.Create;
  begin
  inherited;
  ObjList:=nil;
  end;

procedure TRtcInfo.Clear;
  var
    index:String;
    ob:TObject;
  begin
  inherited;
  if assigned(ObjList) then
    begin
    while ObjList.Count>0 do
      begin
      index:=ObjList.search_min(ob);
      ObjList.remove(index);
      if ob is TRtcObject then
        TRtcObject(ob).Kill;
      end;
    ObjList.Free;
    ObjList:=nil;
    end;
  end;

function TRtcInfo.Get_Object(const index: String): TObject;
  begin
  if assigned(ObjList) then
    Result:=ObjList.search(UpperCase(index))
  else
    Result:=nil;
  end;

procedure TRtcInfo.Set_Object(const index: String; obj: TObject);
  var
    uindex:string;
  begin
  if not assigned(ObjList) then
    ObjList:=tStrObjList.Create(32);

  uindex:=UpperCase(index);
  if ObjList.search(uindex)<>nil then
    begin
    if Obj=nil then
      ObjList.remove(uindex)
    else
      ObjList.change(uindex,Obj);
    end
  else if obj<>nil then
    ObjList.insert(uindex, obj);
  end;

function TRtcInfo.Get_ChildInfo(const index: String): TRtcInfo;
  var
    o:TObject;
  begin
  o:=Get_Object(index);
  if (o<>nil) and (o is TRtcInfo) then
    Result:=TRtcInfo(o)
  else if AutoCreate then
    begin
    Result:=TRtcInfo.Create;
    Set_ChildInfo(index,Result);
    end
  else
    Result:=nil;
  end;

procedure TRtcInfo.Set_ChildInfo(const index: String; const _Value: TRtcInfo);
  begin
  Set_Object(index, _Value);
  end;

function TRtcInfo.Count: integer;
  begin
  Result:=inherited Count + ObjCount;
  end;

function TRtcInfo.ObjCount: integer;
  begin
  if assigned(ObjList) then
    Result:=ObjList.Count
  else
    Result:=0;
  end;

const
  CRLF=#13#10;

type
  TStringObject = class
    value:String;
    end;

  TUploadFileObject = class
    filename:string;
    start,count:longint;
    end;

procedure TRtcInfo.SetNil(const index: string);
  begin
  if Obj[index]<>nil then
    begin
    if Obj[index] is TRtcObject then
      TRtcObject(Obj[index]).Kill;
    Obj[index]:=nil;
    end;
  end;

function TRtcInfo.NewChild(const index: String): TRtcInfo;
  begin
  Result:=TRtcInfo.Create;
  Child[index]:=Result;
  end;

{ TRtcHttpHeader }

constructor TRtcHttpHeader.Create;
  begin
  inherited;
  FValues:=tRtcFastStrObjList.Create;
  FCookie:=nil;
  end;

destructor TRtcHttpHeader.Destroy;
  begin
  FValues.DestroyObjects;
  FValues.Free;
  if assigned(FCookie) then
    begin
    FCookie.Free;
    FCookie:=nil;
    end;
  inherited;
  end;

procedure TRtcHttpHeader.Clear;
  begin
  FValues.DestroyObjects;
  FValues.Clear;
  if assigned(FCookie) then
    begin
    FCookie.Free;
    FCookie:=nil;
    end;
  end;

function TRtcHttpHeader.GetHeader(const index: String): String;
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieName(index) then
    begin
    if not assigned(FCookie) then
      Result:=''
    else
      Result:=FCookie.Text;
    end
  else
    begin
    i:=FValues.Find(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        Result:=''
      else if obj is TStringObject then
        Result:=TStringObject(obj).Value
      else
        Result:='';
      end
    else
      Result:='';
    end;
  end;

procedure TRtcHttpHeader.SetHeader(const index: String; const Value: String);
  var
    i:integer;
    obj:TObject;
  begin
  if isCookieName(index) then
    Cookie.Text:=Value
  else
    begin
    i:=FValues.Find(index);
    if i>=0 then
      begin
      obj:=FValues.Objects[i];
      if not assigned(obj) then
        begin
        obj:=TStringObject.Create;
        TStringObject(obj).Value:=Value;
        FValues.Objects[i]:=obj;
        end
      else
        begin
        if not (obj is TStringObject) then
          begin
          obj.Free;
          obj:=TStringObject.Create;
          FValues.Objects[i]:=obj;
          end;
        TStringObject(obj).Value:=Value;
        end;
      end
    else
      begin
      obj:=TStringObject.Create;
      TStringObject(obj).value:=Value;
      FValues.Add(index, obj);
      end;
    end;
  end;

function TRtcHttpHeader.GetHeaderCount: integer;
  begin
  if assigned(FCookie) and (FCookie.Text<>'') then
    Result:=FValues.Count+1
  else
    Result:=FValues.Count;
  end;

function TRtcHttpHeader.GetIHeader(index: integer): String;
  var
    obj:TObject;
  begin
  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringObject then
      Result:=TStringObject(obj).value
    else
      Result:='';
    end
  else if (index=FValues.Count) and assigned(FCookie) then
    Result:=FCookie.Text
  else
    Result:='';
  end;

procedure TRtcHttpHeader.SetIHeader(index: integer; const Value: String);
  var
    obj:TObject;
  begin
  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      begin
      obj:=TStringObject.Create;
      TStringObject(obj).Value:=Value;
      FValues.Objects[index]:=obj;
      end
    else
      begin
      if not (obj is TStringObject) then
        begin
        obj.Free;
        obj:=TStringObject.Create;
        FValues.Objects[index]:=obj;
        end;
      TStringObject(obj).value:=Value;
      end;
    end
  else if (index=FValues.Count) and assigned(FCookie) then
    Cookie.Text:=Value;
  end;

function TRtcHttpHeader.GetIHeaderName(index: integer): String;
  begin
  if index<FValues.Count then
    Result:=FValues.Strings[index]
  else if (index=FValues.Count) and assigned(FCookie) then
    Result:=GetCookieName
  else
    Result:='';
  end;

procedure TRtcHttpHeader.SetIHeaderName(index: integer; const Value: String);
  begin
  if index<FValues.Count then
    FValues.Strings[index]:=Value;
  end;

function TRtcHttpHeader.GetHeaderText: String;
  var
    a:integer;
  begin
  Result:='';
  for a:=0 to ItemCount-1 do
    if ItemValue[a]<>'' then
      Result:=Result+ItemName[a]+': '+ItemValue[a]+CRLF;
  end;

function TRtcHttpHeader.GetContentLength: int64;
  var
    s:String;
  begin
  s:=Value['CONTENT-LENGTH'];
  if s='' then
    Result:=0
  else
    Result:=StrToInt64(Trim(s));
  end;

procedure TRtcHttpHeader.SetContentLength(const _Value: int64);
  begin
  Value['CONTENT-LENGTH']:=IntToStr(_Value);
  end;

function TRtcHttpHeader.GetContentType: String;
  begin
  Result:=Value['CONTENT-TYPE'];
  end;

procedure TRtcHttpHeader.SetContentType(const _Value: String);
  begin
  Value['CONTENT-TYPE']:=_Value;
  end;

procedure TRtcHttpHeader.SetHeaderText(const _Value: String);
  var
    MyPos:integer;
    StatusLine,
    HeadStr,
    left:String;
  begin
  if _Value='' then Exit;

  HeadStr:=_Value+#10;

  // Scan for all header attributes ...
  MyPos:=Pos(#10, HeadStr);
  while (MyPos>1) do // at least 1 character inside line
    begin
    if HeadStr[MyPos-1]=#13 then
      StatusLine:=Copy(HeadStr,1,MyPos-2)
    else
      StatusLine:=Copy(HeadStr,1,MyPos-1);

    Delete(HeadStr,1,MyPos);

    MyPos:=Pos(':',StatusLine);
    if MyPos>0 then
      begin
      left:=TrimCopy(StatusLine,1,MyPos-1);
      Delete(StatusLine,1,MyPos);
      StatusLine:=Trim(StatusLine);
      Value[left]:=StatusLine;
      end;
    MyPos:=Pos(#10, HeadStr);
    end;
  end;

function TRtcHttpHeader.GetCookie: TRtcHttpValues;
  begin
  if not assigned(FCookie) then
    FCookie:=TRtcHttpValues.Create;
  Result:=FCookie;
  end;

function TRtcHttpHeader.isCookieName(const Value: String): boolean;
  begin
  Result:=CompareText(Value,GetCookieName)=0;
  end;

{ TRtcRequest }

constructor TRtcRequest.Create;
  begin
  inherited;
  FInfo:=nil;
  FQuery:=nil;
  FParams:=nil;
  end;

destructor TRtcRequest.Destroy;
  begin
  FMethod:='';
  FFileName:='';
  FFullName:='';
  if assigned(FInfo) then
    begin
    FInfo.Free;
    FInfo:=nil;
    end;
  if assigned(FQuery) then
    begin
    FQuery.Free;
    FQuery:=nil;
    end;
  if assigned(FParams) then
    begin
    FParams.Free;
    FParams:=nil;
    end;
  inherited;
  end;

procedure TRtcRequest.Clear;
  begin
  inherited;
  if assigned(FInfo) then
    begin
    FInfo.Free;
    FInfo:=nil;
    end;
  if assigned(FQuery) then
    begin
    FQuery.Free;
    FQuery:=nil;
    end;
  if assigned(FParams) then
    begin
    FParams.Free;
    FParams:=nil;
    end;
  FMethod:='';
  FFileName:='';
  FFullName:='';
  FClose:=False;
  end;

function TRtcRequest.GetRequestAgent: String;
  begin
  Result:=Value['USER-AGENT'];
  end;

function TRtcRequest.GetRequestHost: String;
  begin
  Result:=Value['HOST'];
  end;

function TRtcRequest.GetRequestReferer: String;
  begin
  Result:=Value['REFERER'];
  end;

procedure TRtcRequest.SetRequestAgent(const _Value: String);
  begin
  Value['USER-AGENT']:=_Value;
  end;

procedure TRtcRequest.SetRequestHost(const _Value: String);
  begin
  Value['HOST']:=_Value;
  end;

procedure TRtcRequest.SetRequestReferer(const _Value: String);
  begin
  Value['REFERER']:=_Value;
  end;

function TRtcRequest.GetURI: String;
  begin
  if Query.Text<>'' then
    Result:=FFullName+'?'+Query.Text
  else
    Result:=FFullName;
  end;

procedure TRtcRequest.SetURI(const _Value: String);
  var
    i:integer;
  begin
  i:=Pos('?',_Value);
  if i<=0 then
    begin
    if FileName='' then
      FileName:=_Value
    else
      FFullName:=_Value;
    Query.Clear;
    end
  else
    begin
    if FileName='' then
      FileName:=Copy(_Value,1,i-1)
    else
      FFullName:=Copy(_Value,1,i-1);
    Query.Text:=Copy(_Value,i+1,length(_Value)-i);
    end;
  end;

function TRtcRequest.GetURL: String;
  begin
  Result:=Host+URI;
  end;

procedure TRtcRequest.SetHeaderText(const _Value: String);
  var
    MyPos:integer;
    StatusLine,
    HeadStr:String;
  begin
  if _Value='' then Exit;

  HeadStr:=_Value+CRLF;

  // scan HTTP Method, FileName and Params
  MyPos:=Pos(CRLF,HeadStr);
  StatusLine:=Copy(HeadStr,1,MyPos-1);
  if (MyPos>8) and
     ((Copy(StatusLine,MyPos-8,7)='HTTP/1.') or
      (Copy(StatusLine,MyPos-9,8)='HTTPS/1.')) then
    begin
    Delete(HeadStr,1,MyPos+Length(CRLF)-1);

    { Our line probably looks like this:
      GET /xyz HTTP/1.1 }
    MyPos:=Pos(' ',StatusLine); // first space before FileName
    if MyPos>0 then
      begin
      Method:=Copy(StatusLine,1,MyPos-1); // Request Method
      Delete(StatusLine,1,MyPos);

      MyPos:=Pos(' ',StatusLine); // space after FileName
      if MyPos>0 then
        begin
        URI:=Copy(StatusLine,1,MyPos-1); // Request URI
        Delete(StatusLine,1,MyPos); // StatusText
        end;
      end;
    end;

  inherited SetHeaderText(HeadStr);
  end;

function TRtcRequest.GetMethod: String;
  begin
  Result:=FMethod;
  end;

procedure TRtcRequest.SetMethod(const _Value: String);
  begin
  if FMethod<>_Value then
    FMethod:=UpperCase(_Value);
  end;

function TRtcRequest.GetCookieName: String;
  begin
  Result:='COOKIE';
  end;

function TRtcRequest.GetParams: TRtcHttpValues;
  begin
  if not assigned(FParams) then
    FParams:=TRtcHttpValues.Create;
  Result:=FParams;
  end;

function TRtcRequest.GetQuery: TRtcHttpValues;
  begin
  if not assigned(FQuery) then
    FQuery:=TRtcHttpValues.Create;
  Result:=FQuery;
  end;

function TRtcRequest.GetInfo: TRtcInfo;
  begin
  if not assigned(FInfo) then
    FInfo:=TRtcInfo.Create;
  Result:=FInfo;
  end;

procedure TRtcRequest.SetFileName(const _Value: String);
  begin
  if FFileName=FFullName then
    begin
    FFileName := _Value;
    FFullName := _Value;
    end
  else
    FFileName:=_Value;
  end;

function TRtcRequest.GetForwardedFor: String;
  begin
  Result:=Value['X-FORWARDED-FOR'];
  end;

procedure TRtcRequest.SetForwardedFor(const _Value: String);
  begin
  Value['X-FORWARDED-FOR']:=_Value;
  end;

{ TRtcResponse }

constructor TRtcResponse.Create;
  begin
  inherited;
  end;

destructor TRtcResponse.Destroy;
  begin
  inherited;
  end;

procedure TRtcResponse.Clear;
  begin
  inherited;
  end;

function TRtcResponse.GetCookieName: String;
  begin
  Result:='SET-COOKIE';
  end;

procedure TRtcResponse.SetHeaderText(const _Value: String);
  var
    MyPos:integer;
    StatusLine,
    HeadStr,
    left:String;
  begin
  if _Value='' then Exit;

  HeadStr:=_Value+CRLF;

  // scan HTTP status code and text
  if (CompareText(Copy(HeadStr,1,5),'HTTP/')=0) or
     (CompareText(Copy(HeadStr,1,6),'HTTPS/')=0) then
    begin
    MyPos:=Pos(CRLF,HeadStr);
    StatusLine:=Copy(HeadStr,1,MyPos-1);
    Delete(HeadStr,1,MyPos+Length(CRLF)-1);

    { Our line probably looks like this:
      HTTP/1.1 200 OK }
    MyPos:=Pos(' ',StatusLine); // first space before StatusCode
    if MyPos>0 then
      begin
      StatusCode:=0;
      StatusText:='';

      Delete(StatusLine,1,MyPos);

      MyPos:=Pos(' ',StatusLine); // space after StatusCode
      if MyPos>0 then
        begin
        left:=Copy(StatusLine,1,MyPos-1); // StatusCode
        Delete(StatusLine,1,MyPos); // StatusText

        if (left<>'') and (StatusLine<>'') then
          begin
          try
            StatusCode:=StrToInt64(left);
            StatusText:=StatusLine;
          except
            // if there is something wrong with this, just ignore the exception
            end;
          end;
        end;
      end;
    end;

  inherited SetHeaderText(HeadStr);
  end;

{ TRtcHttpValues }

constructor TRtcHttpValues.Create;
  begin
  inherited;
  FValChange:=False;
  FTxtChange:=False;
  FCacheSize:=0;
  FTempFileName:='';
  FTempFileSize:=0;
  FValues:=tRtcFastStrObjList.Create;
  FOrigQuery:='';
  FDelimiter:='';
  end;

destructor TRtcHttpValues.Destroy;
  begin
  FDelimiter:='';
  FOrigQuery:='';
  FValues.DestroyObjects;
  FValues.Free;
  if FTempFileName<>'' then
    begin
    Delete_File(FTempFileName);
    FTempFileName:='';
    FTempFileSize:=0;
    end;
  inherited;
  end;

procedure TRtcHttpValues.Clear;
  begin
  FTxtChange:=False;
  FValChange:=False;

  if FTempFileName<>'' then
    Delete_File(FTempFileName);
  FTempFileSize:=0;
  FTempFileName:='';
  FDelimiter:='';
  FOrigQuery:='';

  FValues.DestroyObjects;
  FValues.Clear;
  end;

function TRtcHttpValues.GetItemCount: integer;
  begin
  PrepareValues;

  Result:=FValues.Count;
  end;

function TRtcHttpValues.GetItemName(index: integer): String;
  begin
  PrepareValues;

  if index<FValues.Count then
    Result:=FValues.Strings[index]
  else
    Result:='';
  end;

function TRtcHttpValues.GetItemValue(index: integer): String;
  var
    obj:TObject;
  begin
  PrepareValues;

  if index<FValues.Count then
    begin
    obj:=FValues.objects[index];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringObject then
      Result:=TStringObject(obj).value
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

function TRtcHttpValues.GetValue(const index: String): String;
  var
    i:integer;
    obj:TObject;
  begin
  PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    obj:=FValues.objects[i];
    if not assigned(obj) then
      Result:=''
    else if obj is TStringObject then
      Result:=TStringObject(obj).value
    else if obj is TUploadFileObject then
      Result:=TUploadFileObject(obj).filename
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else
    Result:='';
  end;

procedure TRtcHttpValues.SetItemName(index: integer; const Value: String);
  begin
  PrepareValues;

  if index<FValues.Count then
    if Value<>FValues.Strings[index] then
      begin
      FValues.Strings[index]:=Value;
      FValChange:=True;
      end;
  end;

procedure TRtcHttpValues.SetItemValue(index: integer; const Value: String);
  var
    obj:TObject;
  begin
  PrepareValues;

  if index<FValues.Count then
    begin
    obj:=FValues.Objects[index];
    if not assigned(obj) then
      begin
      if Value<>'' then
        begin
        obj:=TStringObject.Create;
        TStringObject(obj).Value:=Value;
        FValues.Objects[index]:=obj;
        FValChange:=True;
        end;
      end
    else if obj is TStringObject then
      begin
      if TStringObject(obj).Value<>Value then
        begin
        if Value='' then
          begin
          obj.Free;
          FValues.Objects[index]:=nil; // Delete(i);
          end
        else
          TStringObject(obj).value:=Value;
        FValChange:=True;
        end;
      end
    else if obj is TUploadFileObject then
      begin
      if TUploadFileObject(obj).filename<>Value then
        begin
        TUploadFileObject(obj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

procedure TRtcHttpValues.SetValue(const index: String; const Value: String);
  var
    i:integer;
    vobj:TObject;
    obj:TStringObject;
  begin
  PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    vobj:=FValues.Objects[i];
    if not assigned(vobj) then
      begin
      if Value<>'' then
        begin
        vobj:=TStringObject.Create;
        TStringObject(vobj).value:=Value;
        FValues.Objects[i]:=vobj;
        FValChange:=True;
        end;
      end
    else if vobj is TStringObject then
      begin
      if TStringObject(vobj).Value<>Value then
        begin
        if Value='' then
          begin
          vobj.Free;
          FValues.Objects[i]:=nil; // Delete(i);
          end
        else
          TStringObject(vobj).value:=Value;
        FValChange:=True;
        end;
      end
    else if vobj is TUploadFileObject then
      begin
      if TUploadFileObject(vobj).filename<>Value then
        begin
        if Value='' then
          begin
          vobj.Free;
          FValues.Objects[i]:=nil; // Delete(i);
          end
        else
          TUploadFileObject(vobj).filename:=Value;
        FValChange:=True;
        end;
      end
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end
  else if Value<>'' then
    begin
    obj:=TStringObject.Create;
    obj.Value:=Value;
    FValues.Add(index, obj);
    FValChange:=True;
    end;
  end;

function TRtcHttpValues.GetDelimiter: String;
  begin
  PrepareValues;

  if FDelimiter<>'' then
    Result:=FDelimiter
  else
    Result:='&';
  end;

procedure TRtcHttpValues.SetDelimiter(const Value: String);
  begin
  PrepareValues;

  if Value<>FDelimiter then
    begin
    FDelimiter:=Value;
    FValChange:=True;
    end;
  end;

function TRtcHttpValues.GetAsText: String;
  begin
  PrepareText;

  if FTempFileName<>'' then
    Result:=Read_File(FTempFileName)
  else
    Result:=FOrigQuery;
  end;

procedure TRtcHttpValues.SetText(const _Value: String);
  begin
  Clear;
  AddText(_Value);
  end;

procedure TRtcHttpValues.AddText(const s: string);
  begin
  PrepareText;

  if FTempFileName<>'' then
    begin
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else if ((FCacheSize=0) and (length(FOrigQuery)+length(s)>=RTC_FORMPOST_CACHESIZE)) then
    begin
    FTempFileName:=GetTempFile;
    if FTempFileName='' then
      raise Exception.Create('Error creating a temporary file.');

    if length(FOrigQuery)>0 then
      begin
      Write_File(FTempFileName,FOrigQuery,FTempFileSize);
      Inc(FTempFileSize,length(FOrigQuery));
      FOrigQuery:='';
      end;
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else if ((FCacheSize>0) and (length(FOrigQuery)+length(s)>=FCacheSize)) then
    begin
    FTempFileName:=GetTempFile;
    if FTempFileName='' then
      raise Exception.Create('Error creating a temporary file.');

    if length(FOrigQuery)>0 then
      begin
      Write_File(FTempFileName,FOrigQuery,FTempFileSize);
      Inc(FTempFileSize,length(FOrigQuery));
      FOrigQuery:='';
      end;
    Write_File(FTempFileName,s,FTempFileSize);
    Inc(FTempFileSize,length(s));
    end
  else
    FOrigQuery:=FOrigQuery+s;

  FTxtChange:=True;
  end;

procedure TRtcHttpValues.PrepareText;
  var
    a:integer;
  begin
  if not FValChange then Exit;
  FValChange:=False;

  if ItemCount=0 then
    FOrigQuery:=''
  else if length(Delimiter)>1 then
    begin
    FOrigQuery:='';
    for a:=0 to ItemCount-1 do
      FOrigQuery:=FOrigQuery+
              '--'+Delimiter+CRLF+
              'Content-Disposition: form-data; name="'+ItemName[a]+'"'+CRLF+
              CRLF+
              ItemValue[a]+CRLF;
    FOrigQuery:=FOrigQuery+'--'+Delimiter+'--'+CRLF;
    end
  else
    begin
    FOrigQuery:=ItemName[0]+'='+ItemValue[0];
    if ItemCount>1 then
      for a:=1 to ItemCount-1 do
        FOrigQuery:=FOrigQuery+Delimiter+ItemName[a]+'='+ItemValue[a];
    end;
  end;

procedure TRtcHttpValues.PrepareValues;
  const
    BUFFER:integer=32000;

  var
    MyPos,MyPos2:integer;

    HeadStr,
    StatusLine,
    left,right:String;

    at,i:integer;
    obj:TStringObject;
    obj2:TUploadFileObject;

  begin
  if not FTxtChange then Exit;
  FTxtChange:=False;

  if (FOrigQuery='') and (FTempFileName='') then
    begin
    Clear;
    Exit;
    end;

  if (length(FDelimiter)<=1) and (FTempFileName<>'') then
    begin
    FOrigQuery:=Read_File(FTempFileName);
    FTempFileName:='';
    FTempFileSize:=0;
    end;

  if (FTempFileName<>'') then
    begin // MULTIPART/FORM-DATA
    // Jump to first Delimiter (ignore preamble)
    at:=Scan_File(FTempFileName,Delimiter,BUFFER,0,FTempFileSize)-2;
    if at>=0 then while Read_File(FTempFileName,at+2,length(Delimiter))=Delimiter do
      begin
      HeadStr:=Read_File(FTempFileName,at,2+length(Delimiter)+2);
      if length(HeadStr)<4+length(Delimiter) then
        Break; // end of file

      if Copy(HeadStr,1,2)<>'--' then
        raise Exception.Create('Missing "--" at the beginning of a new Multipart data');
      Inc(at,2);

      // skip delimiter
      Inc(at,length(Delimiter));
      if Copy(HeadStr,2+length(Delimiter)+1,2)='--' then
        Break
      else if Copy(HeadStr,2+length(Delimiter)+1,2)<>CRLF then
        raise Exception.Create('Missing <CRLF> after Delimiter in Multipart text');
      Inc(at,2);

      // Split Content-disposition line from the rest
      MyPos:=Scan_File(FTempFileName,CRLF,BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise Exception.Create('Missing <CRLF> in Multipart text');
      StatusLine:=Read_File(FTempFileName,at,MyPos-at);
      at:=MyPos+2;

      if StatusLine<>'' then
        begin
        // Remove "Content-disposition:"
        MyPos:=Pos(':',StatusLine);
        if MyPos<=0 then
          raise Exception.Create('Missing ":" (after "Content-disposition"?) in Multipart text');
        if CompareText(TrimCopy(StatusLine,1,MyPos-1),'CONTENT-DISPOSITION')<>0 then
          raise Exception.Create('Missing "Content-disposition" in Multipart data');
        Delete(StatusLine,1,MyPos);

        // Remove "FORM-DATA;"
        MyPos:=Pos(';',StatusLine);
        if MyPos<=0 then
          raise Exception.Create('Missing ";" (after "form-data"?) in Multipart text.');
        if CompareText(TrimCopy(StatusLine,1,MyPos-1),'FORM-DATA')<>0 then
          raise Exception.Create('Invalid disposition type "'+Copy(StatusLine,1,MyPos-1)+'", expecting "FORM-DATA" in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove "name="
        MyPos:=Pos('=',StatusLine);
        if MyPos<=0 then
          raise Exception.Create('Missing "=" (after "name"?) in Multipart text.');
        if CompareText(TrimCopy(StatusLine,1,MyPos-1),'NAME')<>0 then
          raise Exception.Create('Invalid disposition param "'+Copy(StatusLine,1,MyPos-1)+'", expecting "NAME" in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove opening <">
        MyPos:=Pos('"',StatusLine);
        if MyPos<=0 then
          raise Exception.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
        Delete(StatusLine,1,MyPos);

        // Remove closing <">, get the parameter name and check if rest is clear
        MyPos:=Pos('"',StatusLine);
        if MyPos<=0 then
          raise Exception.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

        left:=Copy(StatusLine,1,MyPos-1);

        Delete(StatusLine,1,MyPos);
        StatusLine:=Trim(StatusLine);
        if StatusLine<>'' then
          begin
          // Remove ";" before "filename="
          MyPos:=Pos(';',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Error. Data after content disposition param: "'+StatusLine+'".');
          Delete(StatusLine,1,MyPos);

          // Remove "filename="
          MyPos:=Pos('=',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Missing "=" (after "filename"?) in Multipart text.');
          if CompareText(TrimCopy(StatusLine,1,MyPos-1),'FILENAME')<>0 then
            raise Exception.Create('Invalid disposition param "'+Copy(StatusLine,1,MyPos-1)+'", expecting "FILENAME" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove opening <">
          MyPos:=Pos('"',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove closing <">, get the parameter name and check if rest is clear
          MyPos:=Pos('"',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

          right:=Copy(StatusLine,1,MyPos-1);

          Delete(StatusLine,1,MyPos);
          StatusLine:=Trim(StatusLine);

          if StatusLine<>'' then
            raise Exception.Create('Error. Data after content disposition param: "'+StatusLine+'".');
          end
        else
          right:='';
        end
      else
        raise Exception.Create('Missing field name in Multipart text.');

      StatusLine:=Read_File(FTempFileName,at,2+length(Delimiter));
      if StatusLine='--'+Delimiter then
        Continue;

      // Split Content-disposition line from the rest
      MyPos:=Scan_File(FTempFileName,CRLF,BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise Exception.Create('Missing <CRLF> after content disposition param.');

      while MyPos>at do
        begin
        at:=MyPos+2;
        StatusLine:=Read_File(FTempFileName,at,2+length(Delimiter));
        if StatusLine='--'+Delimiter then
          begin
          MyPos:=-1;
          Break;
          end
        else
          begin
          MyPos:=Scan_File(FTempFileName,CRLF,BUFFER,at,FTempFileSize);
          if MyPos<=0 then
            raise Exception.Create('Missing <CRLF> after content header.');
          end;
        end;

      if MyPos<0 then
        Continue;

      Inc(at,2);
      // Split content data part from the rest
      StatusLine:=Read_File(FTempFileName,at,2+length(Delimiter));
      if StatusLine='--'+Delimiter then
        Continue;

      MyPos:=Scan_File(FTempFileName,CRLF+'--'+Delimiter,BUFFER,at,FTempFileSize);
      if MyPos<0 then
        raise Exception.Create('Delimiter missing after Multipart data.');

      if right='' then
        begin
        StatusLine:=Read_File(FTempFileName,at,MyPos-at);
        at:=MyPos+2;

        // Save
        i:=FValues.Find(left);
        if i>=0 then
          begin
          if not assigned(FValues.Objects[i]) then
            begin
            obj:=TStringObject.Create;
            obj.value:=StatusLine;
            FValues.Objects[i]:=obj;
            end
          else if FValues.Objects[i] is TStringObject then
            begin
            obj:=TStringObject(FValues.Objects[i]);
            obj.value:=StatusLine;
            end
          else
            raise Exception.Create('Duplicate value for "'+left+'" in Multipart text');
          end
        else
          begin
          obj:=TStringObject.Create;
          obj.value:=StatusLine;
          FValues.Add(left, obj);
          end;
        end
      else
        begin
        // Save
        i:=FValues.Find(left);
        if i>=0 then
          begin
          if FValues.Objects[i]=nil then
            begin
            obj2:=TUploadFileObject.Create;
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            FValues.Objects[i]:=obj2;
            end
          else if FValues.Objects[i] is TUploadFileObject then
            begin
            obj2:=TUploadFileObject(FValues.Objects[i]);
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            end
          else
            raise Exception.Create('Duplicate value for "'+left+'" in Multipart text');
          end
        else
          begin
          obj2:=TUploadFileObject.Create;
          obj2.filename:=right;
          obj2.start:=at;
          obj2.count:=MyPos-at;
          FValues.Add(left, obj2);
          end;
        at:=MyPos+2;
        end;
      end;
    end
  else
    begin
    if FDelimiter='' then // Try to recognize the delimiter used in the Query
      begin
      MyPos:=Pos(';',FOrigQuery);
      MyPos2:=Pos('&',FOrigQuery);
      if (MyPos>0) and (MyPos2>0) then
        begin
        if MyPos2<MyPos then
          FDelimiter:='&'
        else
          FDelimiter:=';';
        end
      else if (MyPos>0) then
        FDelimiter:=';'
      else if (MyPos2>0) then
        FDelimiter:='&';
      end;

    if length(Delimiter)>1 then
      begin // MULTIPART/FORM-DATA

      // Jump to first Delmiter (ignore preamble)
      at:=Pos(Delimiter,FOrigQuery)-2;
      if at>0 then while Copy(FOrigQuery,at+2,length(Delimiter))=Delimiter do
        begin
        if at+length(Delimiter)+2>=length(FOrigQuery) then
          Break; // no more data

        if Copy(FOrigQuery,at,2)<>'--' then
          raise Exception.Create('Missing "--" at the beginning of a new Multipart data');
        Inc(at,2);

        // skip delimiter
        Inc(at,length(Delimiter));
        if Copy(FOrigQuery,at,2)='--' then
          Break
        else if Copy(FOrigQuery,at,2)<>CRLF then
          raise Exception.Create('Missing <CRLF> after Delimiter in Multipart text');
        Inc(at,2);

        // Split Content-disposition line from the rest
        MyPos:=PosEx2(CRLF,FOrigQuery,at);
        if MyPos<=0 then
          raise Exception.Create('Missing <CRLF> in Multipart text');
        StatusLine:=Copy(FOrigQuery,at,MyPos-at);
        at:=MyPos+2;

        if StatusLine<>'' then
          begin
          // Remove "Content-disposition:"
          MyPos:=Pos(':',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Missing ":" (after "Content-disposition"?) in Multipart text');
          if CompareText(TrimCopy(StatusLine,1,MyPos-1),'CONTENT-DISPOSITION')<>0 then
            raise Exception.Create('Missing "Content-disposition" in Multipart data');
          Delete(StatusLine,1,MyPos);

          // Remove "FORM-DATA;"
          MyPos:=Pos(';',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Missing ";" (after "form-data"?) in Multipart text.');
          if CompareText(TrimCopy(StatusLine,1,MyPos-1),'FORM-DATA')<>0 then
            raise Exception.Create('Invalid disposition type "'+Copy(StatusLine,1,MyPos-1)+'", expecting "FORM-DATA" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove "name="
          MyPos:=Pos('=',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Missing "=" (after "name"?) in Multipart text.');
          if CompareText(TrimCopy(StatusLine,1,MyPos-1),'NAME')<>0 then
            raise Exception.Create('Invalid disposition param "'+Copy(StatusLine,1,MyPos-1)+'", expecting "NAME" in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove opening <">
          MyPos:=Pos('"',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
          Delete(StatusLine,1,MyPos);

          // Remove closing <">, get the parameter name and check if rest is clear
          MyPos:=Pos('"',StatusLine);
          if MyPos<=0 then
            raise Exception.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

          left:=Copy(StatusLine,1,MyPos-1);

          Delete(StatusLine,1,MyPos);
          StatusLine:=Trim(StatusLine);
          if StatusLine<>'' then
            begin
            // Remove ";" before "filename="
            MyPos:=Pos(';',StatusLine);
            if MyPos<=0 then
              raise Exception.Create('Error. Data after content disposition param: "'+StatusLine+'".');
            Delete(StatusLine,1,MyPos);

            // Remove "filename="
            MyPos:=Pos('=',StatusLine);
            if MyPos<=0 then
              raise Exception.Create('Missing "=" (after "filename"?) in Multipart text.');
            if CompareText(TrimCopy(StatusLine,1,MyPos-1),'FILENAME')<>0 then
              raise Exception.Create('Invalid disposition param "'+Copy(StatusLine,1,MyPos-1)+'", expecting "FILENAME" in Multipart text');
            Delete(StatusLine,1,MyPos);

            // Remove opening <">
            MyPos:=Pos('"',StatusLine);
            if MyPos<=0 then
              raise Exception.Create('Parameter names have to be in quotes. Missing opening <"> in Multipart text');
            Delete(StatusLine,1,MyPos);

            // Remove closing <">, get the parameter name and check if rest is clear
            MyPos:=Pos('"',StatusLine);
            if MyPos<=0 then
              raise Exception.Create('Parameter names have to be in quotes. Missing closing <"> in Multipart text');

            right:=Copy(StatusLine,1,MyPos-1);

            Delete(StatusLine,1,MyPos);
            StatusLine:=Trim(StatusLine);

            if StatusLine<>'' then
              raise Exception.Create('Error. Data after content disposition param: "'+StatusLine+'".');
            end
          else
            right:='';
          end
        else
          raise Exception.Create('Missing field name in Multipart text.');

        StatusLine:=Copy(FOrigQuery,at,2+length(Delimiter));
        if StatusLine='--'+Delimiter then
          Continue;

        // Split Content-disposition line from the rest
        MyPos:=PosEx2(CRLF, FOrigQuery, at);
        if MyPos<=0 then
          raise Exception.Create('Missing <CRLF> after content disposition param.');

        while MyPos>at do
          begin
          at:=MyPos+2;
          StatusLine:=Copy(FOrigQuery,at,2+length(Delimiter));
          if StatusLine='--'+Delimiter then
            begin
            MyPos:=-1;
            Break;
            end
          else
            begin
            MyPos:=PosEx2(CRLF,FOrigQuery,at);
            if MyPos<=0 then
              raise Exception.Create('Missing <CRLF> after content header.');
            end;
          end;

        if MyPos<0 then
          Continue;

        Inc(at,2);
        // Split content data part from the rest
        StatusLine:=Copy(FOrigQuery,at,4+length(Delimiter));
        if StatusLine='--'+Delimiter then
          Continue;

        MyPos:=PosEx2(CRLF+'--'+Delimiter, FOrigQuery, at);
        if MyPos<=0 then
          raise Exception.Create('Delimiter missing after Multipart data.');

        if right='' then
          begin
          StatusLine:=Copy(FOrigQuery,at,MyPos-at);
          at:=MyPos+2;

          // Save
          i:=FValues.Find(left);
          if i>=0 then
            begin
            if not assigned(FValues.Objects[i]) then
              begin
              obj:=TStringObject.Create;
              obj.value:=StatusLine;
              FValues.Objects[i]:=obj;
              end
            else if FValues.Objects[i] is TStringObject then
              begin
              obj:=TStringObject(FValues.Objects[i]);
              obj.value:=StatusLine;
              end
            else
              raise Exception.Create('Duplicate value for "'+left+'" in Multipart text');
            end
          else
            begin
            obj:=TStringObject.Create;
            obj.value:=StatusLine;
            FValues.Add(left,obj);
            end;
          end
        else
          begin
          // Save
          i:=FValues.Find(left);
          if i>=0 then
            begin
            if FValues.Objects[i]=nil then
              begin
              obj2:=TUploadFileObject.Create;
              obj2.filename:=right;
              obj2.start:=at;
              obj2.count:=MyPos-at;
              FValues.Objects[i]:=obj2;
              end
            else if FValues.Objects[i] is TUploadFileObject then
              begin
              obj2:=TUploadFileObject(FValues.Objects[i]);
              obj2.filename:=right;
              obj2.start:=at;
              obj2.count:=MyPos-at;
              end
            else
              raise Exception.Create('Duplicate value for "'+left+'" in Multipart text');
            end
          else
            begin
            obj2:=TUploadFileObject.Create;
            obj2.filename:=right;
            obj2.start:=at;
            obj2.count:=MyPos-at;
            FValues.Add(left,obj2);
            end;
          at:=MyPos+2;
          end;
        end;
      // ignore epilogue
      end
    else // URLEncoded
      begin
      HeadStr:=FOrigQuery+Delimiter;
      // Scan for all header attributes ...
      MyPos:=Pos(Delimiter, HeadStr);
      while (MyPos>=1) do // at least 1 character inside line
        begin
        if MyPos=1 then
          Delete(HeadStr,1,length(Delimiter))
        else
          begin
          StatusLine:=Copy(HeadStr,1,MyPos-1);
          Delete(HeadStr,1,MyPos+Length(Delimiter)-1);

          MyPos:=Pos('=',StatusLine);
          if MyPos>0 then
            begin
            left:=TrimCopy(StatusLine,1,MyPos-1);

            Delete(StatusLine,1,MyPos);
            // Save
            i:=FValues.Find(left);
            if i>=0 then
              begin
              if not assigned(FValues.Objects[i]) then
                begin
                obj:=TStringObject.Create;
                FValues.Objects[i]:=obj;
                end
              else
                obj:=TStringObject(FValues.Objects[i]);
              obj.value:=StatusLine;
              // raise Exception.Create('Duplicate value for "'+left+'" in URLEncoded text');
              end
            else
              begin
              obj:=TStringObject.Create;
              obj.Value:=StatusLine;
              FValues.Add(left, obj);
              end;
            end;
          end;
        MyPos:=Pos(Delimiter, HeadStr);
        end;
      end;
    end;
  end;

function TRtcHttpValues.GetFile(const index:string; const LocalFileName: string): boolean;
  var
    i,start,count:integer;
  begin
  Result:=False;
  PrepareValues;

  i:=FValues.Find(index);
  if i>=0 then
    begin
    if not assigned(FValues.Objects[i]) then
      begin
      Delete_File(LocalFileName);
      end
    else if FValues.objects[i] is TStringObject then
      begin
      Delete_File(LocalFileName);
      Write_File(LocalFileName, TStringObject(FValues.Objects[i]).Value);
      end
    else if FValues.Objects[i] is TUploadFileObject then
      begin
      Delete_File(LocalFileName);

      start:=TUploadFileObject(FValues.Objects[i]).start;
      count:=TUploadFileObject(FValues.Objects[i]).count;

      if FTempFileName<>'' then
        Write_File(LocalFileName, Read_File(FTempFileName,start,count) )
      else
        Write_File(LocalFileName, Copy(FOrigQuery,start,count));

      Result:=True;
      end
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

function TRtcHttpValues.IsFile(const index: string): Boolean;
  var
    i: Integer;
  begin
  Result := False;
  i := FValues.Find(index);
  if i >= 0 then
    Result := FValues.Objects[i] is TUploadFileObject;
  end;

function TRtcHttpValues.GetFile(const index:string; stream:TStream):boolean;
  var
    i, Start, Count: Integer;
  begin
  Result := False;
  PrepareValues;

  i := FValues.Find(index);
  if i >= 0 then
    begin
    if not Assigned(FValues.Objects[i]) then
      begin
      // Do nothing
      end
    else if FValues.Objects[i] is TStringObject then
      begin
      Count := Length(TStringObject(FValues.Objects[i]).Value);
      stream.Write(TStringObject(FValues.Objects[i]).Value[1], Count);
      end
    else if FValues.Objects[i] is TUploadFileObject then
      begin
      Start := TUploadFileObject(FValues.Objects[i]).Start;
      Count := TUploadFileObject(FValues.Objects[i]).Count;

      if FTempFileName = '' then
        stream.Write(Copy(FOrigQuery, Start, Count)[1], Count)
      else
        stream.Write(Read_File(FTempFileName, Start, Count)[1], Count);

      Result := True;
      end
    else
      raise Exception.Create('Fatal error! Wrong object type in HttpValues!');
    end;
  end;

{ TRtcSession }

constructor TRtcSession.Create;
  begin
  inherited;
  FID:=''; FPeerAddr:='';
  end;

destructor TRtcSession.Destroy;
  begin
  FID:=''; FPeerAddr:='';
  inherited;
  end;

initialization
AppFileName:=ExpandUNCFileName(ParamStr(0));
end.
