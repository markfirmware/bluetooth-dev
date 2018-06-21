program BluetoothTest;
{$mode objfpc}{$modeswitch advancedrecords}{$H+}

uses 
{$ifdef BUILD_RPI } BCM2708,BCM2835, {$endif}
{$ifdef BUILD_RPI2} BCM2709,BCM2836, {$endif}
{$ifdef BUILD_RPI3} BCM2710,BCM2837, {$endif}
GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,Console,Logging,Ultibo,WinSock2,
HTTP,WebStatus,SMSC95XX,
Serial,DWCOTG,FileSystem,MMC,FATFS,Keyboard,bcmfw;

const 
 ScanUnitsPerSecond          = 1600;
 ScanInterval                = 1.000;
 ScanWindow                  = 0.250;
 RetentionLimit              = 120*1000*1000;
 FilterMinRxCount            = 5;

 HCI_COMMAND_PKT             = $01;
 HCI_EVENT_PKT               = $04;
 OGF_HOST_CONTROL            = $03;
 OGF_LE_CONTROL              = $08;
 OGF_VENDOR                  = $3f;
 LL_SCAN_PASSIVE             = $00;
 LL_SCAN_ACTIVE              = $01;

 ADV_IND                     = $00; // Connectable undirected advertising(default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising

 // Advertising Data Types
 ADT_FLAGS                   = $01; // Flags
 ADT_INCOMPLETE_UUID16       = $02; // Incomplete List of 16-bit Service Class UUIDs
 ADT_COMPLETE_UUID16         = $03; // Complete List of 16-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID32       = $04; // Incomplete List of 32-bit Service Class UUIDs
 ADT_COMPLETE_UUID32         = $05; // Complete List of 32-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID128      = $06; // Incomplete List of 128-bit Service Class UUIDs
 ADT_COMPLETE_UUDI128        = $07; // Complete List of 128-bit Service Class UUIDs
 ADT_SHORTENED_LOCAL_NAME    = $08; // Shortened Local name
 ADT_COMPLETE_LOCAL_NAME     = $09; // Complete Local name
 ADT_POWER_LEVEL             = $0A; // Tx Power Level
 ADT_DEVICE_CLASS            = $0D; // Class of Device
 ADT_SERVICE_DATA            = $16; // Service data, starts with service uuid followed by data
 ADT_DEVICE_APPEARANCE       = $19; // Device appearance
 ADT_MANUFACTURER_SPECIFIC   = $FF;

 ManufacturerApple           = $004c;
 ManufacturerEstimote        = $015d;
 ManufacturerFlic            = $030f;
 ManufacturerLogitech        = $01da;
 ManufacturerMicrosoft       = $0006;
 ManufacturerTesting         = $ffff;
 EddystoneUuid               = $feaa;

 BDADDR_LEN                  = 6;

type 
 TArrayOfByte = Array of Byte;
 TUuid = Array[0 .. 15] of Byte;
 TBDAddr = Array[0 .. BDADDR_LEN - 1] of Byte;
 TBluetoothWebStatus = class(TWebStatusCustom)
  function DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;override;
 end;
 TMessage = record
  Data:String;
  TimeStamp:LongWord;
  Rssi:Byte;
 end;
 TMessageTrack = record
  Key:String;
  Count:Integer;
  First:TMessage;
  Last:TMessage;
 end;
 TBleConnection = record
  Key:String;
 end;

 TBleAd = record
  BytesLength:Integer;
  Bytes:Array[0 .. 31] of Byte;
  function ToDisplayString:String;
  Case AdType:Integer of 
   ADT_FLAGS                   : (Flags:Byte);
   ADT_INCOMPLETE_UUID16       : ();
   ADT_COMPLETE_UUID16         : (Uuid16:Word);
   ADT_INCOMPLETE_UUID32       : ();
   ADT_COMPLETE_UUID32         : ();
   ADT_INCOMPLETE_UUID128      : ();
   ADT_COMPLETE_UUDI128        : ();
   ADT_SHORTENED_LOCAL_NAME    : ();
   ADT_COMPLETE_LOCAL_NAME     : (NameLength:Integer;Name:Array [0 .. 31] of Byte);
   ADT_POWER_LEVEL             : (PowerLevel:Byte);
   ADT_DEVICE_CLASS            : ();
   ADT_SERVICE_DATA            : (SdUuid16:Word;SdDataLength:Integer;SdData:Array [0 .. 31] of Byte);
   ADT_DEVICE_APPEARANCE       : (Appearance:Word);
   ADT_MANUFACTURER_SPECIFIC   : (Mfr:Word;MfrDataLength:Integer;MfrData:Array [0 .. 31] of Byte);
 end;

 TBleAdPacket = record
  AdsLength:Integer;
  Ads:Array[0 .. 9] of TBleAd;
  function ToDisplayString:String;
  procedure EraseAds;
  procedure AddFlags(Flags:Byte);
  procedure AddServiceAds(Uuid16:Word;Data:Array of Byte);
  function Bytes:TArrayOfByte;
  function IsFlags(Index:Integer;Flags:Byte):Boolean;
  function IsMfr(Index:Integer;Mfr:Word;Prefix:Array of Byte;RemainingSize:Integer):Boolean;
 end;

var 
 ExceptionRestartCounter:Integer;
 DropByte:Boolean;
 ScanRxCount:Integer;
 RestartBroadcastObserveLoop:Boolean;
 BluetoothUartDeviceDescription:String;
 ScanCycleCounter:LongWord;
 ScanIdle:Boolean;
 ScanStartTime:LongWord;
 Margin:LongWord;
 ReadBackLog:Integer;
 BluetoothWebStatus:TBluetoothWebStatus;
 HtmlReportLock:TSemaphoreHandle;
 HtmlReport:String;
 IpAddress:String;
 IpAddressAvailable:Boolean;
 LastDeviceStatus:LongWord;
 SerialDeviceStatusEntryCounter,SerialDeviceStatusExitCounter:LongWord;
 MessageTrackList:Array of TMessageTrack;
 ConnectionList:Array of TBleConnection;
 AdData:Array of Byte;
 HciSequenceNumber:Integer = 0;
 Console1,Console2,Console3:TWindowHandle;
 ch:char;
 UART0:PSerialDevice = Nil;
 KeyboardLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 MonitorLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 IpAddressLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 LedLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 ReadByteCounter:Integer;
 Scheme:Array[0..3] of String = ('http://www.','https://www.','http://','https://');
 Expansion:Array[0..13] of String = ('.com/','.org/','.edu/','.net/','.info/','.biz','.gov/','.com','.org','.edu','.net','.info','.biz','.gov');
 HTTPListener:THTTPListener;
 HTTPRedirect:THTTPRedirect;

function ReadByte:Byte; forward;
procedure StopAdvertising; forward;
procedure StartUndirectedAdvertising; forward;

procedure Log(s:string);
begin
 ConsoleWindowWriteLn(Console1,s);
end;

procedure RestoreBootFile(Prefix,FileName:String);
var 
 Source:String;
begin
 Source:=Prefix + '-' + FileName;
 Log(Format('Restoring from %s ...',[Source]));
 while not DirectoryExists('C:\') do
  sleep(500);
 if FileExists(Source) then
  CopyFile(PChar(Source),PChar(FileName),False);
 Log(Format('Restoring from %s done',[Source]));
end;

function SecondsToTime(Seconds:Integer):TDateTime;
begin
 Result:=(Seconds div PASCAL_TIME_SECONDS_PER_DAY) + ((Seconds mod PASCAL_TIME_SECONDS_PER_DAY) / PASCAL_TIME_SECONDS_PER_DAY);
end;

function TimeToString(Time:TDateTime):String;
begin
 Result:=IntToStr(Trunc(Time)) + ' days ' + TimeToStr(Time);
end;

function TBluetoothWebStatus.DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var 
 Report:String;
 WorkTime:TDateTime;
begin
 AddContent(AResponse,'<meta http-equiv="refresh" content="1">');
 AddContent(AResponse,'<div><big><big><b>This page reloads every second</b></big></big></div>');
 AddContent(AResponse,'ExceptionRestartCounter ' + ExceptionRestartCounter.ToString);
 AddContent(AResponse,'ScanCycleCounter ' + ScanCycleCounter.ToString);
 AddContent(AResponse,'ReadByteCounter ' + ReadByteCounter.ToString);
 WorkTime:=SystemFileTimeToDateTime(UpTime);
 AddContent(AResponse,'Up ' + TimeToString(WorkTime));
 AddContent(AResponse,'<div>');
 AddContent(AResponse,'FilterMinRxCount ' + FilterMinRxCount.ToString);
 AddContent(AResponse,'</div>');
 SemaphoreWait(HtmlReportLock);
 Report:=HtmlReport;
 SemaphoreSignal(HtmlReportLock);
 AddContent(AResponse,'<pre>');
 AddContent(AResponse,Report);
 AddContent(AResponse,'</pre>');
 Result:=True;
end;

function ogf(op:Word):byte;
begin
 Result:=(op shr 10) and $3f;
end;

function ocf(op:Word):Word;
begin
 Result:=op and $3ff;
end;

function ErrToStr(code:byte):string;
begin
 case code of 
  $00:Result:='Success';
  $01:Result:='Unknown HCI Command';
  $02:Result:='Unknown Connection Identifier';
  $03:Result:='Hardware Failure';
  $04:Result:='Page Timeout';
  $05:Result:='Authentication Failure';
  $06:Result:='PIN or Key Missing';
  $07:Result:='Memory Capacity Exceeded';
  $08:Result:='Connection Timeout';
  $09:Result:='Connection Limit Exceeded';
  $0A:Result:='Synchronous Connection Limit To A Device Exceeded';
  $0B:Result:='ACL Connection Already Exists';
  $0C:Result:='Command Disallowed';
  $0D:Result:='Connection Rejected due to Limited Resources';
  $0E:Result:='Connection Rejected due To Security Reasons';
  $0F:Result:='Connection Rejected due to Unacceptable BD_ADDR';
  $10:Result:='Connection Accept Timeout Exceeded';
  $11:Result:='Unsupported Feature or Parameter Value';
  $12:Result:='Invalid HCI Command Parameters';
  $13:Result:='Remote User Terminated Connection';
  $14:Result:='Remote Device Terminated Connection due to Low Resources';
  $15:Result:='Remote Device Terminated Connection due to Power Off';
  $16:Result:='Connection Terminated By Local Host';
  $17:Result:='Repeated Attempts';
  $18:Result:='Pairing Not Allowed';
  $19:Result:='Unknown LMP PDU';
  $1A:Result:='Unsupported Remote Feature / Unsupported LMP Feature';
  $1B:Result:='SCO Offset Rejected';
  $1C:Result:='SCO Interval Rejected';
  $1D:Result:='SCO Air Mode Rejected';
  $1E:Result:='Invalid LMP Parameters / Invalid LL Parameters';
  $1F:Result:='Unspecified Error';
  $20:Result:='Unsupported LMP Parameter Value / Unsupported LL Parameter Value';
  $21:Result:='Role Change Not Allowed';
  $22:Result:='LMP Response Timeout / LL Response Timeout';
  $23:Result:='LMP Error Transaction Collision';
  $24:Result:='LMP PDU Not Allowed';
  $25:Result:='Encryption Mode Not Acceptable';
  $26:Result:='Link Key cannot be Changed';
  $27:Result:='Requested QoS Not Supported';
  $28:Result:='Instant Passed';
  $29:Result:='Pairing With Unit Key Not Supported';
  $2A:Result:='Different Transaction Collision';
  $2B:Result:='Reserved';
  $2C:Result:='QoS Unacceptable Parameter';
  $2D:Result:='QoS Rejected';
  $2E:Result:='Channel Classification Not Supported';
  $2F:Result:='Insufficient Security';
  $30:Result:='Parameter Out Of Mandatory Range';
  $31:Result:='Reserved';
  $32:Result:='Role Switch Pending';
  $33:Result:='Reserved';
  $34:Result:='Reserved Slot Violation';
  $35:Result:='Role Switch Failed';
  $36:Result:='Extended Inquiry Response Too Large';
  $37:Result:='Secure Simple Pairing Not Supported By Host';
  $38:Result:='Host Busy - Pairing';
  $39:Result:='Connection Rejected due to No Suitable Channel Found';
  $3A:Result:='Controller Busy';
  $3B:Result:='Unacceptable Connection Parameters';
  $3C:Result:='Directed Advertising Timeout';
  $3D:Result:='Connection Terminated due to MIC Failure';
  $3E:Result:='Connection Failed to be Established';
  $3F:Result:='MAC Connection Failed';
  $40:Result:='Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock';
 end;
end;

procedure Fail(Message:String);
begin
 raise Exception.Create(Message);
end;

procedure ClearAdvertisingData;
begin
 SetLength(AdData,0);
end;

procedure AddAdvertisingData (Type_ : byte; Data : array of byte); overload;
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=0 to high (Data) do
  AdData[Len + 2 + i]:=Data[i];
end;

procedure AddAdvertisingData (Type_ : byte; Data : string); overload;
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=1 to length (Data) do
  AdData[Len + 1 + i]:=ord (Data[i]);
end;

procedure AddAdvertisingData(Type_:byte); overload;
begin
 AddAdvertisingData (Type_, []);
end;

procedure HciCommand(OpCode:Word; Params:array of byte);
var 
 i:integer;
 Cmd:array of byte;
 res,count:LongWord;
 PacketType,EventCode,PacketLength,CanAcceptPackets,Status:Byte;
 Acknowledged:Boolean;
begin
 Inc(HciSequenceNumber);
 // if OpCode <> $fc4c then
 //  Log(Format('hci %d op %04.4x',[HciSequenceNumber,OpCode]));
 SetLength(Cmd,length(Params) + 4);
 Cmd[0]:=HCI_COMMAND_PKT;
 Cmd[1]:=lo(OpCode);
 Cmd[2]:=hi(OpCode);
 Cmd[3]:=length(Params);
 for i:=0 to length(Params) - 1 do
  Cmd[4 + i]:=Params[i];
 count:=0;
 res:=SerialDeviceWrite(UART0,@Cmd[0],length(Cmd),SERIAL_WRITE_NONE,count);
 if res = ERROR_SUCCESS then
  begin
   Acknowledged:=False;
   while not Acknowledged do
    begin
     PacketType:=ReadByte;
     if PacketType <> HCI_EVENT_PKT then
      Fail(Format('event type not hci event: %d',[PacketType]));
     EventCode:=ReadByte;
     if EventCode = $0E then
      begin
       PacketLength:=ReadByte;
       if PacketLength <> 4 then
        Fail(Format('packet length not 4: %d',[PacketLength]));
       CanAcceptPackets:=ReadByte;
       if CanAcceptPackets <> 1 then
        Fail(Format('can accept packets not 1: %d',[CanAcceptPackets]));
       ReadByte; // completed command low
       ReadByte; // completed command high
       Status:=ReadByte;
       Acknowledged:=True;
      end
     else if EventCode = $0F then
           begin
            PacketLength:=ReadByte;
            if PacketLength <> 4 then
             Fail(Format('packet length not 4: %d',[PacketLength]));
            Status:=ReadByte;
            CanAcceptPackets:=ReadByte;
            if CanAcceptPackets <> 1 then
             Fail(Format('can accept packets not 1: %d',[CanAcceptPackets]));
            ReadByte; // completed command low
            ReadByte; // completed command high
            Acknowledged:=True;
           end
     else
      begin
       PacketLength:=ReadByte;
       Log(Format('HciCommand discarding event %d length %d',[EventCode,PacketLength]));
       for I:=1 to PacketLength do
        ReadByte;
       Sleep(5*1000);
       // Fail(Format('event code not command completed nor status: %02.2x',[EventCode]));
      end;
    end;
   if Status <> 0 then
    Fail(Format('status not 0: %d',[Status]));
  end
 else
  Log('Error writing to BT.');
end;

procedure HciCommand(OGF:byte; OCF:Word; Params:array of byte);
begin
 HciCommand((OGF shl 10) or OCF,Params);
end;

procedure SetLEAdvertisingData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 Len:=Min(Length(Data),31);
 SetLength(Params,Len + 1);
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 HciCommand(OGF_LE_CONTROL,$08,Params);
end;

procedure SetLEScanResponseData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 Len:=Min(Length(Data),31);
 SetLength(Params,Len + 1);
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 HciCommand(OGF_LE_CONTROL,$09,Params);
end;

procedure UpdateBeacon;
var 
 Packet:TBleAdPacket;
 PacketData:Array of Byte;
 I:Integer;
 UpTime:Int64;
 Temperature:Double;
 Message:String;
 PublicIp:String;
const 
 Part1 = 'ultibo';
procedure AddByte(X:Byte);
begin
 SetLength(PacketData,Length(PacketData) + 1);
 PacketData[Length(PacketData) - 1]:=X;
end;
procedure AddWord(X:Word);
begin
 AddByte(Hi(X));
 AddByte(Lo(X));
end;
procedure AddLongWord(X:LongWord);
begin
 AddWord(Hi(X));
 AddWord(Lo(X));
end;
begin
 ClearAdvertisingData;
 SetLength(PacketData,0);
 if (ScanCycleCounter mod 6) = 0 then
  begin
   AddByte($10);
   AddByte($00);
   PublicIp:=SysUtils.GetEnvironmentVariable('PUBLIC_IP_ADDRESS');
   if IpAddressAvailable and (PublicIp <> '') then
    begin
     AddByte($02);
     for I:=Low(PublicIp) to High(PublicIp) do
      AddByte(Ord(PublicIp[I]));
    end
   else
    begin
     AddByte($03);
     for I:=1 to Length(Part1) do
      AddByte(Ord(Part1[I]));
     AddByte($08);
    end;
  end
 else
  begin
   UpTime:=ClockGetTotal;
   Temperature:=TemperatureGetCurrent(TEMPERATURE_ID_SOC) / 1000;
   AddByte($20);
   AddByte($00);
   AddWord(4993);
   AddWord((Trunc(Temperature) shl 8) or Round((Temperature - Trunc(Temperature))*100));
   AddLongWord(UpTime div (1*1000*1000));
   AddLongWord(UpTime div (100*1000));
  end;
 Packet.EraseAds;
 Packet.AddFlags($18);
 Packet.AddServiceAds(EddyStoneUuid,PacketData);
 SetLEAdvertisingData(Packet.Bytes);
 ClearAdvertisingData;
 SetLength(PacketData,0);
 AddByte($ff);
 AddByte($ff);
 AddByte($55);
 AddByte($96);
 AddByte(34);
 Message:=Format('up %s',[TimeToString(SecondsToTime(ClockGetTotal div (1000*1000)))]);
 for I:=Low(Message) to High(Message) do
  AddByte(Ord(Message[I]));
 AddAdvertisingData(ADT_MANUFACTURER_SPECIFIC,PacketData);
 SetLEScanResponseData(AdData);
end;

procedure SetLEAdvertisingParameters(MinInterval,MaxInterval:Word; Type_:byte; OwnAddressType,PeerAddressType:byte; PeerAddr:TBDAddr; ChannelMap,FilterPolicy:byte);
begin
 HciCommand(OGF_LE_CONTROL,$06,[lo(MinInterval),hi(MinInterval),
 lo(MaxInterval),hi(MaxInterval),
 Type_,OwnAddressType,PeerAddressType,
 PeerAddr[0],PeerAddr[1],PeerAddr[2],
 PeerAddr[3],PeerAddr[4],PeerAddr[5],
 ChannelMap,FilterPolicy]);
end;

procedure StartLeAdvertising;
var 
 ZeroAddress:TBDAddr = ($00,$00,$00,$00,$00,$00);
begin
 SetLEAdvertisingParameters(1000,1000,ADV_SCAN_IND,$00,$00,ZeroAddress,$07,$00);
 UpdateBeacon;
 StartUndirectedAdvertising;
end;

function dBm(Rssi:Byte):String;
var 
 si:String;
begin
 if Rssi = 127 then si := 'NU'
 else if Rssi > 128 then si := '-' + IntToStr (256 - Rssi) + 'dBm'
 else if Rssi <= 09 then si := '+0' + IntToStr (Rssi) + 'dBm'
 else if Rssi <= 20 then si := '+' + IntToStr (Rssi) + 'dBm'
 else si := '?dBm';
 Result:=si;
end;

procedure ProcessConnections;
begin
end;

procedure EndOfScan;
var 
 Now:LongWord;
 I:Integer;
 Report:String;
 Line:String;
 Color:LongWord;
 StyleColor:String;
 CounterWidth:Integer;
 LineFormat:String;
 LineHtml:String;
 HttpPos:Integer;
 JustUrl:String;
procedure Cull;
var 
 NewList:Array of TMessageTrack;
 I,J:Integer;
 AtLeastOneExpired:Boolean;
begin
 AtLeastOneExpired:=False;
 for I:=0 to High(MessageTrackList) do
  begin
   if LongWord(Now - MessageTrackList[I].Last.TimeStamp) > RetentionLimit then
    begin
     if not AtLeastOneExpired then
      begin
       AtLeastOneExpired:=True;
       SetLength(NewList,I);
       for J:=0 to I - 1 do
        NewList[J]:=MessageTrackList[J];
      end;
    end
   else if AtLeastOneExpired then
         begin
          SetLength(NewList,Length(NewList) + 1);
          NewList[Length(NewList) - 1]:=MessageTrackList[I];
         end;
  end;
 if AtLeastOneExpired then
  MessageTrackList:=NewList;
end;
begin
 StopAdvertising;
 StartLeAdvertising;
 Now:=ClockGetCount;
 Cull;
 ConsoleWindowSetXY(Console2,1,1);
 Report:='';
 CounterWidth:=1;
 for I:=0 to High(MessageTrackList) do
  if Length(IntToStr(MessageTrackList[I].Count)) > CounterWidth then
   CounterWidth:=Length(IntToStr(MessageTrackList[I].Count));
 for I:=High(MessageTrackList) downto 0 do
  with MessageTrackList[I] do
   begin
    if Count < FilterMinRxCount then
     continue;
    if LongWord(Now - Last.TimeStamp) > RetentionLimit - 15*1000*1000 then
     Color:=COLOR_RED
    else if LongWord(Now - Last.TimeStamp) > RetentionLimit - 30*1000*1000 then
          Color:=COLOR_GRAY
    else
     Color:=COLOR_YELLOW;
    LineFormat:=Format('%%7s %%%dd %%s %%s',[CounterWidth]);
    Line:=Format(LineFormat,[dBm(Last.Rssi),Count,Key,Last.Data]);
    ConsoleWindowSetForecolor(Console2,Color);
    ConsoleWindowWrite(Console2,Line);
    StyleColor:=(Color and $ffffff).ToHexString(6);
    HttpPos:=Pos('http://',Line);
    if HttpPos = 0 then
     HttpPos:=Pos('https://',Line);
    if HttpPos <> 0 then
     begin
      JustUrl:=RightStr(Line,Length(Line) - HttpPos + 1);
      JustUrl:=LeftStr(JustUrl,Length(JustUrl) - 1);
      LineHtml:=LeftStr(Line,HttpPos - 1) + Format('<a href=%s>%s</a>',[JustUrl,JustUrl]);
     end
    else
     LineHtml:=Line;
    Report:=Report + '<div style=background-color:#' + StyleColor +';">' + LineHtml + '</div>';
    ConsoleWindowClearEx(Console2,ConsoleWindowGetX(Console2),ConsoleWindowGetY(Console2),ConsoleWindowGetMaxX(Console2),ConsoleWindowGetY(Console2),False);
    ConsoleWindowWriteLn(Console2,'');
   end;
 ConsoleWindowClearEx(Console2,ConsoleWindowGetX(Console2),ConsoleWindowGetY(Console2),ConsoleWindowGetMaxX(Console2),ConsoleWindowGetMaxY(Console2),False);
 SemaphoreWait(HtmlReportLock);
 HtmlReport:=Report;
 SemaphoreSignal(HtmlReportLock);
end;

function EventReadFirstByte:Byte;
var 
 c:LongWord;
 b:Byte;
 res:Integer;
 Now:LongWord;
 EntryTime:LongWord;
begin
 Result:=0;
 EntryTime:=ClockGetCount;
 while LongWord(ClockGetCount - EntryTime) < 10*1000*1000 do
  begin
   Now:=ClockGetCount;
   c:=0;
   res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
   if (res = ERROR_SUCCESS) and (c = 1) then
    begin
     Result:=b;
     Inc(ReadByteCounter);
     if ScanIdle then
      begin
       ScanIdle:=False;
       ScanStartTime:=Now;
       ScanRxCount:=0;
       if (ScanCycleCounter >= 1) and (LongWord(Now - EntryTime) div 1000 < Margin) then
        begin
         Margin:=LongWord(Now - EntryTime) div 1000;
         LoggingOutput(Format('lowest available processing time between scans is now %5.3fs',[Margin / 1000]));
        end;
      end;
     Inc(ScanRxCount);
     exit;
    end
   else
    begin
     if (not ScanIdle) and (LongWord(Now - ScanStartTime)/(1*1000*1000)  > ScanWindow + 0.200)  then
      begin
       ScanIdle:=True;
       Inc(ScanCycleCounter);
       EndOfScan;
       ProcessConnections;
      end;
     ThreadYield;
    end;
  end;
 Fail('timeout waiting for serial read byte');
end;

function ReadByte:Byte;
var 
 c:LongWord;
 b:Byte;
 res:Integer;
 EntryTime:LongWord;
 SerialStatus:LongWord;
begin
 Result:=0;
 EntryTime:=ClockGetCount;
 while LongWord(ClockGetCount - EntryTime) < 1*1000*1000 do
  begin
   c:=0;
   res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
   if (res = ERROR_SUCCESS) and (c = 1) then
    begin
     if DropByte then
      begin
       DropByte:=False;
       break;
      end;
     Result:=b;
     Inc(ReadByteCounter);
     res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_PEEK_BUFFER,c);
     if c > ReadBackLog then
      begin
       ReadBackLog:=c;
       LoggingOutput(Format('highest SERIAL_READ_PEEK_BUFFER is now %d',[ReadBackLog]));
      end;
     Inc(SerialDeviceStatusEntryCounter);
     SerialStatus:=SerialDeviceStatus(UART0);
     Inc(SerialDeviceStatusExitCounter);
     SerialStatus:=SerialStatus and not (SERIAL_STATUS_RX_EMPTY or SERIAL_STATUS_TX_EMPTY);
     if SerialStatus <> LastDeviceStatus then
      begin
       LastDeviceStatus:=SerialStatus;
       LoggingOutput(Format('SerialDeviceStatus changed %08.8x',[SerialStatus]));
      end;
     exit;
    end
   else
    ThreadYield;
  end;
 Fail('timeout waiting for serial read byte');
end;

function IsBlueToothAvailable:Boolean;
begin
 Result:=True;
 Log(Format('Board is %s',[BoardTypeToString(BoardGetType)]));
 case BoardGetType of 
  BOARD_TYPE_RPI3B:
                   begin
                    BluetoothUartDeviceDescription:='BCM2837 PL011 UART';
                    PrepareBcmFirmware(0);
                   end;
  BOARD_TYPE_RPI3B_PLUS:
                        begin
                         BluetoothUartDeviceDescription:='BCM2837 PL011 UART';
                         PrepareBcmFirmware(1);
                        end;
  BOARD_TYPE_RPI_ZERO_W:
                        begin
                         BluetoothUartDeviceDescription:='BCM2835 PL011 UART';
                         PrepareBcmFirmware(0);
                        end;
  else
   begin
    Log('');
    Log('');
    Log('Bluetooth is not available on this board');
    Result:=False;
   end;
 end;
end;

function OpenUART0:boolean;
var 
 res:LongWord;
begin
 Result:=False;
 UART0:=SerialDeviceFindByDescription(BluetoothUartDeviceDescription);
 if UART0 = nil then
  begin
   Log('Can''t find UART0');
   exit;
  end;
 if BoardGetType = BOARD_TYPE_RPI_ZERO_W then
  res:=SerialDeviceOpen(UART0,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_RTS_CTS,0,0)
 else
  res:=SerialDeviceOpen(UART0,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 if res = ERROR_SUCCESS then
  begin
   Result:=True;
   ReadBackLog:=0;
   LastDeviceStatus:=0;
   SerialDeviceStatusEntryCounter:=0;
   SerialDeviceStatusExitCounter:=0;

   GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_IN);

   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_ALT3);     // TXD0
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_ALT3);     // RXD0
   GPIOPullSelect(GPIO_PIN_32,GPIO_PULL_NONE);             //Added
   GPIOPullSelect(GPIO_PIN_33,GPIO_PULL_UP);               //Added

   if BoardGetType = BOARD_TYPE_RPI_ZERO_W then
    begin
     GPIOFunctionSelect(GPIO_PIN_30,GPIO_FUNCTION_ALT3);     // RTS
     GPIOFunctionSelect(GPIO_PIN_31,GPIO_FUNCTION_ALT3);     // CTS
     GPIOPullSelect(GPIO_PIN_30,GPIO_PULL_UP);
     GPIOPullSelect(GPIO_PIN_31,GPIO_PULL_NONE);
    end;

   Sleep(50);
  end;
end;

procedure ResetChip;
begin
 HciCommand(OGF_HOST_CONTROL,$03,[]);
end;

procedure CloseUART0;
begin
 SerialDeviceClose(UART0);
 UART0:=Nil;
end;

procedure BCMLoadFirmware;
var 
 Params:array of byte;
 len:integer;
 Op:Word;
 Index:Integer;
 I:Integer;
 P:Pointer;
function GetByte:Byte;
begin
 Result:=PByte(P)^;
 Inc(P);
 Inc(Index);
end;
begin
 Log('Firmware load ...');
 HciCommand(OGF_VENDOR,$2e,[]);
 Index:=0;
 P:=BcmFirmwarePointer;
 while Index < BcmFirmwareLength do
  begin
   Op:=GetByte;
   Op:=Op or (GetByte shl 8);
   Len:=GetByte;
   SetLength(Params,Len);
   for I:= 0 to Len - 1 do
    Params[I]:=GetByte;
   HciCommand(Op,Params);
  end;
 CloseUart0;
 Sleep(50);
 OpenUart0;
 Sleep(50);
 Log('Firmware load done');
end;

procedure WaitForSDDrive;
begin
 while not DirectoryExists('C:\') do
  sleep(500);
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOMRIGHT;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

procedure SetLEScanParameters(Type_:byte;Interval,Window:Word;OwnAddressType,FilterPolicy:byte);
begin
 HciCommand(OGF_LE_CONTROL,$0b,[Type_,lo(Interval),hi(Interval),lo(Window),hi(Window),OwnAddressType,FilterPolicy]);
end;

procedure SetLEScanEnable(State,Duplicates:boolean);
var 
 Params:Array of Byte;
begin
 SetLength(Params,2);
 if State then
  Params[0]:=$01
 else
  Params[0]:=$00;
 if Duplicates then
  Params[1]:=$01
 else
  Params[1]:=$00;
 HciCommand(OGF_LE_CONTROL,$0c,Params);
end;

procedure StartPassiveScanning;
begin
 SetLEScanParameters(LL_SCAN_PASSIVE,Round(ScanInterval*ScanUnitsPerSecond),Round(ScanWindow*ScanUnitsPerSecond),$00,$00);
 SetLEScanEnable(True,False);
end;

procedure StartActiveScanning;
begin
 SetLEScanParameters(LL_SCAN_ACTIVE,Round(ScanInterval*ScanUnitsPerSecond),Round(ScanWindow*ScanUnitsPerSecond),$00,$00);
 SetLEScanEnable(True,False);
end;

procedure StopScanning;
begin
 SetLEScanEnable(False,False);
end;

// le control
procedure SetLEEventMask(Mask:QWord);
var 
 Params:array of byte;
 MaskHi,MaskLo:DWord;
begin
 MaskHi:=(Mask shr 32) and $FFFFFFFF;
 MaskLo:=Mask and $FFFFFFFF;
 SetLength(Params,8);
 Params[0]:=MaskLo and $ff;   // lsb
 Params[1]:=(MaskLo shr 8) and $ff;
 Params[2]:=(MaskLo shr 16) and $ff;
 Params[3]:=(MaskLo shr 24) and $ff;
 Params[4]:=MaskHi and $ff;   // lsb
 Params[5]:=(MaskHi shr 8) and $ff;
 Params[6]:=(MaskHi shr 16) and $ff;
 Params[7]:=(MaskHi shr 24) and $ff;
 HciCommand(OGF_LE_CONTROL,$01,Params);
end;

function MonitorLoop(Parameter:Pointer):PtrInt;
var 
 Capture:LongWord;
begin
 Result:=0;
 while True do
  begin
   Sleep(1*1000);
   Capture:=SerialDeviceStatusExitCounter;
   if SerialDeviceStatusEntryCounter <> Capture then
    begin
     Sleep(2*1000);
     if SerialDeviceStatusExitCounter = Capture then
      begin
       LoggingOutput(Format('SerialDeviceStatus entry %d exit %d',[SerialDeviceStatusEntryCounter,SerialDeviceStatusExitCounter]));
       exit;
      end;
    end;
  end;
end;

procedure Help;
begin
 Log('');
 Log('H - Help - display this help message');
 Log('L - Loop - restart the broadcast/observe loop');
 Log('D - Drop Byte - drop one byte from the uart rx');
 Log('Q - Quit - use default-config.txt');
 Log('R - Restart - use bluetooth-dev-bluetoothtest-config.txt');
 Log('');
 Log('Legend');
 Log('R/P/? Random/Public/Other MAC Address');
 Log('C/D/S/N/R Connectable/Directed/Scannable/Non-connectable/Response Ad Event Type');
 Log('');
end;

function IpAddressLoop(Parameter:Pointer):PtrInt;
var 
 TCP : TWinsock2TCPClient;
begin
 Result:=0;
 TCP:=TWinsock2TCPClient.Create;
 IpAddress:=TCP.LocalAddress;
 if (IpAddress = '') or (IpAddress = '0.0.0.0') or (IpAddress = '255.255.255.255') then
  begin
   while (IpAddress = '') or (IpAddress = '0.0.0.0') or (IpAddress = '255.255.255.255') do
    begin
     sleep(500);
     IpAddress:=TCP.LocalAddress;
    end;
  end;
 TCP.Free;
 IpAddressAvailable:=True;
end;

function KeyboardLoop(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   if ConsoleGetKey(ch,nil) then
    case uppercase(ch) of 
     'H' : Help;
     'L' : RestartBroadcastObserveLoop:=True;
     'D' : DropByte:=True;
     'Q' : SystemRestart(0);
     'R' :
          begin
           RestoreBootFile('bluetooth-dev-bluetoothtest','config.txt');
           SystemRestart(0);
          end;
     'C' : ConsoleWindowClear(Console1);
    end;
  end;
end;

function MacAddressTypeToStr(MacAddressType:Byte):String;
begin
 case MacAddressType of 
  $00:Result:='P';
  $01:Result:='R';
  else
   Result:='?';
 end;
end;

function AdEventTypeToStr(AdEventType:Byte):String;
begin
 case AdEventType of 
  $00:Result:='C';
  $01:Result:='D';
  $02:Result:='S';
  $03:Result:='N';
  $04:Result:='R';
  else
   Result:='?';
 end;
end;

function AdvertisingTypeToStr(Type_:byte):string;
begin
 case Type_ of 
  ADV_IND           : Result:='connectable undirected advertising (default)';
  ADV_DIRECT_IND_HI : Result:='connectable high duty cycle directed advertising';
  ADV_SCAN_IND      : Result:='scannable undirected advertising';
  ADV_NONCONN_IND   : Result:='non-connectable undirected advertising';
  ADV_DIRECT_IND_LO : Result:='connectable low duty cycle directed advertising';
  else                Result:='reserved for future use (' + Type_.ToHexString(2) + ')';
 end;
end;

function ManufacturerToString(Mfr:Word):String;
begin
 case Mfr of 
  ManufacturerApple:Result:='Apple';
  ManufacturerEstimote:Result:='Estimote';
  ManufacturerFlic:Result:='Flic';
  ManufacturerLogitech:Result:='Logitech';
  ManufacturerMicrosoft:Result:='Microsoft';
  ManufacturerTesting:Result:='Testing';
  else
   Result:=Format('mr,%04.4x',[Mfr]);
 end;
end;

procedure SetLEAdvertisingEnable(State:boolean);
begin
 if State then
  HciCommand(OGF_LE_CONTROL,$0a,[$01])
 else
  HciCommand(OGF_LE_CONTROL,$0a,[$00]);
end;

procedure StartUndirectedAdvertising;
begin
 // ReadLEAdvertisingChannelTxPower;
 SetLEAdvertisingEnable(true);
end;

procedure StopAdvertising;
begin
 SetLEAdvertisingEnable(false);
end;

procedure Track(NewTimeStamp:LongWord;NewRssi:Byte;NewKey:String;NewData:String);
var 
 MessageTrack:TMessageTrack;
 I,N:Integer;
begin
 if LeftStr(NewData,5) = 'salt ' then
  begin
   for I:=0 to High(MessageTrackList) do
    with MessageTrackList[I] do
     if (NewData = Last.Data) and (NewKey <> Key) then
      begin
       Inc(Count);
       Key:=NewKey;
       Last.TimeStamp:=NewTimeStamp;
       Last.Rssi:=NewRssi;
       exit;
      end;
  end;
 for I:=0 to High(MessageTrackList) do
  with MessageTrackList[I] do
   if NewKey = Key then
    begin
     Inc(Count);
     Last.Data:=NewData;
     Last.TimeStamp:=NewTimeStamp;
     Last.Rssi:=NewRssi;
     exit;
    end;
 N:=Length(MessageTrackList);
 SetLength(MessageTrackList,N + 1);
 Inc(N);
 for I:=N - 1 downto 1 do
  MessageTrackList[I]:=MessageTrackList[I - 1];
 MessageTrack.Key:=NewKey;
 MessageTrack.Count:=1;
 MessageTrack.First.Data:=NewData;
 MessageTrack.First.TimeStamp:=NewTimeStamp;
 MessageTrack.First.Rssi:=NewRssi;
 MessageTrack.Last:=MessageTrack.First;
 MessageTrackList[0]:=MessageTrack;
end;

function AsWord(H,L:Byte):Word;
begin
 Result:=(H shl 8) or L;
end;

function UuidToStr (uuid : TUuid) : string;
begin
 Result := format ('%.2X%.2X%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X',
          [uuid[0], uuid[1], uuid[2], uuid[3], uuid[4], uuid[5], uuid[6], uuid[7],
          uuid[8], uuid[9], uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15]])
end;

function ParseAd(Index:Integer;Data:Array of Byte):TBleAd;
var 
 I:Integer;
function NextByte:Byte;
begin
 if Index > High(Data) then
  Fail('ParseAd underflow');
 Result:=Data[Index];
 Inc(Index);
end;
begin
 Result.BytesLength:=NextByte - 1;
 Result.AdType:=NextByte;
 for I:=0 to Result.BytesLength - 1 do
  begin
   if I > High(Result.Bytes) then
    Fail('ParseAd overflow');
   Result.Bytes[I]:=NextByte;
  end;
 case Result.AdType of 
  ADT_FLAGS:
            begin
             Result.Flags:=Result.Bytes[0];
            end;
  ADT_COMPLETE_UUID16:
                      begin
                       Result.Uuid16:=AsWord(Result.Bytes[1],Result.Bytes[0]);
                      end;
  ADT_POWER_LEVEL:
                  begin
                   Result.PowerLevel:=Result.Bytes[0];
                  end;
  ADT_COMPLETE_LOCAL_NAME:
                          begin
                           Result.NameLength:=Result.BytesLength;
                           for I:=0 to Result.NameLength - 1 do
                            Result.Name[I]:=Result.Bytes[I];
                          end;
  ADT_SERVICE_DATA:
                   begin
                    Result.SdUuid16:=AsWord(Result.Bytes[1],Result.Bytes[0]);
                    Result.SdDataLength:=Result.BytesLength - 2;
                    for I:=0 to Result.SdDataLength - 1 do
                     Result.SdData[I]:=Result.Bytes[I + 2];
                   end;
  ADT_DEVICE_APPEARANCE:
                        begin
                         Result.Appearance:=AsWord(Result.Bytes[1],Result.Bytes[0]);
                        end;
  ADT_MANUFACTURER_SPECIFIC:
                            begin
                             Result.Mfr:=AsWord(Result.Bytes[1],Result.Bytes[0]);
                             Result.MfrDataLength:=Result.BytesLength - 2;
                             for I:=0 to Result.MfrDataLength - 1 do
                              Result.MfrData[I]:=Result.Bytes[I + 2];
                            end;
 end;
end;

procedure TBleAdPacket.EraseAds;
begin
 AdsLength:=0;
end;

procedure TBleAdPacket.AddFlags(Flags:Byte);
begin
 Ads[AdsLength].AdType:=ADT_FLAGS;
 Ads[AdsLength].Flags:=Flags;
 Inc(AdsLength);
end;

procedure TBleAdPacket.AddServiceAds(Uuid16:Word;Data:Array of Byte);
var 
 I:Integer;
begin
 Ads[AdsLength].AdType:=ADT_COMPLETE_UUID16;
 Ads[AdsLength].Uuid16:=Uuid16;
 Inc(AdsLength);
 Ads[AdsLength].AdType:=ADT_SERVICE_DATA;
 Ads[AdsLength].SdUuid16:=Uuid16;
 Ads[AdsLength].SdDataLength:=Length(Data);
 for I:=0 to High(Data) do
  Ads[AdsLength].SdData[I]:=Data[I];
 Inc(AdsLength);
end;

function TBleAdPacket.Bytes:TArrayOfByte;
var 
 Index,I:Integer;
procedure PutByte(Data:Byte);
begin
 SetLength(Result,Length(Result) + 1);
 Result[Length(Result) - 1]:=Data;
end;
procedure PutWord(Data:Word);
begin
 PutByte(Lo(Data));
 PutByte(Hi(Data));
end;
begin
 SetLength(Result,0);
 for Index:=0 to AdsLength - 1 do
  with Ads[Index] do
   case AdType of 
    ADT_FLAGS:
              begin
               PutByte(2);
               PutByte(AdType);
               PutByte(Flags);
              end;
    ADT_COMPLETE_UUID16:
                        begin
                         PutByte(3);
                         PutByte(AdType);
                         PutWord(Uuid16);
                        end;
    ADT_SERVICE_DATA:
                     begin
                      PutByte(1 + 2 + SdDataLength);
                      PutByte(AdType);
                      PutWord(SdUuid16);
                      for I:=0 to SdDataLength - 1 do
                       PutByte(SdData[I]);
                     end;
    ADT_MANUFACTURER_SPECIFIC:
                              begin
                               PutByte(1 + 2 + MfrDataLength);
                               PutByte(AdType);
                               PutWord(Mfr);
                               for I:=0 to MfrDataLength - 1 do
                                PutByte(MfrData[I]);
                              end;
   end;
end;

function ParseAdPacket(Index:Integer;Data:Array of Byte):TBleAdPacket;
begin
 Result.AdsLength:=0;
 while Index <= High(Data) do
  begin
   if Result.AdsLength > High(Result.Ads) then
    Fail('ParseAdPacket overflow');
   Result.Ads[Result.AdsLength]:=ParseAd(Index,Data);
   Inc(Index,Result.Ads[Result.AdsLength].BytesLength + 2);
   Inc(Result.AdsLength);
  end;
end;

function TBleAd.ToDisplayString:String;
var 
 I:Integer;
 BytesString:String;
begin
 case AdType of 
  ADT_FLAGS:
            begin
             Result:=Format('fl,%02.2x',[Flags]);
            end;
  ADT_COMPLETE_UUID16:
                      begin
                       Result:=Format('ui,%04.4x',[Uuid16]);
                      end;
  ADT_INCOMPLETE_UUID128:
                         begin
                          Result:='ui,128';
                         end;
  ADT_COMPLETE_LOCAL_NAME:
                          begin
                           BytesString:='';
                           for I:=0 to NameLength - 1 do
                            BytesString:=BytesString + Char(Name[I]);
                           Result:=Format('nm,%s',[BytesString]);
                          end;
  ADT_POWER_LEVEL:
                  begin
                   Result:=Format('pw,%s',[dBm(PowerLevel)]);
                  end;
  ADT_SERVICE_DATA:
                   begin
                    BytesString:='';
                    for I:=0 to SdDataLength - 1 do
                     BytesString:=BytesString + SdData[I].ToHexString(2);
                    Result:=Format('sd,%04.4x,%s',[SdUuid16,BytesString]);
                   end;
  ADT_DEVICE_APPEARANCE:
                        begin
                         if Appearance = $03c1 then
                          Result:='ap,keyboard'
                         else
                          Result:=Format('ap,%04.4x',[Appearance]);
                        end;
  ADT_MANUFACTURER_SPECIFIC:
                            begin
                             //                           BytesString:='';
                             BytesString:=IntToStr(MfrDataLength) + ':';
                             for I:=0 to MfrDataLength - 1 do
                              BytesString:=BytesString + MfrData[I].ToHexString(2);
                             Result:=Format('%s,%s',[ManufacturerToString(Mfr),BytesString]);
                            end;
  else
   begin
    BytesString:='';
    for I:=0 to BytesLength - 1 do
     BytesString:=BytesString + Bytes[I].ToHexString(2);
    Result:=Format('%02.2x,%s',[AdType,BytesString]);
   end;
 end;
end;


function TBleAdPacket.IsFlags(Index:Integer;Flags:Byte):Boolean;
begin
 Result:=False;
 if Index < AdsLength then
  if Ads[Index].AdType = ADT_FLAGS then
   Result:=Ads[Index].Flags = Flags;
end;

function TBleAdPacket.IsMfr(Index:Integer;Mfr:Word;Prefix:Array of Byte;RemainingSize:Integer):Boolean;
var 
 I:Integer;
begin
 Result:=False;
 if Index < AdsLength then
  if Ads[Index].AdType = ADT_MANUFACTURER_SPECIFIC then
   if Ads[Index].Mfr = Mfr then
    if Length(Prefix) + RemainingSize = Ads[Index].MfrDataLength then
     begin
      for I:=0 to High(Prefix) do
       if Ads[Index].MfrData[I] <> Prefix[I] then
        exit;
      Result:=True;
     end;
end;

function TBleAdPacket.ToDisplayString:String;
var 
 I:Integer;
begin
 if IsFlags(0,$1a) and IsMfr(1,ManufacturerApple,[$09,$06,$03],5) then
  begin
   Result:='AppleTv ';
   for I:=0 to 4 do
    begin
     Result:=Result + Ads[1].MfrData[3 + I].ToHexString(2);
     if I = 0 then
      Result:=Result + ' ';
    end;
  end
 else if IsMfr(0,ManufacturerMicrosoft,[$01,$09,$20],4 + 20) then
       begin
        Result:='win10 salt ';
        for I:=0 to 4 - 1 do
         Result:=Result + Ads[0].MfrData[3 + I].ToHexString(2);
        Result:=Result + ' hash ';
        for I:=0 to 20 - 1 do
         Result:=Result + Ads[0].MfrData[3 + 4 + I].ToHexString(2);
       end
 else
  begin
   Result:='';
   for I:=0 to AdsLength - 1 do
    Result:=Result + Ads[I].ToDisplayString + ' ';
  end;
end;

procedure ParseEvent;
var 
 ParsedPacket:TBleAdPacket;
 I:Integer;
 EventType,EventSubtype,EventLength:Byte;
 EddystoneLength,AdType,EddystoneLo,EddystoneHi,EddystoneType,TransmitPower,Rssi,C,AdEventType,AddressType:Byte;
 DataLength,MainLength,FlagsLength,FlagsType,Flags:Byte;
 TlmVersion:Byte;
 TlmBattery:Word;
 TlmTemperature:Word;
 TlmAdvCount:LongWord;
 TlmSecCount:LongWord;
 Event:array of Byte;
 S:string;
 GetByteIndex:Integer;
 AddressString:string;
 MainType,MfrLo,MfrHi,MainValue:Byte;
 MfrType,MfrLength,MfrFirst,MfrSecond:Byte;
 Uuid:TUuid;
 MajorLo,MajorHi,MinorLo,MinorHi:Byte;
 NameSpace:array[0 .. 9] of Byte;
 Instance:array[0 .. 5] of Byte;
 EphemeralId:array[0 .. 7] of Byte;
 AltBeaconUuid:TUuid;
 AltBeaconFourBytes:array [0 .. 3] of Byte;
 AltBeaconReserved:Byte;
 AddressBytes:array[0 .. 5] of Byte;
 LeEventType:Byte;
function GetByte:Byte;
begin
 Result:=Event[GetByteIndex];
 Inc(GetByteIndex);
end;
function GetWord:Word;
begin
 Result:=GetByte;
 Result:=GetByte or (Result shl 8);
end;
function GetLongWord:LongWord;
begin
 Result:=GetWord;
 Result:=GetWord or (Result shl 16);
end;
begin
 EventType:=EventReadFirstByte;
 EventSubtype:=Readbyte;
 EventLength:=ReadByte;
 SetLength(Event,0);
 S:='';
 for I:=1 to EventLength - 1 do
  begin
   SetLength(Event,Length(Event) + 1);
   Event[I - 1]:=ReadByte;
   if I > 11 then
    begin
     S:=S + Event[I - 1].ToHexString(2);
     if I mod 4 = 3 then
      S:=S + ' ';
    end;
  end;
 Rssi:=ReadByte;
 if EventSubType <> $3e then
  begin
   Log(Format('ParseEvent type %02.2x subtype %02.2x length %d discarded',[EventType,EventSubType,EventLength]));
   Sleep(5*1000);
   exit;
  end;
 if S = '' then
  begin
   S:='(no data)';
   exit;
  end;
 GetByteIndex:=0;
 LeEventType:=GetByte;
 GetByte;
 if LeEventType <> $02 then
  begin
   Log(Format('ParseEvent le event type %02.2x length %d discarded',[LeEventType,EventLength]));
   Sleep(5*1000);
   exit;
  end;
 AdEventType:=GetByte;
 AddressType:=GetByte;
 AddressString:='';
 for I:=0 to 5 do
  begin
   AddressBytes[I]:=GetByte;
   AddressString:=AddressBytes[I].ToHexString(2) + AddressString;
  end;
 AddressString:=AddressString + MacAddressTypeToStr(AddressType) + AdEventTypeToStr(AdEventType);
 DataLength:=GetByte;
 ParsedPacket:=ParseAdPacket(GetByteIndex,Event);
 Track(ClockGetCount,Rssi,Format('%s ads',[AddressString]),ParsedPacket.ToDisplayString);
 FlagsLength:=GetByte;
 FlagsType:=GetByte;
 Flags:=GetByte;
 MainLength:=GetByte;
 MainType:=GetByte;
 MfrLo:=GetByte;
 MainValue:=MfrLo;
 MfrHi:=GetByte;
 MfrType:=GetByte;
 MfrLength:=GetByte;
 MfrFirst:=GetByte;
 MfrSecond:=GetByte;
 GetByteIndex:=18;
 EddystoneLength:=GetByte;
 AdType:=GetByte;
 EddystoneLo:=GetByte;
 EddystoneHi:=GetByte;
 EddystoneType:=GetByte;
 if FlagsType = $ff then
  begin
   GetByteIndex:=13;
   MfrLo:=GetByte;
   MfrHi:=GetByte;
   MfrType:=GetByte;
   MfrLength:=GetByte;
   MfrFirst:=GetByte;
   MfrSecond:=GetByte;
   if (AsWord(MfrHi,MfrLo) = Word(ManufacturerTesting)) and (MfrType = $55) and (MfrLength = $96) then
    begin
     S:=Char(MfrSecond);
     while GetByteIndex <= High(Event) do
      begin
       S:=S + Char(GetByte);
      end;
     Track(ClockGetCount,Rssi,Format('%s ult chan %d',[AddressString,MfrFirst]),S);
    end
    // else if (((MfrHi shl 8) or MfrLo) = ManufacturerMicrosoft) and (MfrType = $01) and (MfrLength = $09) and (MfrFirst = $20) then
    //       begin
    //        S:=IntToStr(Length(Event) - 19 ) + ' salt ';
    //        GetByteIndex:=19;
    //        while GetByteIndex <= 22 do
    //         S:=S + GetByte.ToHexString(2);
    //        S:=S + ' hash ';
    //        while GetByteIndex <= High(Event) do
    //         S:=S + GetByte.ToHexString(2);
    //        Track(ClockGetCount,Rssi,Format('%s mfr win10',[AddressString]),S);
    //       end;
    // else
    //  begin
    //   S:='';
    //   GetByteIndex:=15;
    //   while GetByteIndex <= High(Event) do
    //    begin
    //     S:=S + GetByte.ToHexString(2);
    //     if (GetByteIndex mod 4) = 3 then
    //      S:=S + ' ';
    //    end;
    //   Track(ClockGetCount,Rssi,Format('%s mfr %s %s',[AddressString,ManufacturerToString(AsWord(MfrHi,MfrLo)),S]),'');
    //  end;
  end
 else if (FlagsLength = $04) and (FlagsType = $09) then
       begin
        S:='';
        GetByteIndex:=13;
        while GetByteIndex <= High(Event) do
         S:=S + Char(GetByte);
        Track(ClockGetCount,Rssi,Format('%s cnm <%s>',[AddressString,S]),'');
       end
 else if (MainType = $ff) and (((MfrHi shl 8) or MfrLo) = ManufacturerApple) then
       begin
        if (MfrType = $02) and (MfrLength = $15) then
         begin
          GetByteIndex:=20;
          for I:=Low(Uuid) to High(Uuid) do
           Uuid[I]:=GetByte;
          MajorHi:=GetByte;
          MajorLo:=GetByte;
          MinorHi:=GetByte;
          MinorLo:=GetByte;
          TransmitPower:=GetByte;
          Track(ClockGetCount,Rssi,Format('%s ibn %6stx %s mjr %d mnr %d',[AddressString,dBm(TransmitPower),UuidToStr(Uuid),AsWord(MajorHi,MajorLo),AsWord(MinorHi,MinorLo)]),'');
         end
         //      else
         //       begin
         //        S:='';
         //        GetByteIndex:=18;
         //        while GetByteIndex <= High(Event) do
         //         begin
         //          S:=S + GetByte.ToHexString(2);
         //          if (GetByteIndex mod 4) = 2 then
         //           S:=S + ' ';
         //         end;
         //        Track(ClockGetCount,Rssi,Format('%s mfr %s %s',[AddressString,ManufacturerToString(AsWord(MfrHi,MfrLo)),LeftStr(S,4)]),RightStr(S,Length(S) - 4));
         //       end
       end
 else if (MainType = $ff) and (((MfrHi shl 8) or MfrLo) = ManufacturerTesting) and (MfrType = $BE) and (MfrLength = $AC) then
       begin
        GetByteIndex:=20;
        for I:=0 to High(AltBeaconUuid) do
         AltBeaconUuid[I]:=GetByte;
        S:='';
        for I:=0 to High(AltBeaconFourBytes) do
         begin
          AltBeaconFourBytes[I]:=GetByte;
          S:=S + AltBeaconFourBytes[I].ToHexString(2);
         end;
        TransmitPower:=GetByte;
        AltBeaconReserved:=GetByte;
        Track(ClockGetCount,Rssi,Format('%s abn %6stx %s %s reserved %02.2x',[AddressString,dBm(TransmitPower),UuidToStr(AltBeaconUuid),S,AltBeaconReserved]),'');
       end
 else if (MainType = $ff) then
       begin
        S:='';
        GetByteIndex:=18;
        while GetByteIndex <= High(Event) do
         begin
          S:=S + GetByte.ToHexString(2);
          if (GetByteIndex mod 4) = 2 then
           S:=S + ' ';
         end;
        Track(ClockGetCount,Rssi,Format('%s mfr %s %s',[AddressString,ManufacturerToString(AsWord(MfrHi,MfrLo)),S]),'');
       end
 else if (AdType = ADT_SERVICE_DATA) and (((EddystoneHi shl 8) or EddystoneLo) = EddystoneUuid) then
       begin
        if EddystoneType = $00 then
         begin
          TransmitPower:=GetByte;
          S:='namespace ';
          for I:=0 to High(NameSpace) do
           begin
            NameSpace[I]:=GetByte;
            S:=S + NameSpace[I].ToHexString(2);
           end;
          S:=S + ' instance ';
          for I:=0 to High(Instance) do
           begin
            Instance[I]:=GetByte;
            S:=S + Instance[I].ToHexString(2);
           end;
          Track(ClockGetCount,Rssi,Format('%s uid %6stx %s',[AddressString,dBm(TransmitPower),S]),'');
         end
        else if EddystoneType = $10 then
              begin
               TransmitPower:=GetByte;
               C:=GetByte;
               S:=Scheme[C];
               while GetByteIndex <= High(Event) do
                begin
                 C:=GetByte;
                 if C <= High(Expansion) then
                  S:=S + Expansion[C]
                 else
                  S:=S + Char(C);
                end;
               Track(ClockGetCount,Rssi,Format('%s url %6stx %s',[AddressString,dBm(TransmitPower),S]),'');
              end
        else if EddystoneType = $20 then
              begin
               TlmVersion:=GetByte;
               TlmBattery:=GetWord;
               TlmTemperature:=GetWord;
               TlmAdvCount:=GetLongWord;
               TlmSecCount:=GetLongWord;
               Track(ClockGetCount,Rssi,Format('%s tlm',[AddressString]),Format('battery %5.3fV temp %2d.%02.2dC adv %d up %s',[TlmBattery*0.001,(TlmTemperature shr 8),TlmTemperature and $ff,TlmAdvCount,TimeToString(SecondsToTime(TlmSecCount div 10))]));
              end
        else if EddystoneType = $30 then
              begin
               TransmitPower:=GetByte;
               S:='';
               for I:=0 to High(EphemeralId) do
                begin
                 EphemeralId[I]:=GetByte;
                 S:=S + NameSpace[I].ToHexString(2);
                end;
               Track(ClockGetCount,Rssi,Format('%s eid %6stx %s',[AddressString,dBm(TransmitPower),S]),'');
              end
        else
         begin
          Track(ClockGetCount,Rssi,Format('%s hex %s',[AddressString,S]),'');
          Log(Format('%s hex %s',[AddressString,S]));
         end;
       end
 else
  begin
   // Log(Format('message %02.2x %02.2x %d bytes %s',[EventType,EventSubtype,EventLength,S]));
   // Track(ClockGetCount,Rssi,Format('%s hex %s',[AddressString,S]),'');
  end;
end;

procedure FlushRx;
var 
 Res:LongWord;
 C:LongWord;
 B:Byte;
 TimeStamp:LongWord;
begin
 TimeStamp:=ClockGetCount;
 while LongWord(ClockGetCount - TimeStamp) < 1*1000*1000 do
  begin
   Res:=SerialDeviceRead(UART0,@B,1,SERIAL_READ_NON_BLOCK,C);
   if (Res = ERROR_SUCCESS) and (C = 1) then
    TimeStamp:=ClockGetCount
   else
    Sleep(100);
  end;
end;

procedure Flush(Message:String);
begin
 LoggingOutput(Format('Flush %s',[Message]));
 FlushRx;
 try
  StopAdvertising;
 except
  on E:Exception do
       begin
        LoggingOutput(Format('Flush did not stop advertising %s',[E.Message]));
        FlushRx;
       end;
end;
try
 StopScanning;
except
 on E:Exception do
      begin
       LoggingOutput(Format('Flush did not stop scanning %s',[E.Message]));
       FlushRx;
      end;
end;
FlushRx;
end;

function LedLoop(Parameter:Pointer):PtrInt;
var 
 I,N:Integer;
begin
 Result:=0;
 ActivityLedEnable;
 while True do
  begin
   ActivityLedOn;
   Sleep(100);
   ActivityLedOff;
   N:=Min(ScanRxCount,3);
   for I:=1 to N do
    begin
     Sleep(100);
     ActivityLedOn;
     Sleep(100);
     ActivityLedOff;
    end;
   Sleep(900 - 200*N);
  end;
end;

begin
 Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
 Console2 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,False);
 Console3 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,False);
 ConsoleWindowSetBackcolor(Console2,COLOR_BLACK);
 ConsoleWindowSetForecolor(Console2,COLOR_YELLOW);
 ConsoleWindowSetBackcolor(Console3,COLOR_CYAN);
 ConsoleWindowSetForecolor(Console3,COLOR_WHITE);
 ConsoleWindowClear(Console2);
 ConsoleWindowClear(Console3);
 Log('bluetooth-dev');
 RestoreBootFile('default','config.txt');
 StartLogging;

 HtmlReportLock:=SemaphoreCreate(1);
 IpAddressAvailable:=False;
 BeginThread(@KeyboardLoop,Nil,KeyboardLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 BeginThread(@MonitorLoop,Nil,MonitorLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 BeginThread(@IpAddressLoop,Nil,IpAddressLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 Help;
 WaitForSDDrive;

 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 WEBSTATUS_FONT_NAME:='Monospace';
 WebStatusRegister(HTTPListener,'','',False);
 BluetoothWebStatus:=TBluetoothWebStatus.Create('Bluetooth','/bluetooth',1);
 HTTPListener.RegisterDocument('',BluetoothWebStatus);
 HTTPRedirect:=THTTPRedirect.Create;
 HTTPRedirect.Name:='/';
 HTTPRedirect.Location:='/status/bluetooth';
 HTTPListener.RegisterDocument('',HTTPRedirect);

 if IsBlueToothAvailable then
  begin
   ReadByteCounter:=0;
   OpenUart0;
   ResetChip;
   try
    BCMLoadFirmware;
   except
    on E:Exception do
         begin
          LoggingOutput(Format('load exception %s',[E.Message]));
         end;
  end;
 SetLEEventMask($ff);
 Log('Init complete');
 BeginThread(@LedLoop,Nil,LedLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 ExceptionRestartCounter:=0;
 ScanCycleCounter:=0;
 ReadByteCounter:=0;
 while True do
  begin
   try
    RestartBroadcastObserveLoop:=False;
    ReadBackLog:=0;
    DropByte:=False;
    StartLeAdvertising;
    Margin:=High(Margin);
    SetLength(MessageTrackList,0);
    SetLength(ConnectionList,0);
    StartActiveScanning;
    Log('Receiving scan data');
    ScanIdle:=True;
    ScanRxCount:=0;
    while not RestartBroadcastObserveLoop do
     ParseEvent;
   except
    on E:Exception do
         begin
          Inc(ExceptionRestartCounter);
          LoggingOutput(Format('ExceptionRestartCounter %d',[ExceptionRestartCounter]));
          Flush(E.Message);
         end;
  end;
end;
end;
ThreadHalt(0);
end.
