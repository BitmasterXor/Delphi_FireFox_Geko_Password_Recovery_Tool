unit Unit1;

interface

uses
 // Standard Windows and VCL units
 Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
 Vcl.StdCtrls, Vcl.ComCtrls, System.JSON, System.IOUtils, Vcl.Dialogs,
 System.IniFiles, Uni, SQLiteUniProvider, System.NetEncoding;

type
 // Structure for NSS crypto operations
 TSecurityItem = record
   TypeFlag: Integer; // Type of security operation
   DataPtr: PByte;    // Pointer to data
   Length: Cardinal;   // Data length
 end;
 PSecurityItem = ^TSecurityItem;

 // Main form class
 TForm1 = class(TForm)
   ListView1: TListView;      // Display passwords
   Button1: TButton;         // Decrypt button
   ComboBox1: TComboBox;     // Profile selector
   Label1: TLabel;
    Button2: TButton;           // Profile label
   procedure FormCreate(Sender: TObject);
   procedure Button1Click(Sender: TObject);
   procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
 private
   FProfileDir: string;      // Firefox profile directory
   FDBConn: TUniConnection;  // SQLite connection
 end;

var
 Form1: TForm1;
 // NSS function pointers
 NSS: record
   Handle: HMODULE;  // DLL handle
   // Function pointers to NSS crypto APIs
   Init: function(cfg: PAnsiChar): Integer; cdecl;
   Stop: function: Integer; cdecl;
   GetSlot: function: Pointer; cdecl;
   Decrypt: function(const inp, outp: PSecurityItem; ctx: Pointer): Integer; cdecl;
   CheckPwd: function(slot: Pointer; pwd: PAnsiChar): Integer; cdecl;
   NeedLogin: function(slot: Pointer): Integer; cdecl;
 end;

implementation

{$R *.dfm}

uses Unit2;

// Decrypt Base64 encrypted data using NSS
function DecryptData(const Data: string): string;
var
 raw: TBytes;
 inp, outp: TSecurityItem;
begin
 // Decode Base64 to bytes
 raw := TNetEncoding.Base64.DecodeStringToBytes(Data);

 // Setup input security item
 inp.TypeFlag := 0;
 inp.DataPtr := @raw[0];
 inp.Length := Length(raw);

 // Setup empty output security item
 outp := Default(TSecurityItem);

 // Decrypt using NSS
 if NSS.Decrypt(@inp, @outp, nil) = 0 then
   SetString(Result, PAnsiChar(outp.DataPtr), outp.Length)
 else
   Result := '***error***';
end;

// Form creation - initialize DB and load profiles
procedure TForm1.Button2Click(Sender: TObject);
begin
form2.showmodal;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 ini: TIniFile;
 sections: TStringList;
 path: string;
begin
 // Setup SQLite connection
 FDBConn := TUniConnection.Create(nil);
 FDBConn.ProviderName := 'SQLite';

 // Load Firefox profiles from profiles.ini
 path := GetEnvironmentVariable('APPDATA') + '\Mozilla\Firefox\profiles.ini';
 if FileExists(path) then begin
   sections := TStringList.Create;
   ini := TIniFile.Create(path);
   try
     // Read all profile sections
     ini.ReadSections(sections);
     for var i := 0 to sections.Count - 1 do
       if sections[i].StartsWith('Profile') then
         if ini.ReadString(sections[i], 'Path', '') <> '' then
           ComboBox1.Items.Add(ini.ReadString(sections[i], 'Path', ''));
     if ComboBox1.Items.Count > 0 then ComboBox1.ItemIndex := 0;
   finally
     ini.Free;
     sections.Free;
   end;
 end;
end;

// Form destruction - cleanup
procedure TForm1.FormDestroy(Sender: TObject);
begin
 // Shutdown NSS
 if Assigned(NSS.Stop) then NSS.Stop();
 if NSS.Handle <> 0 then FreeLibrary(NSS.Handle);
 // Free DB connection
 FDBConn.Free;
end;

// Main password decryption logic
procedure TForm1.Button1Click(Sender: TObject);
var
 ffdir: string;     // Firefox installation dir
 creds: TJSONArray; // Credentials array
 slot: Pointer;     // NSS key slot

 // Get credentials from JSON or SQLite
 function GetCreds: TJSONArray;
 begin
   try
     // Try loading from logins.json first
     Result := TJSONObject.ParseJSONValue(
       TFile.ReadAllText(FProfileDir + '\logins.json'))
       .GetValue<TJSONArray>('logins');
   except
     // Fall back to SQLite if JSON fails
     Result := TJSONArray.Create;
     if FileExists(FProfileDir + '\signons.sqlite') then
       with TUniQuery.Create(nil) do try
         // Setup SQLite query
         Connection := FDBConn;
         FDBConn.Database := FProfileDir + '\signons.sqlite';
         FDBConn.Connect;
         SQL.Text := 'SELECT * FROM moz_logins';
         Open;
         // Read all credentials
         while not Eof do begin
           Result.AddElement(TJSONObject.Create
             .AddPair('hostname', FieldByName('hostname').AsString)
             .AddPair('encryptedUsername', FieldByName('encryptedUsername').AsString)
             .AddPair('encryptedPassword', FieldByName('encryptedPassword').AsString)
             .AddPair('encType', TJSONNumber.Create(FieldByName('encType').AsInteger)));
           Next;
         end;
       finally
         Free;
       end;
   end;
 end;

begin
 if ComboBox1.ItemIndex < 0 then Exit;

 // Set profile directory
 FProfileDir := ExtractFilePath(GetEnvironmentVariable('APPDATA') +
   '\Mozilla\Firefox\profiles.ini') + ComboBox1.Items[ComboBox1.ItemIndex];

 // Get Firefox installation directory
 if SizeOf(Pointer) = 8 then
   ffdir := 'C:\Program Files\Mozilla Firefox\'
 else
   ffdir := 'C:\Program Files (x86)\Mozilla Firefox\';

 // Change to Firefox directory temporarily
 var olddir := GetCurrentDir;
 SetCurrentDir(ffdir);
 try
   // Load NSS library
   NSS.Handle := LoadLibrary(PChar(ffdir + 'nss3.dll'));
   if NSS.Handle <> 0 then begin
     // Get NSS function pointers
     NSS.Init := GetProcAddress(NSS.Handle, 'NSS_Init');
     NSS.Stop := GetProcAddress(NSS.Handle, 'NSS_Shutdown');
     NSS.GetSlot := GetProcAddress(NSS.Handle, 'PK11_GetInternalKeySlot');
     NSS.Decrypt := GetProcAddress(NSS.Handle, 'PK11SDR_Decrypt');
     NSS.CheckPwd := GetProcAddress(NSS.Handle, 'PK11_CheckUserPassword');
     NSS.NeedLogin := GetProcAddress(NSS.Handle, 'PK11_NeedLogin');

     // Initialize NSS for selected profile
     if NSS.Init(PAnsiChar(AnsiString('sql:' + FProfileDir))) = 0 then begin
       slot := NSS.GetSlot();
       // Check if we can access the key slot
       if (slot <> nil) and
          ((NSS.NeedLogin(slot) = 0) or (NSS.CheckPwd(slot, '') = 0)) then begin
         ListView1.Items.Clear;
         // Get credentials
         creds := GetCreds;
         try
           // Decrypt and display credentials
           for var i := 0 to creds.Count - 1 do
             with ListView1.Items.Add, creds.Items[i] as TJSONObject do begin
               Caption := GetValue<string>('hostname');
               if GetValue<Integer>('encType') = 1 then begin
                 SubItems.Add(DecryptData(GetValue<string>('encryptedUsername')));
                 SubItems.Add(DecryptData(GetValue<string>('encryptedPassword')));
               end else begin
                 SubItems.Add(GetValue<string>('encryptedUsername'));
                 SubItems.Add(GetValue<string>('encryptedPassword'));
               end;
             end;
         finally
           creds.Free;
         end;
       end;
     end;
   end;
 finally
   SetCurrentDir(olddir);
 end;
end;

end.
