unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.VirtualImage, Vcl.BaseImageCollection, vcl.ImageCollection,shellapi;

type
  TForm2 = class(TForm)
    VirtualImage1: TVirtualImage;
    ImageCollection1: TImageCollection;
    Label1: TLabel;
    Label2: TLabel;
    procedure Label2Click(Sender: TObject);
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Label2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://github.com/spawn451', nil, nil, SW_SHOWNORMAL);
end;


procedure TForm2.Label2MouseEnter(Sender: TObject);
begin
self.Label2.Font.Color:=clblue;
end;

procedure TForm2.Label2MouseLeave(Sender: TObject);
begin
 self.Label2.Font.Color:=clpurple;
end;

end.
