unit frmAboutU;

interface

uses WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    Comments: TLabel;
    Panel2: TPanel;
    OKButton: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    CompanyName: TLabel;
    Bevel1: TBevel;
    Version: TLabel;
    memoCredits: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateProductInformation;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  FileVersionInformationU;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  ProductName.Font.Size := ProductName.Font.Size * 2;
  CompanyName.Font.Size := CompanyName.Font.Size + 2;
  UpdateProductInformation;
end;

procedure TfrmAbout.UpdateProductInformation;
var
  LFileVersionInformation: TFileVersionInformation;
begin
  LFileVersionInformation := TFileVersionInformation.Create;
  try
    try
      LFileVersionInformation.FileName := ParamStr(0);
      ProductName.Caption := LFileVersionInformation.ProductName;
      Version.Caption := LFileVersionInformation.FileVersion;
      CompanyName.Caption := LFileVersionInformation.CompanyName;
      Comments.Caption := LFileVersionInformation.Comments;
    except

    end;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

end.
