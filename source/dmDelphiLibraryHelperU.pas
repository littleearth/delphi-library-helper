unit dmDelphiLibraryHelperU;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls;

type
  TdmDelphiLibraryHelper = class(TDataModule)
    ImageListCommon: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDelphiLibraryHelper: TdmDelphiLibraryHelper;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
