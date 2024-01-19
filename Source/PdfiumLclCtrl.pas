unit PdfiumLclCtrl;

interface

uses
  Classes, Controls,
  PdfiumCtrl, PdfiumCore;

type

  { TLCLPdfControl }

  TLCLPdfControl = class(TCustomControl)
  strict private
    FDocument: TPdfDocument;
    FPageIndex: Integer;

    FScaleMode: TPdfControlScaleMode;
    FZoomPercentage: Integer;
    procedure SetScaleMode(AValue: TPdfControlScaleMode);
    procedure SetZoomPercentage(AValue: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String; const Password: String = ''; LoadOption: TPdfDocumentLoadOption = dloDefault);
    function GotoNextPage(ScrollTransition: Boolean = False): Boolean;
    function GotoPrevPage(ScrollTransition: Boolean = False): Boolean;
    procedure HightlightText(const SearchText: string; MatchCase, MatchWholeWord: Boolean);
  public
    property Document: TPdfDocument read FDocument;
    property ScaleMode: TPdfControlScaleMode read FScaleMode write SetScaleMode default smFitAuto;
    property ZoomPercentage: Integer read FZoomPercentage write SetZoomPercentage default 100;
  end;


implementation
uses
  Graphics;

{ TLCLPdfControl }

procedure TLCLPdfControl.SetScaleMode(AValue: TPdfControlScaleMode);
begin
  if FScaleMode=AValue then Exit;
  FScaleMode:=AValue;
end;

procedure TLCLPdfControl.SetZoomPercentage(AValue: Integer);
begin
  if FZoomPercentage=AValue then Exit;
  FZoomPercentage:=AValue;
end;

procedure TLCLPdfControl.Paint;
var
  curPage : TPdfPage;
begin
  inherited Paint;
  Canvas.Brush.Color:= clRed;
  Canvas.FillRect(ClientRect);
  if FPageIndex < FDocument.PageCount then
  begin
    curPage := FDocument.Pages[FPageIndex];
    curPage.DrawToCanvas(Self.Canvas, 0, 0, 100, 100);
  end;

end;

constructor TLCLPdfControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleMode := smFitAuto;
  FZoomPercentage := 100;
  FDocument := TPdfDocument.Create;
  Color:= clGray;
  Width := 130;
  Height := 180;
  FPageIndex := 0;
end;

destructor TLCLPdfControl.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

procedure TLCLPdfControl.LoadFromFile(const FileName: String; const Password: String; LoadOption: TPdfDocumentLoadOption);
begin
  FDocument.LoadFromFile(FileName, Password, LoadOption);
end;

function TLCLPdfControl.GotoNextPage(ScrollTransition: Boolean): Boolean;
begin
  if FPageIndex < FDocument.PageCount - 1 then
  begin
    inc(FPageIndex);
    Invalidate;
  end;
end;

function TLCLPdfControl.GotoPrevPage(ScrollTransition: Boolean): Boolean;
begin
  if (FPageIndex > 0) then
  begin
    dec(FPageIndex);
    Invalidate;
  end;
end;

procedure TLCLPdfControl.HightlightText(const SearchText: string; MatchCase,
  MatchWholeWord: Boolean);
begin

end;

end.
