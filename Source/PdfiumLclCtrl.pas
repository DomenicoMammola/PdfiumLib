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
    FPageWidth: Integer;
    FPageHeight : Integer;
    FX, FY : Integer;

    FScaleMode: TPdfControlScaleMode;
    FZoomPercentage: Integer;
    procedure SetScaleMode(AValue: TPdfControlScaleMode);
    procedure SetZoomPercentage(AValue: Integer);
    procedure AdjustGeometry;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String; const Password: String = ''; LoadOption: TPdfDocumentLoadOption = dloDefault);
    function GotoNextPage(ScrollTransition: Boolean = False): Boolean;
    function GotoPrevPage(ScrollTransition: Boolean = False): Boolean;
    procedure HightlightText(const SearchText: string; MatchCase, MatchWholeWord: Boolean);
    {$ifdef linux}
    // explanation here: https://forum.lazarus.freepascal.org/index.php?topic=38041.0
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    {$endif}
  public
    property Document: TPdfDocument read FDocument;
    property ScaleMode: TPdfControlScaleMode read FScaleMode write SetScaleMode default smFitAuto;
    property ZoomPercentage: Integer read FZoomPercentage write SetZoomPercentage default 100;
  end;


implementation
uses
  Graphics, Math;

{ TLCLPdfControl }

procedure TLCLPdfControl.SetScaleMode(AValue: TPdfControlScaleMode);
begin
  if FScaleMode=AValue then Exit;
  FScaleMode:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TLCLPdfControl.SetZoomPercentage(AValue: Integer);
begin
  if FZoomPercentage=AValue then Exit;
  FZoomPercentage:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TLCLPdfControl.AdjustGeometry;
var
  curPage : TPdfPage;
begin
  if FPageIndex < FDocument.PageCount then
  begin
    curPage := FDocument.Pages[FPageIndex];
    if FScaleMode = smFitAuto then
    begin
      if curPage.Width < curPage.Height then
      begin
        FPageWidth := Self.ClientRect.Width;
        FPageHeight := min(Self.ClientRect.Height, round(curPage.Height * curPage.Width / curPage.Height));
        FX := 0;
        FY := (Self.ClientRect.Height - FPageHeight) div 2;
      end
      else
      begin
        FPageHeight := Self.ClientRect.Height;
        FPageWidth := min(Self.ClientRect.Width, trunc(FPageHeight * curPage.Width / curPage.Height));
        FX := (Self.ClientRect.Width - FPageWidth) div 2;
        FY := 0;
      end;
    end
    else if FScaleMode = smFitWidth then
    begin
      FPageWidth := Self.ClientRect.Width;
      FPageHeight := trunc(curPage.Height * FPageWidth / curPage.Width);
      FX := 0;
      FY := 0;
    end
    else if FScaleMode = smFitHeight then
    begin
      FPageHeight := Self.ClientRect.Height;
      FPageWidth := trunc(curPage.Width * FPageHeight / curPage.Height);
      FX := 0;
      FY := 0;
    end
    else
    begin
      FPageWidth := trunc(curPage.Width);
      FPageHeight := trunc(curPage.Height);
      FX := 0;
      FY := 0;
    end;
    (*
        smFitAuto,
    smFitWidth,
    smFitHeight,
    smZoom
    *)
  end;
end;

procedure TLCLPdfControl.Paint;
var
  curPage : TPdfPage;
  x, y, w, h : Integer;
begin
  inherited Paint;
  Canvas.Brush.Color:= clRed;
  Canvas.FillRect(ClientRect);
  if FPageIndex < FDocument.PageCount then
  begin
    curPage := FDocument.Pages[FPageIndex];
    curPage.DrawToCanvas(Self.Canvas, FX, FY, FPageWidth, FPageHeight);
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
  FPageWidth := 0;
  FPageHeight := 0;
  FX := 0;
  FY := 0;
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
  AdjustGeometry;
end;

function TLCLPdfControl.GotoNextPage(ScrollTransition: Boolean): Boolean;
begin
  Result := false;
  if FPageIndex < FDocument.PageCount - 1 then
  begin
    inc(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

function TLCLPdfControl.GotoPrevPage(ScrollTransition: Boolean): Boolean;
begin
  Result := false;
  if (FPageIndex > 0) then
  begin
    dec(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

procedure TLCLPdfControl.HightlightText(const SearchText: string; MatchCase,
  MatchWholeWord: Boolean);
begin

end;

procedure TLCLPdfControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  AdjustGeometry;
  Invalidate;
end;

end.
