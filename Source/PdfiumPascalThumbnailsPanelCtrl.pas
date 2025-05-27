unit PdfiumPascalThumbnailsPanelCtrl;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, ExtCtrls, Forms, Graphics,
  PdfiumCore;

type

  { TPdfPageThumbnailPanel }

  TPdfPageThumbnailPanel = class(TCustomPanel)
  strict private
    const MARGIN_WIDTH : integer = 20;
  strict private
    FPage : TPdfPage;
    FCachedBitmap : TBitmap;
    FViewportX, FViewportY : integer;
    procedure SetPage(AValue: TPdfPage);
    procedure AdjustGeometry(out aPageWidth, aPageHeight, aViewportX, aViewportY : integer);
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Page : TPdfPage read FPage write SetPage;
  end;

  { TPdfThumbnailsPanel }

  TPdfThumbnailsPanel = class(TCustomControl)
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { TPdfThumbsControl }

  TPdfThumbsControl = class(TCustomPanel)
  strict private
    FDocument: TPdfDocument;
    FScrollBox : TScrollBox;
    FPanels : TList;
    procedure SetDocument(AValue: TPdfDocument);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Document: TPdfDocument read FDocument write SetDocument;
  end;

implementation

uses
  SysUtils, Math, LCLIntf, LCLType,
  PdfiumPascalViewPageCtrl;

{ TPdfPageThumbnailPanel }

procedure TPdfPageThumbnailPanel.SetPage(AValue: TPdfPage);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
  FreeAndNil(FCachedBitmap);
end;

procedure TPdfPageThumbnailPanel.AdjustGeometry (out aPageWidth, aPageHeight, aViewportX, aViewportY : integer);
var
  relPage, relViewport : double;
  ch, cw : integer;
  scroolbarSize : integer;
begin
//  LCLIntf.GetSystemMetrics(SM_CXVSCROLL);
  scroolbarSize := LCLIntf.GetSystemMetrics(SM_CYHSCROLL);
  ch := ClientRect.Height - (2 * MARGIN_WIDTH) - scroolbarSize;
  cw := ClientRect.Width - (2 * MARGIN_WIDTH);
  relPage:= FPage.Height / FPage.Width;
  relViewport:= ch / cw;

  if (relViewport > relPage) then
  begin
    aPageWidth := cw;
    aPageHeight := min(ch, round(aPageWidth * FPage.Height / FPage.Width));
    aViewportX := MARGIN_WIDTH;
    aViewportY := MARGIN_WIDTH + ((ch - aPageHeight) div 2);
  end
  else
  begin
    aPageHeight := ch;
    aPageWidth := min(cw, round(aPageHeight * FPage.Width / FPage.Height));
    aViewportX := MARGIN_WIDTH + ((cw - aPageWidth) div 2);
    aViewportY := MARGIN_WIDTH;
  end;
end;

procedure TPdfPageThumbnailPanel.Paint;
var
  pageWidth, pageHeight : Integer;
begin
  inherited Paint;
  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= Self.Color;
  Canvas.FillRect(ClientRect);

  if Assigned(FPage) then
  begin
    if not Assigned(FCachedBitmap) then
    begin
      AdjustGeometry(pageWidth, pageHeight, FViewportX, FViewportY);

      FCachedBitmap:= TBitmap.Create;
      FCachedBitmap.Width:= pageWidth;
      FCachedBitmap.Height:= pageHeight;
      FCachedBitmap.Canvas.Brush.Style:= bsSolid;
      FCachedBitmap.Canvas.Brush.Color:= clWhite;
      FCachedBitmap.Canvas.FillRect(0, 0, FCachedBitmap.Width, FCachedBitmap.Height);

      if Assigned(GraphicsBackend_DrawPageToCanvas) then
        GraphicsBackend_DrawPageToCanvas(FPage, FCachedBitmap.Canvas, 0, 0, pageWidth, pageHeight, prNormal, [], clWhite);
    end;

    Self.Canvas.Draw(FViewportX, FViewportY, FCachedBitmap);
  end;
end;

constructor TPdfPageThumbnailPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPage := nil;
  FCachedBitmap := nil;
  Color:= clDkGray;
end;

destructor TPdfPageThumbnailPanel.Destroy;
begin
  FreeAndNil(FCachedBitmap);
  inherited Destroy;
end;

{ TPdfThumbnailsPanel }

procedure TPdfThumbnailsPanel.Paint;
begin
  Canvas.Lock;
  try
    Canvas.Pen.Mode := pmCopy;
    Canvas.Brush.Color := Self.Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);

  finally
    Canvas.Unlock;
  end;
end;

procedure TPdfThumbnailsPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TPdfThumbnailsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPdfThumbnailsPanel.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TPdfThumbnailsPanel.Click;
begin
  inherited Click;
end;

procedure TPdfThumbnailsPanel.DblClick;
begin
  inherited DblClick;
end;

constructor TPdfThumbnailsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TPdfThumbnailsPanel.Destroy;
begin
  inherited Destroy;
end;

{ TPdfThumbsControl }

procedure TPdfThumbsControl.SetDocument(AValue: TPdfDocument);
var
  i : integer;
  curPanel : TPdfPageThumbnailPanel;
begin
  FScrollBox.Visible:= false;
  try
    for i := 0 to FPanels.Count - 1 do
      TPdfPageThumbnailPanel(FPanels.Items[i]).Free;
    FPanels.Clear;

    FDocument:=AValue;
    for i := FDocument.PageCount - 1 downto 0 do
    begin
      curPanel := TPdfPageThumbnailPanel.Create(FScrollBox);
      curPanel.Parent := FScrollBox;
      curPanel.Width:= FScrollBox.HorzScrollBar.ClientSizeWithoutBar;
      curPanel.Height:= FScrollBox.HorzScrollBar.ClientSizeWithoutBar;
      curPanel.Align:= alLeft;
      curPanel.Page := FDocument.Pages[i];
      FPanels.Add(curPanel);
    end;
  finally
    FScrollBox.Visible:= true;
  end;

end;


constructor TPdfThumbsControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPanels := TList.Create;
  Self.BorderStyle := bsNone;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.Parent := Self;
  FScrollBox.Align := alClient;
  FScrollBox.VertScrollBar.Visible := false;
end;

destructor TPdfThumbsControl.Destroy;
begin
  FPanels.Free;
  inherited Destroy;
end;

end.
