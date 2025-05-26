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
    FPage : TPdfPage;
    FCachedBitmap : TBitmap;
    procedure SetPage(AValue: TPdfPage);
    procedure AdjustGeometry(out aPageWidth, aPageHeight, aViewportX, aViewportY : integer);
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Page : TPdfPage read FPage write SetPage;
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
  SysUtils, Math,
  PdfiumPascalViewPageCtrl;

{ TPdfPageThumbnailPanel }

procedure TPdfPageThumbnailPanel.SetPage(AValue: TPdfPage);
var
  pageWidth, pageHeight, viewportX, viewportY : Integer;
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
  FreeAndNil(FCachedBitmap);

  FCachedBitmap:= TBitmap.Create;
  FCachedBitmap.Width:= Self.Width;
  FCachedBitmap.Height:= Self.Height;
  if Assigned(GraphicsBackend_DrawPageToCanvas) then
  begin
    AdjustGeometry(pageWidth, pageHeight, viewportX, viewportY);
    GraphicsBackend_DrawPageToCanvas(FPage, FCachedBitmap.Canvas, viewportX, viewportY, pageWidth, pageHeight, prNormal, [], Self.Color);
  end;
end;

procedure TPdfPageThumbnailPanel.AdjustGeometry (out aPageWidth, aPageHeight, aViewportX, aViewportY : integer);
var
  relPage, relViewport : double;
begin
  relPage:= FPage.Height / FPage.Width;
  relViewport:= ClientRect.Height / ClientRect.Width;

  if (relViewport > relPage) then
  begin
    aPageWidth := ClientRect.Width;
    aPageHeight := min(ClientRect.Height, round(aPageWidth * FPage.Height / FPage.Width));
    aViewportX := 0;
    aViewportY := (ClientHeight - aPageHeight) div 2;
  end
  else
  begin
    aPageHeight := ClientRect.Height;
    aPageWidth := min(Self.ClientRect.Width, round(aPageHeight * FPage.Width / FPage.Height));
    aViewportX := (Self.ClientRect.Width - aPageWidth) div 2;
    aViewportY := 0;
  end;
end;

procedure TPdfPageThumbnailPanel.Paint;
begin
  inherited Paint;
  if Assigned(FPage) and Assigned(FCachedBitmap) then
  begin
    Self.Canvas.Draw(0, 0, FCachedBitmap);
  end;
end;

constructor TPdfPageThumbnailPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPage := nil;
  FCachedBitmap := nil;
end;

destructor TPdfPageThumbnailPanel.Destroy;
begin
  FreeAndNil(FCachedBitmap);
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
    begin
      TPdfThumbsControl(FPanels.Items[i]).Free;
    end;
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
