unit PdfiumLclCtrl;

interface

uses
  Classes, Controls, StdCtrls, Contnrs,
  LMessages,
  PdfiumCore;

type
  TPdfControlScaleMode = (
    smFitAuto,
    smFitWidth,
    smFitHeight,
    smZoom
  );

  TLCLPdfControlPdfRectShell = class
  public
    R : TPdfRect;
  end;

  { TLCLPdfControlPdfRects }

  TLCLPdfControlPdfRects = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const aIndex : integer): TLCLPdfControlPdfRectShell;
    procedure Clear;
    function Count : integer;
    function Add: TLCLPdfControlPdfRectShell;
  end;

  { TLCLPdfControl }

  TLCLPdfControl = class(TCustomControl)
  strict private
    FDocument: TPdfDocument;
    FPageIndex: Integer;
    FPageWidth: Integer;
    FPageHeight : Integer;
    FX, FY : Integer;
    FHorizontalScrollbar : TScrollbar;
    FVerticalScrollbar : TScrollBar;
    FHighlightTextRects : TLCLPdfControlPdfRects;

    FScaleMode : TPdfControlScaleMode;
    FZoomPercentage : Integer;
    FAllowFormEvents : Boolean;

    procedure FormInvalidate(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);

    procedure SetScaleMode(AValue: TPdfControlScaleMode);
    procedure SetZoomPercentage(AValue: Integer);
    procedure AdjustGeometry;
    procedure OnChangeHorizontalScrollbar(Sender: TObject);
    procedure OnChangeVerticalScrollbar(Sender: TObject);
    procedure CMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: String; const Password: String = ''; LoadOption: TPdfDocumentLoadOption = dloDefault);
    function GotoNextPage: Boolean;
    function GotoPrevPage: Boolean;
    procedure HightlightText(const SearchText: string; MatchCase, MatchWholeWord: Boolean);
    // explanation here: https://forum.lazarus.freepascal.org/index.php?topic=38041.0
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  public
    property Document: TPdfDocument read FDocument;
    property ScaleMode: TPdfControlScaleMode read FScaleMode write SetScaleMode default smFitAuto;
    property ZoomPercentage: Integer read FZoomPercentage write SetZoomPercentage default 100;
    property AllowFormEvents: Boolean read FAllowFormEvents write FAllowFormEvents default True;
  end;


implementation

uses
  Graphics, Math, Forms, LCLIntf, LCLType;

// https://forum.lazarus.freepascal.org/index.php?topic=32648.0
procedure DrawTransparentRectangle(Canvas: TCanvas; Rect: TRect; Color: TColor; Transparency: Integer);
var
  X: Integer;
  Y: Integer;
  C: TColor;
  R, G, B: Integer;
  RR, RG, RB: Integer;
begin
  RR := GetRValue(Color);
  RG := GetGValue(Color);
  RB := GetBValue(Color);

  for Y := Rect.Top to Rect.Bottom - 1 do
    for X := Rect.Left to Rect.Right - 1 do
    begin
      C := Canvas.Pixels[X, Y];
      R := Round(0.01 * (Transparency * GetRValue(C) + (100 - Transparency) * RR));
      G := Round(0.01 * (Transparency * GetGValue(C) + (100 - Transparency) * RG));
      B := Round(0.01 * (Transparency * GetBValue(C) + (100 - Transparency) * RB));
      Canvas.Pixels[X, Y] := RGB(R, G, B);
    end;
end;

{ TLCLPdfControlPdfRects }

constructor TLCLPdfControlPdfRects.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TLCLPdfControlPdfRects.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TLCLPdfControlPdfRects.Get(const aIndex: integer): TLCLPdfControlPdfRectShell;
begin
  Result := FList.Items[aIndex] as TLCLPdfControlPdfRectShell;
end;

procedure TLCLPdfControlPdfRects.Clear;
begin
  FList.Clear;
end;

function TLCLPdfControlPdfRects.Count: integer;
begin
  Result := FList.Count;
end;

function TLCLPdfControlPdfRects.Add: TLCLPdfControlPdfRectShell;
begin
  Result := TLCLPdfControlPdfRectShell.Create;
  FList.Add(Result);
end;

{ TLCLPdfControl }

procedure TLCLPdfControl.FormInvalidate(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
begin
  Invalidate;
end;

procedure TLCLPdfControl.SetScaleMode(AValue: TPdfControlScaleMode);
begin
  if FScaleMode=AValue then Exit;
  FScaleMode:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TLCLPdfControl.SetZoomPercentage(AValue: Integer);
begin
  if (FZoomPercentage < 1) or (FZoomPercentage > 1000) then
    exit;

  if FZoomPercentage=AValue then Exit;
  FZoomPercentage:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TLCLPdfControl.AdjustGeometry;
  procedure AdjustScrollbar(aScrollbar: TScrollBar; aViewPortSize, aDocumentSize : integer);
  begin
    if aScrollbar.Visible then
    begin
      aScrollbar.PageSize:= (aViewPortSize * aViewPortSize) div aDocumentSize;
      aScrollbar.Max:= aDocumentSize - aViewPortSize + aScrollbar.PageSize - 1;
      aScrollbar.LargeChange:= (aScrollbar.Max - 1) div 6;
      aScrollbar.Position:= min(aScrollbar.Position, aScrollbar.Max - aScrollbar.PageSize);
    end;
  end;
var
  curPage : TPdfPage;
  relPage, relViewport : Single;
begin
  if FPageIndex < FDocument.PageCount then
  begin
    curPage := FDocument.Pages[FPageIndex];

    if FScaleMode = smFitAuto then
    begin
      FHorizontalScrollbar.Visible := false;
      FVerticalScrollbar.Visible := false;

      relPage:= curPage.Height / curPage.Width;
      relViewport:= ClientRect.Height / ClientRect.Width;

      if relViewport > relPage then
      begin
        FPageWidth := ClientRect.Width;
        FPageHeight := min(ClientRect.Height, round(FPageWidth * curPage.Height / curPage.Width));
        FX := 0;
        FY := (ClientHeight - FPageHeight) div 2;
      end
      else
      begin
        FPageHeight := Self.ClientRect.Height;
        FPageWidth := min(Self.ClientRect.Width, round(FPageHeight * curPage.Width / curPage.Height));
        FX := (Self.ClientRect.Width - FPageWidth) div 2;
        FY := 0;
      end;
    end
    else if FScaleMode = smFitWidth then
    begin
      FPageWidth := Self.ClientRect.Width;
      FPageHeight := trunc(curPage.Height * FPageWidth / curPage.Width);
      FHorizontalScrollbar.Visible := false;
      FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
      AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
      FX := 0;
      FY := (ClientRect.Height - FPageHeight) div 2;
      if FHorizontalScrollbar.Visible then
        FY := FY - FHorizontalScrollbar.Height;
      FY := max(0, FY);
    end
    else if FScaleMode = smFitHeight then
    begin
      FPageHeight := Self.ClientRect.Height;
      FPageWidth := trunc(curPage.Width * FPageHeight / curPage.Height);
      FVerticalScrollbar.Visible := false;
      FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
      AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
      FX := (ClientRect.Width - FPageWidth) div 2;
      if FVerticalScrollbar.Visible then
        FX := FX - FVerticalScrollbar.Width;
      FX := max(0, FX);
      FY := 0;
    end
    else
    begin
      FPageWidth := round(curPage.Width * (ZoomPercentage / 100));
      FPageHeight := round(curPage.Height * (ZoomPercentage / 100));
      FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
      FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
      AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
      AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
      FX := (ClientRect.Width - FPageWidth) div 2;
      if FVerticalScrollbar.Visible then
        FX := FX - (FVerticalScrollbar.Width div 2);
      FX := max(0, FX);
      FY := (ClientRect.Height - FPageHeight) div 2;
      if FHorizontalScrollbar.Visible then
        FY := FY - (FHorizontalScrollbar.Height div 2);
      FY := max(0, FY);
    end;
  end;
end;

procedure TLCLPdfControl.OnChangeHorizontalScrollbar(Sender: TObject);
begin
  Invalidate;
end;

procedure TLCLPdfControl.OnChangeVerticalScrollbar(Sender: TObject);
begin
  Invalidate;
end;

procedure TLCLPdfControl.CMMouseWheel(var Message: TLMMouseEvent);
var
  direction : integer;
begin
  if Message.WheelDelta > 0 then
    direction := -1
  else
    direction := 1;
  if GetKeyState(VK_CONTROL) and $8000 <> 0 then  // CTRL pressed
  begin
    if FScaleMode = smZoom then
      Self.SetZoomPercentage(FZoomPercentage + (direction * 10));
  end
  else
    FVerticalScrollbar.Position := min(FVerticalScrollbar.Position + (direction * FVerticalScrollbar.PageSize), FVerticalScrollbar.Max - FVerticalScrollbar.PageSize);
end;

procedure TLCLPdfControl.Paint;
var
  curPage : TPdfPage;
  x, y, i : Integer;
  rect : TRect;
begin
  inherited Paint;
  Canvas.Brush.Color:= clRed;
  Canvas.FillRect(ClientRect);
  if FPageIndex < FDocument.PageCount then
  begin
    curPage := FDocument.Pages[FPageIndex];
    x := FX;
    if FHorizontalScrollbar.Visible then
      x := x - FHorizontalScrollbar.Position;
    y := FY;
    if FVerticalScrollbar.Visible then
      y := y - FVerticalScrollbar.Position;
    curPage.DrawToCanvas(Self.Canvas, x, y, FPageWidth, FPageHeight);

    for i := 0 to FHighlightTextRects.Count - 1 do
    begin
      rect := curPage.PageToDevice(FX, FY, FPageWidth, FPageHeight, FHighlightTextRects.Get(i).R);
      if FHorizontalScrollbar.Visible then
      begin
        rect.Left := rect.Left - FHorizontalScrollbar.Position;
        rect.Right := rect.Right - FHorizontalScrollbar.Position;
      end;
      if FVerticalScrollbar.Visible then
      begin
        rect.Top := rect.Top - FVerticalScrollbar.Position;
        rect.Bottom := rect.Bottom - FVerticalScrollbar.Position;
      end;
      Canvas.Brush.Color:= clYellow;
      DrawTransparentRectangle(Canvas, rect, clYellow, 50);
//      Canvas.FillRect(rect);
    end;
  end;
end;

procedure TLCLPdfControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TLCLPdfControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (FPageIndex < FDocument.PageCount) then
  begin
    curPage := FDocument.Pages[FPageIndex];

    if AllowFormEvents then
    begin
      PagePt := curPage.DeviceToPage(FX, FY, FPageWidth, FPageHeight, X, Y);
      if Button = mbLeft then
      begin
        if curPage.FormEventLButtonDown(Shift, PagePt.X, PagePt.Y) then
          Exit;
      end
      else if Button = mbRight then
      begin
        if curPage.FormEventFocus(Shift, PagePt.X, PagePt.Y) then
          Exit;
        if curPage.FormEventRButtonDown(Shift, PagePt.X, PagePt.Y) then
          Exit;
      end;
    end;
    (*
    if AllowUserTextSelection and not FFormFieldFocused then
    begin
      if Button = mbLeft then
      begin
        PagePt := DeviceToPage(X, Y);
        CharIndex := Page.GetCharIndexAt(PagePt.X, PagePt.Y, MAXWORD, MAXWORD);
        if FCheckForTrippleClick and (CharIndex >= SelStart) and (CharIndex < SelStart + SelLength) then
        begin
          FMousePressed := False;
          KillTimer(Handle, cTrippleClickTimerId);
          FCheckForTrippleClick := False;
          SelectLine(CharIndex);
        end
        else if ssDouble in Shift then
        begin
          FMousePressed := False;
          SelectWord(CharIndex);
          FCheckForTrippleClick := True;
          SetTimer(Handle, cTrippleClickTimerId, GetDoubleClickTime, nil);
        end
        else
        begin
          FCheckForTrippleClick := False;
          SetSelection(False, CharIndex, CharIndex);
        end;
      end;
    end;
    *)
  end;

end;

constructor TLCLPdfControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleMode := smFitAuto;
  FZoomPercentage := 100;
  FAllowFormEvents := true;
  FHighlightTextRects := TLCLPdfControlPdfRects.Create;
  FDocument := TPdfDocument.Create;
  FDocument.OnFormInvalidate := @FormInvalidate;
  Color:= clGray;
  Width := 130;
  Height := 180;
  FPageWidth := 0;
  FPageHeight := 0;
  FX := 0;
  FY := 0;
  FPageIndex := 0;
  FHorizontalScrollbar := TScrollbar.Create(Self);
  FHorizontalScrollbar.Kind:= sbHorizontal;
  FHorizontalScrollbar.Parent := Self;
  FHorizontalScrollbar.Align := alBottom;
  FHorizontalScrollbar.OnChange := @OnChangeHorizontalScrollbar;
  FVerticalScrollbar := TScrollBar.Create(Self);
  FVerticalScrollbar.Kind:= sbVertical;
  FVerticalScrollbar.Parent := Self;
  FVerticalScrollbar.Align := alRight;
  FHorizontalScrollbar.Visible := false;
  FVerticalScrollbar.Visible := false;
  FVerticalScrollbar.OnChange:= @OnChangeVerticalScrollbar;
end;

destructor TLCLPdfControl.Destroy;
begin
  FDocument.Free;
  FHighlightTextRects.Free;
  inherited Destroy;
end;

procedure TLCLPdfControl.LoadFromFile(const FileName: String; const Password: String; LoadOption: TPdfDocumentLoadOption);
begin
  FDocument.LoadFromFile(UnicodeString(FileName), Password, LoadOption);
  FHighlightTextRects.Clear;
  AdjustGeometry;
end;

function TLCLPdfControl.GotoNextPage: Boolean;
begin
  Result := false;
  if FPageIndex < FDocument.PageCount - 1 then
  begin
    FHighlightTextRects.Clear;
    inc(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

function TLCLPdfControl.GotoPrevPage: Boolean;
begin
  Result := false;
  if (FPageIndex > 0) then
  begin
    FHighlightTextRects.Clear;
    dec(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

procedure TLCLPdfControl.HightlightText(const SearchText: string; MatchCase, MatchWholeWord: Boolean);
var
  curPage: TPdfPage;
  CharIndex, CharCount, I, Count: Integer;
begin
  CharIndex := 0;
  CharCount := 0;

  FHighlightTextRects.Clear;
  if (SearchText <> '') and (FPageIndex < FDocument.PageCount) then
  begin
    curPage := FDocument.Pages[FPageIndex];

    if curPage.BeginFind(UnicodeString(SearchText), MatchCase, MatchWholeWord, False) then
    begin
      try
        while curPage.FindNext(CharIndex, CharCount) do
        begin
          Count := curPage.GetTextRectCount(CharIndex, CharCount);
          for I := 0 to Count - 1 do
            FHighlightTextRects.Add.R := curPage.GetTextRect(I);
        end;
      finally
        curPage.EndFind;
      end;
    end;
  end;
  Invalidate;
end;

procedure TLCLPdfControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  AdjustGeometry;
  Invalidate;
end;

end.
