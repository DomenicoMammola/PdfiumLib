unit PdfiumLclCtrl;

interface

uses
  Classes, Controls, StdCtrls, Contnrs, ExtCtrls,
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

  TLCLPdfControlWebLinkClickEvent = procedure(Sender: TObject; Url: string) of object;

  { TLCLPdfControl }

  TLCLPdfControl = class(TCustomControl)
  strict private
    FDocument: TPdfDocument;
    FPageIndex: Integer;
    FPageWidth: Integer;
    FPageHeight : Integer;
    FViewportX, FViewportY : Integer;
    FHorizontalScrollbar : TScrollbar;
    FVerticalScrollbar : TScrollBar;
    FHighlightTextRects : TLCLPdfControlPdfRects;
    FWebLinkInfo: TPdfPageWebLinksInfo;

    FScaleMode : TPdfControlScaleMode;
    FZoomPercentage : Integer;
    FAllowFormEvents : Boolean;

    FOnWebLinkClick : TLCLPdfControlWebLinkClickEvent;

    procedure FormInvalidate(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
    procedure FormOutputSelectedRect(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
    procedure FormGetCurrentPage(Document: TPdfDocument; var Page: TPdfPage);

    procedure SetScaleMode(AValue: TPdfControlScaleMode);
    procedure SetZoomPercentage(AValue: Integer);
    procedure AnalyzeWebLinksOfCurrentPage;
    procedure AdjustGeometry;
    procedure DocumentLoaded;
    function PageIndexValid : boolean;
    procedure OnChangeHorizontalScrollbar(Sender: TObject);
    procedure OnChangeVerticalScrollbar(Sender: TObject);
    procedure CMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure CMMouseleave(var Message: TlMessage); message LM_MOUSELEAVE;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMKeyUp(var Message: TLMKeyUp); message LM_KEYUP;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
//    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
//    procedure UpdateFocus(AFocused: Boolean);
    function PageX : integer;
    function PageY : integer;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
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

    property OnWebLinkClick: TLCLPdfControlWebLinkClickEvent read FOnWebLinkClick write FOnWebLinkClick;
  end;


implementation

uses
  Graphics, Math, Forms, LCLIntf, LCLType, SysUtils;

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

procedure TLCLPdfControl.FormOutputSelectedRect(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
begin
  {$IFDEF LINUX}
  WriteLn('ok');
  {$ENDIF}
end;

procedure TLCLPdfControl.FormGetCurrentPage(Document: TPdfDocument; var Page: TPdfPage);
begin
  if PageIndexValid then
    Page := FDocument.Pages[FPageIndex];
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
  if (AValue < 1) or (AValue > 1000) then
    exit;

  if FZoomPercentage=AValue then Exit;
  FZoomPercentage:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TLCLPdfControl.AnalyzeWebLinksOfCurrentPage;
begin
  FreeAndNil(FWebLinkInfo);
  if PageIndexValid then
    FWebLinkInfo := TPdfPageWebLinksInfo.Create(FDocument.Pages[FPageIndex]);
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
  if not FDocument.Active then
    exit;

  if PageIndexValid then
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
        FViewportX := 0;
        FViewportY := (ClientHeight - FPageHeight) div 2;
      end
      else
      begin
        FPageHeight := Self.ClientRect.Height;
        FPageWidth := min(Self.ClientRect.Width, round(FPageHeight * curPage.Width / curPage.Height));
        FViewportX := (Self.ClientRect.Width - FPageWidth) div 2;
        FViewportY := 0;
      end;
    end
    else if FScaleMode = smFitWidth then
    begin
      FPageWidth := Self.ClientRect.Width;
      FPageHeight := trunc(curPage.Height * FPageWidth / curPage.Width);
      FHorizontalScrollbar.Visible := false;
      FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
      AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
      FViewportX := 0;
      FViewportY := (ClientRect.Height - FPageHeight) div 2;
      if FHorizontalScrollbar.Visible then
        FViewportY := FViewportY - FHorizontalScrollbar.Height;
      FViewportY := max(0, FViewportY);
    end
    else if FScaleMode = smFitHeight then
    begin
      FPageHeight := Self.ClientRect.Height;
      FPageWidth := trunc(curPage.Width * FPageHeight / curPage.Height);
      FVerticalScrollbar.Visible := false;
      FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
      AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
      FViewportX := (ClientRect.Width - FPageWidth) div 2;
      if FVerticalScrollbar.Visible then
        FViewportX := FViewportX - FVerticalScrollbar.Width;
      FViewportX := max(0, FViewportX);
      FViewportY := 0;
    end
    else
    begin
      FPageWidth := max(1, round(curPage.Width * (ZoomPercentage / 100)));
      FPageHeight := max(1, round(curPage.Height * (ZoomPercentage / 100)));
      FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
      FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
      AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
      AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
      FViewportX := (ClientRect.Width - FPageWidth) div 2;
      if FVerticalScrollbar.Visible then
        FViewportX := FViewportX - (FVerticalScrollbar.Width div 2);
      FViewportX := max(0, FViewportX);
      FViewportY := (ClientRect.Height - FPageHeight) div 2;
      if FHorizontalScrollbar.Visible then
        FViewportY := FViewportY - (FHorizontalScrollbar.Height div 2);
      FViewportY := max(0, FViewportY);
    end;
  end;
end;

procedure TLCLPdfControl.DocumentLoaded;
begin
  FHighlightTextRects.Clear;
  FPageIndex:= 0;
  FreeAndNil(FWebLinkInfo);
  if FDocument.Active then
  begin
    AdjustGeometry;
    Invalidate;
    SetFocus;
    AnalyzeWebLinksOfCurrentPage;
  end
  else
    Invalidate;
end;

function TLCLPdfControl.PageIndexValid: boolean;
begin
  Result := (FDocument.Active) and (FPageIndex < FDocument.PageCount);
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

procedure TLCLPdfControl.CMMouseleave(var Message: TlMessage);
begin
  if (Cursor = crIBeam) or (Cursor = crHandPoint) then
  begin
    //if AllowUserTextSelection or Assigned(FOnWebLinkClick) or Assigned(FOnAnnotationLinkClick) or (LinkOptions <> []) then
    Cursor := crDefault;
  end;
  inherited;

end;

procedure TLCLPdfControl.WMKeyDown(var Message: TLMKeyDown);
var
  curPage : TPdfPage;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    curPage.FormEventKeyDown(Message.CharCode, Message.KeyData);
  end;
  inherited;
end;

procedure TLCLPdfControl.WMKeyUp(var Message: TLMKeyUp);
var
  curPage : TPdfPage;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    if curPage.FormEventKeyUp(Message.CharCode, Message.KeyData) then
      Exit;
  end;
  inherited;
end;

procedure TLCLPdfControl.WMChar(var Message: TLMChar);
var
  curPage : TPdfPage;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    if curPage.FormEventKeyPress(Message.CharCode, Message.KeyData) then
      Exit;
  end;
  inherited;
end;

procedure TLCLPdfControl.WMKillFocus(var Message: TLMKillFocus);
var
  curPage : TPdfPage;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    curPage.FormEventKillFocus;
  end;
  //UpdateFocus(false);
end;

(*
procedure TLCLPdfControl.WMSetFocus(var Message: TLMSetFocus);
begin
  UpdateFocus(true);
end;
*)
(*
procedure TLCLPdfControl.UpdateFocus(AFocused: Boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then exit;

  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
end;
*)

function TLCLPdfControl.PageX: integer;
begin
  Result := FViewportX;
  if FHorizontalScrollbar.Visible then
    Result := Result - FHorizontalScrollbar.Position;
end;

function TLCLPdfControl.PageY: integer;
begin
  Result := FViewportY;
  if FVerticalScrollbar.Visible then
    Result := Result - FVerticalScrollbar.Position;
end;

procedure TLCLPdfControl.Paint;
var
  curPage : TPdfPage;
  x, y, i : Integer;
  rect : TRect;
begin
  inherited Paint;
  Canvas.Brush.Color:= Self.Color;
  Canvas.FillRect(ClientRect);
  if PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    x := PageX;
    y := PageY;
    curPage.DrawToCanvas(Self.Canvas, x, y, FPageWidth, FPageHeight);

    for i := 0 to FHighlightTextRects.Count - 1 do
    begin
      rect := curPage.PageToDevice(x, y, FPageWidth, FPageHeight, FHighlightTextRects.Get(i).R);
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
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
begin
  inherited MouseMove(Shift, X, Y);

  if not FDocument.Active then
    exit;

  if AllowFormEvents and PageIndexValid then
  begin
    if (X < FViewportX) or (X > FViewPortX + FPageWidth) or (Y < FViewportY) or (Y > FViewportY + FPageHeight) then
      exit;

    curPage := FDocument.Pages[FPageIndex];
    PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y);
    curPage.FormEventMouseMove(Shift, PagePt.X, PagePt.Y);
    (*
    if curPage.FormEventMouseMove(Shift, PagePt.X, PagePt.Y) then
    begin
      case curPage.HasFormFieldAtPoint(PagePt.X, PagePt.Y) of
        fftUnknown:
          // Could be a annotation link with a URL
          exit;
        fftTextField:
          Self.Cursor := crIBeam;
        fftComboBox,
        fftSignature:
          Self.Cursor := crHandPoint;
      else
        Self.Cursor := crDefault;
      end;
    end
    else
      Self.Cursor := crDefault;
      *)
  end;

end;

procedure TLCLPdfControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
  Url : UnicodeString;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];

    if AllowFormEvents then
    begin
      if not Focused and not(csNoFocus in ControlStyle) then
        SetFocus;

      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y);
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

    if Button = mbLeft then
    begin
      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y);
      Url := '';
      if FWebLinkInfo.IsWebLinkAt(PagePt.X, PagePt.Y, Url) then
      begin
        if Assigned(FOnWebLinkClick) then
          FOnWebLinkClick(Self, Url);
      end;
    end;
  end;

end;

procedure TLCLPdfControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    if AllowFormEvents  then
    begin
      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y);
      if (Button = mbLeft) and curPage.FormEventLButtonUp(Shift, PagePt.X, PagePt.Y) then
        Exit;
      if (Button = mbRight) and curPage.FormEventRButtonUp(Shift, PagePt.X, PagePt.Y) then
        Exit;
    end;
  end;
end;

constructor TLCLPdfControl.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TPdfDocument.Create;
  FDocument.OnFormInvalidate := @FormInvalidate;
  FDocument.OnFormOutputSelectedRect := @FormOutputSelectedRect;
  FDocument.OnFormGetCurrentPage := @FormGetCurrentPage;
  FWebLinkInfo := nil;
  FOnWebLinkClick := nil;

  FScaleMode := smFitAuto;
  FZoomPercentage := 100;
  FAllowFormEvents := true;
  FHighlightTextRects := TLCLPdfControlPdfRects.Create;
  Color:= clGray;
  Width := 130;
  Height := 180;
  FPageWidth := 0;
  FPageHeight := 0;
  FViewportX := 0;
  FViewportY := 0;
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
  FreeAndNil(FWebLinkInfo);
  inherited Destroy;
end;

procedure TLCLPdfControl.LoadFromFile(const FileName: String; const Password: String; LoadOption: TPdfDocumentLoadOption);
begin
  try
    FDocument.LoadFromFile(UnicodeString(FileName), Password, LoadOption);
  finally
    DocumentLoaded;
  end;
end;

function TLCLPdfControl.GotoNextPage: Boolean;
begin
  Result := false;
  if PageIndexValid and (FPageIndex < FDocument.PageCount - 1) then
  begin
    FHighlightTextRects.Clear;
    inc(FPageIndex);
    AnalyzeWebLinksOfCurrentPage;
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

function TLCLPdfControl.GotoPrevPage: Boolean;
begin
  Result := false;
  if PageIndexValid and (FPageIndex > 0) then
  begin
    FHighlightTextRects.Clear;
    dec(FPageIndex);
    AnalyzeWebLinksOfCurrentPage;
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
  if (SearchText <> '') and PageIndexValid then
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
