unit PdfiumPascalViewPageCtrl;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Controls, StdCtrls, Contnrs, Graphics,
  LMessages,
  PdfiumCore;

const
  cPdfControlDefaultDrawOptions = [proAnnotations];
  cHighLightTextColor = clYellow;

type
  TPdfControlScaleMode = (
    smFitAuto,
    smFitWidth,
    smFitHeight,
    smZoom
  );

  TPdfControlDrawPageToCanvasProcedure = procedure (aPage: TPdfPage; aCanvas: TCanvas; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);

  TPdfControlPdfRectShell = class
  public
    R : TPdfRect;
  end;

  { TPdfPageViewControlPdfRects }

  TPdfControlPdfRects = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const aIndex : integer): TPdfControlPdfRectShell;
    procedure Clear;
    function Count : integer;
    function Add: TPdfControlPdfRectShell;
  end;

  TPdfControlWebLinkClickEvent = procedure(Sender: TObject; Url: string) of object;

  { TPdfPageViewControl }

  TPdfPageViewControl = class(TCustomControl)
  strict private
    FDocument: TPdfDocument;
    FPageIndex: Integer;
    FPageWidth: Integer;
    FPageHeight : Integer;
    FViewportX, FViewportY : Integer;
    FHorizontalScrollbar : TScrollbar;
    FVerticalScrollbar : TScrollBar;
    FHighlightTextRects : TPdfControlPdfRects;
    FPageBackgroundColor : TColor;

    FScaleMode : TPdfControlScaleMode;
    FZoomPercentage : Integer;
    FRotation: TPdfPageRotation;
    FAllowFormEvents : Boolean;
    FDrawOptions : TPdfPageRenderOptions;
    FHighLightTextColor : TColor;

    FOnWebLinkClick : TPdfControlWebLinkClickEvent;

    procedure FormInvalidate(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
    procedure FormOutputSelectedRect(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
    procedure FormGetCurrentPage(Document: TPdfDocument; var Page: TPdfPage);

    procedure SetDrawOptions(AValue: TPdfPageRenderOptions);
    procedure SetHighLightTextColor(AValue: TColor);
    procedure SetPageBackgroundColor(AValue: TColor);
    procedure SetRotation(AValue: TPdfPageRotation);
    procedure SetScaleMode(AValue: TPdfControlScaleMode);
    procedure SetZoomPercentage(AValue: Integer);

    procedure AdjustGeometry;
    procedure DocumentLoaded;
    function PageIndexValid : boolean;
    procedure SetSelection(Active: Boolean; StartIndex, StopIndex: Integer);
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

    property OnWebLinkClick: TPdfControlWebLinkClickEvent read FOnWebLinkClick write FOnWebLinkClick;
    property DrawOptions: TPdfPageRenderOptions read FDrawOptions write SetDrawOptions default cPdfControlDefaultDrawOptions;
    property HighLightTextColor : TColor read FHighLightTextColor write SetHighLightTextColor default cHighLightTextColor;
    property Rotation: TPdfPageRotation read FRotation write SetRotation default prNormal;

    property PageBackgroundColor: TColor read FPageBackgroundColor write SetPageBackgroundColor;
  end;


var
  GraphicsBackend_DrawPageToCanvas : TPdfControlDrawPageToCanvasProcedure;

implementation

uses
  Math, Forms, LCLIntf, LCLType, SysUtils;

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

{ TPdfControlPdfRects }

constructor TPdfControlPdfRects.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TPdfControlPdfRects.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TPdfControlPdfRects.Get(const aIndex: integer): TPdfControlPdfRectShell;
begin
  Result := FList.Items[aIndex] as TPdfControlPdfRectShell;
end;

procedure TPdfControlPdfRects.Clear;
begin
  FList.Clear;
end;

function TPdfControlPdfRects.Count: integer;
begin
  Result := FList.Count;
end;

function TPdfControlPdfRects.Add: TPdfControlPdfRectShell;
begin
  Result := TPdfControlPdfRectShell.Create;
  FList.Add(Result);
end;

{ TPdfPageViewControl }

procedure TPdfPageViewControl.FormInvalidate(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
begin
  Invalidate;
end;

procedure TPdfPageViewControl.FormOutputSelectedRect(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect);
begin
  {$IFDEF LINUX}
  WriteLn('ok');
  {$ENDIF}
end;

procedure TPdfPageViewControl.FormGetCurrentPage(Document: TPdfDocument; var Page: TPdfPage);
begin
  if PageIndexValid then
    Page := FDocument.Pages[FPageIndex];
end;

procedure TPdfPageViewControl.SetDrawOptions(AValue: TPdfPageRenderOptions);
begin
  if FDrawOptions = AValue then Exit;
  FDrawOptions := AValue;
  Invalidate;
end;

procedure TPdfPageViewControl.SetHighLightTextColor(AValue: TColor);
begin
  if FHighLightTextColor = AValue then Exit;
  FHighLightTextColor := AValue;
  Invalidate;
end;

procedure TPdfPageViewControl.SetPageBackgroundColor(AValue: TColor);
begin
  if FPageBackgroundColor=AValue then Exit;
  FPageBackgroundColor:=AValue;
  Invalidate;
end;

procedure TPdfPageViewControl.SetRotation(AValue: TPdfPageRotation);
begin
  if FRotation=AValue then Exit;
  FRotation:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TPdfPageViewControl.SetScaleMode(AValue: TPdfControlScaleMode);
begin
  if FScaleMode=AValue then Exit;
  FScaleMode:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TPdfPageViewControl.SetZoomPercentage(AValue: Integer);
begin
  if (AValue < 1) or (AValue > 1000) then
    exit;

  if FZoomPercentage=AValue then Exit;
  FZoomPercentage:=AValue;
  AdjustGeometry;
  Invalidate;
end;

procedure TPdfPageViewControl.AdjustGeometry;
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

      if (FRotation = prNormal) or (FRotation = pr180) then
      begin
        if (relViewport > relPage) then
        begin
          FPageWidth := ClientRect.Width;
          FPageHeight := min(ClientRect.Height, round(FPageWidth * curPage.Height / curPage.Width));
          FViewportX := 0;
          FViewportY := (ClientHeight - FPageHeight) div 2;
        end
        else
        begin
          FPageHeight := ClientRect.Height;
          FPageWidth := min(Self.ClientRect.Width, round(FPageHeight * curPage.Width / curPage.Height));
          FViewportX := (Self.ClientRect.Width - FPageWidth) div 2;
          FViewportY := 0;
        end;
      end
      else
      begin
        if (relViewport > relPage) then
        begin
          FPageWidth := ClientRect.Width;
          FPageHeight := min(ClientRect.Height, round(FPageWidth * curPage.Width / curPage.Height));
          FViewportX := 0;
          FViewportY := (ClientHeight - FPageHeight) div 2;
        end
        else
        begin
          FPageWidth := ClientRect.Height;
          FPageHeight := min(Self.ClientRect.Width, round(FPageWidth * curPage.Height / curPage.Width));
          FViewportX := (Self.ClientRect.Width - FPageHeight) div 2;
          FViewportY := 0;
        end;
      end;
    end
    else if FScaleMode = smFitWidth then
    begin
      if (FRotation = prNormal) or (FRotation = pr180) then
      begin
        FPageWidth := Self.ClientRect.Width;
        FPageHeight := trunc(curPage.Height * FPageWidth / curPage.Width);
        FHorizontalScrollbar.Visible := false;
        FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
        AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
        FViewportX := 0;
        FViewportY := (ClientRect.Height - FPageHeight) div 2;
      end
      else
      begin
        FPageHeight:= Self.ClientRect.Width;
        FPageWidth := trunc(curPage.Width * FPageHeight / curPage.Height);
        FHorizontalScrollbar.Visible := false;
        FVerticalScrollbar.Visible := FPageWidth > Self.ClientRect.Height;
        AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageWidth);
        FViewportX := 0;
        FViewportY := (ClientRect.Height - FPageWidth) div 2;
      end;
      if FHorizontalScrollbar.Visible then
        FViewportY := FViewportY - FHorizontalScrollbar.Height;
      FViewportY := max(0, FViewportY);
    end
    else if FScaleMode = smFitHeight then
    begin
      if (FRotation = prNormal) or (FRotation = pr180) then
      begin
        FPageHeight := Self.ClientRect.Height;
        FPageWidth := trunc(curPage.Width * FPageHeight / curPage.Height);
        FVerticalScrollbar.Visible := false;
        FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
        AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
        FViewportX := (ClientRect.Width - FPageWidth) div 2;
      end
      else
      begin
        FPageWidth := Self.ClientRect.Height;
        FPageHeight := trunc(curPage.Height * FPageWidth / curPage.Width);
        FVerticalScrollbar.Visible := false;
        FHorizontalScrollbar.Visible := FPageHeight > Self.ClientWidth;
        AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageHeight);
        FViewportX := (ClientRect.Width - FPageHeight) div 2;
      end;
      if FVerticalScrollbar.Visible then
        FViewportX := FViewportX - FVerticalScrollbar.Width;
      FViewportX := max(0, FViewportX);
      FViewportY := 0;
    end
    else
    begin
      FPageWidth := max(1, round(curPage.Width * (ZoomPercentage / 100)));
      FPageHeight := max(1, round(curPage.Height * (ZoomPercentage / 100)));
      if (FRotation = prNormal) or (FRotation = pr180) then
      begin
        FHorizontalScrollbar.Visible := FPageWidth > Self.ClientWidth;
        FVerticalScrollbar.Visible := FPageHeight > Self.ClientRect.Height;
        AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageHeight);
        AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageWidth);
        FViewportX := (ClientRect.Width - FPageWidth) div 2;
        FViewportY := (ClientRect.Height - FPageHeight) div 2;
      end
      else
      begin
        FHorizontalScrollbar.Visible := FPageHeight > Self.ClientWidth;
        FVerticalScrollbar.Visible := FPageWidth > Self.ClientRect.Height;
        AdjustScrollbar(FVerticalScrollbar, Self.ClientRect.Height, FPageWidth);
        AdjustScrollbar(FHorizontalScrollbar, Self.ClientRect.Width, FPageHeight);
        FViewportX := (ClientRect.Width - FPageHeight) div 2;
        FViewportY := (ClientRect.Height - FPageWidth) div 2;
      end;
      if FVerticalScrollbar.Visible then
        FViewportX := FViewportX - (FVerticalScrollbar.Width div 2);
      FViewportX := max(0, FViewportX);
      if FHorizontalScrollbar.Visible then
        FViewportY := FViewportY - (FHorizontalScrollbar.Height div 2);
      FViewportY := max(0, FViewportY);
    end;
  end;
end;

procedure TPdfPageViewControl.DocumentLoaded;
begin
  FHighlightTextRects.Clear;
  FPageIndex:= 0;
  if FDocument.Active then
  begin
    AdjustGeometry;
    Invalidate;
    SetFocus;
  end
  else
    Invalidate;
end;

function TPdfPageViewControl.PageIndexValid: boolean;
begin
  Result := (FDocument.Active) and (FPageIndex < FDocument.PageCount);
end;

procedure TPdfPageViewControl.SetSelection(Active: Boolean; StartIndex, StopIndex: Integer);
begin

end;

procedure TPdfPageViewControl.OnChangeHorizontalScrollbar(Sender: TObject);
begin
  Invalidate;
end;

procedure TPdfPageViewControl.OnChangeVerticalScrollbar(Sender: TObject);
begin
  Invalidate;
end;

procedure TPdfPageViewControl.CMMouseWheel(var Message: TLMMouseEvent);
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

procedure TPdfPageViewControl.CMMouseleave(var Message: TlMessage);
begin
  if (*(Cursor = crIBeam) or*) (Cursor = crHandPoint) then
    Cursor := crDefault;
  inherited;
end;

procedure TPdfPageViewControl.WMKeyDown(var Message: TLMKeyDown);
var
  curPage : TPdfPage;
  Shift: TShiftState;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    Shift := KeyDataToShiftState(Message.KeyData);
    curPage.FormEventKeyDown(Message.CharCode, Shift);
  end;
  inherited;
end;

procedure TPdfPageViewControl.WMKeyUp(var Message: TLMKeyUp);
var
  curPage : TPdfPage;
  Shift: TShiftState;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    Shift := KeyDataToShiftState(Message.KeyData);
    if curPage.FormEventKeyUp(Message.CharCode, Shift) then
      Exit;
  end;
  inherited;
end;

procedure TPdfPageViewControl.WMChar(var Message: TLMChar);
var
  curPage : TPdfPage;
  Shift: TShiftState;
begin
  if AllowFormEvents and PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];
    Shift := KeyDataToShiftState(Message.KeyData);
    if curPage.FormEventKeyPress(Message.CharCode, Shift) then
      Exit;
  end;
  inherited;
end;

procedure TPdfPageViewControl.WMKillFocus(var Message: TLMKillFocus);
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
procedure TPdfPageViewControl.WMSetFocus(var Message: TLMSetFocus);
begin
  UpdateFocus(true);
end;
*)
(*
procedure TPdfPageViewControl.UpdateFocus(AFocused: Boolean);
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

function TPdfPageViewControl.PageX: integer;
begin
  Result := FViewportX;
  if FHorizontalScrollbar.Visible then
    Result := Result - FHorizontalScrollbar.Position;
end;

function TPdfPageViewControl.PageY: integer;
begin
  Result := FViewportY;
  if FVerticalScrollbar.Visible then
    Result := Result - FVerticalScrollbar.Position;
end;

procedure TPdfPageViewControl.Paint;
var
  curPage : TPdfPage;
  x, y, i, tmp : Integer;
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
    GraphicsBackend_DrawPageToCanvas(curPage, Self.Canvas, x, y, FPageWidth, FPageHeight, FRotation, FDrawOptions, PageBackgroundColor);

    if FHighlightTextRects.Count > 0 then
    begin
      //if not ((FRotation = prNormal) or (FRotation = pr180)) then
      //begin
      //  tmp := x;
      //  x := y;
      //  y := tmp;
      //end;
      for i := 0 to FHighlightTextRects.Count - 1 do
      begin
        rect := curPage.PageToDevice(0, 0, FPageWidth, FPageHeight, FHighlightTextRects.Get(i).R, FRotation);
        if (FRotation = prNormal) or (FRotation = pr180) then
        begin
          rect.Left := rect.Left + x;
          rect.Right := rect.Right + x;
          rect.Top := rect.Top + y;
          rect.Bottom := rect.Bottom + y;
        end
        else
        begin
          (*
          rect.Left := rect.Left + y;
          rect.Right := rect.Right + y;
          rect.Top := rect.Top + x;
          rect.Bottom := rect.Bottom + x;
          *)
        end;
        if rect.Left > rect.Right then
        begin
          tmp := rect.Left;
          rect.Left := rect.Right;
          rect.Right := tmp;
        end;
        if rect.Top > rect.Bottom then
        begin
          tmp := rect.Bottom;
          rect.Bottom := rect.Top;
          rect.Top := tmp;
        end;
        Canvas.Brush.Color:= FHighlightTextColor;
        DrawTransparentRectangle(Canvas, rect, clYellow, 50);
      end;
    end;
  end;
end;

procedure TPdfPageViewControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
  url : UnicodeString;
begin
  inherited MouseMove(Shift, X, Y);

  if PageIndexValid  then
  begin
    if (X < FViewportX) or (X > FViewPortX + FPageWidth) or (Y < FViewportY) or (Y > FViewportY + FPageHeight) then
      exit;

    curPage := FDocument.Pages[FPageIndex];
    PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y, FRotation);

    Cursor := crDefault;

    if AllowFormEvents then
    begin
      if curPage.FormEventMouseMove(Shift, PagePt.X, PagePt.Y) then
      begin
        if curPage.IsUriLinkAtPoint(PagePt.X, PagePt.Y, url) then
          Cursor:= crHandPoint;
      end;
    end;



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

procedure TPdfPageViewControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  curPage : TPdfPage;
  PagePt : TPdfPoint;
  Url : UnicodeString;
  CharIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if PageIndexValid then
  begin
    curPage := FDocument.Pages[FPageIndex];

    if AllowFormEvents then
    begin
      if not Focused and not(csNoFocus in ControlStyle) then
        SetFocus;

      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y, FRotation);
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
      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y, FRotation);
      Url := '';
      if curPage.IsUriLinkAtPoint(PagePt.X, PagePt.Y, Url) then
      begin
        if Assigned(FOnWebLinkClick) then
          FOnWebLinkClick(Self, String(Url));
      end
      else
      begin
        CharIndex := curPage.GetCharIndexAt(PagePt.X, PagePt.Y, MAXWORD, MAXWORD);
        SetSelection(False, CharIndex, CharIndex);
      end;
    end;
  end;

end;

procedure TPdfPageViewControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
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
      PagePt := curPage.DeviceToPage(PageX, PageY, FPageWidth, FPageHeight, X, Y, FRotation);
      if (Button = mbLeft) and curPage.FormEventLButtonUp(Shift, PagePt.X, PagePt.Y) then
        Exit;
      if (Button = mbRight) and curPage.FormEventRButtonUp(Shift, PagePt.X, PagePt.Y) then
        Exit;
    end;
  end;
end;

constructor TPdfPageViewControl.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TPdfDocument.Create;
  FDocument.OnFormInvalidate := FormInvalidate;
  FDocument.OnFormOutputSelectedRect := FormOutputSelectedRect;
  FDocument.OnFormGetCurrentPage := FormGetCurrentPage;
  FOnWebLinkClick := nil;
  FDrawOptions := cPdfControlDefaultDrawOptions;
  FHighlightTextColor := cHighLightTextColor;
  FRotation := prNormal;

  FScaleMode := smFitAuto;
  FZoomPercentage := 100;
  FAllowFormEvents := true;
  FHighlightTextRects := TPdfControlPdfRects.Create;
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
  FHorizontalScrollbar.OnChange := OnChangeHorizontalScrollbar;
  FVerticalScrollbar := TScrollBar.Create(Self);
  FVerticalScrollbar.Kind:= sbVertical;
  FVerticalScrollbar.Parent := Self;
  FVerticalScrollbar.Align := alRight;
  FHorizontalScrollbar.Visible := false;
  FVerticalScrollbar.Visible := false;
  FVerticalScrollbar.OnChange:= OnChangeVerticalScrollbar;
  FPageBackgroundColor:= clWhite;
end;

destructor TPdfPageViewControl.Destroy;
begin
  FDocument.Free;
  FHighlightTextRects.Free;
  inherited Destroy;
end;

procedure TPdfPageViewControl.LoadFromFile(const FileName: String; const Password: String; LoadOption: TPdfDocumentLoadOption);
begin
  try
    FDocument.LoadFromFile(UnicodeString(FileName), Password, LoadOption);
  finally
    DocumentLoaded;
  end;
end;

function TPdfPageViewControl.GotoNextPage: Boolean;
begin
  Result := false;
  if PageIndexValid and (FPageIndex < FDocument.PageCount - 1) then
  begin
    FHighlightTextRects.Clear;
    inc(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

function TPdfPageViewControl.GotoPrevPage: Boolean;
begin
  Result := false;
  if PageIndexValid and (FPageIndex > 0) then
  begin
    FHighlightTextRects.Clear;
    dec(FPageIndex);
    AdjustGeometry;
    Invalidate;
    Result := true;
  end;
end;

procedure TPdfPageViewControl.HightlightText(const SearchText: string; MatchCase, MatchWholeWord: Boolean);
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

procedure TPdfPageViewControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  AdjustGeometry;
  Invalidate;
end;

end.
