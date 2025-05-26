unit PdfiumCoreExtra;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF FPC}

interface

uses
  PdfiumLib, PdfiumCore
  {$IFDEF FPC},IntfGraphics{$ENDIF}
  ;

type
  { TPdfMatrix }

  // https://pypdfium2.readthedocs.io/en/v4/_modules/pypdfium2/_helpers/matrix.html
  // https://forum.patagames.com/posts/t501-What-Is-Transformation-Matrix-and-How-to-Use-It
  TPdfMatrix = class
  strict private
    FMatrix : FS_MATRIX;
    procedure Multiply(aMatrix : TPdfMatrix); overload;
    procedure Multiply(const a, b, c, d, e, f: Single); overload;
  public
    constructor Create; overload;
    constructor Create(const a, b, c, d, e, f: Single); overload;
    destructor Destroy; override;

    procedure Translate(const deltaX, deltaY : Single);
    procedure Scale(const percIncrementX, percIncrementY : Single);
    procedure Rotate(const aAngle : single; const aCounterClock : boolean = false; const aAngleInRadiant : boolean = false);
    procedure HorizontalFlip;
    procedure VerticalFlip;
    procedure CentralFlip;
    procedure Skew(const x_angle, y_angle : single; aAnglesInRadiant : boolean = False);

    property Handle: FS_MATRIX read FMatrix;
  end;


  { TPdfImage }

  TPdfImage = class
  strict private
    FImage : FPDF_PAGEOBJECT;
    FBitmap : TPdfBitmap;
    {$IFDEF FPC}
    FLazImage : TLazIntfImage;
    {$ENDIF}
    FFileName : String;
    procedure CreateBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddToPage(aDocument: TPdfDocument; aPage : TPdfPage; const x, y: Single);

    property Handle: FPDF_PAGEOBJECT read FImage;
    property FileName : String read FFileName write FFileName;
  end;


implementation

uses
  Math, Graphics, SysUtils;

{ TPdfMatrix }

constructor TPdfMatrix.Create;
begin
  Self.Create(1, 0, 0, 1, 0, 0);
end;

constructor TPdfMatrix.Create(const a, b, c, d, e, f: Single);
begin
  FMatrix.a:= a;
  FMatrix.b:= b;
  FMatrix.c:= c;
  FMatrix.d:= d;
  FMatrix.e:= e;
  FMatrix.f:= f;
end;

destructor TPdfMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TPdfMatrix.Multiply(aMatrix: TPdfMatrix);
begin
  Self.Multiply(aMatrix.Handle.a, aMatrix.Handle.b, aMatrix.Handle.c, aMatrix.Handle.d, aMatrix.Handle.e, aMatrix.Handle.f);
end;

procedure TPdfMatrix.Multiply(const a, b, c, d, e, f: Single);
begin
  FMatrix.a := (FMatrix.a * a) + (FMatrix.b * c);
  FMatrix.b := (FMatrix.a * b) + (FMatrix.b * d);
  FMatrix.c := (FMatrix.c * a) + (FMatrix.d * c);
  FMatrix.d := (FMatrix.c * b) + (FMatrix.d * d);
  FMatrix.e := (FMatrix.e * a) + (FMatrix.f * c) + e;
  FMatrix.f := (FMatrix.e * b) + (FMatrix.f * d) + f;
end;

procedure TPdfMatrix.Translate(const deltaX, deltaY: Single);
begin
  Self.Multiply(1, 0, 0, 1, deltaX, deltaY);
end;

procedure TPdfMatrix.Scale(const percIncrementX, percIncrementY: Single);
begin
  Self.Multiply(percIncrementX, 0, 0, percIncrementY, 0, 0);
end;

procedure TPdfMatrix.Rotate(const aAngle: single; const aCounterClock: boolean; const aAngleInRadiant: boolean);
var
  a : float;
  c, s : double;
begin
  if not aAngleInRadiant then
    a := DegToRad(aAngle)
  else
    a := aAngle;

  c := Cos(a);
  s := Sin(a);
  if aCounterClock then
    Self.Multiply(c, s, (-1 * s), c, 0, 0)
  else
    Self.Multiply(c, (-1 * s), s, c, 0, 0);
end;

procedure TPdfMatrix.HorizontalFlip;
begin
  Self.Multiply(-1, 0, 0, 1, 0, 0);
end;

procedure TPdfMatrix.VerticalFlip;
begin
  Self.Multiply(1, 0, 0, -1, 0, 0);
end;

procedure TPdfMatrix.CentralFlip;
begin
  Self.Multiply(-1, 0, 0, -1, 0, 0);
end;


procedure TPdfMatrix.Skew(const x_angle, y_angle: single; aAnglesInRadiant: boolean);
var
  ax, ay : double;
begin
  if not aAnglesInRadiant then
  begin
    ax := DegToRad(x_angle);
    ay := DegToRad(y_angle);
  end
  else
  begin
    ax:= x_angle;
    ay := y_angle;
  end;
  Self.Multiply(1, Tan(ax), Tan(ay), 1, 0, 0);
end;

{ TPdfImage }

procedure TPdfImage.CreateBitmap;
var
{$IFDEF FPC}
  ext : String;
  pngImg : TPortableNetworkGraphic;
  jpgImg : TJPEGImage;
  bmpImg : TBitmap;
  w, h : integer;
{$ENDIF}
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FLazImage);

  {$IFDEF FPC}
  if FFileName <> '' then
  begin
    ext := LowerCase(ExtractFileExt(FFileName));
    jpgImg := nil;
    bmpImg := nil;
    pngImg := nil;
    try
      if ext = '.png' then
      begin
        pngImg := TPortableNetworkGraphic.Create;
        pngImg.LoadFromFile(FFileName);
        FLazImage := pngImg.CreateIntfImage;
        w := pngImg.Width;
        h := pngImg.Height;
      end
      else if ext = '.bmp' then
      begin
        bmpImg := TBitmap.Create;
        bmpImg.LoadFromFile(FFileName);
        FLazImage := bmpImg.CreateIntfImage;
        w := bmpImg.Width;
        h := bmpImg.Height;
      end
      else if (ext = '.jpg') or (ext = '.jpeg') then
      begin
        jpgImg := TJPEGImage.Create;
        jpgImg.LoadFromFile(FFileName);
        FLazImage := jpgImg.CreateIntfImage;
        w := jpgImg.Width;
        h := jpgImg.Height;
      end
      else
        exit;

      FBitmap := TPdfBitmap.Create(w, h, bfBGRA, FLazImage.PixelData, w *  4);
    finally
      FreeAndNil(jpgImg);
      FreeAndNil(pngImg);
      FreeAndNil(bmpImg);
    end;
  end;
  {$ELSE}
  // missing
  {$ENDIF}
end;

constructor TPdfImage.Create;
begin
  FBitmap := nil;
  FFileName := '';
end;

destructor TPdfImage.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FLazImage);
  inherited Destroy;
end;

procedure TPdfImage.AddToPage(aDocument: TPdfDocument; aPage: TPdfPage; const x, y: Single);
var
  matrix : TPdfMatrix;
begin
  CreateBitmap;
  FImage := FPDFPageObj_NewImageObj(aDocument.Handle);

  matrix := TPdfMatrix.Create;
  try
    matrix.Scale(FBitmap.Width, FBitmap.Height);
    matrix.Translate(x, y);
    FPDFPageObj_SetMatrix(FImage, @matrix.Handle);
    if FPDFImageObj_SetBitmap(nil, 0, FImage, FBitmap.Bitmap) <> 0 then
    begin
      FPDFPage_InsertObject(aPage.Handle, FImage);
      aPage.ApplyChanges;
    end;
  finally
    matrix.Free;
  end;
end;

end.
