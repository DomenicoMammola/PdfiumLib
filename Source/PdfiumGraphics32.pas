unit PdfiumGraphics32;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Graphics,
  PdfiumCore;

procedure DrawPageToBitmap(aPage: TPdfPage; aBitmap: TBitmap; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);
procedure DrawPageToCanvas(aPage: TPdfPage; aCanvas: TCanvas; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);

implementation

uses
  GR32;

procedure DrawPageToBitmap(aPage: TPdfPage; aBitmap: TBitmap; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);
var
  tmpG32Bitmap : TBitmap32;
  PdfBmp: TPdfBitmap;
  w, h : integer;
begin
  if (aRotate = prNormal) or (aRotate = pr180) then
  begin
    w := aWidth;
    h := aHeight;
  end
  else
  begin
    w := aHeight;
    h := aWidth;
  end;

  aBitmap.Width:= w;
  aBitmap.Height:= h;

  tmpG32Bitmap := TBitmap32.Create(w, h);
  try
    PdfBmp := TPdfBitmap.Create(w, h, bfBGRx, tmpG32Bitmap.Bits, w * 4);
    try
      PdfBmp.FillRect(0, 0, w, h, $FF000000 or aPageBackground);
      aPage.DrawToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      aPage.DrawFormToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      aBitmap.Assign(tmpG32Bitmap);
    finally
      PdfBmp.Free;
    end;
  finally
    tmpG32Bitmap.Free;
  end;
end;

procedure DrawPageToCanvas(aPage: TPdfPage; aCanvas: TCanvas; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);
var
  tmpG32Bitmap : TBitmap32;
  PdfBmp: TPdfBitmap;
  w, h : integer;
begin
  if (aRotate = prNormal) or (aRotate = pr180) then
  begin
    w := aWidth;
    h := aHeight;
  end
  else
  begin
    w := aHeight;
    h := aWidth;
  end;

  tmpG32Bitmap := TBitmap32.Create(w, h);
  try
    PdfBmp := TPdfBitmap.Create(w, h, bfBGRx, tmpG32Bitmap.Bits, w * 4);
    try
      PdfBmp.FillRect(0, 0, w, h, $FF000000 or aPageBackground);
      aPage.DrawToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      aPage.DrawFormToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      tmpG32Bitmap.DrawTo(aCanvas.Handle, aX, aY);
    finally
      PdfBmp.Free;
    end;
  finally
    tmpG32Bitmap.Free;
  end;
end;

end.
