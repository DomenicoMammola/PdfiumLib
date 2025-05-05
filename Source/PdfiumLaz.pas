unit PdfiumLaz;

{$mode objfpc}{$H+}

interface

uses
  Graphics,
  PdfiumCore;

procedure DrawPageToBitmap(aPage: TPdfPage; aBitmap: TBitmap; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);

implementation

uses
  IntfGraphics, LCLType;


procedure DrawPageToBitmap(aPage: TPdfPage; aBitmap: TBitmap; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);
var
  tmpLazImage : TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  PdfBmp: TPdfBitmap;
  w, h : integer;
begin
  tmpLazImage := TLazIntfImage.Create(0, 0);
  try
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
    tmpLazImage.DataDescription.Init_BPP32_B8G8R8A8_BIO_TTB(w, h);
    tmpLazImage.CreateData;
    PdfBmp := TPdfBitmap.Create(w, h, bfBGRA, tmpLazImage.PixelData, w * 4);
    try
      PdfBmp.FillRect(0, 0, w, h, $FF000000 or aPageBackground);
      aPage.DrawToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      aPage.DrawFormToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);

      tmpLazImage.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
      aBitmap.Handle:=ImgHandle;
      aBitmap.MaskHandle:=ImgMaskHandle;
    finally
      PdfBmp.Free;
    end;
  finally
    tmpLazImage.Free;
  end;
end;


end.
