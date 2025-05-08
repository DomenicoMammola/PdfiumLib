unit PdfiumImage32;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Graphics,
  PdfiumCore;

procedure DrawPageToCanvas(aPage: TPdfPage; aCanvas: TCanvas; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);

implementation

uses
  Img32;

procedure DrawPageToCanvas(aPage: TPdfPage; aCanvas: TCanvas; aX, aY, aWidth, aHeight: Integer; aRotate: TPdfPageRotation; const aOptions: TPdfPageRenderOptions; aPageBackground: TColor);
var
  tmpImage32 : TImage32;
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

  tmpImage32 := TImage32.Create(w, h);
  try
    PdfBmp := TPdfBitmap.Create(w, h, bfBGRx, tmpImage32.PixelBase, w * 4);
    try
      PdfBmp.FillRect(0, 0, w, h, $FF000000 or aPageBackground);
      aPage.DrawToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      aPage.DrawFormToPdfBitmap(PdfBmp, 0, 0, w, h, aRotate, aOptions);
      tmpImage32.CopyToDc(aCanvas.Handle, aX, aY);
    finally
      PdfBmp.Free;
    end;
  finally
    tmpImage32.Free;
  end;
end;

end.
