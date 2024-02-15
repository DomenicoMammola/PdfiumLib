unit mainfrm2;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, PdfiumCore, ExtCtrls, StdCtrls,
  Spin, ComCtrls, PrintersDlgs, PdfiumLclCtrl;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnPrev: TButton;
    btnNext: TButton;
    btnHighlight: TButton;
    btnScale: TButton;
    OpenButton: TButton;
    chkLCDOptimize: TCheckBox;
    chkSmoothScroll: TCheckBox;
    Edit1: TEdit;
    edtZoom: TSpinEdit;
    btnPrint: TButton;
    PrintDialog1: TPrintDialog;
    OpenDialog1: TOpenDialog;
    ListViewAttachments: TListView;
    SaveDialog1: TSaveDialog;
    chkChangePageOnMouseScrolling: TCheckBox;
    btnAddAnnotation: TButton;
    pnlButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnHighlightClick(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure chkLCDOptimizeClick(Sender: TObject);
    procedure chkSmoothScrollClick(Sender: TObject);
    procedure edtZoomChange(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure ListViewAttachmentsDblClick(Sender: TObject);
    procedure chkChangePageOnMouseScrollingClick(Sender: TObject);
    procedure btnAddAnnotationClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FCtrl: TLCLPdfControl;
    procedure WebLinkClick(Sender: TObject; Url: String);
    procedure AnnotationLinkClick(Sender: TObject; LinkInfo: TPdfLinkInfo; var Handled: Boolean);
    procedure PrintDocument(Sender: TObject);
    procedure ListAttachments;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  TypInfo, Printers;

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  PDFiumDllFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'libpdfium.so';
  {$ELSE}
  PDFiumDllFileName := UnicodeString(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'pdfium.dll');
  {$ENDIF LINUX}

  {$IFDEF CPUX64}
  //PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64\V8XFA';
  //PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x64';
  {$ELSE}
  //PDFiumDllDir := ExtractFilePath(ParamStr(0)) + 'x86';
  {$ENDIF CPUX64}

  FCtrl := TLCLPdfControl.Create(Self);
  FCtrl.Parent := Self;
  FCtrl.Align := alClient;
  FCtrl.Color := clRed;
  FCtrl.AllowFormEvents:= true;
  {$IFDEF UNIX}
  {$IFNDEF LCL_CTRL}
  FCtrl.BufferedPageDraw := false;
  {$ENDIF}
  {$ENDIF}
  //FCtrl.PageBorderColor := clBlack;
  //FCtrl.PageShadowColor := clDkGray;
  FCtrl.ScaleMode := smFitAuto;
  //FCtrl.PageColor := RGB(255, 255, 200);
  //FCtrl.OnWebLinkClick := @WebLinkClick; // disabled due to loTreatWebLinkAsUriAnnotationLink + loAutoOpenURI
  //FCtrl.OnAnnotationLinkClick := @AnnotationLinkClick;
  //FCtrl.LinkOptions := FCtrl.LinkOptions - [loAutoOpenURI] {+ cPdfControlAllAutoLinkOptions};
  //FCtrl.OnPrintDocument := @PrintDocument;

  edtZoom.Value := FCtrl.ZoomPercentage;
  (*
  if FileExists(ParamStr(1)) then
    FCtrl.LoadFromFile(ParamStr(1))
  else if OpenDialog1.Execute then
    FCtrl.LoadFromFile(OpenDialog1.FileName)
  else
  begin
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  *)

  Caption := GetEnumName(TypeInfo(TPdfControlScaleMode), Ord(FCtrl.ScaleMode));
  ListAttachments;
end;



procedure TfrmMain.ListAttachments;
var
  I: Integer;
  Att: TPdfAttachment;
  ListItem: TListItem;
begin
  if (FCtrl.Document <> nil) and FCtrl.Document.Active then
  begin
    ListViewAttachments.Visible := FCtrl.Document.Attachments.Count > 0;

    ListViewAttachments.Items.BeginUpdate;
    try
      for I := 0 to FCtrl.Document.Attachments.Count - 1 do
      begin
        Att := FCtrl.Document.Attachments[I];
        ListItem := ListViewAttachments.Items.Add;
        ListItem.Caption := Format('%s (%d Bytes)', [Att.Name, Att.ContentSize]);
      end;
    finally
      ListViewAttachments.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmMain.btnPrevClick(Sender: TObject);
begin
  FCtrl.GotoPrevPage;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  FCtrl.GotoNextPage;
end;

procedure TfrmMain.btnHighlightClick(Sender: TObject);
begin
  FCtrl.HightlightText(Edit1.Text, False, False);
end;

procedure TfrmMain.btnScaleClick(Sender: TObject);
begin
  if FCtrl.ScaleMode = High(FCtrl.ScaleMode) then
    FCtrl.ScaleMode := Low(FCtrl.ScaleMode)
  else
    FCtrl.ScaleMode := Succ(FCtrl.ScaleMode);
  Caption := GetEnumName(TypeInfo(TPdfControlScaleMode), Ord(FCtrl.ScaleMode));
end;

procedure TfrmMain.WebLinkClick(Sender: TObject; Url: String);
begin
  ShowMessage(Url);
end;

procedure TfrmMain.AnnotationLinkClick(Sender: TObject; LinkInfo: TPdfLinkInfo; var Handled: Boolean);
begin
  Handled := True;
  case LinkInfo.LinkType of
    //altURI:
    //  ShowMessage('URL: ' + LinkAnnotation.LinkUri);

    //altLaunch:
    //  ShowMessage('Launch: ' + LinkAnnotation.LinkFileName);

    altEmbeddedGoto:
      ShowMessage('EmbeddedGoto: ' + LinkInfo.LinkUri);
  else
    Handled := False;
  end;
end;

procedure TfrmMain.PrintDocument(Sender: TObject);
begin
  //TPdfDocumentVclPrinter.PrintDocument(FCtrl.Document, ExtractFileName(FCtrl.Document.FileName));
end;

procedure TfrmMain.chkChangePageOnMouseScrollingClick(Sender: TObject);
begin
  //FCtrl.ChangePageOnMouseScrolling := chkChangePageOnMouseScrolling.Checked;
end;

procedure TfrmMain.chkLCDOptimizeClick(Sender: TObject);
begin
  (*
  if chkLCDOptimize.Checked then
    FCtrl.DrawOptions := FCtrl.DrawOptions + [proLCDOptimized]
  else
    FCtrl.DrawOptions := FCtrl.DrawOptions - [proLCDOptimized];
    *)
end;

procedure TfrmMain.chkSmoothScrollClick(Sender: TObject);
begin
//  FCtrl.SmoothScroll := chkSmoothScroll.Checked;
end;

procedure TfrmMain.edtZoomChange(Sender: TObject);
begin
  FCtrl.ZoomPercentage := edtZoom.Value;
end;

procedure TfrmMain.btnPrintClick(Sender: TObject);
{var
  PdfPrinter: TPdfDocumentPrinter;}
begin
  (*

  FCtrl.PrintDocument; // calls OnPrintDocument->PrintDocument
  //TPdfDocumentVclPrinter.PrintDocument(FCtrl.Document, 'PDF Example Print Job');

{  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := FCtrl.Document.PageCount;

  if PrintDialog1.Execute(Handle) then
  begin
    PdfPrinter := TPdfDocumentVclPrinter.Create;
    try
      //PdfPrinter.FitPageToPrintArea := False;

      if PrintDialog1.PrintRange = prAllPages then
        PdfPrinter.Print(FCtrl.Document)
      else
        PdfPrinter.Print(FCtrl.Document, PrintDialog1.FromPage - 1, PrintDialog1.ToPage - 1); // zero-based PageIndex
    finally
      PdfPrinter.Free;
    end;
  end;}
  *)
end;

procedure TfrmMain.ListViewAttachmentsDblClick(Sender: TObject);
var
  Att: TPdfAttachment;
begin
  if ListViewAttachments.Selected <> nil then
  begin
    Att := FCtrl.Document.Attachments[ListViewAttachments.Selected.Index];
    SaveDialog1.FileName := Att.Name;
    if SaveDialog1.Execute then
      Att.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.btnAddAnnotationClick(Sender: TObject);
begin
//  // Add a new annotation and make it persietent so that is can be shown and saved to a file.
//  FCtrl.CurrentPage.Annotations.NewTextAnnotation('My Annotation Text', TPdfRect.New(200, 750, 250, 700));
//  FCtrl.CurrentPage.ApplyChanges;
////  FCtrl.Document.SaveToFile(ExtractFileDir(ParamStr(0)) + PathDelim + 'Test_annot.pdf');
//
//  // Invalid the buffered image of the page
//  FCtrl.InvalidatePage;
end;

procedure TfrmMain.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    FCtrl.LoadFromFile(OpenDialog1.FileName);
end;

end.
