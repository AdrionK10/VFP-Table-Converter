   h   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        3330	      Courier New        H  #  WINSPOOL HP LaserJet IIISi \\msprint35\corpa                                             ÔHP LaserJet IIISi                 @ w          	,  ,                                                                                 @ MSUDNHP LaserJet IIISi               Ą            d 
                    ˇDRIVER=WINSPOOL
DEVICE=HP LaserJet IIISi
OUTPUT=\\msprint35\corpa
ORIENTATION=1
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=265
PRINTQUALITY=300
DUPLEX=1
YRESOLUTION=300
TTOPTION=2
             titles_by_author.author_id                                      Times New Roman                "Titles By Author"             RALLTRIM(titles_by_author.last_name) + ", " +  ALLTRIM(titles_by_author.first_name)                                              Times New Roman                titles_by_author.title                                        Times New Roman                titles_by_author.publisher_name                                                                Times New Roman                titles_by_author.purchase_price                                                                Times New Roman                "@Z$ 9999.99"                  Times New Roman                "Title"                        Times New Roman                "Publisher Name"               Times New Roman                "Purchase Price"              "Printed: " + DTOC(DATE())                                                                     Times New Roman                 "Page " + ALLTRIM(STR( _pageno))                                                               Times New Roman                Times New Roman                "Date Purchased"              titles_by_author.date_purchased                                                                Times New Roman                titles_by_author.notes_b                                      Times New Roman                Times New Roman                "Description"                  Courier New                    Times New Roman                Times New Roman                Times New Roman                Times New Roman                dataenvironment                LLeft = 355
Top = 321
Width = 520
Height = 200
Name = "Dataenvironment"
                     5PROCEDURE BeforeOpenTables
SET DELETED ON
ENDPROC
           ťţň˙    ˘   ˘                         ;   %   9       Y      S           ü  U  	  G ţ U   BeforeOpenTables,     ˙˙1 a 1                       *       )   ˘                    `      cursor                         ÓLeft = 10
Top = 20
Width = 105
Height = 90
Alias = "titles_by_author"
Database = ..\data\books.dbc
CursorSource = "titles by author"
Name = "Cursor1"
                                                                 