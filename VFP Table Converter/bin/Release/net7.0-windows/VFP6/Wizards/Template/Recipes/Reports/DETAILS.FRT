   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    H  #  WINSPOOL HP LaserJet IIISi \\msprint35\corpa                                             �HP LaserJet IIISi                � @ w          	,  ,                                                                                 @ MSUDNHP LaserJet IIISi               �            d 
                    �DRIVER=WINSPOOL
DEVICE=HP LaserJet IIISi
OUTPUT=\\msprint35\corpa
ORIENTATION=0
PAPERSIZE=1
COPIES=1
DEFAULTSOURCE=265
PRINTQUALITY=300
DUPLEX=1
YRESOLUTION=300
TTOPTION=2
             recipes.recipe_id              recipes.recipe_id                                             Times New Roman                recipes.recipe_name                                           Times New Roman                recipes.recipe_desc                                           Times New Roman                recipes.instructions                                          Times New Roman                recipes.utensils                                              Times New Roman                foodcat.foodcat                                               Times New Roman                recipes.source                                                Times New Roman                recipes.time_to_prep                                          Times New Roman                recipes.number_of_servings                                                                     Times New Roman                Times New Roman                                               "Recipe Description"           Times New Roman                                               "Instructions"                 Times New Roman                                               /"Utensils                                     "                 Times New Roman                                               $"Food Category                     "                            Times New Roman                                               "Vegetarian                   "                                 Times New Roman                                               *"Source                                  "                      Times New Roman                                               '"Time to Prepare                      "                         Times New Roman                                               #"Number of Servings               "                             	Wingdings                      recipes.vegetarian = .T.                                      "C"                            	Wingdings                      recipes.vegetarian = .F.                                      "D"                            Times New Roman                                               #"Recipe Ingredients               "                             Times New Roman                                               "Ingredient"                   Times New Roman                                               
"Quantity"                     Times New Roman                                               
"Comments"                     ingredie.ingredient            Courier New                    recingrd.quantity                                             Courier New                    "99999"                        recingrd.comments              Courier New                    Times New Roman                                               "Recipe ID"                    Courier New                    Times New Roman                Times New Roman                Times New Roman                	Wingdings                      dataenvironment                ILeft = -4
Top = 7
Width = 632
Height = 331
Name = "Dataenvironment"
                        5PROCEDURE BeforeOpenTables
SET DELETED ON
ENDPROC
           ����    �   �                         ;   %   9       Y      S           �  U  	  G � U   BeforeOpenTables,     ��1 a 1                       *       )   �                    M      cursor                         �Left = 10
Top = 20
Width = 95
Height = 91
Alias = "recipes"
Database = ..\data\recipes.dbc
CursorSource = "recipes"
Name = "Cursor1"
                                                                                        cursor                         �Left = 150
Top = 20
Width = 95
Height = 90
Alias = "recingrd"
Database = ..\data\recipes.dbc
CursorSource = "recingrd"
Name = "Cursor2"
                   relation                       �ParentAlias = "recipes"
RelationalExpr = "recipe_id"
ChildAlias = "recingrd"
ChildOrder = "recipe_id"
OneToMany = .T.
Name = "Relation1"
                    cursor                         �Left = 447
Top = 65
Width = 95
Height = 90
Alias = "ingredie"
Database = ..\data\recipes.dbc
CursorSource = "ingredie"
Name = "Cursor3"
                   cursor                         �Left = 305
Top = 181
Width = 95
Height = 90
Alias = "foodcat"
Database = ..\data\recipes.dbc
CursorSource = "foodcat"
Name = "Cursor4"
                    relation                       }ParentAlias = "foodcat"
RelationalExpr = "foodcatid"
ChildAlias = "recipes"
ChildOrder = "foodcatid"
Name = "Relation2"
                                      relation                       ParentAlias = "recingrd"
RelationalExpr = "ingred_id"
ChildAlias = "ingredie"
ChildOrder = "ingred_id"
Name = "Relation3"
                              