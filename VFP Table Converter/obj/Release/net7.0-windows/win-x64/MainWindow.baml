   M S B A M L   `   `   `  ����   VFP Table Converter$    VFP_Table_Converter.MainWindow   W SPresentationCore, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\ XPresentationFramework, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35e aSystem.Windows.Controls.Ribbon, Version=7.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089R NWindowsBase, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35F 9http://schemas.microsoft.com/winfx/2006/xaml/presentation     R NSystem.Xaml, Version=7.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml       � Title$  VFPTC��$	��450��$	��800��.���  � RowDefinitions �� $
��Auto ��� $��* ��� $
��Auto � � ColumnDefinitions �� $��* �.+��� -    ��Name$ btnChooseFile��$��Import��$
��Left=�$��
10,10,10,5q�$	��Top=�� $��1�� �� $��* �.+��� $
��Auto=�$
��Auto=�.���  �� $��* ��� $��* �.+��� -    ��Name$ txtFileContent��$��	10,5,5,10q� �� TextWrapping$
 Wrap=� �� AcceptsReturn$
 True���� -   $ txtVfpScript��$��1��$��	10,5,5,10q�$
 Wrap=�$
 True���� $��2��$��0�� �� Orientation$ 
Horizontal=�$��Right=�$��
10,5,10,10q�.+��� -   $ btnRegenerateScript��$��Convert to VFP Script��$��5q�$	��150���� -   $ btnCreateVfpScript��$��Save VFP Script��$��5q�$	��120���� -   $ btnRunVfpScript��$��Run VFP Script��$��5q�$	��120���� -   $ btnLocateVfp6��$��Locate VFP 6��$��5q�$	��120��