   M S B A M L   `   `   `  ����  VFP Table Converter$    VFP_Table_Converter.MainWindow   5   	   W SPresentationCore, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\ XPresentationFramework, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35e aSystem.Windows.Controls.Ribbon, Version=7.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089R NWindowsBase, Version=7.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35F 9http://schemas.microsoft.com/winfx/2006/xaml/presentation     5   	   R NSystem.Xaml, Version=7.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml     5   	     � Title$  VFPTC��5   	   $	��450��6   $	��800��6$   .��5      �  � RowDefinitions 5   
   �� 5      $
��Auto �6   �� 5      $��* �6   �� 5	      $
��Auto �6    � ColumnDefinitions 5   
   �� 5      $��* �6   .+�5   
   �� -   5         ��Name$ btnChooseFile��5      $��Import��6(   $
��Left=�69   $��
10,10,10,5q�6T   $	��Top=�6h   6�   � 5   
   $��1��6    5      �� 5      $��* �6#   .+�5      �� $
��Auto=�6   $
��Auto=�6@   .��5      �  5      �� 5      $��* �6+   �� 5      $��* �6+   .+�5      �� -   5         ��Name$ txtFileContent��5      $��	10,5,5,10q�66    �� TextWrapping$
 Wrap=�6I    �� AcceptsReturn$
 True��6]   �� 5      -   5        $ txtVfpScript��5      $��1��64   $��	10,5,5,10q�6D   $
 Wrap=�6W   $
 True��6k   5       5!      5"      �� 5$   
   $��2��6   $��0��6"    �� Orientation$ 
Horizontal=�62   $��Right=�6K   $��
10,5,10,10q�6g   .+�5%      �� -   5        $ btnRegenerateScript��5%      $��Convert to VFP Script��62   $��5q�6R   $	��150��6]   6i   �� 5&      -   5        $ btnCreateVfpScript��5&      $��Save VFP Script��61   $��5q�6K   $	��120��6V   6b   �� 5'      -   5        $ btnRunVfpScript��5'      $��Run VFP Script��6.   $��5q�6G   $	��120��6R   6^   �� 5(      -   5        $ btnLocateVfp6��5(      $��Locate VFP 6��6,   $��5q�6C   $	��120��6N   6Z   5)      5*      5+      