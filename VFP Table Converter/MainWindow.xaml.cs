using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Text;
using System.Windows;
using Microsoft.Win32;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using System.Linq;

namespace VFP_Table_Converter
{
    public partial class MainWindow : Window
    {
        private string FilePath { get; set; } = "";
        private string DatabasePath { get; set; } = @"C:\Thrive\sfdata\vista.dbc";
        private string Vfp6Path { get; set; } = "";
        private string TableName { get; set; } = "";

        public MainWindow()
        {
            InitializeComponent();

            Vfp6Path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "VFP6\\VFP6.exe");
            btnRunVfpScript.IsEnabled = true;
        }

        private void BtnChooseFile_Click(object sender, RoutedEventArgs e)
        {
            var dlg = new OpenFileDialog
            {
                DefaultExt = ".cs",
                Filter = "C# Files (*.cs)|*.cs"
            };
            
            var result = dlg.ShowDialog();
            
            if (result != true) return;
            
            FilePath = dlg.FileName;
            txtFileContent.Text = File.ReadAllText(FilePath);

            // Set the table name to the abbreviation of the file name here.
            var fileName = Path.GetFileNameWithoutExtension(FilePath);
            TableName = Abbreviate(fileName);

            // Generate and Display Vfp Script
            GenerateAndDisplayVfpScript(txtFileContent.Text, TableName);
        }

        private void BtnCreateVfpScript_Click(object sender, RoutedEventArgs e)
        {
            if (string.IsNullOrEmpty(txtVfpScript.Text))
            {
                MessageBox.Show("No VFP Script to save.");
                return;
            }

            var saveFileDialog = new SaveFileDialog
            {
                Filter = "PRG files (*.prg)|*.prg",
                DefaultExt = "prg",
                AddExtension = true
            };

            if (saveFileDialog.ShowDialog() != true) return;
            
            File.WriteAllText(saveFileDialog.FileName, txtVfpScript.Text);
            MessageBox.Show("VFP Script saved successfully.");
        }

        private void BtnRunVfpScript_Click(object sender, RoutedEventArgs e)
        {
            if (string.IsNullOrEmpty(FilePath))
            {
                MessageBox.Show("No VFP Script to run.");
                return;
            }

            if (!File.Exists(Vfp6Path))
            {
                MessageBox.Show("VFP6 executable not found.");
                return;
            }

            // Get the directory of the vista.dbc file
            var dbDirectory = Path.GetDirectoryName(DatabasePath);

            // Create a temporary PRG file
            var tempPrgFileName = "tempScript.prg";
            var tempPrgPath = Path.Combine(dbDirectory ?? "", tempPrgFileName);
            File.WriteAllText(tempPrgPath, txtVfpScript.Text);

            var process = new System.Diagnostics.Process
            {
                StartInfo = new System.Diagnostics.ProcessStartInfo
                {
                    FileName = Vfp6Path,
                    Arguments = $"-c \"{tempPrgPath}\"",
                    WorkingDirectory = dbDirectory, // Set the working directory to the directory of vista.dbc
                    UseShellExecute = false,
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    CreateNoWindow = true,
                    Verb = "runas" // Set to run as administrator
                }
            };

            try
            {
                process.Start();

                // Read and discard the output and error streams
                process.StandardOutput.ReadToEnd();
                process.StandardError.ReadToEnd();

                // Wait for the process to exit
                process.WaitForExit();

                // Do something after the process has completed
                // ...

                MessageBox.Show("VFP script executed silently.");
            }
            catch (System.ComponentModel.Win32Exception ex)
            {
                // Catch the exception if user cancels the UAC prompt or if the process fails to run as administrator
                MessageBox.Show("Failed to run VFP script as administrator: " + ex.Message);
            }

            // Delete the temporary PRG file
            File.Delete(tempPrgPath);
        }



        private void BtnLocateVfp6_Click(object sender, RoutedEventArgs e)
        {
            var dlg = new OpenFileDialog();
            dlg.DefaultExt = ".exe";
            dlg.Filter = "VFP 6 Executable (VFP6.exe)|VFP6.exe";
            var result = dlg.ShowDialog();
            if (result == true)
            {
                Vfp6Path = dlg.FileName;
                MessageBox.Show("VFP 6 executable located successfully.");
            }
        }

        private void BtnRegenerateScript_Click(object sender, RoutedEventArgs e)
        {
            // Try to extract the class name from the file content
            var match = System.Text.RegularExpressions.Regex.Match(txtFileContent.Text, @"class\s+(\w+)");
            if (match.Success)
            {
                // If a class name was found, use it as the table name
                TableName = Abbreviate(match.Groups[1].Value);
            }

            // Call the method that generates the script from the .cs file
            GenerateAndDisplayVfpScript(txtFileContent.Text, TableName);
        }

        private void GenerateAndDisplayVfpScript(string csFileContent, string customTableName)
        {
            if (string.IsNullOrEmpty(csFileContent))
            {
                MessageBox.Show("No file content to convert.");
                return;
            }

            var vfpScript = ConvertToVfpScript(csFileContent, customTableName);

            txtVfpScript.Text = vfpScript;
        }

        private string ConvertToVfpScript(string csFileContent, string customTableName)
        {
            var vfpScriptBuilder = new StringBuilder();

            var syntaxTree = CSharpSyntaxTree.ParseText(csFileContent);
            var root = syntaxTree.GetCompilationUnitRoot();

            // Find the class declaration
            var classDeclaration = root.DescendantNodes()
                .OfType<ClassDeclarationSyntax>()
                .FirstOrDefault();

            if (classDeclaration == null)
            {
                MessageBox.Show("No class declaration found.");
                return string.Empty;
            }

            // Determine the property containing "id" as the TAG for the INDEX
            var tagProperty = classDeclaration.Members.OfType<PropertyDeclarationSyntax>()
                .Select(property => property.Identifier.ValueText)
                .FirstOrDefault(propertyName => propertyName
                    .Contains("id", StringComparison.CurrentCultureIgnoreCase));

            // Generate VFP table structure
            vfpScriptBuilder.AppendLine("CD \"C:\\Thrive\\sfdata\\\"");
            vfpScriptBuilder.AppendLine($"OPEN DATABASE \"{DatabasePath}\" SHARED");
            vfpScriptBuilder.AppendLine();
            vfpScriptBuilder.AppendLine($"CREATE TABLE {customTableName} ;");

            var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();
            for (var i = 0; i < properties.Count; i++)
            {
                var propertyName = properties[i].Identifier.ValueText;
                var vfpType = FindType(properties[i].Type.ToString());
                var abbreviatedPropertyName = Abbreviate(propertyName);
                vfpScriptBuilder.AppendLine($"\t{(i == 0 ? "(" : "")}{abbreviatedPropertyName} {vfpType}{(i < properties.Count - 1 ? abbreviatedPropertyName.Equals(Abbreviate(tagProperty?? ""))? $" PRIMARY KEY DEFAULT SysgenPk(\"{customTableName}\") ," : ", ;" : ")")}");
            }

            vfpScriptBuilder.AppendLine();
            vfpScriptBuilder.AppendLine("USE sysgenpk");
            vfpScriptBuilder.AppendLine($"INSERT INTO sysgenpk (table, current) VALUES ('{customTableName}', 0)");
            vfpScriptBuilder.AppendLine();
            vfpScriptBuilder.AppendLine("CLOSE DATABASE");

            return vfpScriptBuilder.ToString();
        }

        private string FindType(string type)
        {
            var vfpType = "";
            type = type.Replace("?", "");
            switch (type)
            {
                case "int":
                    vfpType = "I(4)";
                    break;
                case "uint":
                    vfpType = "I(4)";
                    break;
                case "long":
                    vfpType = "I(8)";
                    break;
                case "ulong":
                    vfpType = "I(8)";
                    break;
                case "short":
                    vfpType = "I(2)";
                    break;
                case "ushort":
                    vfpType = "I(2)";
                    break;
                case "byte":
                    vfpType = "I(1)";
                    break;
                case "sbyte":
                    vfpType = "I(1)";
                    break;
                case "string":
                    vfpType = "C(30)";
                    break;
                case "char":
                    vfpType = "C(1)";
                    break;
                case "DateTime":
                    vfpType = "T(8)";
                    break;
                case "TimeSpan":
                    vfpType = "C(16)";
                    break;
                case "decimal":
                    vfpType = "N(13,5)";
                    break;
                case "float":
                    vfpType = "F(7)";
                    break;
                case "double":
                    vfpType = "F(16)";
                    break;
                case "bool":
                    vfpType = "L";
                    break;
                case "byte[]":
                    vfpType = "M";
                    break;
                case "object":
                    vfpType = "O";
                    break;
                default:
                    vfpType = "C(30)";
                    break;
            }

            return vfpType;
        }
        
        private string Abbreviate(string propertyName)
        {
            // Split the property name into separate words based on uppercase letters
            var words = Regex.Split(propertyName, @"(?<!^)(?=[A-Z])").Select(w => w.ToLower()).ToArray();

            // Create a list to store word lengths
            var wordLengths = words.Select(word => word.Length).ToList();

            var totalLength = words.Sum(word => word.Length);

            // Shorten each word while the total length is more than 10
            while (totalLength > 10)
            {
                // Find the longest word
                var maxIndex = wordLengths.IndexOf(wordLengths.Max());

                if (words[maxIndex].Length > 2)
                {
                    // Store the word before modifications to check if it has been changed
                    var oldWord = words[maxIndex];

                    // Remove inner vowels and repeating characters
                    words[maxIndex] = RemoveInnerVowels(words[maxIndex]);
                    words[maxIndex] = RemoveRepeatingCharacters(words[maxIndex]);

                    // Update word length
                    wordLengths[maxIndex] = words[maxIndex].Length;

                    // If the word hasn't been shortened, remove one character from the middle
                    if (words[maxIndex] == oldWord)
                    {
                        words[maxIndex] = words[maxIndex].Remove(words[maxIndex].Length / 2, 1);
                        wordLengths[maxIndex] = words[maxIndex].Length;
                    }
                }
                else
                {
                    break; // Stop the loop if the longest word is 2 characters or less
                }

                totalLength = wordLengths.Sum();
            }

            // Combine the words back
            var result = string.Join("", words);

            // If the combined result is still too long, remove the last characters until it fits
            if (result.Length > 10)
            {
                result = result.Substring(0, 10);
            }

            return result;
        }

        private string RemoveInnerVowels(string word)
        {
            if (word.Length <= 2)
            {
                return word;
            }

            var wordBuilder = new StringBuilder(word);
            for (var i = 1; i < wordBuilder.Length - 1; i++)
            {
                if (!IsVowel(wordBuilder[i])) continue;
                wordBuilder.Remove(i, 1);
                i--;
            }

            return wordBuilder.ToString();
        }

        private string RemoveRepeatingCharacters(string word)
        {
            if (word.Length <= 2)
            {
                return word;
            }

            var wordBuilder = new StringBuilder(word.Length);
            wordBuilder.Append(word[0]);
            var lastChar = word[0];

            for (var i = 1; i < word.Length - 1; i++)
            {
                if (word[i] != lastChar)
                {
                    wordBuilder.Append(word[i]);
                    lastChar = word[i];
                }
            }

            wordBuilder.Append(word[word.Length - 1]);

            return wordBuilder.ToString();
        }

        private bool IsVowel(char c)
        {
            return "aeiou".Contains(c);
        }
    }
}

