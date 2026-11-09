# ifx 2025.2 on Windows only (see IntelLLVM.cmake): removes the leading U+FEFF
# and turns path backslashes into slashes in cpp's arguments; prints the new file.
param([Parameter(ValueFromRemainingArguments = $true)] [string[]]$Rest)

$lines = New-Object System.Collections.Generic.List[string]
foreach ($a in $Rest) {
  if ($a.StartsWith('@') -and (Test-Path -LiteralPath $a.Substring(1))) {
    $text = [System.IO.File]::ReadAllText($a.Substring(1)).TrimStart([char]0xFEFF)
    $lines.Add(($text -replace '\\(?!")', '/'))   # keep \" escapes
  } else {
    $lines.Add('"' + ($a.TrimStart([char]0xFEFF) -replace '\\(?!")', '/') + '"')
  }
}
$Out = [System.IO.Path]::GetTempFileName()   # unique name, safe for parallel builds
[System.IO.File]::WriteAllText($Out, ($lines -join "`n"), (New-Object System.Text.UTF8Encoding($false)))
Write-Output $Out
