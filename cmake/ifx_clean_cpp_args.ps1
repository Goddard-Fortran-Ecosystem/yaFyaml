# ifx 2025.2 on Windows only (see IntelLLVM.cmake): runs cpp (first argument) on
# its arguments with the leading U+FEFF removed and path backslashes turned into slashes.
param([string]$Cpp, [Parameter(ValueFromRemainingArguments = $true)] [string[]]$Rest)

$lines = New-Object System.Collections.Generic.List[string]
foreach ($a in $Rest) {
  if ($a.StartsWith('@') -and (Test-Path -LiteralPath $a.Substring(1))) {
    $text = [System.IO.File]::ReadAllText($a.Substring(1)).TrimStart([char]0xFEFF)
    $lines.Add(($text -replace '\\(?!")', '/'))   # keep \" escapes
  } else {
    $lines.Add('"' + ($a.TrimStart([char]0xFEFF) -replace '\\(?!")', '/') + '"')
  }
}
$tmp = [System.IO.Path]::GetTempFileName()   # unique name, safe for parallel builds
try {
  [System.IO.File]::WriteAllText($tmp, ($lines -join "`n"), (New-Object System.Text.UTF8Encoding($false)))
  $psi = New-Object System.Diagnostics.ProcessStartInfo
  $psi.FileName = $Cpp
  $psi.Arguments = '-P -traditional-cpp -undef @"' + $tmp + '"'
  $psi.UseShellExecute = $false   # output is not redirected, so cpp writes straight to our stdout
  $p = [System.Diagnostics.Process]::Start($psi)
  $p.WaitForExit()
  exit $p.ExitCode
} finally {
  Remove-Item -LiteralPath $tmp -ErrorAction SilentlyContinue
}
