# Nex installer for Windows
# Usage: irm https://raw.githubusercontent.com/Ronmenator/Nex/master/install.ps1 | iex

$ErrorActionPreference = "Stop"

$repo  = "Ronmenator/Nex"
$arch  = "windows-x86_64"
$nexHome = "$env:USERPROFILE\.nex"
$nexBin  = "$nexHome\bin"

Write-Host ""
Write-Host "  Nex Installer" -ForegroundColor Cyan
Write-Host "  ─────────────" -ForegroundColor DarkGray
Write-Host ""

# 1. Fetch latest release info
Write-Host "  Fetching latest release..." -ForegroundColor Gray
try {
    $release = Invoke-RestMethod -Uri "https://api.github.com/repos/$repo/releases/latest" -Headers @{ "User-Agent" = "nex-installer" }
} catch {
    Write-Host "  ERROR: Failed to fetch release info from GitHub." -ForegroundColor Red
    Write-Host "  $_" -ForegroundColor DarkRed
    exit 1
}

$tag = $release.tag_name
$version = $tag.TrimStart("v")
Write-Host "  Latest version: $tag" -ForegroundColor Green
Write-Host ""

# 2. Find the zip asset
$asset = $release.assets | Where-Object { $_.name -like "*$arch*.zip" } | Select-Object -First 1

if (-not $asset) {
    Write-Host "  ERROR: No release asset found for $arch." -ForegroundColor Red
    Write-Host "  Available assets:" -ForegroundColor Yellow
    $release.assets | ForEach-Object { Write-Host "    - $($_.name)" }
    exit 1
}

$downloadUrl = $asset.browser_download_url
$zipName = $asset.name

# 3. Download
$tempDir = Join-Path $env:TEMP "nex-install-$([System.IO.Path]::GetRandomFileName())"
New-Item -ItemType Directory -Path $tempDir -Force | Out-Null
$zipPath = Join-Path $tempDir $zipName

Write-Host "  Downloading $zipName..." -ForegroundColor Gray
try {
    Invoke-WebRequest -Uri $downloadUrl -OutFile $zipPath -UseBasicParsing
} catch {
    Write-Host "  ERROR: Download failed." -ForegroundColor Red
    Write-Host "  $_" -ForegroundColor DarkRed
    Remove-Item -Recurse -Force $tempDir -ErrorAction SilentlyContinue
    exit 1
}

# 4. Extract
Write-Host "  Extracting to $nexHome..." -ForegroundColor Gray
$extractDir = Join-Path $tempDir "extracted"
Expand-Archive -Path $zipPath -DestinationPath $extractDir -Force

# The zip contains a top-level directory like nex-v0.1.88-windows-x86_64/
$innerDirs = Get-ChildItem -Path $extractDir -Directory
if ($innerDirs.Count -eq 1) {
    $sourceDir = $innerDirs[0].FullName
} else {
    $sourceDir = $extractDir
}

# 5. Install - create ~/.nex and copy contents
if (Test-Path $nexHome) {
    # Back up existing bin if present
    $existingBin = Join-Path $nexHome "bin"
    if (Test-Path $existingBin) {
        $backupDir = Join-Path $nexHome "backup-$(Get-Date -Format 'yyyyMMdd-HHmmss')"
        Write-Host "  Backing up existing install to $backupDir..." -ForegroundColor Yellow
        New-Item -ItemType Directory -Path $backupDir -Force | Out-Null
        Copy-Item -Path "$existingBin\*" -Destination $backupDir -Recurse -Force
    }
}

New-Item -ItemType Directory -Path $nexHome -Force | Out-Null

# Copy bin/, libs/, config/
foreach ($subdir in @("bin", "libs", "config")) {
    $src = Join-Path $sourceDir $subdir
    $dst = Join-Path $nexHome $subdir
    if (Test-Path $src) {
        if (Test-Path $dst) {
            Remove-Item -Recurse -Force $dst
        }
        Copy-Item -Path $src -Destination $dst -Recurse -Force
        Write-Host "  Installed $subdir/" -ForegroundColor Green
    }
}

# 6. Add to PATH
$currentPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($currentPath -split ";" | Where-Object { $_ -eq $nexBin }) {
    Write-Host "  PATH already contains $nexBin" -ForegroundColor DarkGray
} else {
    [Environment]::SetEnvironmentVariable("Path", "$nexBin;$currentPath", "User")
    $env:Path = "$nexBin;$env:Path"
    Write-Host "  Added $nexBin to PATH" -ForegroundColor Green
}

# 7. Clean up temp files
Remove-Item -Recurse -Force $tempDir -ErrorAction SilentlyContinue

# 8. Verify
Write-Host ""
$nexExe = Join-Path $nexBin "nex.exe"
if (Test-Path $nexExe) {
    Write-Host "  Nex $tag installed successfully!" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Location:  $nexHome" -ForegroundColor Gray
    Write-Host "  Binary:    $nexExe" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  Restart your terminal, then run:" -ForegroundColor Yellow
    Write-Host "    nex --version" -ForegroundColor White
} else {
    Write-Host "  WARNING: nex.exe not found at $nexExe" -ForegroundColor Red
    Write-Host "  The installation may be incomplete." -ForegroundColor Yellow
}
Write-Host ""
