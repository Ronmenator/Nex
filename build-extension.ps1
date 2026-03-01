# build-extension.ps1 — Build the Nex VS Code extension (.vsix)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$extDir = Join-Path $PSScriptRoot "vscode-extension"

Push-Location $extDir
try {
    # Install dependencies if needed
    if (-not (Test-Path "node_modules")) {
        Write-Host "Installing dependencies..." -ForegroundColor Cyan
        npm install
    }

    # Compile TypeScript
    Write-Host "Compiling TypeScript..." -ForegroundColor Cyan
    npx tsc -p .
    if ($LASTEXITCODE -ne 0) { throw "TypeScript compilation failed" }

    # Package into .vsix
    Write-Host "Packaging extension..." -ForegroundColor Cyan
    npx @vscode/vsce package --no-dependencies
    if ($LASTEXITCODE -ne 0) { throw "VSIX packaging failed" }

    $vsix = Get-ChildItem -Filter "*.vsix" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
    $dest = Join-Path $PSScriptRoot $vsix.Name
    Move-Item $vsix.FullName $dest -Force

    Write-Host ""
    Write-Host "Built: $($vsix.Name)" -ForegroundColor Green
    Write-Host "Install with:" -ForegroundColor Gray
    Write-Host "  code --install-extension $($vsix.Name)" -ForegroundColor Yellow
}
finally {
    Pop-Location
}
