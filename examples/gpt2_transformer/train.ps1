# train.ps1 — Build and run GPT-2 training with Nex + libtorch
#
# Usage:
#   .\examples\gpt2_transformer\train.ps1            (from repo root)
#   .\train.ps1                                       (from examples\gpt2_transformer)

$ErrorActionPreference = "Stop"

# ---------------------------------------------------------------------------
# Ensure full PATH (Cursor terminals may have a truncated PATH)
# ---------------------------------------------------------------------------

$env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
            [System.Environment]::GetEnvironmentVariable("Path", "User")

# ---------------------------------------------------------------------------
# Resolve paths
# ---------------------------------------------------------------------------

$ScriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Definition
$ProjectDir = Join-Path $ScriptDir "."

# Walk up to the repo root (where Cargo.toml lives)
$RepoRoot = $ScriptDir
while ($RepoRoot -and -not (Test-Path (Join-Path $RepoRoot "Cargo.toml"))) {
    $RepoRoot = Split-Path -Parent $RepoRoot
}
if (-not $RepoRoot) {
    Write-Error "Could not find repo root (no Cargo.toml found above $ScriptDir)"
    exit 1
}

Write-Host ""
Write-Host "=== GPT-2 Training Script (Nex + libtorch) ===" -ForegroundColor Cyan
Write-Host "  repo root : $RepoRoot"
Write-Host "  project   : $ProjectDir"
Write-Host ""

# ---------------------------------------------------------------------------
# 1. Build the Nex toolchain with torch support
# ---------------------------------------------------------------------------

Write-Host "[1/3] Building Nex toolchain (--features torch)..." -ForegroundColor Yellow
Push-Location $RepoRoot
try {
    cargo build --features torch --bin nex 2>&1 | ForEach-Object { Write-Host "  $_" }
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Cargo build failed (exit code $LASTEXITCODE)"
        exit $LASTEXITCODE
    }
    Write-Host "  Build OK" -ForegroundColor Green
} finally {
    Pop-Location
}

# ---------------------------------------------------------------------------
# 2. Locate the nex binary and libtorch
# ---------------------------------------------------------------------------

$NexBin = Join-Path $RepoRoot "target\debug\nex.exe"
if (-not (Test-Path $NexBin)) {
    Write-Error "nex binary not found at $NexBin"
    exit 1
}

# Find the libtorch lib directory (downloaded by torch-sys during build)
$TorchLib = Get-ChildItem -Recurse -Filter "torch_cpu.dll" `
    -Path (Join-Path $RepoRoot "target\debug\build") -ErrorAction SilentlyContinue |
    Select-Object -First 1 -ExpandProperty DirectoryName
if ($TorchLib) {
    $env:Path = "$TorchLib;$env:Path"
    Write-Host ""
    Write-Host "[2/3] libtorch: $TorchLib" -ForegroundColor Yellow
} else {
    Write-Host ""
    Write-Host "[2/3] WARNING: libtorch DLLs not found — runtime may fail" -ForegroundColor Red
}
Write-Host "       nex:      $NexBin" -ForegroundColor Yellow

# ---------------------------------------------------------------------------
# 3. Run GPT-2 training
# ---------------------------------------------------------------------------

Write-Host ""
Write-Host "[3/3] Running GPT-2 training..." -ForegroundColor Yellow
Write-Host ("-" * 60)

Push-Location $RepoRoot
try {
    & $NexBin run $ProjectDir
    $ExitCode = $LASTEXITCODE
} finally {
    Pop-Location
}

Write-Host ("-" * 60)
if ($ExitCode -eq 0) {
    Write-Host ""
    Write-Host "Training finished successfully." -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "Training exited with code $ExitCode" -ForegroundColor Red
}
exit $ExitCode
