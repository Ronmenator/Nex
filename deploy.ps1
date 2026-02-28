# deploy.ps1 - Build and deploy Nex (debug + release)

$env:LIBTORCH = "F:\libtorch"
$env:LIBTORCH_CXX11_ABI = "0"
Remove-Item Env:LIBTORCH_USE_PYTORCH -ErrorAction SilentlyContinue

Write-Host "=== cargo xtask deploy ===" -ForegroundColor Cyan
cargo xtask deploy
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host ""
Write-Host "=== cargo xtask release ===" -ForegroundColor Cyan
cargo xtask release
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host ""
Write-Host "Done." -ForegroundColor Green
