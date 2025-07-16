#
# Se der erro, verifique se a variável de ambiente NugetApiKey foi definida com um valor válido
# 

if (-not $env:NugetApiKey) {
    Write-Output "Variável de ambiente não existe ou está vazia."
    exit 1
}

# Definições de variáveis
$nugetApiKey = $env:NugetApiKey
$projectFilePath = "Unimake.EBank.Solutions\Unimake.EBank.Solutions.csproj" 
$testProjectPath = "..\test\Unimake.EBank.Solutions.Tests\Unimake.EBank.Solutions.Tests.csproj"
$nugetSource = "https://api.nuget.org/v3/index.json" 

# Gera os números de versão com base na data/hora atual
$dataAtual = Get-Date -Format "yyyy.MM.dd.HHmm"
$dataVersao = Get-Date -Format "yyyyMMdd.HHmm"
$revisao = (Get-Date).Second.ToString("00")

# Novos valores de versão
$assemblyVersion = "$dataAtual"
$packageVersion = "$dataVersao.$revisao"

Write-Host "Nova versão gerada: AssemblyVersion=$assemblyVersion, Version=$packageVersion"

# Atualiza os valores no arquivo .csproj
Write-Host "Atualizando versões no arquivo do projeto..."
(Get-Content $projectFilePath) -replace "<AssemblyVersion>.*?</AssemblyVersion>", "<AssemblyVersion>$assemblyVersion</AssemblyVersion>" `
    -replace "<Version>.*?</Version>", "<Version>$packageVersion</Version>" |
Set-Content $projectFilePath

# Atualiza o arquivo changelog.md com a nova versão

# Caminho do README
$changelogPath = "..\CHANGELOG.md"

# Bloco da nova versão
$versaoMarkdown = @"

## Versão : $packageVersion
_https://www.nuget.org/packages/Unimake.EBank.Solutions/${packageVersion}_
---
`r`n
"@

# Lê o conteúdo atual do readme
$linhas = Get-Content $changelogPath

# Inicializa a posição como -1 (não encontrada)
$indiceVersoes = -1

# Procura o índice da linha que contém "# 🔖 Changelog"
for ($i = 0; $i -lt $linhas.Count; $i++) {
    if ($linhas[$i] -like "*# 🔖 Changelog*") {
        $indiceVersoes = $i
        break
    }
}

if ($indiceVersoes -ge 0) {
    # Divide o markdown por linha (sem \r\n) e insere logo após
    $markdownLinhas = $versaoMarkdown.TrimEnd().Split("`n")
    $linhas = $linhas[0..$indiceVersoes] + $markdownLinhas + $linhas[($indiceVersoes + 1)..($linhas.Count - 1)]

    # Salva o conteúdo modificado
    Set-Content -Path $changelogPath -Value $linhas -Encoding UTF8
} else {
    Write-Host "Seção '# 🔖 Changelog' não encontrada. Adicionando ao final do arquivo." -ForegroundColor Yellow
    Add-Content -Path $changelogPath -Value $versaoMarkdown -Encoding UTF8
}

# Abre o arquivo para edição
Write-Host "Abrindo o CHANGELOG.md para edição..."
Start-Process -FilePath $changelogPath -Wait

# Compila apenas o projeto específico
Write-Host "Compilando o projeto..."
& dotnet build $projectFilePath /p:Configuration=Release
if ($LASTEXITCODE -ne 0) {
    Write-Host "Erro na compilação!" -ForegroundColor Red
    exit 1
}

# Neste projeto ainda não é possível rodar 100% dos testes de unidade ¯\(°_o)/¯
# # Executa os testes
# Write-Host "Executando testes unitários..."
# & dotnet test $testProjectPath /p:Configuration=Debug-Unimake --no-build --verbosity normal
# if ($LASTEXITCODE -ne 0) {
#     Write-Host "Os testes falharam! O pacote não será publicado." -ForegroundColor Red
#     exit 1
# }

# Empacota o projeto
Write-Host "Empacotando o projeto..."
& dotnet pack $projectFilePath /p:Configuration=Release --no-build
if ($LASTEXITCODE -ne 0) {
    Write-Host "Erro ao empacotar!" -ForegroundColor Red
    exit 1
}

# Encontra o último pacote gerado na pasta bin\Release do projeto
Write-Host "Procurando o último pacote NuGet..."
$package = Get-ChildItem -Path (Join-Path (Split-Path -Parent $projectFilePath) "bin\Release\") -Filter "*.nupkg" | 
Sort-Object LastWriteTime -Descending | 
Select-Object -First 1

if (-not $package) {
    Write-Host "Nenhum pacote NuGet encontrado!" -ForegroundColor Red
    exit 1
}

Write-Host "Pacote encontrado: $($package.FullName)"

# Publica o pacote no NuGet
Write-Host "Publicando pacote no NuGet..."
& dotnet nuget push $package.FullName -k $nugetApiKey -s $nugetSource --skip-duplicate

# Verifica se a publicação foi bem-sucedida
if ($LASTEXITCODE -ne 0) {
    Write-Host "Erro ao publicar o pacote no NuGet!" -ForegroundColor Red
    exit 1
}

Write-Host "Publicação concluída com sucesso!" -ForegroundColor Green

Start-Process "https://www.nuget.org/packages/Unimake.EBank.Solutions/$packageVersion"
