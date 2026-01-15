# 🔖 Changelog

## Versão : 20260115.1205.59
_https://www.nuget.org/packages/Unimake.EBank.Solutions/20260115.1205.59_
fix: Referência de objeto não definida para uma instância de um objeto.

• Ajusta tratamento de NullReferenceException para evitar acesso a objeto nulo
• Adiciona validações de nulidade antes de utilizar dependências e resultados de chamadas
• Atualiza fluxo de execução para garantir inicialização adequada dos objetos envolvidos

---

## Versão : 20251114.0208.14

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20251114.0208.14_

Nesta atualização, foram adicionadas novas funcionalidades e melhorias importantes no tratamento de respostas da API.

### ✨ Funcionalidades

-   **`PDFService`** adicionado para manipulação de operações relacionadas a PDF, com métodos para obter documentos em **Base64**, **arquivo binário** ou **HTML**.
-   **`PDFTest`** criado para validar o comportamento do `PDFService`, incluindo testes para retornos em Base64, binário e HTML.

### ⚙️ Melhorias Técnicas

-   **`APIClient`** aprimorado para suportar respostas nos formatos **`byte[]`** e **`string`**, com melhor tratamento de erros e lógica atualizada de desserialização JSON.
-   Atualização da dependência **`Unimake.EBank.Primitives`** para incluir novos recursos.
-   Inclusão de namespaces adicionais e melhorias na inicialização dos testes.

---

## Versão : 20251106.1529.36

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20251106.1529.36_

fix: Incluído "PAGO" na definição de status do PIXPayload ID #178028

---

## Versão : 20251103.1722.21

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20251103.1722.21_

fix: JWT inválido. Esperado formato com 3 partes. ID #178028

---

## Versão : 20251013.1803.32

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20251013.1803.32_

Implementação dos tipos:

-   `PIXViolationResponse`: Representa a resposta de uma violação PIX, contendo informações detalhadas sobre o erro ocorrido
-   `PIXViolation`: Representa uma violação específica em uma requisição PIX.

O método `PIXJwtDecoder.GetJwtPayloadAsync(string url, SslProtocols? sslProtocols = null)` pede o [SslProtocols](https://learn.microsoft.com/en-us/dotnet/api/system.security.authentication.sslprotocols) como parâmetro, caso não informado, será usado o padrão do sistema operacional.

O método `PIXJwtDecoder.GetJwtPayloadAsync(string url, SslProtocols? sslProtocols = null)` tenta recuperar pelo menos o TxId, mesmo se o banco retornar 400 (Bad REquest)

---

## Versão : 20250908.0954.58

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250908.0954.58_

Implementação de HttpClientLoggingHandler, responsável por logs Http em chamadas com HttpClient.

---

## Versão : 20250905.1512.47

_https://www.nuget.org/packages/Unimake.Primitives/20250905.1512.47_

Implementação de HttpClientLoggingHandler, responsável por logs Http em chamadas com HttpClient.

## Versão : 20250822.1517.39

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250822.1517.39_

Atualização de pacotes.

---

## Versão : 20250808.1837.42

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250808.1837.42_

❌ Consulte: [BREAKING CHANGES](BREAKING_CHANGES.md#versão--20250808183742)

fix: Corrigido nome do enumerador `PagadorAlegaNaoTerRecebidoAMercadoria, NotaFiscal, Fatura` para `PagadorAlegaNaoTerRecebidoAMercadoria_NF_Ou_Fatura`

---

## Versão : 20250716.0200.59

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250716.0200.59_

Validação de requisição de extrato antes de consumir o serviço.

---

## Versão : 20250715.1745.53

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250715.1745.53_

Implementação do serviço de extrato.
