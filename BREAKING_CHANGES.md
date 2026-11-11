# 🚨 Breaking Changes

# 🎧 Suporte

- Para mais informações e suporte, acesse: [https://unimake.app/problems](https://unimake.app/problems).
- Para dúvidas ou ajuda com a migração, abra uma **issue** em: [Unimake/EBank/issues](https://github.com/Unimake/EBank/issues).

Agradecemos pela compreensão e colaboração! 🚀

---

## Versão : AAAAMMDD.HHMM.SS

_https://www.nuget.org/packages/Unimake.EBank.Solutions/AAAAMMDD.HHMM.SS_

Esta versão atualiza o `Unimake.EBank.Primitives` de [20260731.1624.57](https://www.nuget.org/packages/Unimake.EBank.Primitives/20260731.1624.57) para [20261111.1123.46](https://www.nuget.org/packages/Unimake.EBank.Primitives/20261111.1123.46). Algumas mudanças do Primitives chegam a quem usa este pacote e podem exigir ajustes no código.

### ❌ Pagamentos: `MultiplosPagamentosRequest` foi removido

A requisição única de consulta e cancelamento de pagamentos foi dividida em duas classes, uma para cada operação. As duas têm as mesmas propriedades de antes (`Banco`, `Agencia`, `Conta` e `Pagamentos`) e herdam de `EBank.Solutions.Primitives.Abstractions.Pagamento.MultiplosPagamentosRequestBase`.

| Método do `PagamentoService` | Antes                        | Agora                              |
| ---------------------------- | ---------------------------- | ---------------------------------- |
| `CancelarPagamentoAsync`     | `MultiplosPagamentosRequest` | `MultiplosPagamentosCancelRequest` |
| `ConsultarPagamentosAsync`   | `MultiplosPagamentosRequest` | `MultiplosPagamentosQueryRequest`  |

Antes:

```csharp
var response = await service.ConsultarPagamentosAsync(new MultiplosPagamentosRequest
{
    Pagamentos = [ new IdentificadorPagamentoItem { /* ... */ } ]
}, scope);
```

Agora:

```csharp
var response = await service.ConsultarPagamentosAsync(new MultiplosPagamentosQueryRequest
{
    Pagamentos = [ new IdentificadorPagamentoItem { /* ... */ } ]
}, scope);
```

### ❌ `ConfigurationId`: novo comportamento e nova exceção

- O método `ShouldSerializeConfigurationId()` foi removido de `RequestBase` e de `RegisterRequestBase`. O `ConfigurationId` preenchido na requisição passa a ser enviado no corpo da chamada à API. Se você sobrescrevia esse método numa classe própria, remova a sobrescrita (o compilador acusa o erro `CS0115`).
- A API passa a validar o `ConfigurationId`. Se ele não existir para o cliente, ou a configuração estiver incompleta, a resposta é HTTP 400. Confira se o valor informado é o mesmo cadastrado nas configurações do e-Bank. Requisições sem `ConfigurationId` continuam funcionando como antes.
- O construtor `InvalidBankCredentialsException(string configurationId)` foi removido. Para tratar configuração inexistente ou incompleta, use a nova exceção `EBank.Solutions.Primitives.Exceptions.Security.ConfigurationIdNotFoundException`, que expõe o valor recebido em `ConfigurationId`.
- A API passa a recusar requisições que informam `ConfigurationId` diferentes: na query string e no corpo, ou em itens distintos do corpo (por exemplo, numa lista de boletos ou de pagamentos). Antes, o valor da query string era usado sem aviso. Agora a resposta é HTTP 400 com a nova exceção `EBank.Solutions.Primitives.Exceptions.Security.ConfigurationIdMismatchException`, que expõe o valor resolvido em `ConfigurationId` e o valor divergente em `BodyConfigurationId`. Informe o mesmo `ConfigurationId` em todos os itens, ou apenas em um lugar. A comparação ignora maiúsculas, minúsculas e espaços nas pontas.

### ❌ Boleto: `DataVencimento` da prorrogação passa a ser `BankDate`

`ExtendPaymentRequest.DataVencimento` (e `IExtendPaymentRequest` / `ExtendPaymentRequestBase`) mudou de `DateTime` para `BankDate`.

O `BankDate` converte de e para `DateTime` de forma implícita, então atribuições como `DataVencimento = DateTime.Today.AddDays(10)` continuam compilando. É preciso ajustar o código que usa direto na propriedade membros de `DateTime` que o `BankDate` não tem, como `DayOfWeek` ou `AddMonths`. Nesse caso, converta antes: `request.DataVencimento.ToDateTime().AddMonths(1)`. Recompile a aplicação, pois o tipo da propriedade mudou.

### ❌ PIX: `LocationId` removido da consulta

As propriedades `PIXGetRequest.LocationId` e `PIXGetRequest.LocationIdPresent` foram removidas. Remova as referências a elas.

### ❌ JSON: conversor de `BankDate` renomeado

- `EBank.Solutions.Primitives.Json.Converters.BankDateJsonConverter` passa a se chamar `TextJsonBankDateJsonConverter` (System.Text.Json).
- Foi criado o `NewtonsoftBankDateJsonConverter`, já aplicado ao tipo `BankDate`. Com o Newtonsoft.Json, o `BankDate` passa a ser lido e gravado como data, e não mais como objeto. Se você registrava um conversor próprio para `BankDate` no Newtonsoft, revise se ele ainda é necessário.

### ❌ Extrato: membros de `CodigoHistoricoExtrato` renomeados

Os valores numéricos não mudaram, apenas os nomes. Atenção se você grava ou compara esses valores como texto.

| Antes                           | Agora                           |
| ------------------------------- | ------------------------------- |
| `AplicacaoFundoEmpresarialDi60` | `AplicacaoFundoEmpresarialDI60` |
| `ResgateCDBDi`                  | `ResgateCDBDI`                  |

### ⚠️ Interfaces com novos membros

Só afeta quem implementa estas interfaces em classes próprias. Quem apenas usa as classes do pacote não precisa fazer nada.

| Interface                   | Novo membro                                                   |
| --------------------------- | ------------------------------------------------------------- |
| `IPDFResponse`              | `FichaAceiteContent FichaAceiteContent { get; set; }`         |
| `IQueryInformationResponse` | `List<HistoricoBoleto> Historico { get; set; }`               |
| `ILancamentoResponse`       | `List<OcorrenciaPagamentoResponse> Ocorrencias { get; set; }` |

---

## Versão : 20260629.1822.51

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20260629.1822.51_

feat: Implementar PagamentoService. ID #185186
- Adicionado o PagamentoService com métodos para operações de pagamento (autorizar, cancelar, consultar).
- Introduzido o PagamentoTest com testes para fluxos de pagamento.
- Atualizadas as versões dos pacotes NuGet EBank.Primitives e Primitives.
- Aprimorado o TestBase para suportar novos tipos de requisição e melhorar a configuração dos testes.

A partir desta versão [20260629.1822.51](https://www.nuget.org/packages/Unimake.EBank.Solutions/20260629.1822.51), aplicações que utilizam o módulo de **pagamentos** poderão precisar de ajustes.

### ❌ Namespace renomeado

| Antes      | Agora       |
| ---------- | ----------- |
| `Payments` | `Pagamento` |

Caso o compilador informe que o namespace `Payments` não existe, substitua todas as referências por `Pagamento`.

---

## Versão : 20250820.2046.57

_https://www.nuget.org/packages/Unimake.EBank.Primitives/20250820.2046.57_

## ❌ Remoções e Renomeações
https://www.nuget.org/packages/Unimake.EBank.Primitives/20250808.1837.42
Corrigido nome do enumerador `AssociacaodePais,AmigosePessoascomDeficiencia = 247,` para `AssociacaodePaisAmigosePessoascomDeficiencia = 247,`

---

## Versão : 20250808.1837.42

_https://www.nuget.org/packages/Unimake.EBank.Solutions/20250808.1837.42_

## ❌ Remoções e Renomeações
https://www.nuget.org/packages/Unimake.EBank.Primitives/20250808.1837.42
Corrigido nome do enumerador `PagadorAlegaNaoTerRecebidoAMercadoria, NotaFiscal, Fatura` para `PagadorAlegaNaoTerRecebidoAMercadoria_NF_Ou_Fatura`

----
## Versão 20250516.1838.2
https://www.nuget.org/packages/Unimake.EBank.Primitives/20250516.1838.2

Nesta atualização, foram feitas diversas mudanças significativas que podem impactar projetos que utilizam este pacote.

## ❌ Remoções e Renomeações

### 🚫 Tipos Removidos

Foram removidos os seguintes tipos:

- `CancelRequest`
- `CancelRequestBase`
- `InformPaymentRequest`
- `InformPaymentRequestBase`
- `ICancelRequest`
- `ICancelResponse`
- `IInformPaymentRequest`
- `IInformPaymentResponse`

Foram removidos os métodos:

- `BilletService.InformPaymentAsync()`
- `BilletService.CancelAsync()`

### ✏️ Renomeações

Os seguintes componentes foram renomeados:

- `InformPaymentRequestValidator` → `BaixarRequestValidator`
- Exceção `CancelResponseException` → `BaixarResponseException`
- Interface `ICancelRequestValidatorService` → `IBaixarRequestValidatorService`

## 🔧 Como adaptar seu código

Caso seu projeto utilize algum dos tipos ou componentes mencionados acima, será necessário atualizar as referências conforme abaixo:

- Substitua os tipos `CancelRequest` e `InformPaymentRequest` por `BaixarRequest`.
- Substitua os tipos `CancelResponse` e `InformPaymentResponse` por `BaixarResponse`.
- Use as interfaces `IBaixarRequest` e `IBaixarResponse` no lugar das interfaces removidas.
- Atualize o nome da exceção e dos validadores conforme as novas definições.
- Alterar os nomes dos métodos `BilletService.InformPaymentAsync()` e `BilletService.CancelAsync()` para `BilletService.BaixarAsync()`

----
## Versão 20250320.359.0
https://www.nuget.org/packages/Unimake.EBank.Solutions/20250320.359.0

Nesta atualização, foram feitas diversas mudanças significativas que podem impactar projetos que utilizam este pacote.

## ❌ Remoções

Foram removidos os seguintes componentes:

- Conversores para JSON
- Extensões de validação e validadores (`validators`)
- Serviços de cobrança, pagamento e varredura **CNAB**
- Serviços de cobrança, pagamento e varredura para **APIs**
- Contrato de escopos
- Abstrações para o servidor de arquivos **CNAB**
- Tipos **`Request`** (agora todas as requisições devem ser tipadas pelo pacote: [Unimake.EBank.Primitives](https://www.nuget.org/packages/Unimake.EBank.Primitives/))
- Tipos **`Response`** (agora todas as respostas devem ser tipadas pelo pacote: [Unimake.EBank.Primitives](https://www.nuget.org/packages/Unimake.EBank.Primitives/))

### 🔧 Como adaptar seu código

Se você utilizava alguma das funcionalidades removidas, será necessário atualizar seu código para utilizar soluções alternativas. Em especial:

- **Requisições e respostas** devem ser tipadas através do pacote [Unimake.EBank.Primitives](https://www.nuget.org/packages/Unimake.EBank.Primitives/).
