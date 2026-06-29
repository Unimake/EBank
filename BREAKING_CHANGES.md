# 🚨 Breaking Changes

# 🎧 Suporte

- Para mais informações e suporte, acesse: [https://unimake.app/problems](https://unimake.app/problems).
- Para dúvidas ou ajuda com a migração, abra uma **issue** em: [Unimake/EBank/issues](https://github.com/Unimake/EBank/issues).

Agradecemos pela compreensão e colaboração! 🚀

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
