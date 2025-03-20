# 🚨 Breaking Changes

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

## 🔧 Como adaptar seu código

Se você utilizava alguma das funcionalidades removidas, será necessário atualizar seu código para utilizar soluções alternativas. Em especial:

- **Requisições e respostas** devem ser tipadas através do pacote [Unimake.EBank.Primitives](https://www.nuget.org/packages/Unimake.EBank.Primitives/).
- Caso precise de suporte ou tenha alguma dúvida sobre a migração, abra uma **issue** em: [Unimake/EBank/issues](https://github.com/Unimake/EBank/issues).

Para mais informações e suporte, acesse: [https://unimake.com.br/suporte](https://unimake.com.br/suporte).

Agradecemos pela compreensão e colaboração! 🚀

---

Se precisar de mais alguma alteração, é só avisar! 😊