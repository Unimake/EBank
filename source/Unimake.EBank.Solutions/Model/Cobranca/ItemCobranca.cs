using EBank.Solutions.Primitives.CNAB.CNAB400;
using EBank.Solutions.Primitives.Enumerations;
using Newtonsoft.Json;
using System;
using System.ComponentModel;
using System.Diagnostics;

namespace Unimake.EBank.Solutions.Model.Cobranca
{
    /// <summary>
    /// Retorno de cobrança
    /// </summary>
    public class ItemCobranca
    {
        #region Public Properties

        /// <summary>
        /// Código da agência do cliente
        /// </summary>
        [JsonProperty("Agencia")]
        public long Agencia { get; set; }

        /// <summary>
        /// Código da agência cobradora
        /// </summary>
        [JsonProperty("AgenciaCobradora")]
        public long AgenciaCobradora { get; set; }

        /// <summary>
        /// Banco
        /// </summary>
        [JsonProperty("Banco")]
        public Banco Banco { get; set; }

        /// <summary>
        /// Indicador se o boleto é DDA
        /// </summary>
        [JsonProperty("BoletoDDA")]
        public TipoDeBoletoDDA? BoletoDDA { get; set; }

        /// <summary>
        /// Código da carteira do cliente
        /// </summary>
        [JsonProperty("CodigoCarteira")]
        public char CodigoCarteira { get; set; }

        /// <summary>
        /// Código da conta
        /// </summary>
        [JsonProperty("Conta")]
        public long Conta { get; set; }

        /// <summary>
        /// Dígido verificador da agência do cliente
        /// </summary>
        [JsonProperty("DAC")]
        public long DAC { get; set; }

        /// <summary>
        /// Dígito verificador da agência de cobrança
        /// </summary>
        [JsonProperty("DacAgenciaCobradora")]
        public long DACAgenciaCobradora { get; set; }

        /// <summary>
        /// Data em que o crédito ocorreu
        /// </summary>
        [JsonProperty("DataCredito")]
        public DateTime DataCredito { get; set; }

        /// <summary>
        /// Data da ocorrência do crédito
        /// </summary>
        [JsonProperty("DataOcorrencia")]
        public DateTime DataOcorrencia { get; set; }

        /// <summary>
        /// Data de vencimento do crédito
        /// </summary>
        [JsonProperty("DataVencimento")]
        public DateTime DataVencimento { get; set; }

        /// <summary>
        /// Espécie do título
        /// </summary>
        [JsonProperty("Especie")]
        public TipoDeEspecieDoTitulo? Especie { get; set; }

        /// <summary>
        /// Número de inscrição, CPF, CNPJ ou outro documento
        /// </summary>
        [JsonProperty("Inscricao")]
        public long Inscricao { get; set; }

        /// <summary>
        /// Indicador de da ocorrência, varia de banco para banco
        /// </summary>
        [JsonProperty("InstrucaoCanceladaOuComplementoDaOcorrencia")]
        public short InstrucaoCanceladaOuComplementoDaOcorrencia { get; set; }

        /// <summary>
        /// </summary>
        [JsonProperty("Liquidacao")]
        public string Liquidacao { get; set; }

        /// <summary>
        /// </summary>
        [JsonProperty("Mensagem")]
        public string Mensagem { get; set; }

        /// <summary>
        /// Nome do cliente
        /// </summary>
        [JsonProperty("Nome")]
        public string Nome { get; set; }

        /// <summary>
        /// Indicador de nosso número
        /// </summary>
        [JsonProperty("NossoNumero")]
        public long NossoNumero { get; set; }

        /// <summary>
        /// Indicador de nosso número
        /// </summary>
        [JsonIgnore]
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        [EditorBrowsable(EditorBrowsableState.Never)]
        public long NossoNumero1 { get => NossoNumero; set => NossoNumero = value; }

        /// <summary>
        /// Número da carteira do cliente
        /// </summary>
        [JsonProperty("NumeroCarteira")]
        public short NumeroCarteira { get; set; }

        /// <summary>
        /// Número do documento
        /// </summary>
        [JsonProperty("NumeroDocumento")]
        public string NumeroDocumento { get; set; }

        /// <summary>
        /// Código da ocorrência no banco
        /// </summary>

        /// <summary>
        /// Quando arquivo CNAB, indica a sequência do registro no arquivo
        /// </summary>
        public int NumeroSequencialDoRegistroNoArquivo { get; set; }

        /// <summary>
        /// Indica o código da ocorrência de retorno do arquivo
        /// </summary>
        [JsonProperty("Ocorrencia")]
        public CodigoDeOcorrenciaDeRetorno Ocorrencia { get; set; }

        /// <summary>
        /// Tipo de arquivo CNAB
        /// </summary>
        public TipoDeRegistro Tipo { get; set; }

        /// <summary>
        /// Indica o tipo de inscrição informada na propriedade <see cref="Inscricao"/>
        /// </summary>
        [JsonProperty("TipoInscricao")]
        public TipoDeInscricao TipoInscricao { get; set; }

        /// <summary>
        /// Informação dada pela empresa
        /// </summary>
        [JsonProperty("UsoDaEmpresa")]
        public string UsoDaEmpresa { get; set; }

        /// <summary>
        /// Valor do abatimento da cobrança, se existir
        /// </summary>
        [JsonProperty("ValorAbatimento")]
        public decimal ValorAbatimento { get; set; }

        /// <summary>
        /// Valor do desconto da cobrança, se existir
        /// </summary>
        [JsonProperty("ValorDescontos")]
        public decimal ValorDescontos { get; set; }

        /// <summary>
        /// Valor das despesas de cobrança, se existir
        /// </summary>
        [JsonProperty("ValorDespesaCobranca")]
        public decimal ValorDespesaCobranca { get; set; }

        /// <summary>
        /// </summary>
        [JsonProperty("ValorIOF")]
        public decimal ValorIOF { get; set; }

        /// <summary>
        /// </summary>
        [JsonProperty("ValorJurosMoraMulta")]
        public decimal ValorJurosMoraMulta { get; set; }

        /// <summary>
        /// Valor da cobrança
        /// </summary>
        [JsonProperty("ValorNominal")]
        public decimal ValorNominal { get; set; }

        /// <summary>
        /// Valor de créditos a cobrança
        /// </summary>
        [JsonProperty("ValorOutrosCreditos")]
        public decimal ValorOutrosCreditos { get; set; }

        /// <summary>
        /// Valor da cobrança
        /// </summary>
        [JsonProperty("ValorPrincipal")]
        public decimal ValorPrincipal { get; set; }

        #endregion Public Properties
    }
}