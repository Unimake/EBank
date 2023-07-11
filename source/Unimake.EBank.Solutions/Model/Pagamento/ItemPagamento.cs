using EBank.Solutions.Primitives.CNAB;
using EBank.Solutions.Primitives.CNAB.CNAB240;
using EBank.Solutions.Primitives.CNAB.CNAB240.Campo;
using EBank.Solutions.Primitives.Enumerations;
using Newtonsoft.Json;
using System;

namespace Unimake.EBank.Solutions.Model.Pagamento
{
    /// <summary>
    /// Retorno do pagamento
    /// </summary>
    public class ItemPagamento
    {
        #region Public Properties

        /// <summary>
        /// Aviso ao Favorecido
        /// </summary>
        [JsonProperty("Aviso")]
        public string Aviso { get; set; }

        /// <summary>
        /// Banco do favorecido
        /// </summary>
        public Banco Banco { get; set; }

        /// <summary>
        /// A câmara pela qual transitará a transferência. A critério de cada banco
        /// </summary>
        [JsonProperty("CodigoDaCamaraCentralizadora")]
        public CodigoDaCamaraCentralizadora CodigoDaCamaraCentralizadora { get; set; }

        /// <summary>
        /// Código adotado pela FEBRABAN, para identificar a ação a ser realizada com o lançamento enviado no arquivo.
        /// </summary>
        [JsonProperty("CodigoDaInstrucaoParaMovimento")]
        public CodigoDaInstrucaoParaMovimento CodigoDaInstrucaoParaMovimento { get; set; }

        /// <summary>
        /// Código/ Número do documento
        /// </summary>
        [JsonProperty("CodigoDoc")]
        public string CodigoDoc { get; set; }

        /// <summary>
        /// Identificador da finalidade do TED
        /// </summary>
        [JsonProperty("CodigoFinalidadeTED")]
        public string CodigoFinalidadeTED { get; set; }

        /// <summary>
        /// Dados da conta corrente
        /// </summary>
        [JsonProperty("ContaCorrente")]
        public ContaCorrente ContaCorrente { get; set; }

        /// <summary>
        /// Data efetiva em que o pagamento acontecerá
        /// </summary>
        [JsonProperty("DataEfetivacaoPagamento")]
        public DateTime DataEfetivacaoPagamento { get; set; }

        /// <summary>
        /// Data em que o arquivo de retorno de pagamento foi gerado
        /// </summary>
        public DateTime DataGeracaoArquivo { get; set; }

        /// <summary>
        /// Data do Pagamento P009
        /// Posição: 94 até 101 (8)
        /// </summary>
        public DateTime DataPagamento { get; set; }

        /// <summary>
        /// Quando informada constará em todos os avisos e/ou documentos originados dos detalhes desse lote.
        /// </summary>
        [JsonProperty("Informacao2")]
        public string Informacao2 { get; set; }

        /// <summary>
        /// Nome do Favorecido G013
        /// Posição: 44 até 73 (30)
        /// </summary>
        public string NomeFavorecido { get; set; }

        /// <summary>
        /// Número de identificação no banco
        /// </summary>
        [JsonProperty("NossoNumero")]
        public string NossoNumero { get; set; }

        /// <summary>
        /// Ocorrências, quando retorno
        /// </summary>
        [JsonProperty("Ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }

        /// <summary>
        /// Nº do Doc Atribuído pela Empresa G064
        /// Posição: 74 até 93 (20)
        /// </summary>
        public string SeuNumero { get; set; }

        /// <summary>
        /// Identifica o tipo de movimento
        /// </summary>
        [JsonProperty("TipoDeMovimento")]
        public TipoDeMovimento TipoDeMovimento { get; set; }

        /// <summary>
        /// Valor do pagamento autorizado
        /// </summary>
        [JsonProperty("ValorPagamento")]
        public decimal ValorPagamento { get; set; }

        /// <summary>
        /// Valor real do lançamento
        /// </summary>
        [JsonProperty("ValorReal")]
        public decimal ValorReal { get; set; }

        #endregion Public Properties
    }
}