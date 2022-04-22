using EBank.Solutions.Primitives.Billet.Models;
using Newtonsoft.Json;
using System;

namespace Unimake.EBank.Solutions.Services.Billet.Response
{
    /// <summary>
    /// Resposta da consulta de boletos
    /// </summary>
    public class QueryResponse
    {
        #region Public Properties

        /// <summary>
        /// Data de emissão do boleto
        /// </summary>
        [JsonProperty("dataEmissao")]
        public DateTime DataEmissao { get; set; }

        /// <summary>
        /// Data da liquidação do boleto, se liquidado
        /// </summary>
        [JsonProperty("dataLiquidacao")]
        public DateTime DataLiquidacao { get; set; }

        /// <summary>
        /// Data do vencimento do boleto
        /// </summary>
        [JsonProperty("dataVencimento")]
        public DateTime DataVencimento { get; set; }

        /// <summary>
        /// Linha digitável do boleto
        /// </summary>
        [JsonProperty("linhaDigitavel")]
        public string LinhaDigitavel { get; set; }

        /// <summary>
        /// Número do boleto gerado pela empresa no momento da emissão (SeuNumero)
        /// </summary>
        [JsonProperty("numeroNaEmpresa")]
        public string NumeroNaEmpresa { get; set; }

        /// <summary>
        /// Número do boleto gerado pelo banco (NossoNumero)
        /// </summary>
        [JsonProperty("numeroNoBanco")]
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Dados do pagador do boleto
        /// </summary>
        [JsonProperty("pagador")]
        public Pagador Pagador { get; set; }

        /// <summary>
        /// Conteúdo em PDF no formato Base64. Pode ser nulo ou vazio
        /// </summary>
        [JsonProperty("pdfContent")]
        public string PdfContent { get; set; }

        /// <summary>
        /// Situação do boleto
        /// <para>Consultar o manual de cada banco para entender a situação retornada aqui</para>
        /// </summary>
        [JsonProperty("situacao")]
        public string Situacao { get; set; }

        /// <summary>
        /// Código do estado da requisição
        /// </summary>
        [JsonProperty("statusCode")]
        public long StatusCode { get; set; }

        /// <summary>
        /// Valor do boleto
        /// </summary>
        [JsonProperty("valor")]
        public long Valor { get; set; }

        /// <summary>
        /// Valor de desconto do boleto
        /// </summary>
        [JsonProperty("valorDesconto")]
        public long ValorDesconto { get; set; }

        /// <summary>
        /// Valor dos juros do boleto
        /// </summary>
        [JsonProperty("valorJuros")]
        public long ValorJuros { get; set; }

        /// <summary>
        /// Valor que foi liquidado no boleto
        /// </summary>
        [JsonProperty("valorLiquidado")]
        public long ValorLiquidado { get; set; }

        #endregion Public Properties
    }
}