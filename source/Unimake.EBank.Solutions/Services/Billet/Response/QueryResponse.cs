using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations.Billet;
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
        public DateTime? DataEmissao { get; set; }

        /// <summary>
        /// Data em que o boleto foi liquidado
        /// </summary>
        public DateTime? DataLiquidacao { get; set; }

        /// <summary>
        /// Date de vencimento do boleto
        /// </summary>
        public DateTime? DataVencimento { get; set; }

        /// <summary>
        /// Linha digitável do boleto
        /// </summary>
        public string LinhaDigitavel { get; set; }

        /// <summary>
        /// Identificação do Título na Empresa (seu número).
        /// </summary>
        public string NumeroNaEmpresa { get; set; }

        /// <summary>
        /// Número do documento no banco (nosso número).
        /// <para>Regras para validação:</para>
        /// </summary>
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Dados do pagador do boleto
        /// </summary>
        public Pagador Pagador { get; set; }

        /// <summary>
        /// Arquivo PDF do boleto em base 64.
        /// </summary>
        public string PdfContent { get; set; }

        /// <summary>
        /// Situação do boleto no momento da pesquisa. <see cref="SituacaoBoleto"/>
        /// </summary>
        public SituacaoBoleto Situacao { get; set; }

        /// <summary>
        /// Valor do boleto
        /// </summary>
        public decimal Valor { get; set; }

        /// <summary>
        /// Valor do desconto aplicado no momento do pagamento do boleto.
        /// </summary>
        public decimal ValorDesconto { get; set; }

        /// <summary>
        /// Valor dos juros aplicados no momento do pagamento do boleto
        /// </summary>
        public decimal ValorJuros { get; set; }

        /// <summary>
        /// Valor real pago do boleto
        /// </summary>
        public decimal ValorLiquidado { get; set; }

        #endregion Public Properties
    }
}