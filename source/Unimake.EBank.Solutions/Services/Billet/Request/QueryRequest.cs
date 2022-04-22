using Newtonsoft.Json;
using System;
using Unimake.EBank.Solutions.Services.Billet.Abstract;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// Dados para requisição de consulta do boleto
    /// <inheritdoc cref="RequestBase"/>
    /// </summary>
    public class QueryRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Última data para pesquisa do boleto com base na emissão
        /// </summary>
        [JsonProperty("dataEmissaoFinal")]
        public DateTime? DataEmissaoFinal { get; set; }

        /// <summary>
        /// Data de início para pesquisa do boleto com base na emissão
        /// </summary>
        [JsonProperty("dataEmissaoInicial")]
        public DateTime? DataEmissaoInicial { get; set; }

        /// <summary>
        /// Última data para pesquisa do boleto com base na liquidação
        /// </summary>
        [JsonProperty("dataLiquidacaoFinal")]
        public DateTime? DataLiquidacaoFinal { get; set; }

        /// <summary>
        /// Data de incício para pesquisa do boleto com base na liquidação
        /// </summary>
        [JsonProperty("dataLiquidacaoInicial")]
        public DateTime? DataLiquidacaoInicial { get; set; }

        /// <summary>
        /// Número do boleto gerado pela empresa (SeuNumero)
        /// </summary>
        [JsonProperty("numeroNaEmpresa")]
        public string NumeroNaEmpresa { get; set; }

        /// <summary>
        /// Número do boleto gerado pelo banco (NossoNumero)
        /// </summary>
        [JsonProperty("numeroNoBanco")]
        public string NumeroNoBanco { get; set; }

        #endregion Public Properties
    }
}