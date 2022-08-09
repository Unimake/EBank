using Newtonsoft.Json;
using System.Collections.Generic;

namespace Unimake.EBank.Solutions.Model.Paged
{
    /// <summary>
    /// Response paginado
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class PagedResponse<T>
    {
        #region Public Properties

        /// <summary>
        /// Página atual
        /// </summary>
        [JsonProperty("CurrentPage")]
        public long CurrentPage { get; set; }

        /// <summary>
        /// Se verdadeiro, possui próximo registro
        /// </summary>
        [JsonProperty("HasNext")]
        public bool HasNext { get; set; }

        /// <summary>
        /// Se verdadeiro, possui registro anterior
        /// </summary>
        [JsonProperty("HasPrevious")]
        public bool HasPrevious { get; set; }

        /// <summary>
        /// Itens retornados por página
        /// </summary>
        [JsonProperty("Items")]
        public List<T> Items { get; set; }

        /// <summary>
        /// Contagem de itens retornados
        /// </summary>
        [JsonProperty("ItemsCount")]
        public long ItemsCount { get; set; }

        /// <summary>
        /// Tamanho da página
        /// </summary>
        [JsonProperty("PageSize")]
        public long PageSize { get; set; }

        /// <summary>
        /// Quantidade de registro no banco de dados
        /// </summary>
        [JsonProperty("TotalCount")]
        public long TotalCount { get; set; }

        /// <summary>
        /// Quantidade de páginas no banco de dados
        /// </summary>
        [JsonProperty("TotalPages")]
        public long TotalPages { get; set; }

        #endregion Public Properties
    }
}