using EBank.Solutions.Primitives.Abstractions.Paged;
using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Http;

namespace Unimake.EBank.Solutions.Services.Abstractions.Request
{
    /// <summary>
    /// Requisição para os tipos baseados em arquivos, como extrato, cobrança e pagamentos.
    /// </summary>
    public abstract class FileRequestBase : PagedRequestBase
    {
        #region Public Properties

        /// <summary>
        /// Número da conta corrente para pesquisa de arquivos
        /// </summary>
        public string AccountNumber { get; set; }

        /// <summary>
        /// Banco para filtro do arquivo
        /// </summary>
        public Banco Bank { get; set; }

        /// <summary>
        /// Intervalo de datas para fim da pesquisa do arquivo
        /// </summary>
        public DateTime EndDate { get; set; }

        /// <summary>
        /// Nome do arquivo
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        /// CNPJ ou identificação do cliente
        /// </summary>
        public string Registration { get; set; }

        /// <summary>
        /// Intervalo de datas para início da pesquisa do arquivo
        /// </summary>
        public DateTime StartDate { get; set; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Retorna a query string
        /// </summary>
        /// <returns></returns>
        public QueryString ToQueryString()
        {
            if(!EndDate.IsValid())
            {
                EndDate = StartDate;
            }

            var query = new QueryString
            {
                { nameof(Bank), (int)Bank },
                { nameof(StartDate), $"{StartDate:yyyy-MM-dd}" },
                { nameof(EndDate), $"{EndDate:yyyy-MM-dd}" },
                { nameof(PageNumber), PageNumber },
                { nameof(PageSize), PageSize }
            };

            if(!string.IsNullOrWhiteSpace(AccountNumber))
            {
                query.Add(nameof(AccountNumber), AccountNumber);
            }

            if(!string.IsNullOrWhiteSpace(Name))
            {
                query.Add(nameof(Name), Name);
            }

            if(!string.IsNullOrWhiteSpace(Registration))
            {
                query.Add(nameof(Registration), Registration);
            }

            return query;
        }

        #endregion Public Methods
    }
}