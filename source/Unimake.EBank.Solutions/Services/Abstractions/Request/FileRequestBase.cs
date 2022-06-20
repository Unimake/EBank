using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Text;

namespace Unimake.EBank.Solutions.Services.Abstractions.Request
{
    /// <summary>
    /// Requisição para os tipos baseados em arquivos, como extrato, cobrança e pagamentos.
    /// </summary>
    public abstract class FileRequestBase
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
        public string ToQueryString()
        {
            if(!EndDate.IsValid())
            {
                EndDate = StartDate;
            }

            var sb = new StringBuilder()
               .Append($"{nameof(Bank)}={(int)Bank}&")
               .Append($"{nameof(StartDate)}={StartDate:yyyy-MM-dd}&")
               .Append($"{nameof(EndDate)}={EndDate:yyyy-MM-dd}&");

            if(!string.IsNullOrWhiteSpace(AccountNumber))
            {
                _ = sb.Append($"{nameof(AccountNumber)}={AccountNumber}&");
            }

            if(!string.IsNullOrWhiteSpace(Name))
            {
                _ = sb.Append($"{nameof(Name)}={Name}&");
            }

            if(!string.IsNullOrWhiteSpace(Registration))
            {
                _ = sb.Append($"{nameof(Registration)}={Registration}&");
            }

            return sb.ToString();
        }

        #endregion Public Methods
    }
}