using EBank.Solutions.Primitives.PIX.Models.Consulta;
using EBank.Solutions.Primitives.PIX.Request.Cobranca;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using EBank.Solutions.Primitives.PIX.Response;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;
using Unimake.Primitives.Collections.Page;

namespace Unimake.EBank.Solutions.Services.PIX
{
    /// <summary>
    /// Serviços PIX
    /// </summary>
    public class PIXService
    {
        #region Public Methods

        /// <summary>
        /// Cria uma cobrança PIX<br/>
        /// Se informado o parâmetro <see cref="PIXCobrancaCreateRequest.TxId"/>
        /// será gerado uma cobrança com o identificador de transação informado. pattern: [a-zA-Z0-9]{26,35}
        /// </summary>
        /// <param name="request">Requisição de cobrança PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PIXCobrancaResponse> CreateCobAsync(PIXCobrancaCreateRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "pix/Cobranca").PostAsync<PIXCobrancaResponse>(request);

        /// <summary>
        /// Faz uma pesquisa pela chave de transação do PIX
        /// </summary>
        /// <param name="request">Dados da requisição PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PagedList<PIXItem>> GetAsync(PIXGetRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "pix/consultar").PostAsync<PagedList<PIXItem>>(request);

        /// <summary>
        /// Consulta uma cobrança PIX<br/>
        /// É obrigatório informar o <see cref="PIXCobrancaGetRequest.TxId"/> o mesmo que foi criado em <see cref="CreateCobAsync(PIXCobrancaCreateRequest, AuthenticatedScope)"/>
        /// </summary>
        /// <param name="request">Requisição de cobrança PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PIXCobrancaResponse> QueryCobAsync(PIXCobrancaGetRequest request, AuthenticatedScope authenticatedScope)
        {
            var client = new APIClient(authenticatedScope, "pix/Cobranca")
            {
                QueryString =
                {
                    { nameof(request.TxId), request.TxId },
                    { nameof(request.GerarQRCode), request.GerarQRCode },
                    { nameof(request.Testing), request.Testing }
                }
            };

            if(string.IsNullOrWhiteSpace(request.ConfigurationId))
            {
                client.QueryString.AddOrUpdateValue(nameof(request.NumeroAgencia), request.NumeroAgencia);
                client.QueryString.AddOrUpdateValue(nameof(request.NumeroConta), request.NumeroConta);
                client.QueryString.AddOrUpdateValue(nameof(request.Banco), (int)request.Banco);
                client.QueryString.AddOrUpdateValue(nameof(request.Inscricao), request.Inscricao);
            }

            return await client.GetAsync<PIXCobrancaResponse>(request);
        }

        #endregion Public Methods
    }
}