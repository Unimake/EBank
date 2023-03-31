using EBank.Solutions.Primitives.Exceptions;
using EBank.Solutions.Primitives.PIX.Models.Consulta;
using EBank.Solutions.Primitives.PIX.Request.Cobranca;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using EBank.Solutions.Primitives.PIX.Response;
using Newtonsoft.Json;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.Primitives.Collections.Page;

namespace Unimake.EBank.Solutions.Services.PIX
{
    /// <summary>
    /// Serviços PIX
    /// </summary>
    public class PIXService
    {
        #region Private Methods

        /// <summary>
        /// Prepara a resposta e retorna.
        /// </summary>
        /// <typeparam name="T">Tipo de resultado</typeparam>
        /// <param name="response">Resposta recebida do servidor</param>
        /// <returns></returns>
        /// <exception cref="ResponseException">Exceção lançada caso ocorra erro no servidor</exception>
        private async Task<T> PrepareResponseAsync<T>(System.Net.Http.HttpResponseMessage response)
        {
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return JsonConvert.DeserializeObject<T>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                });
            }

            var errors = ExceptionObject.FromJson(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);
            throw new ResponseException(errors.Message, (int)response.StatusCode);
        }

        #endregion Private Methods

        #region Public Methods

        /// <summary>
        /// Cria uma cobrança PIX<br/>
        /// Se informado o parâmetro <see cref="PIXCobrancaCreateRequest.TxId"/>
        /// será gerado uma cobrança com o identificador de transação informado. pattern: [a-zA-Z0-9]{26,35}
        /// </summary>
        /// <param name="request">Requisição de cobrança PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PIXCobrancaResponse> CreateCobAsync(PIXCobrancaCreateRequest request, AuthenticatedScope authenticatedScope)
        {
            var client = new APIClient(authenticatedScope, "pix/Cobranca");
            var response = await client.PostAsync(request);
            return await PrepareResponseAsync<PIXCobrancaResponse>(response);
        }

        /// <summary>
        /// Faz uma pesquisa pela chave de transação do PIX
        /// </summary>
        /// <param name="request">Dados da requisição PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PagedList<PIXItem>> GetAsync(PIXGetRequest request, AuthenticatedScope authenticatedScope)
        {
            var client = new APIClient(authenticatedScope, "pix/consultar");
            var response = await client.PostAsync(request);
            return await PrepareResponseAsync<PagedList<PIXItem>>(response);
        }

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
                    { nameof(request.NumeroAgencia), request.NumeroAgencia },
                    { nameof(request.NumeroConta), request.NumeroConta },
                    { nameof(request.Banco), (int)request.Banco },
                    { nameof(request.TxId), request.TxId },
                    { nameof(request.Inscricao), request.Inscricao },
                    { nameof(request.GerarQRCode), request.GerarQRCode },
                    { nameof(request.Testing), request.Testing }
                }
            };

            var response = await client.GetAsync();
            return await PrepareResponseAsync<PIXCobrancaResponse>(response);
        }

        #endregion Public Methods
    }
}