using EBank.Solutions.Primitives.PDF.Request;
using EBank.Solutions.Primitives.PDF.Response;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;

namespace Unimake.EBank.Solutions.Services.PDF
{
    /// <summary>
    /// Serviço para operações relacionadas a PDFs.
    /// </summary>
    /// <remarks>
    /// Esta classe fornece métodos assíncronos que invocam endpoints remotos através do <see cref="APIClient"/>.
    /// Cada método aceita um objeto de requisição e um escopo autenticado, e retorna um <see cref="PDFResponse"/>.
    /// </remarks>
    public class PDFService
    {
        #region Public Methods

        /// <summary>
        /// Obtém um PDF codificado em Base64.
        /// </summary>
        /// <param name="request">Objeto com os parâmetros necessários para gerar o PDF em Base64.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>
        /// Uma tarefa que representa a operação assíncrona. O resultado contém um <see cref="PDFResponse"/>
        /// com a representação Base64 do PDF e informações adicionais.
        /// </returns>
        public async Task<PDFResponse> GetAsBase64Async(PDFRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "pdf/GetBase64").PostAsync<PDFResponse>(request);

        /// <summary>
        /// Obtém um PDF como arquivo binário.
        /// </summary>
        /// <param name="request">Objeto com os parâmetros necessários para gerar o PDF.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>
        /// Uma tarefa que representa a operação assíncrona. O resultado contém um <see cref="PDFResponse"/>
        /// com os dados do arquivo e informações adicionais.
        /// </returns>
        public async Task<byte[]> GetAsFileAsync(PDFRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "pdf/GetAsFile").PostAsync<byte[]>(request);

        /// <summary>
        /// Obtém um PDF convertido para HTML.
        /// </summary>
        /// <param name="request">Objeto com os parâmetros necessários para gerar o HTML a partir do PDF.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>
        /// Uma tarefa que representa a operação assíncrona. O resultado contém um <see cref="PDFResponse"/>
        /// com o HTML gerado e metadados relacionados.
        /// </returns>
        public async Task<string> GetAsHtmlAsync(PDFRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "pdf/GetAsHtml").PostAsync<string>(request);

        #endregion Public Methods
    }
}