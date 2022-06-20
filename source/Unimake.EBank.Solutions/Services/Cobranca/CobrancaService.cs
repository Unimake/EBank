using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Cobranca.Request;
using Unimake.EBank.Solutions.Services.Cobranca.Response;

namespace Unimake.EBank.Solutions.Services.Cobranca
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}"/>
    /// </summary>
    public class CobrancaService : FileServiceBase<CobrancaRequest, CobrancaResponse, ArquivoCobrancaResponse>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}.Path"/>
        /// </summary>
        protected override string Path => "Cobranca";

        #endregion Protected Properties
    }
}