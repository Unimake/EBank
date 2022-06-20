using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Extrato.Request;
using Unimake.EBank.Solutions.Services.Extrato.Response;

namespace Unimake.EBank.Solutions.Services.Extrato
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}"/>
    /// </summary>
    public class ExtratoService : FileServiceBase<ExtratoRequest, ExtratoResponse, ArquivoExtratoResponse>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}.Path"/>
        /// </summary>
        protected override string Path => "Extrato";

        #endregion Protected Properties
    }
}