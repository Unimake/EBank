using Unimake.EBank.Solutions.Model.Cobranca;
using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Cobranca.Request;

namespace Unimake.EBank.Solutions.Services.Cobranca
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}"/>
    /// </summary>
    public class CobrancaService : FileServiceBase<CobrancaRequest, ItemCobranca, ItemCobrancaJson, ItemCobrancaCNAB>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.Path"/>
        /// </summary>
        protected override string Path => "Cobranca";

        #endregion Protected Properties
    }
}