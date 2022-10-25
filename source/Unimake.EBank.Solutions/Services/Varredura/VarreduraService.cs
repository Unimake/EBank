using Newtonsoft.Json;
using Unimake.EBank.Solutions.Converters.Json;
using Unimake.EBank.Solutions.Model.Varredura;
using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Varredura.Request;

namespace Unimake.EBank.Solutions.Services.Varredura
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}"/>
    /// </summary>
    public class VarreduraService : FileServiceBase<VarreduraRequest, ItemVarredura, ItemVarreduraJson, ItemVarreduraCNAB>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.Path"/>
        /// </summary>
        protected override string Path => "Varredura";

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.GetConverter"/>
        /// </summary>
        /// <returns></returns>
        protected override JsonConverter GetConverter() => new VarreduraConverter();

        #endregion Protected Methods
    }
}