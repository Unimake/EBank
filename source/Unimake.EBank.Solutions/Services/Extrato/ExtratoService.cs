using Newtonsoft.Json;
using Unimake.EBank.Solutions.Converters.Json;
using Unimake.EBank.Solutions.Model.Extrato;
using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Extrato.Request;

namespace Unimake.EBank.Solutions.Services.Extrato
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}"/>
    /// </summary>
    public class ExtratoService : FileServiceBase<ExtratoRequest, ItemExtrato, ItemExtratoJson, ItemExtratoCNAB>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.Path"/>
        /// </summary>
        protected override string Path => "Extrato";

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.GetConverter"/>
        /// </summary>
        /// <returns></returns>
        protected override JsonConverter GetConverter() => new ExtratoConverter();

        #endregion Protected Methods
    }
}