using EBank.Solutions.Primitives.Billet.Models;
using Newtonsoft.Json;
using Unimake.EBank.Solutions.Services.Billet.Contract;

namespace Unimake.EBank.Solutions.Services.Billet.Abstractions.Request
{
    /// <summary>
    /// <inheritdoc cref="IRequest"/>
    /// </summary>
    public abstract class RequestBase : Services.Abstractions.Request.RequestBase, IRequest
    {
        #region Public Properties

        /// <summary>
        /// <inheritdoc cref="IRequest.Beneficiario"/>
        /// </summary>
        [JsonProperty("beneficiario")]
        public Beneficiario Beneficiario { get; set; }

        #endregion Public Properties
    }
}