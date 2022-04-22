using EBank.Solutions.Primitives.Billet.Models;
using Newtonsoft.Json;
using Unimake.EBank.Solutions.Services.Billet.Contract;

namespace Unimake.EBank.Solutions.Services.Billet.Abstract
{
    /// <summary>
    /// <inheritdoc cref="IRequest"/>
    /// </summary>
    public abstract class RequestBase : IRequest
    {
        #region Public Properties

        /// <summary>
        /// <inheritdoc cref="IRequest.Beneficiario"/>
        /// </summary>
        [JsonProperty("beneficiario")]
        public Beneficiario Beneficiario { get; set; }

        /// <summary>
        /// <inheritdoc cref="Services.Contract.IRequest.Testing"/>
        /// </summary>
        [JsonProperty("testing")]
        public bool Testing { get; set; }

        #endregion Public Properties
    }
}