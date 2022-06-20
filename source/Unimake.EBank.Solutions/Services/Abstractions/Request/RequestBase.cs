using Newtonsoft.Json;

namespace Unimake.EBank.Solutions.Services.Abstractions.Request
{
    /// <summary>
    /// <inheritdoc cref="Contract.IRequest"/>
    /// </summary>
    public abstract class RequestBase : Contract.IRequest
    {
        #region Public Properties

        /// <summary>
        /// <inheritdoc cref="Contract.IRequest.Testing"/>
        /// </summary>
        [JsonProperty("testing")]
        public bool Testing { get; set; }

        #endregion Public Properties
    }
}