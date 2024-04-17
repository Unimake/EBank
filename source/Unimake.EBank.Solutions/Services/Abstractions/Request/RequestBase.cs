using Newtonsoft.Json;

namespace Unimake.EBank.Solutions.Services.Abstractions.Request
{
    /// <summary>
    /// <inheritdoc cref="Contract.IRequest"/>
    /// </summary>
    public abstract class RequestBase : Contract.IRequest
    {
        #region Public Properties

        /// <inheritdoc cref="Contract.IRequest.ConfigurationId"/>
        [JsonProperty("configurationId")]
        public string ConfigurationId { get; set; }

        /// <inheritdoc cref="Contract.IRequest.Testing"/>
        [JsonProperty("testing")]
        public bool Testing { get; set; }

        #endregion Public Properties
    }
}