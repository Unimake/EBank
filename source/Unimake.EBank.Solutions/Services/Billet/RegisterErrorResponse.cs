using Newtonsoft.Json;
using Unimake.EBank.Solutions.Converters.Json;

namespace Unimake.EBank.Solutions.Services.Billet
{
    /// <summary>
    /// Erros que foram lançados ao emitir o Billet
    /// </summary>
    public class RegisterErrorResponse
    {
        #region Public Properties

        /// <summary>
        /// Mensagens de erros. Normalmente é gerada com "nome do campo:mensagem de erro".
        /// <para>A validação na emissão de Billets considera todos os campos</para>
        /// </summary>
        [JsonProperty("errors")]
        [JsonConverter(typeof(ErrorsResponseConverter))]
        public string Errors { get; set; }

        /// <summary>
        /// Estado da requisição
        /// </summary>
        [JsonProperty("status")]
        public string Status { get; set; }

        /// <summary>
        /// Título principal do erro na requisição
        /// </summary>
        [JsonProperty("title")]
        public string Title { get; set; }

        /// <summary>
        /// Identificador de rastreabilidade do erro. É gerado um novo a cada erro.
        /// </summary>
        [JsonProperty("traceId")]
        public string TraceId { get; set; }

        /// <summary>
        /// Tipo do erro.
        /// </summary>
        [JsonProperty("type")]
        public string Type { get; set; }

        #endregion Public Properties
    }
}