using Newtonsoft.Json;
using System.Collections.Generic;
using System.Linq;
using Unimake.EBank.Solutions.Converters.Json;

namespace Unimake.EBank.Solutions.Services.Billet
{
    /// <summary>
    /// Erros que foram lançados ao emitir o Billet
    /// </summary>
    public class RegisterErrorResponse
    {
        #region Private Fields

        private IEnumerable<KeyValuePair<string, List<string>>> _errors;

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Detalhes do erro ocorrido.
        /// </summary>
        [JsonProperty("detail")]
        public string Detail { get; set; }

        /// <summary>
        /// Mensagens de erros. Normalmente é gerada com "nome do campo:mensagem de erro".
        /// <para>A validação na emissão de Billets considera todos os campos</para>
        /// <para>Se não foi retornado os erros, será exibido a mensagem da propriedade Details</para>
        /// </summary>
        [JsonProperty("errors")]
        [JsonConverter(typeof(ErrorsResponseConverter))]
        public IEnumerable<KeyValuePair<string, List<string>>> Errors
        {
            get
            {
                if(_errors?.Any() ?? false)
                {
                    foreach(var kvp in _errors)
                    {
                        yield return kvp;
                    }

                    yield break;
                }

                if(!string.IsNullOrEmpty(Detail))
                {
                    yield return new KeyValuePair<string, List<string>>(nameof(Detail), new List<string> { Detail });
                    yield break;
                }

                //avoid null exception
                yield return new KeyValuePair<string, List<string>>();
                yield break;
            }
            set => _errors = value;
        }

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