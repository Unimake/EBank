using System.Threading.Tasks;

namespace System.Net.Http
{
    /// <summary/>
    public static class HttpResponseMessageExtensions
    {
        #region Public Methods

        /// <summary>
        /// Verifica se a resposta foi executada com sucesso, retorna verdadeiro ou erros.
        /// </summary>
        /// <param name="msg"></param>
        /// <returns></returns>
        /// <exception cref="UnauthorizedAccessException"></exception>
        public static bool IsSuccessStatusCode(this HttpResponseMessage msg)
        {
            if(msg.StatusCode == System.Net.HttpStatusCode.Unauthorized)
            {
                throw new UnauthorizedAccessException("Não foi possível autenticar no servidor. Verifique suas credenciais");
            }

            if(msg.StatusCode == System.Net.HttpStatusCode.Forbidden)
            {
                throw new UnauthorizedAccessException("Você não tem permissão para acessar este recurso. Verifique suas credenciais");
            }

            return msg.IsSuccessStatusCode;
        }

        /// <summary>
        /// Faz a leitura da resposta no formato json
        /// </summary>
        /// <param name="msg"></param>
        /// <returns></returns>
        public static async Task<string> ReadAsJsonAsync(this HttpResponseMessage msg)
        {
            var json = await msg.Content.ReadAsStringAsync();
            return string.IsNullOrWhiteSpace(json) ? $"{{" +
                $"'message':'{msg.ReasonPhrase}'," +
                $"'status':{(int)msg.StatusCode}, " +
                $"'title':'{msg.ReasonPhrase}'," +
                $"'traceId':'NO_TRACE_ID'}}" : json;
        }

        #endregion Public Methods
    }
}