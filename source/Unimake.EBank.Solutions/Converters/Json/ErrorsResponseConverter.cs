using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Unimake.EBank.Solutions.Converters.Json
{
    /// <summary>
    /// O E-bank retorna os erros em um formato de lista, pois a validação do Billet, valida todos os campos e retorna quais estão com problemas.
    /// <para>este conversor, transforma em uma string com o nome do campo e a mensagem do erro.</para>
    /// </summary>
    public class ErrorsResponseConverter : JsonConverter
    {
        #region Public Methods

        /// <summary>
        /// Sempre verdadeiro, assume que tudo pode ser convertido, uma vez que o desenvolvedor decorou a propriedade com este conversor.
        /// </summary>
        /// <param name="objectType">Tipo de objeto que está sendo recebido</param>
        /// <returns></returns>
        public override bool CanConvert(Type objectType)
        {
            return true;
        }

        /// <summary>
        /// Faz a leitura do JSON e retorna a mensagem no formato campo:mensagem de erro
        /// </summary>
        /// <param name="reader">Leitor</param>
        /// <param name="objectType">tipo de objeto</param>
        /// <param name="existingValue">Valor existente, pode ser nulo</param>
        /// <param name="serializer">Serializador que está sendo usando pelo JSON</param>
        /// <returns></returns>
        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            var result = new Dictionary<string, List<string>>();
            var jObject = JObject.Load(reader);

            foreach(var item in jObject)
            {
                result.Add(item.Key, item.Value.Select(s => s.ToString()).ToList());
            }

            return result;
        }

        /// <summary>
        /// Escreve o JSON
        /// </summary>
        /// <param name="writer">Escritor do JSON</param>
        /// <param name="value">Valor que deverá ser escrito</param>
        /// <param name="serializer">Serializador que está sendo usando pelo JSON</param>
        /// <exception cref="NotImplementedException">Lançada sempre, este método ainda não é utilizado</exception>
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            throw new NotImplementedException();
        }

        #endregion Public Methods
    }
}