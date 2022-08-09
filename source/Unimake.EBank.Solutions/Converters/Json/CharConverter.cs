using Newtonsoft.Json;
using System;

namespace Unimake.EBank.Solutions.Converters.Json
{
    /// <summary>
    /// Conversor para tipo char
    /// </summary>
    public class CharConverter : JsonConverter
    {
        #region Public Methods

        /// <summary>
        /// Se o tipo for char, retorna verdadeiro
        /// </summary>
        /// <param name="objectType">Tipo de objeto</param>
        /// <returns></returns>
        public override bool CanConvert(Type objectType) => objectType == typeof(char);

        /// <summary>
        /// Faz a leitura do json e retorna
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="objectType"></param>
        /// <param name="existingValue"></param>
        /// <param name="serializer"></param>
        /// <returns></returns>
        /// <exception cref="NotImplementedException"></exception>
        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            var result = reader.Value?.ToString() ?? "";

            if(string.IsNullOrWhiteSpace(result))
            {
                return default(char);
            }

            return result[0];
        }

        /// <summary>
        /// Escreve o valor do JSON
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value"></param>
        /// <param name="serializer"></param>
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer) => writer.WriteValue(value);

        #endregion Public Methods
    }
}