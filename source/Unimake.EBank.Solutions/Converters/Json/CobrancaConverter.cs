using EBank.Solutions.Primitives.Enumerations.CNAB;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;

namespace Unimake.EBank.Solutions.Converters.Json
{
    /// <summary>
    /// Conversor para os tipos interface o E-Bank
    /// </summary>
    public sealed class CobrancaConverter : JsonConverter
    {
        #region Public Properties

        /// <summary>
        /// Retorna falso, pois este conversor não é utilizado para escrita
        /// </summary>
        public override bool CanWrite => false;

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Se verdadeiro, pode converter entre os tipos
        /// </summary>
        /// <param name="objectType"></param>
        /// <returns></returns>
        public override bool CanConvert(Type objectType) =>
            objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Contract.ILote) ||
            objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Contract.IRegistroDetalhe) ||
            objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Contract.IRegistro) ||
            objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB400.Contract.IRegistro);

        /// <summary>
        ///
        /// </summary>
        /// <param name="reader"></param>
        /// <param name="objectType"></param>
        /// <param name="existingValue"></param>
        /// <param name="serializer"></param>
        /// <returns></returns>
        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            var jObject = JObject.Load(reader);
            var type = default(Type);

            if(objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Contract.ILote))
            {
                type = typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Cobranca.LoteDeTitulosEmCobranca);
            }
            else if(objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB240.Contract.IRegistroDetalhe))
            {
                type = Type.GetType($"EBank.Solutions.Primitives.CNAB.CNAB240.Cobranca.Registro.DetalheSegmento{jObject.SelectToken("Servico.Segmento")}, EBank.Solutions.Primitives");
            }
            else if(objectType == typeof(global::EBank.Solutions.Primitives.CNAB.CNAB400.Contract.IRegistro))
            {
                switch(UConvert.ToAny<TipoDeRegistro400>(jObject.GetValue("Tipo")))
                {
                    case TipoDeRegistro400.Detalhe1OuTransacao1:
                        type = typeof(global::EBank.Solutions.Primitives.CNAB.CNAB400.Cobranca.Retorno.Detalhe1);
                        break;

                    case TipoDeRegistro400.Detalhe4:
                        type = typeof(global::EBank.Solutions.Primitives.CNAB.CNAB400.Cobranca.Retorno.Detalhe4);
                        break;

                    case TipoDeRegistro400.Detalhe5CobrancaEmailOuDadosSacadorAvalista:
                    case TipoDeRegistro400.Trailer:
                    case TipoDeRegistro400.Detalhe2ComplementoMulta:
                    case TipoDeRegistro400.Header:
                    default:
                        break;
                }
            }

            if(type == null)
            {
                throw new TypeLoadException($"Não foi possível converter o tipo '{objectType.FullName}'.");
            }

            var value = jObject.ToString();
            var result = JsonConvert.DeserializeObject(value, type, new JsonSerializerSettings
            {
                NullValueHandling = NullValueHandling.Ignore,
                Converters = new List<JsonConverter>
                {
                    new CobrancaConverter(),
                    new CharConverter()
                }
            });
            return result;
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="writer"></param>
        /// <param name="value"></param>
        /// <param name="serializer"></param>
        /// <exception cref="NotImplementedException"></exception>
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer) => throw new NotImplementedException();

        #endregion Public Methods
    }
}