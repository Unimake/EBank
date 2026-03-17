using EBank.Solutions.Primitives.CustomTypes;
using Newtonsoft.Json;
using System;
using System.Globalization;
using System.Text.Json;
using System.Text.Json.Serialization;
using Xunit;
using STJJsonException = System.Text.Json.JsonException;
using STJJsonSerializer = System.Text.Json.JsonSerializer;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class Bug181897
    {
        #region Private Classes

        private sealed class BankDatePayload
        {
            #region Public Properties

            [JsonProperty("date")]
            [JsonPropertyName("date")]
            public BankDate Date { get; set; }

            #endregion Public Properties
        }

        private sealed class NewtonsoftBankDateJsonConverter : Newtonsoft.Json.JsonConverter<BankDate>
        {
            #region Public Methods

            public override BankDate ReadJson(JsonReader reader, Type objectType, BankDate existingValue, bool hasExistingValue, Newtonsoft.Json.JsonSerializer serializer)
            {
                if(reader.TokenType == JsonToken.Null)
                {
                    return default;
                }

                if(reader.TokenType == JsonToken.String)
                {
                    var str = (string)reader.Value;

                    if(string.IsNullOrWhiteSpace(str))
                    {
                        return default;
                    }

                    if(DateTimeOffset.TryParse(str, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind, out var dto))
                    {
                        return new BankDate(dto.DateTime);
                    }

                    if(DateTime.TryParse(str, CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt))
                    {
                        return new BankDate(dt);
                    }

                    throw new JsonSerializationException($"Invalid bank date: {str}");
                }

                if(reader.TokenType == JsonToken.Integer)
                {
                    var unix = Convert.ToInt64(reader.Value, CultureInfo.InvariantCulture);
                    var dt = DateTimeOffset.FromUnixTimeMilliseconds(unix).DateTime;
                    return new BankDate(dt);
                }

                throw new JsonSerializationException($"Invalid token for BankDate: {reader.TokenType}");
            }

            public override void WriteJson(JsonWriter writer, BankDate value, Newtonsoft.Json.JsonSerializer serializer) => writer.WriteValue(value.ToString());

            #endregion Public Methods
        }

        #endregion Private Classes

        #region Public Methods

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_desserializar_bankdate_com_system_text_json_a_partir_de_string()
        {
            const string json = """
                            { "date": "2025-03-25T18:40:00-03:00" }
                            """;

            var payload = STJJsonSerializer.Deserialize<BankDatePayload>(json);

            Assert.NotNull(payload);
            Assert.Equal(new DateTime(2025, 3, 25), payload!.Date.ToDateTime());
            Assert.Equal(DateTimeKind.Unspecified, payload.Date.ToDateTime().Kind);
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_desserializar_bankdate_com_system_text_json_a_partir_de_unix_time_ms()
        {
            var unixTimeMs = new DateTimeOffset(2025, 3, 26, 1, 40, 0, TimeSpan.Zero).ToUnixTimeMilliseconds();
            var json = $"{{ \"date\": {unixTimeMs} }}";

            var payload = STJJsonSerializer.Deserialize<BankDatePayload>(json);

            Assert.NotNull(payload);
            Assert.Equal(new DateTime(2025, 3, 26), payload!.Date.ToDateTime());
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_lancar_erro_quando_system_text_json_receber_data_invalida()
        {
            const string json = """
                            { "date": "31/31/2025" }
                            """;

            _ = Assert.Throws<STJJsonException>(() => STJJsonSerializer.Deserialize<BankDatePayload>(json));
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_normalizar_hora_e_kind_ao_criar_bankdate()
        {
            var source = new DateTime(2025, 8, 17, 14, 33, 45, DateTimeKind.Utc);

            var bankDate = new BankDate(source);
            var result = bankDate.ToDateTime();

            Assert.Equal(new DateTime(2025, 8, 17), result);
            Assert.Equal(DateTimeKind.Unspecified, result.Kind);
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_realizar_conversao_implicita_de_e_para_datetime()
        {
            DateTime source = new(2026, 1, 2, 22, 11, 10, DateTimeKind.Local);

            BankDate bankDate = source;
            DateTime roundtrip = bankDate;

            Assert.Equal(new DateTime(2026, 1, 2), roundtrip);
            Assert.Equal(DateTimeKind.Unspecified, roundtrip.Kind);
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_serializar_bankdate_com_system_text_json_no_formato_iso()
        {
            var payload = new BankDatePayload
            {
                Date = new DateTime(2025, 12, 31, 23, 59, 59, DateTimeKind.Utc)
            };

            var json = STJJsonSerializer.Serialize(payload);

            Assert.Contains("\"date\":\"2025-12-31\"", json, StringComparison.Ordinal);
        }

        [Trait("BugFix", "#181897")]
        [Fact]
        public void Deve_serializar_e_desserializar_bankdate_com_newtonsoft_json()
        {
            var settings = new JsonSerializerSettings();
            settings.Converters.Add(new NewtonsoftBankDateJsonConverter());

            var payload = new BankDatePayload
            {
                Date = new DateTime(2026, 6, 15, 8, 25, 10, DateTimeKind.Local)
            };

            var json = JsonConvert.SerializeObject(payload, settings);
            var result = JsonConvert.DeserializeObject<BankDatePayload>(json, settings);

            Assert.Contains("\"date\":\"2026-06-15\"", json, StringComparison.Ordinal);
            Assert.NotNull(result);
            Assert.Equal(new DateTime(2026, 6, 15), result!.Date.ToDateTime());
            Assert.Equal(DateTimeKind.Unspecified, result.Date.ToDateTime().Kind);
        }

        #endregion Public Methods
    }
}