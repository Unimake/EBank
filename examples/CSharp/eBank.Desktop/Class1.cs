using Newtonsoft.Json.Linq;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using System.Web;
using Unimake.Cryptography.Enumerations;
using Unimake.Cryptography.Exceptions;
using Unimake.Cryptography.JWT;

namespace EBank.Solutions.Primitives.Security
{
    public abstract class LinkSigner
    {
        public static readonly DateTime UnixEpoch;

        private LinkSigner()
        {
        }

        private static string EnsureURL(string url, IEnumerable<(string Name, object Value)> queryString)
        {
            url = url.TrimEnd('/');
            url = Uri.EscapeUriString(url + NameValueToQueryString(queryString));
            return url;
        }

        private static string NameValueToQueryString(IEnumerable<(string Name, object Value)> values)
        {
            if (values == null)
            {
                return "";
            }

            StringBuilder stringBuilder = new StringBuilder();
            string text = null;
            foreach (var value2 in values)
            {
                string value = HttpUtility.UrlEncode(value2.Value?.ToString() ?? "");
                StringBuilder stringBuilder2 = stringBuilder;
                StringBuilder.AppendInterpolatedStringHandler handler = new StringBuilder.AppendInterpolatedStringHandler(1, 3, stringBuilder2);
                handler.AppendFormatted(text);
                handler.AppendFormatted(value2.Name);
                handler.AppendLiteral("=");
                handler.AppendFormatted(value);
                stringBuilder2.Append(ref handler);
                text = text ?? (text = "&");
            }

            stringBuilder.Insert(0, "?");
            return stringBuilder.ToString();
        }

        static LinkSigner()
        {
            UnixEpoch = DateTime.Parse("1970-01-01T00:00:00");
        }

        public static Dictionary<string, object> Decode(string url, string publicKey, bool verify, string encodedParamenterName = "code")
        {
            string? text = HttpUtility.ParseQueryString(new Uri(url).Query)[encodedParamenterName];
            if (string.IsNullOrWhiteSpace(text))
            {
                throw new ArgumentOutOfRangeException("There is no '" + encodedParamenterName + "' in the QueryString ", (Exception?)null);
            }

            return JsonWebToken.Decode(text, publicKey, verify);
        }

        public static long GetEpoch()
        {
            return (long)(DateTime.UtcNow - UnixEpoch).TotalSeconds;
        }

        public static string SignLink(string issuer, string subject, IEnumerable<(string Key, object Value)> claims, string publicKey, IEnumerable<(string Name, object Value)> queryString = null, string encodedParamenterName = "code")
        {
            return SignLink("", issuer, subject, claims, publicKey, queryString, encodedParamenterName).TrimStart('?');
        }

        public static string SignLink(string url, string issuer, string subject, IEnumerable<(string Key, object Value)> claims, string publicKey, IEnumerable<(string Name, object Value)> queryString = null, string encodedParamenterName = "code", long? iat = null)
        {
            Dictionary<string, object> dictionary = new Dictionary<string, object>();
            dictionary.Add("sub", subject);
            dictionary.Add("iss", issuer);
            dictionary.Add("iat", iat ?? GetEpoch());
            if (claims != null)
            {
                foreach (var claim in claims)
                {
                    dictionary.Add(claim.Key, claim.Value);
                }
            }

            dictionary.Add("path", EnsureURL(url, null));
            dictionary.Add("query", NameValueToQueryString(queryString));
            (string, object)[] source = (queryString ?? new (string, object)[0]).ToArray();
            string item = JsonWebToken.Encode(dictionary, publicKey);
            source = source.Append((encodedParamenterName, item)).ToArray();
            return EnsureURL(url, source) ?? "";
        }

        public static Dictionary<string, object> ValidateAndGetValues(string url, string publicKey, string encodedParamenterName = "code")
        {
            return Decode(url, publicKey, verify: true, encodedParamenterName);
        }

        public static bool ValidateIatExpiration(Dictionary<string, object> decode, ExpirationInterval expirationInterval = ExpirationInterval.Hours, int interval = 48, bool throwError = true)
        {
            if (decode == null)
            {
                throw new ArgumentNullException("decode");
            }

            DateTime unixEpoch = UnixEpoch;
            DateTime dateTime = unixEpoch.AddSeconds(Convert.ToInt64(decode["iat"]));
            TimeSpan timeSpan = DateTime.UtcNow - dateTime;
            (bool, string) tuple = (false, "");
            switch (expirationInterval)
            {
                case ExpirationInterval.Seconds:
                    tuple.Item1 = (double)timeSpan.Ticks / 10000000.0 > (double)interval;
                    tuple.Item2 = "Segundos";
                    break;
                case ExpirationInterval.Minutes:
                    tuple.Item1 = (double)timeSpan.Ticks / 600000000.0 > (double)interval;
                    tuple.Item2 = "Minutos";
                    break;
                case ExpirationInterval.Days:
                    tuple.Item1 = (double)timeSpan.Ticks / 864000000000.0 > (double)interval;
                    tuple.Item2 = "Dias";
                    break;
                default:
                    tuple.Item1 = (double)timeSpan.Ticks / 36000000000.0 > (double)interval;
                    tuple.Item2 = "Horas";
                    break;
            }

            if (tuple.Item1)
            {
                DefaultInterpolatedStringHandler defaultInterpolatedStringHandler = new DefaultInterpolatedStringHandler(52, 2);
                defaultInterpolatedStringHandler.AppendLiteral("O token informado tem mais de ");
                defaultInterpolatedStringHandler.AppendFormatted(interval);
                defaultInterpolatedStringHandler.AppendLiteral(" ");
                defaultInterpolatedStringHandler.AppendFormatted(tuple.Item2);
                defaultInterpolatedStringHandler.AppendLiteral(" e não é mais válido.");
                string message = defaultInterpolatedStringHandler.ToStringAndClear();
                Trace.WriteLine(message);
                if (!throwError)
                {
                    return false;
                }

                throw new SecurityTokenExpiredException(message);
            }

            return true;
        }
    }
}

namespace Unimake.Cryptography.Enumerations
{
    public enum ExpirationInterval
    {
        Seconds,
        Minutes,
        Hours,
        Days
    }
}

namespace Unimake.Cryptography.Exceptions
{
    public sealed class SecurityTokenExpiredException : Exception
    {
        public SecurityTokenExpiredException()
        {
        }

        public SecurityTokenExpiredException(string message)
            : base(message)
        {
        }
    }
}

namespace Unimake.Cryptography.JWT
{
    public abstract class JsonWebToken
    {
        private static Func<byte[], byte[], byte[]> hashAlgorithm;

        private JsonWebToken()
        {
        }

        private static byte[] Base64UrlDecode(string input)
        {
            string text = input;
            text = text.Replace('-', '+');
            text = text.Replace('_', '/');
            switch (text.Length % 4)
            {
                case 2:
                    text += "==";
                    break;
                case 3:
                    text += "=";
                    break;
                default:
                    throw new Exception("Illegal base64url string!");
                case 0:
                    break;
            }

            return Convert.FromBase64String(text);
        }

        private static string Base64UrlEncode(byte[] input)
        {
            return Convert.ToBase64String(input).Split('=')[0].Replace('+', '-').Replace('/', '_');
        }

        static JsonWebToken()
        {
            hashAlgorithm = delegate (byte[] key, byte[] value)
            {
                using HMACSHA512 hMACSHA = new HMACSHA512(key);
                return hMACSHA.ComputeHash(value);
            };
        }

        public static Dictionary<string, object> Decode(string token, string publicKey, bool verify = true)
        {
            string[] array = token.Split('.');
            string text = array[0];
            string text2 = array[1];
            byte[] inArray = Base64UrlDecode(array[2]);
            JObject jObject = JObject.Parse(Encoding.UTF8.GetString(Base64UrlDecode(text)));
            JObject jObject2 = JObject.Parse(Encoding.UTF8.GetString(Base64UrlDecode(text2)));
            byte[] bytes = Encoding.UTF8.GetBytes(text + "." + text2);
            byte[] bytes2 = Encoding.UTF8.GetBytes(publicKey);
            _ = (string?)jObject["alg"];
            byte[] inArray2 = hashAlgorithm(bytes2, bytes);
            string text3 = Convert.ToBase64String(inArray);
            string text4 = Convert.ToBase64String(inArray2);
            if (verify && text3 != text4)
            {
                throw new CryptographicException($"Invalid signature. Expected {text3} got {text4}");
            }

            return jObject2.ToObject<Dictionary<string, object>>();
        }

        public static string Encode(object payload, string publicKey)
        {
            return Encode(payload, Encoding.UTF8.GetBytes(publicKey));
        }

        public static string Encode(object payload, byte[] keyBytes)
        {
            List<string> list = new List<string>();
            var value = new
            {
                alg = "HS512",
                typ = "JWT"
            };
            byte[] bytes = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(value, Formatting.None));
            byte[] bytes2 = Encoding.UTF8.GetBytes(JsonConvert.SerializeObject(payload, Formatting.None));
            list.Add(Base64UrlEncode(bytes));
            list.Add(Base64UrlEncode(bytes2));
            string s = string.Join(".", list.ToArray());
            byte[] bytes3 = Encoding.UTF8.GetBytes(s);
            byte[] input = hashAlgorithm(keyBytes, bytes3);
            list.Add(Base64UrlEncode(input));
            return string.Join(".", list.ToArray());
        }
    }
}