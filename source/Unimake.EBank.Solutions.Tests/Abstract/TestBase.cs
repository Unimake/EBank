using Newtonsoft.Json;
using Xunit.Abstractions;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Tests.Abstract
{
    public abstract class TestBase
    {
        #region Private Fields

        private readonly ITestOutputHelper output;
        private JsonSerializerSettings _jsonSettings;

        #endregion Private Fields

        #region Private Properties

        private JsonSerializerSettings JsonSettings => _jsonSettings ??= _jsonSettings = new JsonSerializerSettings
        {
            Formatting = Formatting.Indented,
            MissingMemberHandling = MissingMemberHandling.Ignore,
            NullValueHandling = NullValueHandling.Include,
            ReferenceLoopHandling = ReferenceLoopHandling.Ignore
        };

        #endregion Private Properties

        #region Public Constructors

        public TestBase(ITestOutputHelper output) => this.output = output;

        #endregion Public Constructors

        #region Public Methods

        public void DumpAsJson(object value) =>
            output.WriteLine(SerializeObject(value, JsonSettings));

        #endregion Public Methods
    }
}