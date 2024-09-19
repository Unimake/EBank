using Unimake.EBank.Solutions.Converters;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Converters
{
    public class DigitableLineToBarCodeConverterTest : TestBase
    {
        #region Public Constructors

        public DigitableLineToBarCodeConverterTest(ITestOutputHelper output) : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        // Este teste foi realizado com boletos reais, foram removidos para não infringir a LGPD
        // [Theory]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // [InlineData("", "")]
        // public void Convert(string digitableLine, string expectedBarCode)
        // {
        //     var codeBar = DigitableLineToBarCodeConverter.ToBarCode(digitableLine);
        //     Assert.Equal(expectedBarCode, codeBar);
        // }

        #endregion Public Methods
    }
}