using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Converters
{
    public class DigitableLineToBarCodeConverterTest(ITestOutputHelper output) : TestBase(output)
    {

        #region Public Constructors

        #endregion Public Constructors

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
    }
}