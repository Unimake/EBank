using EBank.Solutions.Primitives.Enumerations;

namespace eBankTest
{
    internal class Prop 
    {
        //Ambiente de homologação do boleto, só boleto tem.
        public static string TesteBoletoAppId = "11111111111111111111111111111111";
        public static string TesteBoletoSecret = "11111111111111111111111111111111";

        //Rudolf - Sicoob
        public static string RudolfAppId = "11111111111111111111111111111111";
        public static string RudolfSecret = "11111111111111111111111111111111";
        public static string RudolfAccountNumber = "11111";
        public static Banco RudolfBanco = Banco.Sicoob;

        //Unimake - Sicredi
        public static string UnimakeAppId = "11111111111111111111111111111111";
        public static string UnimakeSecret = "11111111111111111111111111111111";
        public static string UnimakeAccountNumber = "111111";
        public static Banco UnimakeBanco = Banco.Sicredi;

        //Protecoat - Itaú
        public static string ProtecoatAppId = "";
        public static string ProtecoatSecret = "";
        public static string ProtecoatAccountNumber = "";
        public static Banco ProtecoatBanco = Banco.Itau;

        //Dream Solutions
        public static string DreamAppId = "11111111111111111111111111111111";
        public static string DreamSecret = "11111111111111111111111111111111";
        public static string DreamAccountNumber = "1111111";
        public static Banco DreamBanco = Banco.Sicoob;

        //Whats Dream Solutions
        public static string DreamWhatsAppId = "11111111111111111111111111111111";
        public static string DreamWhatsSecret = "11111111111111111111111111111111";
        public static string DreamWhatsPublicKey = "1231I123Q123L123T123Z213W123c12312g31293123=";
    }
}
