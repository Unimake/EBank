using System;
using System.Collections.Generic;
using System.Linq;
using static System.Console;

namespace Unimake.EBank.Solutions.QueryExample
{
    public class MessageCenter
    {
        #region Private Constructors

        private MessageCenter()
        {
        }

        #endregion Private Constructors

        #region Private Methods

        private static void WriteLine(string line)
        {
            var size = 81 - line.Length;
            Console.WriteLine($"| {line}{"".PadLeft(size)}|");
        }

        private static void WriteTitle(string title) => WriteLine($"        {title}");

        #endregion Private Methods

        #region Internal Methods

        internal static void Log(string log)
        {
            var oldColor = ForegroundColor;
            ForegroundColor = ConsoleColor.Blue;
            Console.WriteLine(log);
            ForegroundColor = oldColor;
        }

        #endregion Internal Methods

        #region Public Methods

        public static void Alert(string message, string title = "Aviso!")
        {
            var oldColor = ForegroundColor;
            ForegroundColor = ConsoleColor.DarkYellow;

            DrawLine();
            WriteTitle(title);

            ForegroundColor = ConsoleColor.Yellow;

            foreach(var item in Split(message, 80))
            {
                WriteLine(item);
            }

            DrawLine();
            NewLine();

            ForegroundColor = oldColor;
        }

        public static void DrawLine() => Console.WriteLine("_".PadLeft(84, '_'));

        public static void Error(Exception ex)
        {
            var oldColor = ForegroundColor;
            ForegroundColor = ConsoleColor.DarkRed;
            DrawLine();
            WriteTitle("ERROR!!!");
            ForegroundColor = ConsoleColor.Red;
            var message = ex.GetAllMessages();

            foreach(var item in Split(message, 80))
            {
                WriteLine(item);
            }

            DrawLine();
            NewLine();

            ForegroundColor = oldColor;
        }

        public static void NewLine() => Console.WriteLine();

        public static IEnumerable<string> Split(string str, int chunkSize) =>
            str.Length <= chunkSize
                ? (new string[] { str })
                : Enumerable.Range(0, str.Length / chunkSize)
                         .Select(i => str.Substring(i * chunkSize, chunkSize));

        #endregion Public Methods
    }
}