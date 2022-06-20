using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace Unimake.EBank.Solutions.Resolver
{
    internal static class AssemblyResolver
    {
        #region Private Methods

        private static Assembly AssemblyResolve(string path)
        {
            try
            {
                var fileInfo = new FileInfo(path);
                return fileInfo.Exists ? Assembly.LoadFile(fileInfo.FullName) : null;
            }
            catch { }

            return default;
        }

        private static string GetCodeBaseDirectory()
        {
            var result = Assembly.GetExecutingAssembly().Location;

            if(string.IsNullOrWhiteSpace(result))
            {
                return "";
            }

            var uri = new UriBuilder(result);
            result = Uri.UnescapeDataString(uri.Path);
            result = Path.GetDirectoryName(result);

            return result;
        }

        #endregion Private Methods

        #region Public Methods

        public static Assembly AssemblyResolve(object sender, ResolveEventArgs args)
        {
            try
            {
                var parts = args?.Name?.Split(',');

                if(parts.IsNullOrEmpty())
                {
                    return default;
                }

                var name = parts.FirstOrDefault();

                //by RelativeSearchPath
                var searchPath = AppDomain.CurrentDomain.RelativeSearchPath ?? "";
                var result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }

                //by codebase
                searchPath = GetCodeBaseDirectory();
                result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }

                //and  finally, by current directory
                searchPath = Directory.GetCurrentDirectory();
                result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }
            }
            catch { }

            return default;
        }

        #endregion Public Methods
    }
}