using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace AkkaRoslynAnalyzer
{
    public class ConstructorInfo
    {
        public List<ITypeSymbol> ArgumentList { get; }
        
        public ConstructorInfo(List<ITypeSymbol> argumentList)
        {
            ArgumentList = argumentList;
        }

        public bool EqualToArgList(params ITypeSymbol[] args) => ArgumentList.SequenceEqual(args);
    }
}