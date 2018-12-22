using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace AkkaRoslynAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class AkkaRoslynAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "AkkaRoslynAnalyzer";
        
        private const string PropsClassName = "Props";
        private const string CreateFunctionName = "Create";

        private static readonly LocalizableString Title = 
            new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat =
            new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = 
            new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Argument List";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context) 
            => context.RegisterSyntaxNodeAction(Action, SyntaxKind.InvocationExpression);

        private void Action(SyntaxNodeAnalysisContext analysisContext)
        {
            if (!analysisContext.Node.GetFirstToken().Text.Equals(PropsClassName)) return;

            if (!(analysisContext.Node is InvocationExpressionSyntax expressionSyntax)) return;

            var nodes = expressionSyntax.DescendantNodes().ToList();
            
            var identifiers = nodes.OfType<IdentifierNameSyntax>()
                                   .ToList();
            
            // check whether generic function call or not
            // in case of invocation like Props.Create<MyActor>() second IdentifierNameSyntax will be Create
            var result = identifiers[1].Identifier.Text.Equals(CreateFunctionName)
                         ? HandleNonGenericFunctionInvoke(expressionSyntax, nodes, analysisContext.SemanticModel)
                         : HandleGenericFunctionInvoke(expressionSyntax, nodes, analysisContext.SemanticModel);
            
            analysisContext.ReportDiagnostic(result);
        }

        // var pr = Props.Create<MyActorWithParam>();
        private Diagnostic HandleGenericFunctionInvoke(InvocationExpressionSyntax expressionSyntax,
                                                       List<SyntaxNode> nodes,
                                                       SemanticModel semanticModel)
        {
            var argList = GetArgumentList(expressionSyntax, semanticModel);
            
            var actorType = nodes.OfType<GenericNameSyntax>()
                                 .FirstOrDefault()
                                 ?.TypeArgumentList.Arguments
                                 .FirstOrDefault();
            
            var typeInfo = semanticModel.GetTypeInfo(actorType);
            if (typeInfo.Type.TypeKind == TypeKind.TypeParameter) return null;
            
            var constructors = GetConstructors(typeInfo);
            
            return constructors.Any(x => x.EqualToArgList(argList))
                   ? null
                   : Diagnostic.Create(Rule, expressionSyntax.GetLocation());
        }
        
        // Props.Create(typeof(MyActorWithParam), 1, typeof(MyActorWithParam));
        private Diagnostic HandleNonGenericFunctionInvoke(InvocationExpressionSyntax expressionSyntax, 
                                                          List<SyntaxNode> nodes,
                                                          SemanticModel semanticModel)
        {
            var argList = GetArgumentList(expressionSyntax, semanticModel).Skip(1).ToArray();

            // we cant detect the Actor type if not typeof used
            if (!(nodes.OfType<ArgumentSyntax>()
                       .FirstOrDefault()
                       ?.DescendantNodes()
                       .First() is TypeOfExpressionSyntax))
                return null;
            
            var actorType = nodes.OfType<IdentifierNameSyntax>()
                                 .Skip(2)
                                 .FirstOrDefault();
            
            var constructors = GetConstructors(semanticModel.GetTypeInfo(actorType));
            
            return constructors.Any(x => x.EqualToArgList(argList))
                   ? null
                   : Diagnostic.Create(Rule, expressionSyntax.GetLocation());
        }
        
        private ITypeSymbol[] GetArgumentList(InvocationExpressionSyntax invocationExpression, SemanticModel model)
            => invocationExpression.ArgumentList.Arguments
                .Select(syntax => model.GetTypeInfo(syntax.Expression).Type)
                .Where(x => x != null)
                .ToArray();

        private List<ConstructorInfo> GetConstructors(TypeInfo typeInfo)
        {
            var result = new List<ConstructorInfo>();
            if (typeInfo.Type is INamedTypeSymbol type)
            {
                var ctors = type.Constructors;
                foreach (var methodSymbol in ctors)
                {
                    var @params = methodSymbol.Parameters.Select(symbol => symbol.Type).ToList();
                    result.Add(new ConstructorInfo(@params));
                }
            }

            return result;
        }
    }
}
