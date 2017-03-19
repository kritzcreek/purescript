module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , flattenReexports
       ) where

import           Protolude hiding ((&), to, from)

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

type Module = (P.ModuleName, [IdeDeclarationAnn])

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions
  :: [Filter]
  -> Matcher IdeDeclarationAnn
  -> [Module]
  -> [Match IdeDeclarationAnn]
getCompletions filters matcher modules =
  runMatcher matcher (completionsFromModules (applyFilters filters modules))

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  completionsFromModules (applyFilters (equalityFilter search : filters) modules)

completionsFromModules :: [Module] -> [Match IdeDeclarationAnn]
completionsFromModules = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, x)) decls

flattenReexports :: [Match IdeDeclarationAnn] -> [Match IdeDeclarationAnn]
flattenReexports initial = fst <$> Map.elems (foldr go Map.empty initial)
  where
    go (Match (moduleName, d@(IdeDeclarationAnn (view annExportedFrom -> Just origin) decl))) =
      Map.alter
      (insertReexport moduleName origin d)
      (getNS decl (P.runModuleName origin <> "." <> identifierFromIdeDeclaration decl))
    go (Match (moduleName, d@(IdeDeclarationAnn _ decl))) =
      Map.alter
      (insertDeclaration moduleName d)
      (getNS decl (P.runModuleName moduleName <> "." <> identifierFromIdeDeclaration decl))
    insertReexport moduleName origin d old = case old of
      Nothing -> Just (Match (origin, d), [moduleName])
      Just x -> Just (second (moduleName :) x)
    insertDeclaration moduleName d old = case old of
      Nothing -> Just (Match (moduleName, d), [])
      Just x -> Just x


getNS :: IdeDeclaration -> Text -> IdeDeclNamespace
getNS d = case d of
  IdeDeclValue _ -> IdeNSValue
  IdeDeclType _ -> IdeNSType
  IdeDeclTypeSynonym _ -> IdeNSType
  IdeDeclDataConstructor _ -> IdeNSValue
  IdeDeclTypeClass _ -> IdeNSType
  IdeDeclValueOperator _ -> IdeNSValue
  IdeDeclTypeOperator _ -> IdeNSType
  IdeDeclKind _ -> IdeNSKind
