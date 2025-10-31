{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
-- For testing
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- for manual generic implementation
--{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module WPC.CmmSerde where


import GHC.Types.Var              (Var, mkGlobalVar)
import GHC.Types.Id.Info          (IdDetails(VanillaId), vanillaIdInfo)
import GHC.Types.Name             (mkSystemName)
import GHC.Types.Name.Occurrence  (mkVarOcc)
import GHC.Types.Unique           (mkUniqueGrimily)
import GHC.Builtin.Types          (unitTy)   -- un tipo levantado (lifted), simple y seguro
--being imported for defaultVar


import Data.Aeson (ToJSON(..))
import Data.Word (Word64)

-- API pública de Cmm/Hoopl dentro de GHC:
import GHC.Cmm.Dataflow.Label (Label, LabelMap, mapToList)
import GHC.Types.Unique         (getUnique, getKey)


import qualified GHC.Plugins as GHC.Plugins

import GHC.Cmm.Type (CmmType, typeWidth, isGcPtrType, isFloatType)


import GHC.Types.Unique.DSet ( UniqDSet (..))

import GHC.Data.FastString

import GHC.Types.CostCentre ( CCFlavour) 
import GHC.Types.CostCentre.State (CostCentreIndex )
import GHC.Data.Strict ( Maybe(..) )

-- ADD this instead:
import Data.Primitive.ByteArray
  ( ByteArray, MutableByteArray
  , sizeofByteArray, indexByteArray
  , newByteArray, writeByteArray, unsafeFreezeByteArray)
import Control.Monad.ST (runST)


import Data.Aeson (ToJSON(..), Value(..), object, (.=))
import Data.Text (Text)



-- Standard library imports
import Data.Aeson (FromJSON (..), Value, defaultOptions, genericParseJSON, withObject, withText)
import Data.Aeson.Types (Parser, (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Text
import qualified Data.Text.Encoding as TE
import Data.Word (Word64,Word8)

import Data.Aeson (eitherDecode) -- For testing
import qualified Data.ByteString.Lazy.Char8 as LBS

-- GHC Generics (basic + detailed)
import GHC.Generics (
    C1,
    D1,
    -- 'MetaData, 'MetaCons, 'MetaSel
    -- PrefixI, InfixI ...
    -- NoSourceUnpackedness
    -- NoSourceStrictness
    DecidedStrictness (..), -- DecidedLazy
    FixityI (..),
    Generic (..),
    Generic1,
    K1 (..),
    M1 (..),
    Meta (..),
    Rec0,
    S1,
    SourceStrictness (..),
    SourceUnpackedness (..),
    (:*:) (..),
    (:+:) (..),
 )

-- GHC internals: Cmm & related
import GHC.Cmm
import GHC.Cmm.CLabel (CLabel, ForeignLabelSource (..), mkForeignLabel) -- label type used by Section and CmmProc
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label

-- import GHC.Cmm.Reg (GlobalReg) -- register type used by CmmProc
-- Other GHC internals

import GHC.Data.FastString (fsLit)
import GHC.Types.Basic (FunctionOrData (..))
import GHC.Unit.Types (stringToUnitId)

import GHC.Types.CostCentre (CostCentreStack,  CostCentre(..))

import GHC.Types.Unique (Unique, mkUnique, mkUniqueGrimily, mkUniqueIntGrimily,getKey)

-- import GHC.Runtime.Heap.Layout (SMRep)

import qualified Data.Semigroup as GHC.Runtime.Heap.Layout
import GHC.Runtime.Heap.Layout (ArgDescr (..), SMRep (..))

-- import GHC.Types.Var (Var(..))
import GHC.Types.Var (Var (), mkCoVar, mkExportedLocalVar, mkGlobalVar, mkLocalVar)

import GHC.Core.TyCo.Rep (Mult, Type)
import GHC.Types.Id.Info (IdDetails, IdInfo, vanillaIdInfo)
import GHC.Types.Name (Name)

import qualified GHC.Plugins as GHC.Types.FM -- debo deprecar esto
import GHC.Types.Id.Info (IdDetails (..))
--import qualified GHC.Types.Unique.DFM as GHC.Types.FM

import GHC.Tc.Utils.TcType (ConcreteTvOrigin (..))

import Data.ByteString.Short (ShortByteString (..))

import GHC.Types.Name (
    Name,
    mkExternalName,
    mkInternalName,
    mkSystemName,
    mkSystemNameAt,
    nameOccName,
    nameUnique,
    setNameLoc,
 )

-- Piezas requeridas por esos ctors
import GHC.Types.Name.Occurrence (OccName, mkOccName, mkTcOcc, mkVarOcc)
import GHC.Types.SrcLoc (SrcSpan, noSrcSpan)
import GHC.Types.Unique (Unique)
import GHC.Unit.Module (Module)

import GHC.Types.ForeignCall (CCallSpec (..), CCallTarget (..), ForeignCall (..))
import qualified GHC.Types.ForeignCall as GHC.Types -- sus

-- import Data.Array.Byte (ByteArray(..))

-- Deja este para el TIPO ByteArray (sigue siendo el mismo tipo)
import Data.Array.Byte (ByteArray)

-- QUITA las funciones que antes intentabas traer de Data.Array.Byte
--   , MutableByteArray
--   , newByteArray
--   , writeByteArray
--   , unsafeFreezeByteArray

import GHC.Core.Class (Class (..))
import GHC.Core.ConLike (ConLike (..))
import GHC.Core.PatSyn (PatSyn (..))

import GHC.Builtin.PrimOps (PrimOp (..))

--Generic GHC.Core.TyCo.Rep.Type
import GHC.Core.TyCo.Rep (Type (..),TyLit(..))
--import GHC.Types (Tycon(..))

import GHC.Core.TyCon (TyCon(..))

import GHC.Core.TyCo.Rep (Coercion(..))
import GHC.Core.Coercion.Axiom (CoAxiom)

import GHC.Core.Coercion.Axiom ( CoAxiom(..), Branched(..),Branches(..),BranchIndex(..),CoAxBranch(..))


import GHC.Arr (Array(..), array)

import GHC.Types.Tickish ( GenTickish (..)) 


import Control.Applicative ((<|>))
import GHC.Types.CostCentre
  ( CostCentreStack
  , CostCentre(..)         -- only needed if you decode CostCentre
  , currentCCS
  , dontCareCCS
  , mkSingletonCCS
  )

--https://hackage-content.haskell.org/package/ghc-9.10.3/docs/src/GHC.Cmm.html#CmmProgram
-- CmmProgram is a list of CmmGroup, which is basically the same type i am serializying 
-- Allow Aeson Generic-based instance at the top level
deriving instance Generic (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
instance FromJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)

deriving instance Generic CmmTopInfo

-- h
instance FromJSON CmmTopInfo

deriving instance Generic (GenCmmGraph CmmNode)

-- g
instance FromJSON (GenCmmGraph CmmNode)

-- CmmNode is higher-kinded (Extensibility -> Extensibility -> *)


instance ToJSON CLabel where
  toJSON :: CLabel -> Value
  toJSON lbl =
    case maybeForeignLabel lbl of
      Prelude.Just (nameFS, stdcallBytes, src, fod) ->
        object
          [ "tag"          .= String "ForeignLabel"
          , "name"         .= unpackFS nameFS
          , "stdcallBytes" .= stdcallBytes
          , "source"       .= encodeFLSrc src
          , "kind"         .= encodeFOD fod
          ]
      Prelude.Nothing ->
        object [ "_unsupported" .= True
               , "note" .= String "CLabel variant not handled (only ForeignLabel)"
               ]
    where
      -- You have no direct pattern access to ForeignLabel,
      -- but GHC.Cmm.CLabel exposes query functions that tell you if it's one.
      -- If your build of GHC exports `foreignLabelSrc` etc., use those instead.
      --
      -- Here we emulate that by falling back to Nothing for non-ForeignLabel.

      maybeForeignLabel
        :: CLabel -> Prelude.Maybe (FastString, Prelude.Maybe Int, ForeignLabelSource, FunctionOrData)
      maybeForeignLabel _ = Prelude.Nothing
      -- Replace with real extractor if available (pattern or accessor).

      encodeFLSrc :: ForeignLabelSource -> Value
      encodeFLSrc src = case src of
        ForeignLabelInThisPackage     -> object ["tag" .= String "InThisPackage"]
        ForeignLabelInExternalPackage -> object ["tag" .= String "InExternalPackage"]
        --ForeignLabelInPackage uid     -> object ["tag" .= String "InPackage", "unitId" .= Prelude.show uid]
        ForeignLabelInPackage uid -> object ["tag" .= String "InPackage", "unitId" .= String (pack (GHC.Types.FM.unitIdString uid))]

      encodeFOD :: FunctionOrData -> Text
      encodeFOD IsFunction = "Function"
      encodeFOD IsData     = "Data"


--https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10.2-release/compiler/GHC/Cmm/CLabel.hs?ref_type=tags
-- main CLabel parser
instance FromJSON CLabel where
    parseJSON = withObject "CLabel" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "ForeignLabel" -> do
                name <- o .: "name"
                bytes <- o .:? "stdcallBytes"
                srcVal <- o .: "source"
                src <- parseFLSrc srcVal
                fod <- o .: "kind" >>= parseFOD
                pure (mkForeignLabel (fsLit name) bytes src fod)

            -- You can add other supported smart constructors here (mkCmmInfoLabel, mkCmmEntryLabel, etc.)
            other -> fail $ "Unsupported CLabel tag: " <> unpack other

-- parse a ForeignLabelSource
parseFLSrc :: Value -> Parser ForeignLabelSource
parseFLSrc = withObject "ForeignLabelSource" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
        "InThisPackage" -> pure ForeignLabelInThisPackage
        "InExternalPackage" -> pure ForeignLabelInExternalPackage
        "InPackage" -> ForeignLabelInPackage . stringToUnitId <$> o .: "unitId"
        other -> fail $ "Unknown ForeignLabelSource: " <> unpack other

parseFOD :: Text -> Parser FunctionOrData
parseFOD t
    | t == pack "Function" = pure IsFunction
    | t == pack "Data" = pure IsData
    | otherwise = fail $ "Unknown kind: " <> unpack t

deriving instance Generic GlobalReg
instance FromJSON GlobalReg

deriving instance Generic Section
instance FromJSON Section

-- These instances are needed for CmmTopInfos

-- LabelMap: tienes FromJSON manual [(Word64,a)] -> LabelMap a
--instance ToJSON a => ToJSON (LabelMap a) where
--  toJSON _ = dummyVal "LabelMap dummy instance (no Label→Word64 extractor disponible)"

-- LabelMap: ToJSON correcto, empata con tu FromJSON [(Word64,a)] -> LabelMap a
{-instance ToJSON a => ToJSON (LabelMap a) where
  toJSON lm =
    toJSON [ (labelToWord64 l, v) | (l, v) <- mapToList lm ]
    where
      -- Label -> Word64 usando el Unique interno
      labelToWord64 :: Label -> Word64
      labelToWord64 = fromIntegral . getKey . labelToUnique
-}

-- Label -> Word64, usando solo proyectores públicos
labelToWord64 :: Label -> Word64
labelToWord64 = fromIntegral . getKey . getUnique

-- Serializa como [(Word64, a)] para espejar tu FromJSON manual
instance ToJSON a => ToJSON (LabelMap a) where
  toJSON lm = toJSON [ (labelToWord64 l, v) | (l, v) <- mapToList lm ]

-- JSON como lista de pares [(Word64, a)] -> LabelMap a
instance (FromJSON a) => FromJSON (LabelMap a) where
    parseJSON v = do
        ps <- (parseJSON v :: Parser [(Word64, Value)])
        pairs <-
            mapM
                ( \(w, val) -> do
                    x <- parseJSON val -- x :: a (se infiere por el 'a' del instance)
                    pure (mkHooplLabel w, x)
                )
                ps
        pure (mapFromList pairs)

deriving instance Generic CmmStackInfo
instance FromJSON CmmStackInfo


-- | O â†’ O nodes: parse JSON into real constructors with arguments.
parseNodeO_O_json :: Value -> Parser (CmmNode O O)
parseNodeO_O_json = withObject "CmmNode O O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
        "CmmComment" -> do
            txt <- o .: "contents" :: Parser Text
            pure (CmmComment (fsLit (unpack txt)))

        "CmmTick" -> do
            tick <- o .: "contents" :: Parser CmmTickish
            pure (CmmTick tick)

        "CmmUnwind" -> do
            regs <- o .: "contents" :: Parser [(GlobalReg, Prelude.Maybe CmmExpr)]
            pure (CmmUnwind regs)

        "CmmAssign" -> do
            (reg, expr) <- o .: "contents" :: Parser (CmmReg, CmmExpr)
            pure (CmmAssign reg expr)

        "CmmStore" -> do
            (addr, rhs, align) <- o .: "contents" :: Parser (CmmExpr, CmmExpr, AlignmentSpec)
            pure (CmmStore addr rhs align)

        "CmmUnsafeForeignCall" -> do
            (tgt, results, args) <- o .: "contents" :: Parser (ForeignTarget, [CmmFormal], [CmmActual])
            pure (CmmUnsafeForeignCall tgt results args)

        other ->
            fail ("Unsupported CmmNode O O tag: " <> unpack other)




--deriving instance Generic (GenTickish 'TickishPassCmmC)

-- CmmTickish / ForeignTarget: FromJSON dummy
instance ToJSON CmmTickish where
--  toJSON _ = dummyVal "CmmTickish dummy instance"
  toJSON _ = undefined

instance FromJSON CmmTickish where
--parseJSON _ = fail "FromJSON CmmTickish: dummy instance"
  parseJSON _ = undefined 

--This two seem like instances i dont have to handle


instance ToJSON ForeignTarget where
--  toJSON _ = dummyVal "ForeignTarget dummy instance"
  toJSON _ = undefined

instance FromJSON ForeignTarget where
    --parseJSON _ = fail "FromJSON ForeignTarget: dummy instance"
    parseJSON _ = undefined 

-- needed because of instance FromJSON CmmTopInfo

-- C â†’ O: parse a real JSON object
-- Accepted shape (minimum):
--   { "tag": "CmmEntry", "label": <Word64|Label>, "scope": "GlobalScope" }
-- - "scope" is optional; for now only "GlobalScope" is supported.
parseNodeC_O_json :: Value -> Parser (CmmNode C O)
parseNodeC_O_json = withObject "CmmNode C O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
        "CmmEntry" -> do
            lbl <- o .: "label" :: Parser Label
            mscope <- o .:? "scope" :: Parser (Prelude.Maybe Text)
            scope <- case mscope of
                Prelude.Nothing -> pure GlobalScope
                Prelude.Just "GlobalScope" -> pure GlobalScope
                Prelude.Just other -> fail ("Unsupported CmmTickScope: " <> unpack other)
            pure (CmmEntry lbl scope)
        other ->
            fail ("Unsupported CmmNode C O tag: " <> unpack other)


-- CmmNode C O  (entry node)
instance ToJSON (CmmNode C O) where
  toJSON :: CmmNode C O -> Value
  toJSON (CmmEntry lbl _scope) =
    -- Your FromJSON only accepts "GlobalScope", so we encode that.
    object [ "tag"    .= String "CmmEntry"
           , "label"  .= lbl
           , "scope"  .= String "GlobalScope"
           ]


-- | C â†’ O nodes
instance FromJSON (CmmNode C O) where
    parseJSON = parseNodeC_O_json


-- CmmNode O O  (middle nodes)
instance ToJSON (CmmNode O O) where
  toJSON :: CmmNode O O -> Value
  toJSON (CmmComment fs) =
    -- FromJSON expects Text in "contents" which it turns into FastString via fsLit
    object [ "tag" .= String "CmmComment"
           , "contents" .= (String . pack . unpackFS) fs
           ]

  toJSON (CmmTick tick) =
    object [ "tag" .= String "CmmTick"
           , "contents" .= tick
           ]

  toJSON (CmmUnwind regs) =
    -- [(GlobalReg, Maybe CmmExpr)]
    object [ "tag" .= String "CmmUnwind"
           , "contents" .= regs
           ]

  toJSON (CmmAssign reg expr) =
    -- (CmmReg, CmmExpr)
    object [ "tag" .= String "CmmAssign"
           , "contents" .= toJSON (reg, expr)
           ]

  toJSON (CmmStore addr rhs align) =
    -- (CmmExpr, CmmExpr, AlignmentSpec)
    object [ "tag" .= String "CmmStore"
           , "contents" .= toJSON (addr, rhs, align)
           ]

  toJSON (CmmUnsafeForeignCall tgt results args) =
    -- (ForeignTarget, [CmmFormal], [CmmActual])
    -- (where CmmFormal ~ LocalReg, CmmActual ~ CmmExpr)
    object [ "tag" .= String "CmmUnsafeForeignCall"
           , "contents" .= toJSON (tgt, results, args)
           ]


-- | O â†’ O nodes
instance FromJSON (CmmNode O O) where
    parseJSON = parseNodeO_O_json


-- | O â†’ C nodes: parse JSON into real constructors with arguments.
parseNodeO_C_json :: Value -> Parser (CmmNode O C)
parseNodeO_C_json = withObject "CmmNode O C" $ \o -> do
  tag <- o .: "tag" :: Parser Text
  case tag of
    -- { "tag":"CmmBranch", "label": <Label> }
    "CmmBranch" -> do
      lbl <- o .: "label" :: Parser Label
      pure (CmmBranch lbl)

    -- { "tag":"CmmCondBranch",
    --   "contents": [ <pred :: CmmExpr>, <true :: Label>, <false :: Label>, <likely :: Maybe Bool> ] }
    "CmmCondBranch" -> do
      (p, t, f, lk) <- o .: "contents" :: Parser (CmmExpr, Label, Label, Prelude.Maybe Bool)
      pure CmmCondBranch
            { cml_pred   = p
            , cml_true   = t
            , cml_false  = f
            , cml_likely = lk
            }

    -- { "tag":"CmmCall",
    --   "contents": [ <target :: CmmExpr>
    --               , <cont   :: Maybe Label>
    --               , <regs   :: [GlobalReg]>
    --               , <args   :: Int>
    --               , <retArgs:: Int>
    --               , <retOff :: Int> ] }
    "CmmCall" -> do
      (tgt, mcont, regs, args, retArgs, retOff)
        <- o .: "contents" :: Parser (CmmExpr, Prelude.Maybe Label, [GlobalReg], Int, Int, Int)
      pure CmmCall
            { cml_target   = tgt
            , cml_cont     = mcont
            , cml_args_regs= regs
            , cml_args     = args
            , cml_ret_args = retArgs
            , cml_ret_off  = retOff
            }

    -- Pendientes: requieren tipos/instancias extra (SwitchTargets, ForeignTarget, CmmFormal/Actual, â€¦)
    "CmmSwitch"       -> fail "parseNodeO_C_json: CmmSwitch no soportado aÃºn (SwitchTargets)."
    "CmmForeignCall"  -> fail "parseNodeO_C_json: CmmForeignCall no soportado aÃºn (ForeignTarget/[CmmFormal]/[CmmActual])."

    other -> fail ("Unsupported CmmNode O C tag: " <> unpack other)

-- | O â†’ C nodes
instance FromJSON (CmmNode O C) where
  parseJSON = parseNodeO_C_json

-- CmmNode O C  (last nodes)
instance ToJSON (CmmNode O C) where
  toJSON :: CmmNode O C -> Value
  toJSON (CmmBranch lbl) =
    object [ "tag"   .= String "CmmBranch"
           , "label" .= lbl
           ]

  toJSON CmmCondBranch{ cml_pred = p
                      , cml_true = t
                      , cml_false = f
                      , cml_likely = lk
                      } =
    -- (CmmExpr, Label, Label, Maybe Bool)
    object [ "tag" .= String "CmmCondBranch"
           , "contents" .= toJSON (p, t, f, lk)
           ]

  toJSON CmmCall{ cml_target   = tgt
                , cml_cont     = mcont
                , cml_args_regs= regs
                , cml_args     = args
                , cml_ret_args = retArgs
                , cml_ret_off  = retOff
                } =
    -- (CmmExpr, Maybe Label, [GlobalReg], Int, Int, Int)
    object [ "tag" .= String "CmmCall"
           , "contents" .= toJSON (tgt, mcont, regs, args, retArgs, retOff)
           ]


--deriving instance Generic  ( GHC.Cmm.Dataflow.Block.Block CmmNode GHC.Cmm.Dataflow.Block.C GHC.Cmm.Dataflow.Block.C )

-- Graph' Block CmmNode C C  (closed entry, closed exit)
instance
  FromJSON
    ( GHC.Cmm.Dataflow.Graph.Graph'
        GHC.Cmm.Dataflow.Block.Block
        CmmNode
        GHC.Cmm.Dataflow.Block.C
        GHC.Cmm.Dataflow.Block.C
    )
  where
  parseJSON = withObject "Graph' Block CmmNode C C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- Accept the legacy 3-tuple but ignore entry/exit blocks for C/C graphs.
      -- { "tag":"GMany"
      -- , "contents": [ <_entry :: Block O C>            -- ignored
      --               , <body   :: LabelMap (Block C C)>  -- used
      --               , <_exit  :: Block C O> ] }         -- ignored
      "GMany" -> do
        (_entryOC, bodyCC, _exitCO)
          <- o .: "contents"
             :: Parser
                  ( Block CmmNode O C
                  , LabelMap (Block CmmNode C C)
                  , Block CmmNode C O
                  )
        -- Closed/Closed graphs carry no explicit entry/exit blocks:
        -- entry  :: MaybeO C (Block O C)  = NothingO
        -- exit   :: MaybeO C (Block C O)  = NothingC
        pure (GMany NothingO bodyCC NothingO)

      other ->
        fail ("Unsupported Graph' tag: " <> unpack other)



-- Block C O  (entry node + middle)
instance ToJSON (Block CmmNode C O) where
  toJSON :: Block CmmNode C O -> Value
  toJSON (BlockCO hd BNil) =
    object [ "tag" .= String "BFirst"
           , "contents" .= hd
           ]
  toJSON (BlockCO hd mid) =
    object [ "tag" .= String "BHead"
           , "contents" .= toJSON (hd, mid)
           ]



-- Block O O  (pure middle)
instance ToJSON (Block CmmNode O O) where
  toJSON :: Block CmmNode O O -> Value
  toJSON BNil =
    object [ "tag" .= String "BNil" ]
  toJSON (BMiddle n) =
    object [ "tag" .= String "BMiddle"
           , "contents" .= n
           ]
  toJSON (BCat b1 b2) =
    object [ "tag" .= String "BCat"
           , "contents" .= toJSON (b1, b2)
           ]
  toJSON (BCons n b) =
    object [ "tag" .= String "BCons"
           , "contents" .= toJSON (n, b)
           ]
  toJSON (BSnoc b n) =
    object [ "tag" .= String "BSnoc"
           , "contents" .= toJSON (b, n)
           ]


-- Block C O  (entry node + open-open middle)
instance FromJSON (Block CmmNode C O) where
  parseJSON = withObject "Block C O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BFirst", "contents": <CmmNode C O> }
      "BFirst" -> do
        hd <- o .: "contents" :: Parser (CmmNode C O)
        pure (BlockCO hd BNil)  -- single-entry, empty middle

      -- { "tag":"BHead", "contents": [ <CmmNode C O>, <Block O O> ] }
      "BHead" -> do
        (hd, mid) <- o .: "contents" :: Parser (CmmNode C O, Block CmmNode O O)
        pure (BlockCO hd mid)

      -- { "tag":"BCat", "contents": [ <Block C O>, <Block O O> ] }
      "BCat" -> do
        (b1, b2) <- o .: "contents" :: Parser (Block CmmNode C O, Block CmmNode O O)
        pure (blockAppend b1 b2)  -- append OO tail to CO, stays CO

      other -> fail ("Unsupported Block C O tag: " <> unpack other)


-- Block O O  (pure middle)
instance FromJSON (Block CmmNode O O) where
  parseJSON = withObject "Block O O" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BNil" }
      "BNil"     -> pure BNil
      -- { "tag":"BMiddle", "contents": <CmmNode O O> }
      "BMiddle"  -> BMiddle <$> (o .: "contents" :: Parser (CmmNode O O))
      -- { "tag":"BCat", "contents": [ <Block O O>, <Block O O> ] }
      "BCat"     -> do (b1,b2) <- o .: "contents"; pure (BCat b1 b2)
      -- { "tag":"BCons", "contents": [ <CmmNode O O>, <Block O O> ] }
      "BCons"    -> do (n,b) <- o .: "contents";  pure (BCons n b)
      -- { "tag":"BSnoc", "contents": [ <Block O O>, <CmmNode O O> ] }
      "BSnoc"    -> do (b,n) <- o .: "contents";  pure (BSnoc b n)
      other      -> fail ("Unsupported Block O O tag: " <> unpack other)


-- Block O C  (middle + last node)
instance ToJSON (Block CmmNode O C) where
  toJSON :: Block CmmNode O C -> Value
  toJSON (BlockOC BNil lt) =
    object [ "tag" .= String "BLast"
           , "contents" .= lt
           ]
  toJSON (BlockOC mid lt) =
    object [ "tag" .= String "BTail"
           , "contents" .= toJSON (mid, lt)
           ]


-- Block O C  (open-open middle + last node)
instance FromJSON (Block CmmNode O C) where
  parseJSON = withObject "Block O C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BLast", "contents": <CmmNode O C> }
      "BLast" -> do
        lt <- o .: "contents" :: Parser (CmmNode O C)
        pure (BlockOC BNil lt)     -- empty middle + last

      -- { "tag":"BTail", "contents": [ <Block O O>, <CmmNode O C> ] }
      "BTail" -> do
        (mid, lt) <- o .: "contents" :: Parser (Block CmmNode O O, CmmNode O C)
        pure (BlockOC mid lt)

      -- { "tag":"BCat", "contents": [ <Block O O>, <Block O C> ] }
      "BCat" -> do
        (b1, b2) <- o .: "contents" :: Parser (Block CmmNode O O, Block CmmNode O C)
        pure (blockAppend b1 b2)    -- OO + OC â†’ OC

      other -> fail ("Unsupported Block O C tag: " <> unpack other)


-- Block C C  (entry + middle + last)
instance ToJSON (Block CmmNode C C) where
  toJSON :: Block CmmNode C C -> Value
  toJSON (BlockCC hd mid lt) =
    -- Choose the "BHead" shape your parser accepts: (CmmNode C O, Block O C)
    object [ "tag" .= String "BHead"
           , "contents" .= toJSON (hd, BlockOC mid lt)
           ]



-- Block C C  (entry node + middle OO + last node)
instance FromJSON (Block CmmNode C C) where
  parseJSON = withObject "Block C C" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      -- { "tag":"BCat",  "contents": [ <Block C O>, <Block O C> ] }
      "BCat" -> do
        (bco, boc) <- o .: "contents" :: Parser (Block CmmNode C O, Block CmmNode O C)
        let (hd, mid1) = blockSplitHead bco
            (mid2, lt) = blockSplitTail boc
            mid        = blockConcat [mid1, mid2]
        pure (BlockCC hd mid lt)

      -- { "tag":"BHead", "contents": [ <CmmNode C O>, <Block O C> ] }
      "BHead" -> do
        (hd, boc) <- o .: "contents" :: Parser (CmmNode C O, Block CmmNode O C)
        let (mid, lt) = blockSplitTail boc
        pure (BlockCC hd mid lt)

      -- { "tag":"BTail", "contents": [ <Block C O>, <CmmNode O C> ] }
      "BTail" -> do
        (bco, lt) <- o .: "contents" :: Parser (Block CmmNode C O, CmmNode O C)
        let (hd, mid) = blockSplitHead bco
        pure (BlockCC hd mid lt)

      other -> fail ("Unsupported Block C C tag: " <> unpack other)

-- import GHC.Cmm.Dataflow.Graph are imported because of
-- import GHC.Cmm.Dataflow.Block  the above

---Needed for GenCmmGraph CmmNode




instance ToJSON Label where
  toJSON     = toJSON . labelToWord64
  toEncoding = toEncoding . labelToWord64

instance FromJSON Label where
    parseJSON v = mkHooplLabel <$> (parseJSON v :: Parser Word64)

deriving instance Generic SectionType
instance FromJSON SectionType


-- GenCmmStatics 'False: acepta CmmStatics y CmmStaticsRaw
instance FromJSON (GenCmmStatics 'False) where
  parseJSON = withObject "GenCmmStatics 'False" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "CmmStatics" -> do
        (lbl, itbl, ccs, payload, nonPtrs) <-
          o .: "contents"
            :: Parser (CLabel, CmmInfoTable, CostCentreStack, [CmmLit], [CmmLit])
        pure (CmmStatics lbl itbl ccs payload nonPtrs)

      "CmmStaticsRaw" -> do
        (lbl, statics) <-
          o .: "contents"
            :: Parser (CLabel, [CmmStatic])
        pure (CmmStaticsRaw lbl statics)

      other -> fail $ "GenCmmStatics 'False: unknown tag " <> unpack other


instance ToJSON (GenCmmStatics 'False) where
  toJSON :: GenCmmStatics 'False -> Value
  toJSON (CmmStatics lbl itbl ccs payload nonPtrs) =
    object [ "tag" .= String "CmmStatics"
           , "contents" .= toJSON (lbl, itbl, ccs, payload, nonPtrs)
           ]
  toJSON (CmmStaticsRaw lbl statics) =
    object [ "tag" .= String "CmmStaticsRaw"
           , "contents" .= toJSON (lbl, statics)
           ]


-- CmmStatic aparece en CmmStaticsRaw :: CLabel -> [CmmStatic]
-- Necesaria para que genericParseJSON de GenCmmStatics 'True compile.
deriving instance Generic CmmStatic
instance FromJSON CmmStatic
instance ToJSON CmmStatic

-- Necessary for CmmStatic instance
instance FromJSON BS.ByteString where
    parseJSON = withText "ByteString" (pure . TE.encodeUtf8)

instance ToJSON BS.ByteString where
  toJSON :: BS.ByteString -> Value
  toJSON bs = String (TE.decodeUtf8 bs)



instance ToJSON (GenCmmStatics 'True) where
  toJSON :: GenCmmStatics 'True -> Value
  toJSON (CmmStaticsRaw lbl statics) =
    object [ "tag" .= String "CmmStaticsRaw"
           , "contents" .= toJSON (lbl, statics)
           ]

-- GenCmmStatics 'True: solo permite CmmStaticsRaw
instance FromJSON (GenCmmStatics 'True) where
  parseJSON = withObject "GenCmmStatics 'True" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "CmmStaticsRaw" -> do
        (lbl, statics) <-
          o .: "contents"
            :: Parser (CLabel, [CmmStatic])
        pure (CmmStaticsRaw lbl statics)

      "CmmStatics" ->
        fail "GenCmmStatics 'True cannot be constructed with CmmStatics"

      other -> fail $ "GenCmmStatics 'True: unknown tag " <> unpack other

deriving instance Generic CmmLit
instance FromJSON CmmLit --where
--   parseJSON _ = fail "dummy"

-- data structures are not all in scope

deriving instance Generic CostCentre
--instance FromJSON CostCentre -- Check Begin and End
instance FromJSON CostCentre --where
--  parseJSON = undefined
instance ToJSON CostCentre



-- ==========================
-- ToJSON for CostCentreStack
-- ==========================
instance ToJSON CostCentreStack where
  toJSON :: CostCentreStack -> Value
  toJSON ccs
    | ccs == currentCCS =
        object ["tag" .= String "CurrentCCS"]
    | ccs == dontCareCCS =
        object ["tag" .= String "DontCareCCS"]
    | otherwise =
        -- For stacks created via mkSingletonCCS or others carrying a CostCentre
        -- we must represent them as SingletonCCS <CostCentre>
        object ["tag" .= String "SingletonCCS", "contents" .= extractCC ccs]
    where
      -- Try to project out the CostCentre of a singleton CCS.
      -- For complex stacks (nested etc.) you'd extend this,
      -- but mkSingletonCCS builds a 1-element stack, which your FromJSON expects.
      extractCC :: CostCentreStack -> CostCentre
      extractCC = currentCostCentre
        where
          -- Accessor defined in GHC.Types.CostCentre
          currentCostCentre = \_ -> error "extractCC: real CostCentre extraction not available in this context"
          -- If you have `ccsCC :: CostCentreStack -> CostCentre`, use that instead



instance FromJSON CostCentreStack where
  parseJSON v =
       parseAsText v
   <|> parseAsObj  v
    where
      -- Accept: "CurrentCCS" / "DontCareCCS"
      parseAsText :: Value -> Parser CostCentreStack
      parseAsText = withText "CostCentreStack" $ \t ->
        case t of
          "CurrentCCS"  -> pure currentCCS
          "DontCareCCS" -> pure dontCareCCS
          other         -> fail $ "Unknown CostCentreStack tag: " <> unpack other

      -- Accept the tagged-object shapes:
      --   { "tag":"CurrentCCS" }
      --   { "tag":"DontCareCCS" }
      --   { "tag":"SingletonCCS", "contents": <CostCentre> }  -- or "cc": <...>
      parseAsObj :: Value -> Parser CostCentreStack
      parseAsObj = withObject "CostCentreStack" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
          "CurrentCCS"   -> pure currentCCS
          "DontCareCCS"  -> pure dontCareCCS
          "SingletonCCS" -> mkSingletonCCS <$> (o .: "contents" <|> o .: "cc")
          other          -> fail $ "Unknown CostCentreStack tag: " <> unpack other



--deriving instance Generic CostCentreStack
--instance FromJSON CostCentreStack where
--    parseJSON :: Value -> Parser CostCentreStack
--    parseJSON _ = fail "dummy"



deriving instance Generic CmmInfoTable

instance FromJSON CmmInfoTable --where
-- parseJSON _ = fail "dummy"

--instance FromJSON CmmInfoTable 
--this depends on GHC.Types.Var.Var, which seems to depend on a lot of GHC stuff
-- maybe i should not bother to serialize GHC.Types.Var.Var? 

-- | A single, process-stable dummy Var (a global Id with type 'Any :: Type').

-- | Var placeholder por defecto: un Id global 'VanillaId' con tipo () :: Type.
defaultVar :: Var
defaultVar =
  let u   = mkUniqueGrimily 0
      occ = mkVarOcc "placeholderVar"
      nm  = mkSystemName u occ
      ty  = unitTy
  in  mkGlobalVar VanillaId nm ty vanillaIdInfo

instance ToJSON Var where
  toJSON _ =
    object
      [ "tag"    .= ("Var" :: String)
      , "status" .= ("not handled" :: String)
      , "note"   .= ("will be deserialized back into defaultVar" :: String)
      ]

instance FromJSON Var where
  parseJSON _ = pure defaultVar

deriving instance Generic GHC.Types.ForeignCall.ForeignCall
--instance FromJSON GHC.Types.ForeignCall.ForeignCall

deriving instance Generic GHC.Types.ForeignCall.CCallSpec


--instance FromJSON GHC.Types.ForeignCall.CCallSpec where
--    parseJSON =
--        error "Falla pues"

deriving instance Generic GHC.Types.ForeignCall.CCallTarget


--instance FromJSON GHC.Types.CCallTarget where
--    parseJSON =
--        error "Falla pues"

--Begin

{-
instance ToJSON CCFlavour where
  toJSON cc = case Prelude.show (toConstr cc) of
    "CafCC" ->
      object ["tag" .= String "CafCC"]
    "IndexedCC" ->
      case ( cast (gmapQi 0 id cc) :: Prelude.Maybe IndexedCCFlavour
           , cast (gmapQi 1 id cc) :: Prelude.Maybe CostCentreIndex ) of
        (Prelude.Just flav, Prelude.Just cci) ->
          object [ "tag"     .= String "IndexedCC"
                 , "flavour" .= flav
                 , "index"   .= unCostCentreIndex cci
                 ]
        _ -> object ["tag" .= String "IndexedCC", "error" .= String "unpack-failed"]
    other -> object ["tag" .= String (T.pack other)]
-}

--deriving instance Generic GHC.Types.CostCentre.CCFlavour
instance FromJSON GHC.Types.CostCentre.CCFlavour where
    parseJSON =
        error "falla pues"


--Csaba says to alwayys use CafCC to not need Indexed CCflavour
--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/src/GHC.Types.CostCentre.html#IndexedCCFlavour

--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/src/GHC.Types.CostCentre.html#ppFlavourLblComponent
--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/src/GHC.Types.CostCentre.html#mkExprCCFlavour

--seems like this type is source annotations cmm can prescind from 
instance ToJSON GHC.Types.CostCentre.CCFlavour  where
  toJSON = undefined
--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/GHC-Types-CostCentre.html

deriving instance Generic SrcSpan
instance FromJSON SrcSpan --where
instance ToJSON SrcSpan
--   parseJSON =
--       error "falla pues"


deriving instance Generic GHC.Types.FM.UnhelpfulSpanReason
instance FromJSON GHC.Types.FM.UnhelpfulSpanReason where
instance ToJSON GHC.Types.FM.UnhelpfulSpanReason where
--   parseJSON =
--       error "falla pues"


--deriving instance Generic GHC.Types.FM.RealSrcSpan
instance FromJSON GHC.Types.FM.RealSrcSpan where
   parseJSON =
       error "falla pues"
--looks like something related to source annotations, and thus not strictl necessary?
instance ToJSON  GHC.Types.FM.RealSrcSpan where
  toJSON = undefined



deriving instance Generic (GHC.Data.Strict.Maybe GHC.Types.FM.BufSpan)
instance FromJSON (GHC.Data.Strict.Maybe GHC.Types.FM.BufSpan) 
instance ToJSON (GHC.Data.Strict.Maybe GHC.Types.FM.BufSpan) 
--  where
--    parseJSON =
--      error "falla pues"




deriving instance Generic GHC.Types.FM.BufSpan
instance FromJSON GHC.Types.FM.BufSpan
instance ToJSON GHC.Types.FM.BufSpan
--  where
--    parseJSON =
--      error "falla pues"


deriving instance Generic GHC.Types.FM.BufPos
instance FromJSON GHC.Types.FM.BufPos
instance ToJSON GHC.Types.FM.BufPos

--  where
--    parseJSON =
--      error "falla pues"



deriving instance Generic (GHC.Types.FM.GenModule GHC.Types.FM.Unit)
instance FromJSON (GHC.Types.FM.GenModule GHC.Types.FM.Unit)
instance ToJSON (GHC.Types.FM.GenModule GHC.Types.FM.Unit) 

deriving instance Generic GHC.Types.FM.ModuleName
instance FromJSON GHC.Types.FM.ModuleName
instance ToJSON GHC.Types.FM.ModuleName
--  where
--    parseJSON =
--        error "falla pues"


deriving instance Generic (GHC.Types.FM.GenUnit GHC.Types.FM.UnitId)
instance FromJSON (GHC.Types.FM.GenUnit GHC.Types.FM.UnitId)
instance ToJSON (GHC.Types.FM.GenUnit GHC.Types.FM.UnitId)
--  where
--    parseJSON =
--        error "falla pues"


deriving instance Generic (GHC.Types.FM.GenInstantiatedUnit GHC.Types.FM.UnitId)
instance FromJSON (GHC.Types.FM.GenInstantiatedUnit GHC.Types.FM.UnitId)
instance ToJSON (GHC.Types.FM.GenInstantiatedUnit GHC.Types.FM.UnitId)

--  where
--    parseJSON =
--        error "falla pues"


--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/src/GHC.Types.Unique.DSet.html#UniqDSet
--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/Language-Haskell-Syntax-Module-Name.html#t:ModuleName
--deriving instance Generic (GHC.Types.Unique.DSet.UniqDSet GHC.Types.FM.ModuleName)
--deriving Generic not possible

instance FromJSON (GHC.Types.Unique.DSet.UniqDSet GHC.Plugins.ModuleName) where
  parseJSON = undefined
    

instance ToJSON (GHC.Types.Unique.DSet.UniqDSet GHC.Plugins.ModuleName) where
  toJSON = undefined

deriving instance Generic (GHC.Types.FM.Definite GHC.Types.FM.UnitId)
instance FromJSON (GHC.Types.FM.Definite GHC.Types.FM.UnitId)
instance ToJSON (GHC.Types.FM.Definite GHC.Types.FM.UnitId)
--  where
--    parseJSON =
--        error "falla pues"

deriving instance Generic GHC.Types.FM.UnitId
instance FromJSON GHC.Types.FM.UnitId
instance ToJSON GHC.Types.FM.UnitId

deriving instance Generic GHC.Types.FM.FastString
instance FromJSON GHC.Types.FM.FastString
instance ToJSON GHC.Types.FM.FastString


-- FromJSON/ToJSON para el FastZString de GHC (tipo abstracto)
instance FromJSON FastZString where
  parseJSON = withText "FastZString" $ \t ->
    -- Text -> UTF-8 ByteString -> FastString -> (Z-encode) -> FastZString
    pure (zEncodeFS (mkFastStringByteString (TE.encodeUtf8 t)))

instance ToJSON FastZString where
  toJSON fzs =
    -- zString :: FastZString -> String
    -- lo empaquetamos como Text para emitir un JSON string
    String (pack (zString fzs))

--End

-- deriving instance Generic Data.ByteString.Short.ShortByteString
instance FromJSON Data.ByteString.Short.ShortByteString
instance ToJSON Data.ByteString.Short.ShortByteString


--instance FromJSON ByteArray where
--    parseJSON =
--        error "FromJSON Name (dummy): Name es abstracto; haremos una decodificaciÃ³n manual usando mk*Name."
-- Encode as [Word8]
-- Decode: [Word8] -> ByteArray
instance FromJSON ByteArray where
  parseJSON v = do
    ws <- (parseJSON v :: Parser [Word8])
    Prelude.pure $ runST $ do
      let n = Prelude.length ws
      mba <- newByteArray n
      let fill i []     = Prelude.pure ()
          fill i (x:xs) = writeByteArray mba i x >> fill (i Prelude.+ 1) xs
      fill 0 ws
      unsafeFreezeByteArray mba

-- ToJSON: ByteArray -> [Word8]
instance ToJSON ByteArray where
  toJSON :: ByteArray -> Value
  toJSON ba =
    let n :: Int
        n = sizeofByteArray ba

        go :: Int -> [Word8] -> [Word8]
        go i acc
          | i Prelude.< n = go (i Prelude.+ 1) (indexByteArray ba i : acc)
          | Prelude.otherwise = Prelude.reverse acc

        bytes :: [Word8]
        bytes = go 0 []
    in toJSON bytes

deriving instance Generic ProfilingInfo
instance FromJSON ProfilingInfo
--instance ToJSON ProfilingInfo

deriving instance Generic GHC.Runtime.Heap.Layout.SMRep
instance FromJSON GHC.Runtime.Heap.Layout.SMRep

deriving instance Generic ClosureTypeInfo
instance FromJSON ClosureTypeInfo

deriving instance Generic GHC.Runtime.Heap.Layout.ArgDescr
instance FromJSON GHC.Runtime.Heap.Layout.ArgDescr

deriving instance Generic CmmExpr
instance FromJSON CmmExpr
instance ToJSON CmmExpr

deriving instance Generic MachOp
instance FromJSON MachOp

deriving instance Generic FMASign
instance FromJSON FMASign

deriving instance Generic AlignmentSpec
instance FromJSON AlignmentSpec

deriving instance Generic Area
instance FromJSON Area

deriving instance Generic CmmReg
instance FromJSON CmmReg

deriving instance Generic GlobalRegUse
instance FromJSON GlobalRegUse

deriving instance Generic LocalReg
instance FromJSON LocalReg

-- Emulacion del derivado genÃ©rico para: newtype Unique = MkUnique Word64
-- Se decodifica exactamente como Word64 y luego se construye el Unique.
instance FromJSON GHC.Types.Unique.Unique where
    parseJSON v =
        (GHC.Types.Unique.mkUniqueGrimily <$> (parseJSON v :: Parser Word64))
--is this used by something in a cmm module?

-- | Encode a Unique as the underlying numeric key.
instance ToJSON GHC.Types.Unique.Unique where
  toJSON :: GHC.Types.Unique.Unique -> Value
  toJSON u =
    -- getKey :: Unique -> Int  (exported)
    -- Emit as Word64 to match your FromJSON that uses mkUniqueGrimily :: Word64 -> Unique
    toJSON (fromIntegral (GHC.Types.Unique.getKey u) :: Word64)



-- reemplaza tu main por este
main_legacy :: IO ()
main_legacy = do
    putStrLn "== CmmNode C O JSON tests =="

    -- 1) OK: objeto vÃ¡lido
    let ok = "{\"tag\":\"CmmEntry\",\"label\":0,\"scope\":\"GlobalScope\"}"
    test "OK (object with tag/label/scope)" ok

    -- 2) OK sin scope (usa GlobalScope por defecto)
    let okNoScope = "{\"tag\":\"CmmEntry\",\"label\":1}"
    test "OK (object without scope â†’ defaults to GlobalScope)" okNoScope

    -- 3) Falla: scope no soportado
    let badScope = "{\"tag\":\"CmmEntry\",\"label\":0,\"scope\":\"LocalScope\"}"
    test "FAIL (unsupported scope)" badScope

    -- 4) Falla: tag no soportado
    let badTag = "{\"tag\":\"NotEntry\",\"label\":0}"
    test "FAIL (unsupported tag)" badTag

    -- 5) Falla: falta label
    let missingLabel = "{\"tag\":\"CmmEntry\"}"
    test "FAIL (missing label)" missingLabel

    -- 6) Falla: string â€œlegacyâ€ (ya no aceptamos Text puro)
    let legacy = "\"CmmEntry\""
    test "FAIL (string form no longer accepted)" legacy

    putStrLn "== Done =="
  where
    test msg js = do
        putStrLn $ "â†’ " <> msg
        case eitherDecode @(CmmNode C O) (LBS.pack js) of
            Right _ -> putStrLn "   decoded: OK"
            Left e -> putStrLn $ "   decoded: ERROR â†’ " <> e

-- ===== FromJSON for CmmType (emulating genericParseJSON defaultOptions) =====
-- JSON shape expected (TaggedObject):
--   { "tag":"CmmType", "contents":[ <CmmCat>, <Width> ] }
--   CmmCat:
--     { "tag":"BitsCat" }
--     { "tag":"FloatCat" }
--     { "tag":"GcPtrCat" }
--     { "tag":"VecCat", "contents":[ <Length :: Int>, <CmmCat> ] }
--   Width:
--     { "tag":"W8" | "W16" | "W32" | "W64" | "W128" | "W256" | "W512" }

-- Open mirror of the internal category so we can rebuild using public ctors
data CmmCatOpen
    = OGcPtr
    | OBits
    | OFloat
    | OVec Int CmmCatOpen
    deriving (Show, Eq)


-- CmmType y auxiliares: ToJSON manual (complementa tu FromJSON)
instance ToJSON CmmCatOpen where
  toJSON :: CmmCatOpen -> Value
  toJSON cat = case cat of
    OGcPtr ->
      object [ "tag" .= String "GcPtrCat" ]
    OBits ->
      object [ "tag" .= String "BitsCat" ]
    OFloat ->
      object [ "tag" .= String "FloatCat" ]
    OVec n sub ->
      object [ "tag" .= String "VecCat"
             , "contents" .= toJSON (n, sub)
             ]

instance FromJSON CmmCatOpen where
    parseJSON :: Value -> Parser CmmCatOpen
    parseJSON = withObject "CmmCat" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "GcPtrCat" -> pure OGcPtr
            "BitsCat" -> pure OBits
            "FloatCat" -> pure OFloat
            "VecCat" -> do
                cs <- o .: "contents"
                (n, sub) <- Data.Aeson.parseJSON cs :: Parser (Int, CmmCatOpen)
                pure (OVec n sub)
            other -> fail ("Unknown CmmCat tag: " <> unpack other)


instance ToJSON Width where
  toJSON :: Width -> Value
  toJSON w = case w of
    W8   -> object ["tag" .= String "W8"]
    W16  -> object ["tag" .= String "W16"]
    W32  -> object ["tag" .= String "W32"]
    W64  -> object ["tag" .= String "W64"]
    W128 -> object ["tag" .= String "W128"]
    W256 -> object ["tag" .= String "W256"]
    W512 -> object ["tag" .= String "W512"]

instance FromJSON Width where
    parseJSON = withObject "Width" $ \o -> do
        tag <- o .: "tag" :: Parser Text
        case tag of
            "W8" -> pure W8
            "W16" -> pure W16
            "W32" -> pure W32
            "W64" -> pure W64
            "W128" -> pure W128
            "W256" -> pure W256
            "W512" -> pure W512
            other -> fail ("Unknown Width tag: " <> unpack other)

--https://hackage-content.haskell.org/package/ghc-9.10.2/docs/src/GHC.Cmm.Type.html


-- Needed (if not already):
-- import GHC.Cmm.Type
--   ( CmmType, Width(..)
--   , typeWidth, isGcPtrType, isFloatType, isBitsType
--   , isVecType, vecLength, vecElemType
--   , cmmBits, cmmFloat, vec
--   )
-- import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, withObject, (.:))
-- import Data.Aeson.Types (Parser)
-- import Data.Text (Text)
-- import qualified Data.Text as T

-- Shape we emit/consume:
-- { "tag":"CmmType"
-- , "contents": [ <CmmCatOpen>, <Width> ]
-- }
-- where CmmCatOpen is:
--   { "tag":"BitsCat" }
--   { "tag":"FloatCat" }
--   { "tag":"GcPtrCat" }              -- decode needs Platform (see note)
--   { "tag":"VecCat", "contents":[ <len :: Int>, <sub :: CmmCatOpen> ] }

-- Your open mirror (already in your file)
-- data CmmCatOpen = OGcPtr | OBits | OFloat | OVec Int CmmCatOpen

-- ===== ToJSON =====
instance ToJSON CmmType where
  toJSON :: CmmType -> Value
  toJSON ty
    | isVecType ty =
        let n   = vecLength ty
            ety = vecElemType ty
            w   = typeWidth ty
            cat = toOpen ety  -- category of the element type
        in object [ "tag"      .= String "CmmType"
                  , "contents" .= toJSON (OVec n cat, w)
                  ]
    | isFloatType ty =
        object [ "tag"      .= String "CmmType"
               , "contents" .= toJSON (OFloat, typeWidth ty)
               ]
    | isGcPtrType ty =
        object [ "tag"      .= String "CmmType"
               , "contents" .= toJSON (OGcPtr, typeWidth ty)
               ]
    | otherwise {- isBitsType ty -} =
        object [ "tag"      .= String "CmmType"
               , "contents" .= toJSON (OBits, typeWidth ty)
               ]
    where
      -- Encode only the *category* for an element type; its width comes
      -- from the outer 'typeWidth ty' (same convention as your FromJSON).
      toOpen :: CmmType -> CmmCatOpen
      toOpen et
        | isVecType et   = OVec (vecLength et) (toOpen (vecElemType et))
        | isFloatType et = OFloat
        | isGcPtrType et = OGcPtr
        | otherwise      = OBits

-- ===== FromJSON =====
instance FromJSON CmmType where
  parseJSON = withObject "CmmType" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "CmmType" -> do
        (cat, w) <- o .: "contents" :: Parser (CmmCatOpen, Width)
        build cat w
      other -> fail ("Expected tag CmmType, got: " <> unpack other)
    where
      build :: CmmCatOpen -> Width -> Parser CmmType
      build OBits w       = pure (cmmBits w)
      build OFloat w      = pure (cmmFloat w)
      build (OVec n c) w  = vec n <$> build c w
      -- NOTE: Cmm GC-pointer requires a Platform (gcWord :: Platform -> CmmType).
      -- If you have a Platform at hand, replace this failure with:
      --   pure (setCmmTypeWidth w (gcWord platform))
      build OGcPtr _      = fail "CmmType: decoding GcPtrCat requires Platform (use a Platform-aware parser)"




--------------------------------------------------------------------------------
-- ToJSON instances (FINAL) — respeta tu regla automática/dummy
-- Pegar exactamente al final de Main.hs
--------------------------------------------------------------------------------

-- Helper para dummies
--dummyVal :: Text -> Value
--dummyVal msg = object ["_dummy" .= True, "note" .= msg]

--------------------------------------------------------------------------------
-- AUTOMÁTICAS: tipos con (deriving Generic ..) + (instance FromJSON ..) en TU archivo
--   → instancia vacía `instance ToJSON …` (Aeson usa Generic por default)
--------------------------------------------------------------------------------

instance ToJSON (GenCmmDecl CmmStatics CmmTopInfo CmmGraph)
instance ToJSON CmmTopInfo
instance ToJSON (GenCmmGraph CmmNode)
instance ToJSON GlobalReg
instance ToJSON Section
instance ToJSON CmmStackInfo
instance ToJSON SectionType
--instance ToJSON CmmStatic
instance ToJSON CmmLit
instance ToJSON CmmInfoTable
instance ToJSON ProfilingInfo
instance ToJSON GHC.Runtime.Heap.Layout.SMRep
instance ToJSON ClosureTypeInfo
instance ToJSON GHC.Runtime.Heap.Layout.ArgDescr
--instance ToJSON CmmExpr
instance ToJSON MachOp
instance ToJSON FMASign
instance ToJSON AlignmentSpec
instance ToJSON Area
instance ToJSON CmmReg
instance ToJSON GlobalRegUse
instance ToJSON LocalReg

--------------------------------------------------------------------------------
-- DUMMY: tipos con FromJSON MANUAL en TU archivo
-- (incluye además los necesarios para resolver campos internos de las automáticas)
--------------------------------------------------------------------------------

-- LabelMap: tienes FromJSON manual [(Word64,a)] -> LabelMap a
--instance ToJSON a => ToJSON (LabelMap a) where
--toJSON _ = dummyVal "LabelMap dummy instance (no Label→Word64 extractor disponible)"

-- ByteString: FromJSON manual en tu archivo
--instance ToJSON BS.ByteString where
--  toJSON :: BS.ByteString -> Value
--  toJSON _ = dummyVal "ByteString dummy instance"

-- ShortByteString: declaraste FromJSON; damos ToJSON dummy
--instance ToJSON Data.ByteString.Short.ShortByteString where
--  toJSON _ = dummyVal "ShortByteString dummy instance"

-- CLabel: FromJSON manual
--instance ToJSON CLabel where
--  toJSON _ = dummyVal "CLabel dummy instance"

-- Label: FromJSON manual (Word64 -> mkHooplLabel)
--instance ToJSON Label where
 -- toJSON _ = dummyVal "Label dummy instance"

-- CostCentre / CostCentreStack: FromJSON manual/indefinido
--instance ToJSON CostCentre where
--  toJSON _ = dummyVal "CostCentre dummy instance"

--instance ToJSON CostCentreStack where
--  toJSON _ = dummyVal "CostCentreStack dummy instance"

-- Var: FromJSON manual (dummy)
--instance ToJSON GHC.Types.Var.Var where
--  toJSON _ = dummyVal "Var dummy instance"

-- CmmTickish / ForeignTarget: FromJSON dummy
--instance ToJSON CmmTickish where
--toJSON _ = dummyVal "CmmTickish dummy instance"

--instance ToJSON ForeignTarget where
--toJSON _ = dummyVal "ForeignTarget dummy instance"

-- Graph' Block CmmNode C C: FromJSON manual
--instance ToJSON (Graph' Block CmmNode C C) where
--  toJSON _ = dummyVal "Graph' Block CmmNode C C dummy instance"

-- Blocks: FromJSON manual (todas las variantes)
--instance ToJSON (Block CmmNode C O) where
-- toJSON _ = dummyVal "Block C O dummy instance"

--instance ToJSON (Block CmmNode O O) where
-- toJSON _ = dummyVal "Block O O dummy instance"





--instance ToJSON (Block CmmNode O C) where
--  toJSON _ = dummyVal "Block O C dummy instance"

--instance ToJSON (Block CmmNode C C) where
--  toJSON _ = dummyVal "Block C C dummy instance"

-- CmmNode: FromJSON manual (todas las variantes)
--instance ToJSON (CmmNode C O) where
--  toJSON _ = dummyVal "CmmNode C O dummy instance"

--instance ToJSON (CmmNode O O) where
--  toJSON _ = dummyVal "CmmNode O O dummy instance"

--instance ToJSON (CmmNode O C) where
--  toJSON _ = dummyVal "CmmNode O C dummy instance"


-- GenCmmStatics 'False / 'True: FromJSON manual
--instance ToJSON (GenCmmStatics 'False) where
--  toJSON :: GenCmmStatics False -> Value
--  toJSON _ = dummyVal "GenCmmStatics 'False dummy instance"

--instance ToJSON (GenCmmStatics 'True) where
--  toJSON _ = dummyVal "GenCmmStatics 'True dummy instance"

-- Label: FromJSON manual (Word64 -> mkHooplLabel)
--instance ToJSON Label where
--  toJSON _ = dummyVal "Label dummy instance"

-- Unique: FromJSON manual (Word64 -> mkUniqueGrimily)
--instance ToJSON GHC.Types.Unique.Unique where
--  toJSON _ = dummyVal "Unique dummy instance"

-- ByteArray: FromJSON manual (error/dummy)
--instance ToJSON ByteArray where
--  toJSON _ = dummyVal "ByteArray dummy instance"

-- CmmType y auxiliares: FromJSON manual
--instance ToJSON CmmCatOpen where
 --toJSON _ = dummyVal "CmmCatOpen dummy instance"

--instance ToJSON Width where
 --toJSON _ = dummyVal "Width dummy instance"

--instance ToJSON CmmType where
--toJSON _ = dummyVal "CmmType dummy instance"


-- ==============================================
-- ToJSON for Graph' Block CmmNode C C (closed/closed)
-- Mirrors the manual FromJSON that accepts only:
--   { "tag":"GMany"
--   , "contents": [ <_entry :: Block O C>         -- ignored by parser
--                 , <body   :: LabelMap (Block C C)>
--                 , <_exit  :: Block C O> ] }     -- ignored by parser
-- ==============================================
instance ToJSON (Graph' Block CmmNode C C) where
  toJSON :: Graph' Block CmmNode C C -> Value
  toJSON (GMany _ bodyCC _) =
    object
      [ "tag"      .= String "GMany"
      , "contents" .= toJSON (dummyEntryOC, encodeBody bodyCC, dummyExitCO)
      ]
    where
      -- The FromJSON *parses* entry/exit but then discards them for C/C graphs.
      -- We therefore emit minimal, well-formed placeholders that your existing
      -- Block/CmmNode decoders will accept.

      -- Block O C  ≈  { "tag":"BLast", "contents": <CmmNode O C> }
      -- CmmNode O C ≈ { "tag":"CmmBranch", "label": <Word64> }
      dummyEntryOC :: Value
      dummyEntryOC =
        object
          [ "tag"      .= String "BLast"
          , "contents" .= object
              [ "tag"   .= String "CmmBranch"
              , "label" .= (0 :: Word64)
              ]
          ]

      -- Block C O  ≈  { "tag":"BFirst", "contents": <CmmNode C O> }
      -- CmmNode C O ≈ { "tag":"CmmEntry", "label": <Word64>, "scope":"GlobalScope" }
      dummyExitCO :: Value
      dummyExitCO =
        object
          [ "tag"      .= String "BFirst"
          , "contents" .= object
              [ "tag"   .= String "CmmEntry"
              , "label" .= (0 :: Word64)
              , "scope" .= String "GlobalScope"
              ]
          ]

      -- Your FromJSON for LabelMap expects a JSON list of pairs [(Word64, a)].
      -- We don’t currently have a Label -> Word64 projector, so we serialize an
      -- empty body (information-losing but valid). When you expose a projector
      -- (e.g., labelUnique :: Label -> Word64) and a mapToList, replace this.
      encodeBody :: LabelMap (Block CmmNode C C) -> Value
      encodeBody _ = toJSON ([] :: [(Word64, Value)])



-- | Simple test function to verify the module is linked correctly.
serdeTest :: Bool
serdeTest = True