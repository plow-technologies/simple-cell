{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable #-}

{-| 
    This module defines the template haskell interface to Data.Cell which actually generates the types we will use
|-} 

module SimpleStore.Cell.TH ( 
    makeStoreCell
  ) where

--Meta 
import Language.Haskell.TH

-- Controls
import Data.Traversable

-- Component Libraries
import SimpleStore.Cell.TH.StoreMakers

-- | use this function to make your acid Cell 
-- > $(makeAcidCell 'yourCellKey 'emptyState 'SomeTarget)
makeStoreCell :: Name -> Name -> Name ->  Q [Dec]
makeStoreCell ckN initN stN = do 
  Data.Traversable.sequence $ allStoreMakers <*> [ckN] <*> [initN] <*> [stN]
