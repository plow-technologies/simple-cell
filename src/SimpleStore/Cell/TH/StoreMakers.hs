{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{- | StateMakers are TH functions that assemble the insert state, delete state and update state pieces of a live DB
     entity.
|-}
module SimpleStore.Cell.TH.StoreMakers (
    allStoreMakers
  ) where

import           Language.Haskell.TH
import           SimpleStore.Cell.DIG

type CellKeyName     = Name
type InitializerName = Name
type StoreName       = Name

allStoreMakers :: [CellKeyName -> InitializerName -> StoreName -> Q Dec]
allStoreMakers = [ makeInitializeXSimpleCell
                 , makeInitializeXSimpleCellAndErrors
                 , makeInsertXSimpleCell
                 , makeDeleteXSimpleCell
                 , makeFoldlWithKeyXSimpleCell
                 , makeTraverseWithKeyXSimpleCell
                 , makeCheckpointsXSimpleCell
                 , makeCreateCheckpointAndCloseXSimpleCell
                 , makeRepsertXSimpleCell
                 , makeGetXSimpleCell
                 ]


-- -- The X represents the position of the incoming type in the filename
makeInitializeXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeInitializeXSimpleCell ckN initN stN = funD (buildInitName stN)
                                             [clause [] (normalB initializeSimpleCellTH ) []  ]
  where
    initializeSimpleCellTH = appE (appE (varE 'initializeSimpleCell ) (varE ckN)) (varE initN)


makeInitializeXSimpleCellAndErrors ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeInitializeXSimpleCellAndErrors ckN initN stN = funD (buildInitWithErrorsName stN)
                                             [clause [] (normalB initializeSimpleCellTH ) []  ]
  where
    initializeSimpleCellTH = appE (appE (varE 'initializeSimpleCellAndErrors ) (varE ckN)) (varE initN)

buildInitName :: StoreName -> Name
buildInitName stN = mkName . concat $ [ "initialize"
                                      , nameBase stN
                                      , "SC"
                                      ]

buildInitWithErrorsName :: StoreName -> Name
buildInitWithErrorsName stN = mkName . concat $ [ "initialize" , nameBase stN , "WithErrorsSC" ]

makeInsertXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeInsertXSimpleCell ckN _initN stN = funD (buildInsertName stN)
                                          [clause [] (normalB insertSimpleCellTH) []  ]
  where
    insertSimpleCellTH =appE (varE 'insertStore ) (varE ckN)

buildInsertName :: StoreName -> Name
buildInsertName stN = mkName . concat $ [ "insert"
                                        , nameBase stN
                                        , "SC"
                                        ]


makeRepsertXSimpleCell :: CellKeyName -> InitializerName -> StoreName -> Q Dec
makeRepsertXSimpleCell ckN _initN stN = funD (buildRepsertName stN)
                                           [clause [] (normalB repsertSimpleCellTH) []  ]
  where
    repsertSimpleCellTH = appE (varE 'repsertStore ) (varE ckN)

buildRepsertName :: StoreName -> Name
buildRepsertName stN = mkName . concat $ [ "repsert"
                                         , nameBase stN
                                         , "SC"
                                         ]


makeDeleteXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeDeleteXSimpleCell ckN _ stN = funD (buildDeleteName stN)
                                     [clause [] (normalB deleteSimpleCellTH) []  ]
  where
    deleteSimpleCellTH =  appE (varE 'deleteStore )
                               (varE ckN)

buildDeleteName :: StoreName -> Name
buildDeleteName stN = mkName . concat $ [ "delete"
                                        , nameBase stN
                                        , "SC"
                                        ]


makeGetXSimpleCell :: CellKeyName -> InitializerName -> StoreName -> Q Dec
makeGetXSimpleCell _ckN _ stN = funD (buildGetName stN)
                                  [clause [] (normalB getSimpleCellTH) [] ]
  where
    getSimpleCellTH = varE 'getStore

buildGetName :: StoreName -> Name
buildGetName stN = mkName . concat $ [ "get"
                                     , nameBase stN
                                     , "SC"
                                     ]

makeFoldlWithKeyXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeFoldlWithKeyXSimpleCell ckN _ stN = funD (buildFoldlWithKeyName stN)
                                           [clause [] (normalB foldlWithKeySimpleCellTH) []  ]
  where
    foldlWithKeySimpleCellTH = appE (varE 'storeFoldrWithKey )
                                    (varE ckN)

buildFoldlWithKeyName :: StoreName -> Name
buildFoldlWithKeyName stN = mkName . concat $ [ "foldlWithKey"
                                              , nameBase stN
                                              , "SC"
                                              ]

makeTraverseWithKeyXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeTraverseWithKeyXSimpleCell ckN _ stN = funD (buildTraverseWithKeyName stN)
                                             [clause [] (normalB traverseWithKeySimpleCellTH) []  ]
  where
    traverseWithKeySimpleCellTH = appE (varE 'storeTraverseWithKey_ )
                                       (varE ckN)

buildTraverseWithKeyName :: StoreName -> Name
buildTraverseWithKeyName stN = mkName . concat $ [ "traverseWithKey"
                                                 , nameBase stN
                                                 , "SC_"
                                                 ]


makeCheckpointsXSimpleCell ::  CellKeyName -> InitializerName -> StoreName -> Q Dec
makeCheckpointsXSimpleCell ckN _ stN = funD (buildCheckpointAllStoresInCellName stN)
                                       [clause [] (normalB checkpointAllStoresInCellTH) []  ]
  where
    checkpointAllStoresInCellTH  = appE (varE 'checkpointAllStoresInCell )
                                        (varE ckN)

buildCheckpointAllStoresInCellName :: StoreName -> Name
buildCheckpointAllStoresInCellName stN = mkName . concat $ [ "checkpoints"
                                                           , nameBase stN
                                                           , "SC" ]

makeCreateCheckpointAndCloseXSimpleCell :: CellKeyName -> InitializerName -> StoreName -> Q Dec
makeCreateCheckpointAndCloseXSimpleCell _ckN _ stN = funD (buildCheckPointAndCloseName stN)
                                                      [clause [] (normalB createCheckpointAndCloseSimpleCellTH) []  ]
  where
    createCheckpointAndCloseSimpleCellTH = varE 'createCellCheckPointAndClose

buildCheckPointAndCloseName :: StoreName -> Name
buildCheckPointAndCloseName stN = mkName . concat $ [ "createCheckpointAndClose"
                                                    , nameBase stN
                                                    , "SC"
                                                    ]
