# simple-cell
Simple Cell takes a filename generating function and a Serializeable datatype
it then generates the machinery to handle the creation of multiple atomic values of this type according to the unique keys of each one.

This avoids the write locking problem in Acid systems by using two separated data ideas for each piece.


## Usage

### Inputs ...

SimpleCells are designed to take a function to retrieve keys and a function to make those keys into filenames

The output of the functions should be unique *SimpleCell* does not check this for you

``` haskell


-- given

-- The user supplies an atomic simple state variable

mySimpleStateVariable = x :: SomeSimpleState


-- define

myRootDir :: FilePath
myRootDir = "./stateSpace"


-- Fill this guy
data CellKey k h s t st = CellKey { getKey :: st -> (DirectedKeyRaw k h s t)
                                  , codeCellKeyFilename :: (DirectedKeyRaw k h s t) -> Text
                                  , decodeCellKeyFilename :: Text -> (DirectedKeyRaw k h s t)
                                  }
                    

$(makeSimpleCell `myCellKey 'initialState ''SomeSimpleState )

```


### Creates ...

Here is what the template haskell call above generates.

``` haskell

-- data types for managing states in the cell both active and dormant
data CellCore k h s t stlive stdormant = CellCore { 
      ccLive :: (M.Map (DirectedKeyRaw k h s t) stlive )
      ccDormant :: (M.Map (DirectedKeyRaw k h s t) stdormant )
    }

type TCellCore k h s t stlive stdormant = TVar (CellCore k h s t (TVar stlive) (TVar stdormant))

-- Generate dig for CellCoreDormant
-- These are not directly availalbe to the user
insertCellSomeSimplePath :: CellCore -> SomeSimpleState -> Update ...
deleteCellSomeSimplePath :: CellCore -> SomeSimpleState -> Update ...
getCellSomeSimplePath    :: CellCore -> SomeSimpleState -> Query ...   

-- DIG structure 
data CellCore  k src dst tm tvlive stdormant = CellCore { 
       ccLive     :: TVar (M.Map (DirectedKeyRaw  k src dst tm) tvlive )
      ,ccDormant :: stdormant
    }


-- UI Functions
insertState :: SimpleCell -> <SomeSimpleState> -> IO (EventResult InsertSimpleCellPathFileKey)
deleteState :: SimpleCell -> DirectedKeyRaw -> IO Bool
getState      :: SimpleCell -> DirectedKeyRaw -> IO (Either SimpleCellError SomeSimpleState)
queryCell   :: SimpleCell -> (SomeSimpleState -> a ) -> IO (Either SimpleCellError (monoid a))

-- updateState
-- deleteWhere


type SimpleCellSomeSimpleState = SimpleCell SomeSimpleState


makeCellSomeSimpleState :: IO SimpleCellSomeSimpleState

``` 

### Use

``` haskell


someStateManip :: IO ()
    acell <- makeCellSomeSimpleState
    drId  <- insertState acell SomeSimpleState
    rslt  <- queryCell acell allConsistentStates 
    ast   <- getState acell drId
    deleteState drId 
    

```

a *DirectedKeyRaw* is a key with a source and a destination.  The source and destination are very arbitrary but were
created to deal with database keys coming from multiple locations.  

