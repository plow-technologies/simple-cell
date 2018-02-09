[![Build Status](https://jenkins.plowtech.net/buildStatus/icon?job=simple-cell)](https://jenkins.plowtech.net/view/master/job/simple-cell/)
# simple-cell
Simple Cell takes a filename generating function and a Serializeable datatype
it then generates the machinery to handle the creation of multiple atomic values of this type according to the unique keys of each one.

This avoids the write locking problem in Acid systems by using two separated data ideas for each piece.




# Working with SimpleCell

SimpleCells are designed to take a function to retrieve keys and a function to make those keys into filenames.

Then it maintains a keyvalue pairing of filename to `SimpleStore`.  This allows you to do non-locked lookup of
entities in a cell.  See the tests for a worked example of this process.

Below is a short example derived from those tests using: `CellKeyStore` as the type to be stored.

By convention, simple-cell use a type suffixed with 'Store'  either as a newtype wrapped entity or as an entity with
DB specific properties (like external keys). 
eg:
```haskell
 
newtype Sample = Sample { getSample :: [Fields]}
 
data SampleStore = SampleStore {
           sampleKey   :: !Int 
           sampleValue :: !Maybe Sample
           } 
    deriving (Show,Eq,Generic)


```
Again for a worked example [look at the tests](https://github.com/plow-technologies/simple-cell/blob/master/test/TestImport.hs) 

## SimpleCell Workflow

1. Generate your functions

2. initialize the store in IO

3. Either pass the cell as an argument to any needed function or derive some other method of getting a global cell (ReaderT 
   for instance).
   
4. make inserts with `insert<YourType>SC` 
5. batch operations are done with `foldlWithKeySampleSC`
6. getStores out of the Cell with getSampleSC.  Use a dummy value to grab something by key


``` haskell

type SampleStoreCell =  SimpleCell SampleKey SampleSrc SampleDst SampleTime SampleStore (SimpleStore SampleStore)

awayServerExample :: SampleStoreCell ->  Application
awayServerExample ... -- Some servant app

-- Some example routines
queryParamH :: SampleStoreCell -> Int -> EitherT ServantErr IO SampleStoreCell
queryParamH cell key = do
  maybeSample <- liftIO $ getSampleStoreSC cell (SampleStore key Nothing)
  case maybeSample of
    Nothing -> sendError "sample not found"
    (Just s) -> return s


repsertParamH :: SampleStoreCell -> Sample -> Int -> Either ServantErr IO ()
repsertParamH cell sample key = do
  maybeSample <- liftIO $ getSampleStoreSC cell (SampleStore key Nothing)
  case maybeSample of
    Nothing -> liftIO $ insertSampleStoreSC cell (SampleStore key sample)
    (Just store) -> modifySimpleStore (\_ -> (SampleStore key sample) ) store >>
                    createCheckpoint store
  
main = do
  cell <- initializeSampleSC "testSampleStoreCell" 
  serve $ awayServerExample cell


```

# Relevant Types:

+  DirectedKeyRaw
+  CellKey
+  SimpleCell
+  SimpleStore

## DirectedKeyRaw

The `DirectedKeyRaw` is usually the most confusing type.  Mostly because there are so many fields,
It is called a *directed key* because it has a source and a destination field which imply a direction.

``` haskell
data DirectedKeyRaw skey src dst datetime = DKeyRaw {  getSimpleKey :: !skey,
                                                       getSource    :: !src,
                                                       getDest      :: !dst,
                                                       getDateTime  :: !datetime}
  deriving(Eq,Ord,Generic)

-- Also instances for Hashable and serialize

```
The `getDateTime` field refers to the date and time of the serialization instance.  At some point we will rename this to 
`getVersionNumber`.  


## CellKey 

The `CellKey` gives the implementation for lookup, encoding and decoding of a DirectedKey.


``` haskell
data CellKey k src dst tm st  = CellKey { getKey                :: st -> DirectedKeyRaw k src dst tm,

                                          codeCellKeyFilename   :: DirectedKeyRaw k src dst tm -> Text,
                                          
                                          decodeCellKeyFilename :: (Text -> Either Text (DirectedKeyRaw k src dst tm) })

```


You are welcome to define your own custom encoding and decoding functions or use the ones provided by the
[directed key package](https://github.com/plow-technologies/directed-keys/blob/master/src/DirectedKeys.hs).

Namely, `encodeKey` and `decodeKey` from `directed-key` plus `decodeUtf8` and `encodeUtf8` provided by the `text` package.


## SimpleCell 

The SimpleCell is not generally used directly.  Instead it is accessed by the template haskell generated functions.

However, what it contains is: 

``` haskell
data SimpleCell  k src dst tm stlive stdormant = SimpleCell {
      cellCore     :: !(TCellCore  k src dst tm (SimpleStore stlive) stdormant )
    , cellKey      :: !(CellKey  k src dst tm stlive)
    , cellParentFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir
    , cellRootFP   :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir/openLocalStateFromdir
    }
   deriving (Typeable,Generic)
```

* The `cellCore` is the internal in-memory representation of the map from keys to simple-stores.
* The `cellKey` is the one provided by the user. 
* The `cellParentFP` is the file path that the Root of the project is in
* The `cellRootFP` is the file path that the cell occupies

## SimpleStore

The [SimpleStore](https://github.com/plow-technologies/simple-store) type is a location 
for an in-memory store of any serializable haskell type.  It produces fast writes and 
very durable data storage.  Multiple backup versions of a store are kept.

The important functions to access it are the `modifySimpleStore` and `updateSimpleStore`
which are described in its documentation.


# Usage guidelines for fast cells

Simple Cell can be thought of as a persistence layer of a write optimized database.  
However, it leaves it up to the user to make decisions as to how to use this layer.

This can cause problems but here are a few guidelines.


## Checkpoints take time
Many systems can take over a second to do a checkpoint on a file.  

If your writes are coming in at a faster pace than that you will need to tune the OS in order to get per modification 
disk guarantees. 

One very good option is to use the `creatCheckpointsXSC` function, which will checkpiont every store in your cell. 
You can set this up in its own thread and then have your writes work asynchronously to the saving.  This isn't always ideal
but often is a great way of handling the problem of disk access. 

The createCheckpointsXSC happens synchronously so a good way to call it is ...
``` haskell 
forever (createCheckpointsXSC yourSimpleCell >> someDelay)
```

