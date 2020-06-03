data StructPath =
    SField String
  | SIx MIndex
--  | SList [SField] -- Consider this for later, would require merging and likely runtime errors

data StructTerm =

-- | Convenience tool to automate extraction of MATLAB datastructures.
mxGetPath :: (MXArrayComponent a, MXArrayComponent b)
  => [StructPath] -> MXArray a -> MIO (Either String (MXArray b))
mxGetPath pathCur:pathRest arr = do
mxGetPath pathCur:[] arr = do

-- TODO - how to dwe deal with the type of non-MXArray vs MXArray in retrievals?
-- TODO: maye a new class that includes both options?

mxGetPathPart :: (MXArrayComponent a) => StructPath -> MXArray a -> MIO (Either String MAnyArray)
mxGetPathPart (SField field) arr = do
mxGetPathPart (SIx ix) arr = do
