{-------------------------------------------------------------------------------

A printing shop runs 16 batches (jobs) every week and each batch requires a
sheet of special colour-proofing paper of size A5.

Every Monday morning, the foreman opens a new envelope, containing a large sheet
of the special paper with size A1.

He proceeds to cut it in half, thus getting two sheets of size A2. Then he cuts
one of them in half to get two sheets of size A3 and so on until he obtains the
A5-size sheet needed for the first batch of the week.

All the unused sheets are placed back in the envelope.

                                --------------
                                |      |     |
                                |      |  A  |
                                |      |  3  |
                                |  A2  |-----|
                                |      |  |A5|
                                |      |A4|--|
                                |      |  |A5|
                                --------------

At the beginning of each subsequent batch, he takes from the envelope one sheet
of paper at random. If it is of size A5, he uses it. If it is larger, he repeats
the 'cut-in-half' procedure until he has what he needs and any remaining sheets
are always placed back in the envelope.

Excluding the first and last batch of the week, find the expected number of
times (during each week) that the foreman finds a single sheet of paper in the
envelope.

Give your answer rounded to six decimal places using the format x.xxxxxx .

--------------------------------------------------------------------------------

The approach is to compute the probability density of the envelop content for
the 14 batches 2 to 15.

-------------------------------------------------------------------------------}

import qualified Data.Map as M
import qualified Data.List as L

-- A State corresponds to the number of sheets of each size in the envelop.
-- Constraint: a State instance must have 4 values. The first one corresponds to
-- the number of A5 sheets, the second one of A4 sheets, ...
type State = [Int]

-- A StateSpace associates each state to its probability of realisation.
type StateSpace = M.Map State Float


-- Given a state, 'stepState' returns all possible outcomes after the batch
-- processing, with the associated probability
stepState :: State -> [(State, Float)]
stepState sheets =
    let nSheets = fromIntegral $ sum sheets
    in [ ((zipWith (+) hs (repeat 1)) ++ (l-1:ls), fromIntegral l / nSheets)
       | (hs, (l:ls)) <- [splitAt i sheets | i <- [0..4]], l > 0]


-- Given a state space, 'stepStateSpace' returns it updated by the batch
-- processing
stepStateSpace :: StateSpace -> StateSpace
stepStateSpace stateSpace = L.foldl' iter M.empty $ M.assocs stateSpace
  where
    iter :: StateSpace -> (State, Float) -> StateSpace
    iter stateSpace (state, p)
      = L.foldl' update stateSpace $ stepState state
      where
        update ::  StateSpace -> (State, Float) -> StateSpace
        update stateSpace (state, p') = M.insertWith (+) state (p*p') stateSpace


euler = sum $ singleSheetPropaPerDay
  where
    initialState = ([1, 1, 1, 1], 1)

    -- The state space for the 14 batches in the middle
    allStates = take 14 $ iterate stepStateSpace $ M.fromList [initialState]

    singleSheetPropaPerDay = map ( L.foldl' (\a (_, b) -> a+b) 0
                                 . discardMultiSheets
                                 . M.assocs) allStates

    discardMultiSheets = filter (\ (s, _) -> sum s == 1)

round6 x = fromIntegral (round $ x * 1000000) / 1000000

main = putStrLn $ concat ["Euler 151: ", show $ round6 euler, "\n" ]