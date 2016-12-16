data Stack a = NilStack
               | Stiva a (Stack a)
               deriving (Show)

data Queue a = NilQueue
               | Coada a (Queue a)
               deriving (Show)

coadaTest = Coada 2 (Coada 3 (Coada 4 NilQueue))
stivaTest = Stiva 5 (Stiva 6 (Stiva 7 NilStack))

--- Ex 1
emptyStack :: Stack a
emptyStack = NilStack

emptyQueue :: Queue a
emptyQueue = NilQueue

pushStack :: a -> Stack a -> Stack a
pushStack element stiva =  Stiva element stiva

pushQueue :: a -> Queue a -> Queue a
pushQueue element coada = case coada of NilQueue -> Coada element NilQueue
                                        Coada c_element c_queue -> Coada c_element (pushQueue element c_queue)

popStack :: Stack a -> Maybe (a, Stack a)
popStack stiva = case stiva of NilStack -> Nothing
                               Stiva element stiva -> Just (element, stiva)

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue coada = case coada of NilQueue -> Nothing
                               Coada element coada -> Just (element, coada)

--- Ex 2
simulateStack :: Queue a -> Queue a -> Queue a -> Queue a
simulateStack NilQueue resultQueue notQueue = resultQueue
simulateStack (Coada element initQueue) resultQueue notQueue = pushQueue element (simulateStack initQueue resultQueue notQueue)

--- Ex 3
-- a
rev :: Stack a -> Stack a -> Stack a
rev NilStack stiva_2 = stiva_2
rev (Stiva element stiva_1) stiva_2 = rev stiva_1 (pushStack element stiva_2)

-- b
verifica :: Eq a => (Queue a, Stack a) -> Bool
verifica (NilQueue, NilStack) = True
verifica (NilQueue, _)        = False
verifica (_,NilStack)         = False
verifica (Coada e_coada coada, Stiva e_stiva stiva) = (e_coada == e_stiva) && (verifica (coada, stiva))

-- Ex 4
spushQueue :: a -> (Stack a, Stack a) -> (Stack a, Stack a)
spushQueue = undefined

spopQueue :: a -> (Stack a, Stack a) -> (Stack a, Stack a)
spopQueue = undefined

-- Ex 5
buildQueueRec :: String -> Queue Char
buildQueueRec [] = NilQueue
buildQueueRec (element:elemente) = pushQueue element (buildQueueRec elemente)

buildQueue :: String -> Queue Char
buildQueue elemente = simulateStack (buildQueueRec elemente) NilQueue NilQueue

buildString :: Queue Char -> String
buildString NilQueue = []
buildString (Coada element coada) = element : buildString coada

main :: IO String
main = do
  string <- getLine;
  let result = buildString (simulateStack (buildQueue string) NilQueue NilQueue);
  return result;
