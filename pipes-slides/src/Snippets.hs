{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Snippets where

import Data.String.QQ

import Lucid.Base (Html, toHtmlRaw)

import Language.Haskell.HsColour.HTML
import Language.Haskell.HsColour.Colourise

subduedred = Rgb 182 73 38
lighttan = Rgb 220 207 143
slightorange = Rgb 181 137 0
darkblue = Rgb 88 110 117
slightlylighterbg = Rgb 24 25 20

myColourPrefs = ColourPrefs
  { keyword  = [Foreground subduedred] -- let
  , keyglyph = [Foreground subduedred] -- like =

  , layout   = [Foreground lighttan] -- parentheses, brackets, commas
  , comment  = [Foreground darkblue, Italic]
  , conid    = [Foreground slightorange, Normal] -- class constraints
  , varid    = [Foreground lighttan, Normal] -- function names
  , conop    = [Foreground lighttan] -- like 2:[5], the :
  , varop    = [Foreground lighttan]

  , string   = [Foreground darkblue]
  , char     = [Foreground darkblue]
  , number   = [Foreground darkblue]

  , cpp      = [Foreground lighttan,Dim]
  , selection = [Foreground lighttan]
  , variantselection = [Dim, Foreground Red, Underscore]
  , definition = [Foreground lighttan, Normal]
  }

-- HACK There's no easy way to alter the pre tag that comes from hscolour.
centerPre :: String -> String
centerPre ('<':'p':'r':'e':'>':xs) = "<pre style=\"text-align: center\">" ++ xs
centerPre _ = error "This should not happen"

hcode :: String -> Html ()
hcode = toHtmlRaw . centerPre . hscolour myColourPrefs False 0

hcode_lines :: String -> Html()
hcode_lines = toHtmlRaw . hscolour myColourPrefs True 1

functional = hcode_lines [s|
  process :: String -> String
  process = take 50 . reverse . drop 5 . map toLower
|]

functionalio = hcode_lines [s|
  main :: IO ()
  main = readFile "input.txt" >>= doSomething >>= writeFile "output.txt"
    where doSomething content = undefined
|]

functionaliorepeat = hcode_lines [s|
  main :: IO ()
  main = forM_ [1..10000] $ \i -> do
    let (inf,outf) = ("input" ++ show i ++ ".txt", "output" ++ show i ++ ".txt")
    readFile inf >>= doSomething >>= writeFile outf
    where doSomething content = undefined
|]

copyloop = hcode_lines [s|
  copyloop :: IO ()
  copyloop = withFile "input.txt" ReadMode $ \inf ->
              withFile "output.txt" WriteMode $ \outf ->
                loop inf outf
    where
      loop inf outf = do
        str <- B.hGet inf 4096
        if B.null str
          then return ()
          else do
            B.hPut outf str
            loop inf outf
|]

copyloopwithlogging = hcode_lines [s|
  copyloop :: IO ()
  copyloop = withFile "input.txt" ReadMode $ \inf ->
              withFile "output.txt" WriteMode $ \outf ->
                loop inf outf
    where
      loop inf outf = do
        str <- B.hGet inf 4096
        log str
        if B.null str
          then return ()
          else do
            B.hPut outf str
            loop inf outf

      log :: String -> IO ()
      log = undefined
|]

copyloopwithloggingisinteresting = hcode_lines [s|
  copyloop :: IO ()
  copyloop = withFile "input.txt" ReadMode $ \inf ->
              withFile "output.txt" WriteMode $ \outf ->
                loop inf outf
    where
      loop inf outf = do
        str <- B.hGet inf 4096
        when (isInteresting str) $ log str
        if B.null str
          then return ()
          else do
            B.hPut outf str
            loop inf outf

      log :: String -> IO ()
      log = undefined

      isInteresting :: String -> Bool
      isInteresting str = undefined
|]

copyloopwithloggingisinterestingwithproblem = hcode_lines [s|
  copyloop :: IO ()
  copyloop = withFile "input.txt" ReadMode $ \inf ->
              withFile "output.txt" WriteMode $ \outf ->
                loop inf outf
    where
      loop inf outf = do
        str <- B.hGet inf 4096
        when (isInteresting str) $ log str
         --                  ^ How do we know was delimited correctly?
        if B.null str
          then return ()
          else do
            B.hPut outf str
            loop inf outf

      log :: String -> IO ()
      log = undefined

      isInteresting :: String -> Bool
      isInteresting str = undefined
|]

copyloopwithloggingisinterestingwithsteps = hcode_lines [s|
  copyloop :: IO ()
  copyloop = withFile "input.txt" ReadMode $ \inf -> -- step 1
              withFile "output.txt" WriteMode $ \outf -> -- step 4
                loop inf outf
    where
      loop inf outf = do
        str <- B.hGet inf 4096 -- step 2
        when (isInteresting str) $ log str -- step 3
        if B.null str
          then return ()
          else do
            B.hPut outf str
            loop inf outf

      log :: String -> IO ()
      log = undefined

      isInteresting :: String -> Bool
      isInteresting str = undefined
|]




simpleproducer1 = hcode_lines [s|
  heytwice :: Monad m => Producer String m ()
  heytwice = do
    yield "hey."
    yield "HEY!"
|]



simpleproducer2 = hcode_lines [s|
  getLine :: IO String
  
  stdin :: Producer String IO r
  stdin = forever $ do
    str <- lift getLine :: Producer a IO String
    yield str           :: Monad m => Producer String m ()
|]

simpleconsumer1 = hcode_lines [s|
  countLettersOnce :: Consumer String IO ()
  countLettersOnce = do
    str <- await
    lift . putStrLn . show . length $ str
|]

simpleconsumer2 = hcode_lines [s|
  countLettersTwice :: Consumer String IO ()
  countLettersTwice = do
    countLettersOnce
    countLettersOnce
|]

simpleconsumer3 = hcode_lines [s|
  countLetters :: Consumer String IO r
  countLetters = forever countLettersOnce
|]

simpleconsumer4 = hcode_lines [s|
  hello :: Consumer String IO ()

  hello = forever $ do
    str <- await
    lift . putStrLn $ "Hello " ++ str ++ "!"

  hello = forever $ await >>= lift . putStrLn . (\str -> "Hello " ++ str ++ "!")
|]


firsteffect = hcode_lines [s|
  firstEffect :: Effect IO r
  firstEffect = stdin >-> countLetters
|]
firsteffectend = hcode_lines [s|
  firstEffectTwice :: Effect IO ()
  firstEffectTwice = stdin >-> countLettersTwice
|]



firsteffect_extratypes = hcode_lines [s|
  stdin :: Producer String IO r
  countLetters :: Consumer String IO r
|]

runeffect = hcode [s|
  runEffect :: Monad m => Effect m a -> m a
|]

runeffect_firstexample = hcode_lines [s|
  > runEffect $ stdin >-> countLetters :: IO r
  asdf
  4
  qwerty
  5
  more
  4
  ... and so on
|]


firstpipe = hcode_lines [s|
  import Data.Char (toUpper)

  upperPrint :: Pipe String String IO r
  upperPrint = forever $ do
    str <- await
    lift $ putStrLn (map toUpper str)
    yield str
|]

secondpipe = hcode_lines [s|
  divPipe :: Monad m => Pipe Int Double m r
  divPipe = forever $ do
    num1 <- await
    num2 <- await
    yield $ fromIntegral num1 / fromIntegral num2
|]


{-
pipescategories :: Html ()
pipescategories = [s|
                        Identity     | Composition |  Point-ful
                 +-------------------+-------------+-------------+
respond category |   respond/await   |     />/     |     //>     |
request category |   request/yield   |     \>\     |     >\\     |
   push category |   push            |     >~>     |     >>~     |
   pull category |   pull            |     >+>     |     +>>     |
Kleisli category |   return          |     >=>     |     >>=     |
                 +-------------------+-------------+-------------+

|]
-}
proxydeclaration = hcode_lines [s|
   data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
|]

proxytype = hcode [s|
  data Proxy a' a b' b m r
|]

voidtype = hcode [s|
  type X = Void

   -- Void has no constructor
  data Void
|]

proxydirections = hcode_lines [s|
     Upstream | Downstream
         +---------+
         |         |
     a' <==       <== b'
         |         |
     a  ==>       ==> b
         |    |    |
         +----|----+
              v
              r
|]

yieldtype = hcode [s|
  yield :: Monad m => a -> Producer' a m ()
|]

awaittype = hcode [s|
  await :: Monad m => Consumer' a m a
|]

basicproducer = hcode [s|
  Monad m => Producer a m r
|]


producertype = hcode [s|
  type Producer b = Proxy X () () b 
|]

producertypeapost = hcode [s|
  type Producer' b = forall x' x. Proxy x' x () b m r
|]


basicconsumer = hcode [s|
  Monad m => Consumer a m r
|]


consumertype = hcode [s|
  type Consumer a = Proxy () a () X
|]

consumertypeapost = hcode [s|
  type Consumer' a = forall y' y. Proxy () a y' y m r
|]



basicpipe = hcode [s|
  Monad m => Pipe a b m r
|]

pipetype = hcode [s|
  type Pipe a b = Proxy () a () b 
|]

effecttype = hcode [s|
  type Effect = Proxy X () () X
|]

pulloperator = hcode_lines [s|
  -- The real type of >->
  (>->) infixl 7
    :: Monad m
    => Proxy a' a () b m r
    -> Proxy () b c' c m r
    -> Proxy a' a c' c m r
|]

chainexamples = hcode_lines [s|
   -- Monad m =>
  let pr = _ :: Producer a   m r
      p1 = _ :: Pipe     a b m r
      p2 = _ :: Pipe     b c m Int -- exhaustible
      cs = _ :: Consumer c   m r

  let
    foo :: Producer b m r
    foo = pr >-> p1
    
    foo2 :: Producer c m r
    foo2 = pr >-> p1 >-> p2

    foo3 :: Consumer b m Int
    foo3 = p2 >-> cs

    foo4 :: Effect m Int -- ends when p2 ends
    foo4 = pr >-> p1 >-> p2 >-> cs
|]

pulloperatorsummaryeffect = hcode_lines [s|
  -- For now think of >-> as this type.
  (>->) :: Monad m => Producer b m r -> Consumer b m r -> Effect m r
|]

pulloperatorsummary = hcode_lines [s|
  -- In effect >-> has these types. (Monad m =>)
(>->) :: Producer b m r -> Pipe     b c m r -> Producer   c m r
(>->) :: Pipe   a b m r -> Consumer b   m r -> Consumer a   m r
(>->) :: Pipe   a b m r -> Pipe     b c m r -> Pipe     a c m r
(>->) :: Producer b m r -> Consumer b   m r -> Effect       m r
|]

pipesprelude1 = hcode_lines [s|
  import Pipes.Prelude as P

  -- print each element as it is consumed.
  print :: (MonadIO m, Show a) => Consumer' a m r 

  -- Any foldable into a producer.
  each (Monad m, Foldable f) => f a -> Producer' a m () 
  > runEffect $ each [1,2,3] >-> P.print
  1
  2
  3

  -- Run possibly effectful functions on each input.
  map   :: Monad m => (a -> b)    -> Pipe a b m r 
  mapM  :: Monad m => (a -> m b)  -> Pipe a b m r 
  mapM_ :: Monad m => (a -> m ()) -> Consumer' a m r 

  > runEffect $ each [1,2,3] >-> P.map (+1) >-> P.print
  2
  3
  4
|]

pipesprelude2 = hcode_lines [s|
  -- produces strings from stdin until end of input
  stdinLn :: MonadIO m => Producer' String m () 

  -- pass elements along until condition is met, then end
  take :: Monad m => Int -> Pipe a a m ()
  takeWhile :: Monad m => (a -> Bool) -> Pipe a a m () 

  > runEffect $ P.stdinLn >-> P.takeWhile (/= "quit") >->
      P.mapM_ (print . length)
  foo
  3
  bar
  4
  quit
  >
  -- skip elements until a condition is met
  drop :: Monad m => Int -> Pipe a a m r 
  dropWhile :: Monad m => (a -> Bool) -> Pipe a a m r 

  -- elementwise tests on producers
  null :: Monad m => Producer a m () -> m Bool
  all :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
  any :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
  find :: Monad m => (a -> Bool) -> Producer a m () -> m (Maybe a)
|]

pipesprelude3 = hcode_lines [s|
  findIndex :: Monad m => (a -> Bool) -> Producer a m ()
      -> m (Maybe Int)

  sum :: (Monad m, Num a) => Producer a m () -> m a 

  toList :: Producer a Identity () -> [a]
  toListM :: Monad m => Producer a m () -> m [a]

  zip :: Monad m => Producer a m r -> Producer b m r ->
          Producer' (a, b) m r

  -- among many others.
|]

nexttype = hcode [s|
  next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r)) 
|]

fouridentityfuntions = hcode_lines [s|
  respond :: Monad m => a -> Proxy x' x a' a m a'
  request :: Monad m => a' -> Proxy a' a y' y m a 
  push :: Monad m => a -> Proxy a' a a' a m r 
  pull :: Monad m => a' -> Proxy a' a a' a m r 
|]

yieldawaitasidentity = hcode_lines [s|
  yield = respond :: a -> Proxy x' x a' a m ()
  await = request () :: Proxy () a y' y m a
|]


fileHandleExample = hcode_lines [s|
  import System.IO as IO
  import qualified Data.ByteString as B

  fromFile :: Handle -> Producer B.ByteString IO ()
  -- fromFile' :: Handle -> () -> Proxy x x' () B.ByteString IO ()
  fromFile h = go
    where
      go = do
        eof <- liftIO $ hIsEOF h
        unless eof $ do
          str <- liftIO $ B.hGetSome h 4096
          yield str
          go

  > withFile "foo.txt" ReadMode $ \h ->
      runEffect $ fromFile h >-> P.print
|]

fileHandleExampleAlt = hcode_lines [s|
  fromFile :: Handle -> Int -> Producer B.ByteString IO ()

  fromFile' :: Handle -> () -> Proxy x x' () B.ByteString IO ()
  -- fromFile' :: Handle -> () -> Server () B.ByteString IO ()
  fromFile' h () = go
    where
      go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
          str <- liftIO $ B.hGetSome h 4096
          () <- respond str -- aka yield str
          go


  f (>->) g = (\() -> f) +>> g

  > withFile "foo.txt" ReadMode $ \h ->
      runEffect $ fromFile h +>> P.print
|]

fileHandleExampleReq = hcode_lines [s|
  printStuff :: Client Int B.ByteString IO ()
  printStuff :: Proxy x x' Int B.ByteString IO ()
  printStuff = forever $ do
        request 5 >>= lift . Prelude.print
        request 10 >>= lift . Prelude.print
        request 12 >>= lift . Prelude.print
|]

counterexample = hcode_lines [s|
  counterExample :: Pipe a a IO r
  counterExample = flip evalStateT 0 loop
    where
      loop :: StateT Int (Pipe a a IO r)
      loop = forever $ do
        a <- lift await
        modify (+1)
        count <- get
        liftIO . putStrLn $ "so far: " ++ show count
        lift $ yield a

  testCounter :: IO ()
  testCounter = runEffect $ P.each "abc" >-> counterExample >-> P.print
|]

counterexampleoutput = hcode_lines [s|
>testCounter
so far: 1
'a'
so far: 2
'b'
so far: 3
'c'
|]


counterexample2 = hcode_lines [s|
  import Pipes.Lift as P

  -- instance MonadState s m => MonadState s (Proxy a' a b' b m)

  counterExample :: Pipe a a IO r
  counterExample = P.evalStateP 0 loop
    where
      loop :: Pipe a a (StateT Int IO) r
      loop = forever $ do
        a <- await
        modify (+1)
        count <- get
        liftIO . putStrLn $ "so far: " ++ show count
        yield a

  testCounter :: IO ()
  testCounter = runEffect $ P.each "abc" >-> counterExample >-> P.print
|]

pipeszlib = hcode [s|
  -- for both gzip and zlib
  compress :: MonadIO m
           => CompressionLevel
           -> Producer' ByteString m r
           -> Producer' ByteString m r

  decompress :: MonadIO m
           => Producer' ByteString m r
           -> Producer' ByteString m r
|]


pipesnetwork = hcode [s|
  -- a consumer of data from a socket, inexhaustible
  toSocket :: MonadIO m => Socket ->
               Consumer' ByteString m r

  -- a producer that reads at most n bytes, exhausted if connection closes.
  fromSocket :: MonadIO m => Socket > Int ->
                 Producer' ByteString m ()
|]

pipesnetworkexample = hcode_lines [s|
  import qualified Pipes.Network.TCP as P
  import Pipes.Prelude as P (print)
  import Network.Simple.TCP (serve, HostPreference(..))
  import qualified Data.ByteString.Char8 as B (getLine, putStrLn)

  netExample :: IO ()
  netExample = serve (Host "127.0.0.1") "8000" $ \(sock, _) -> do
      runEffect $ fromSocket sock 4096 >-> doSomething >-> toSocket sock

  stdinExample :: IO ()
  stdinExample = runEffect $ 
    forever (lift B.getLine >>= yield) >-> doSomething >->
      forever (await >>= lift . B.putStrLn)

  -- This function doesn't care how it is used.
  doSomething :: Monad m => Pipe B.ByteString B.ByteString m r
  doSomething = undefined
|]

pipessafeexample = hcode_lines [s|
  import qualified Pipes.Safe as P
  import System.IO as IO

  -- An actual usable version of this fuction in Pipes.Safe.Prelude
  -- is called readFile, along with counterparts writeFile, withFile
  readFileExample :: FilePath -> Producer' String (P.SafeT IO) ()
  readFileExample file = P.bracket
      (do h <- IO.openFile file IO.ReadMode
          putStrLn $ "{" ++ file ++ " open}"
          return h )
      (\h -> do
          IO.hClose h
          putStrLn $ "{" ++ file ++ " closed}" )
      P.fromHandle -- :: Handle -> Producer' String IO ()

  -- mnemonic SafeT stands for Safe Transformer.
  P.runSafeT :: (MonadMask m, MonadIO m) => SafeT m r -> m r 
|]

pipessafeexampleoutput = hcode_lines [s|
  > P.runSafeT $ runEffect $ readFileExample "blah.txt" >-> P.print
  {blah.txt open}
  "file contents!"
  {blah.txt closed}
|]


pipessafemultipleexamples = hcode_lines [s|
  -- can be run from within a Proxy, finalizes without exiting.
  -- mnemonic, P stands for Proxy
  runSafeP :: (MonadMask m, MonadIO m) => Effect (SafeT m) r -> Effect' m r 

  -- same as before
  readFileExample :: FilePath -> Producer' String (P.SafeT IO) ()

  writeOutputExample :: IO ()
  writeOutputExample = runEffect $ do
    P.runSafeP $ (readFileExample "blah.txt") >-> P.print
    P.runSafeP $ (readFileExample "blah2.txt") >-> P.print
|]

pipessafemultipleexamplesoutput = hcode_lines [s|
  >writeOutputExample 
  {blah.txt open}
  "file contents!"
  {blah.txt closed}
  {blah2.txt open}
  "blah2 contents!"
  {blah2.txt closed}
|]

pipessafeerrorfunctions = hcode_lines [s|
class (MonadCatch m, MonadMask m, MonadIO m, MonadIO (Base m)) => MonadSafe m
  type Base (m :: * -> *) :: * -> *

instance MonadSafe m => MonadSafe (Proxy a' a b' b m)
 type Base (Proxy a' a b' b m :: * -> *) :: * -> *


onException :: MonadSafe m => m a -> Base m b -> m a

-- On exception, Pipe ends cleany with a (Left exception)
-- NOTE: catchP and tryP are in pipes-safe 2.2.5 which hasn't hit lts yet.
tryP :: (MonadSafe m, Exception e) =>
  Proxy a' a b' b m r -> Proxy a' a b' b m (Either e r)
catchP :: (MonadSafe m, Exception e) =>
  Proxy a' a b' b m r -> (e -> Proxy a' a b' b m r) -> Proxy a' a b' b m r

finally :: MonadSafe m => m a -> Base m b -> m a

bracket :: MonadSafe m => Base m a -> (a -> Base m b) -> (a -> m c) -> m c
bracket_ :: MonadSafe m => Base m a -> Base m b -> m c -> m c
|]


parsingproblem = hcode_lines [s|
  someText :: Monad m => Producer String m ()

  parseHelloWorld :: forall m. Monad m => Consumer String m (Maybe (String, String))
  parseHelloWorld = do
    hello <- parseString "hello" :: Consumer String m (Maybe String)
    world <- parseString "world" :: Consumer String m (Maybe String)
    return ((,) <$> hello <*> world)

  test :: Monad m => m (Maybe (String, String))
  test = runEffect $ someText >-> parseHelloWorld
|]

parsingproblem2 = hcode_lines [s|
  parseString :: Monad m => String -> Consumer String m (Maybe String)
  parseString str = loop ""

    where
      loop accum = do
        chunk <- await
        let sofar = accum ++ chunk

        -- do we have enough text yet?
        if (length str > length sofar)
          then loop sofar -- no, await more text

          -- yes, test what we have with what we are looking for
          else if take (length str) sofar == str

            then do
              -- what do we do with all the extra text we took?
              giveBackToProducer $ drop (length str) sofar
              return (Just str)
            else do
              -- what if we entirely failed?
              giveBackToProducer sofar
              return Nothing

      giveBackToProducer = undefined
|]

leftovertypes = hcode_lines [s|
  -- Essentially, adds leftover type to its consumer
  Pipe i o m r -> Pipe l i o m r

  -- But with pipes being bidirectional, you'd end up with something like
  data Proxy l1 l2 a' a b' b m r
|]

parsertype = hcode_lines [s|
  type Parser a m r = StateT (Producer a) m r
|]

drawcode = hcode_lines [s|
  import Control.Monad.State as S

  draw :: Monad m => Parser a m (Maybe a)
  draw = do
      p <- S.get
      x <- lift (next p)
      case x of
          Left   r      -> do
              S.put (return r)
              return Nothing
          Right (a, p') -> do
              S.put p'
              return (Just a)
|]

undrawcode = hcode_lines [s|
  unDraw :: Monad m => a -> Parser a m ()
  unDraw a = S.modify (yield a >>)
|]

parsingexample = hcode_lines [s|
  parseString :: Monad m => String -> Parser String m (Maybe String)
  parseString str = loop ""
    where
      loop accum = do
        chunk <- draw -- :: Parser (Maybe String)
        case chunk of
          Nothing -> return Nothing
          Just chunk' -> do
            let sofar = accum ++ chunk'

            -- do we have enough text yet?
            if (length str > length sofar)
              then loop sofar -- no, await more text

              -- yes, test what we have with what we are looking for
              else if take (length str) sofar == str

                then do
                  -- what do we do with all the extra text we took?
                  unDraw $ drop (length str) sofar
                  return (Just str)
                else do
                  -- what if we entirely failed?
                  unDraw sofar
                  return Nothing
|]

parserexecute = hcode_lines [s|
  import Control.Monad.State
  import Pipes.Prelude as P

  -- type Parser a m r = StateT (Producer a) m r
  -- runStateT :: StateT s m a -> m (a,s)
  -- runStateT (undefined :: Parser a m r) :: m (a,s)
  -- parseString :: String -> P.Parser String m (Maybe String)

  parseHelloWorld :: P.Parser String m (Maybe (String, String))
  parseHelloWorld = do
    hello <- parseString "hello"
    world <- parseString "world"
    return ((,) <$> hello <*> world)

  testParse = do
    (res, rest) <- runStateT parseHelloWorld (yield "helloworldblah")
    print res
    runEffect (rest >-> P.print)

  >testParse
  Just ("hello","world")
  "blah"
|]

pipesasync = hcode_lines [s|
  buffer :: (MonadBaseControl IO m, MonadBaseControl IO (Base m), MonadSafe m) =>
  Int ->
  Proxy a' a () b m r ->
  Proxy () b c' c m r ->
  Proxy a' a c' c m r

  -- very similar type to >->
  >&> = buffer 16
|]

pipesasyncexample = hcode_lines [s|
  -- a single threaded program
  runSafeT $ runEffect $ readFile "input.txt" >->
    doSomethingCheap >-> doSomethingExpensiveAgain >->
    writeFile "output.txt"

  -- a concurrent program that does the exact same thing (ignoring interleaved IO effects)
  runSafeT $ runEffect $ readFile "input.txt" >->
    doSomethingCheap >&> doSomethingExpensiveAgain >->
    writeFile "output.txt"
|]

pipesareassociative = hcode_lines [s|
  x >-> (y >-> z)
  -- is equivalent to
  (x >-> y) >-> z
|]

pipesasyncarealmostassociative = hcode_lines [s|
  x >-> (y >&> z)
  -- is almost equivalent to, but the concurrency
  -- occurs differently.
  (x >-> y) >&> z
|]

pipesprocessexample = hcode_lines [s|
>>>
  -- a complicated example (straight from docs) of what can be done.
  test :: IO ExitCode
  test = execute (piped (shell "{ cat ; sleep 1 ; echo eee 1>&2 ; }")) $ 
        (\_ _ o e oe ec -> (o,e,oe,ec)) 
        <$>
        feedBytes (Just "aaa") -- <- first _
        <*> 
        feedBytes (Just "bbb") -- <- second _
        <*> 
        foldOut intoLazyBytes  -- <- o (output)
        <*>
        foldErr intoLazyBytes  -- <- oe (output error)
        <*>
        foldOutErr (combined (PT.lines PT.utf8x) (PT.lines PT.utf8x) PT.intoLazyText)
        -- ^ ec -- output and error combined
        --           ^ combined - combines stdout and stderr
        --                      ^ PT.lines splits takes chunks text into lines
        --                              ^ PT.utf8x encodes bytestring into text
        <*>
        exitCode -- <- return the exit code instead of ()

  > test
  ("aaabbb","eee\n","aaabbb\neee\n",ExitSuccess)
|]

pipesprocesspractical = hcode_lines [s|
  import System.Process.Streaming as PS

  lsServer :: Producer B.ByteString IO () -> Consumer B.ByteString IO ()
              -> IO ExitCode
  lsServer prod cons = do
    let cmd = shell "telnet server.com"
    execute (piped cmd) (PS.foldOut (PS.withConsumer cons) *> PS.feedProducer prod *> exitCode)

  main :: IO ExitCode
  main = lsServer (yield "somecommand\n") (forever $ await >>= lift . print)
|]


monadtypes = hcode_lines [s|
  (>>=) :: Monad m => m b -> (b -> m c) -> m c
  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
|]


actualpulloperator = hcode [s|
  f (>->) g = (\() -> f) +>> g
|]

originalproxies = hcode_lines [s|
  stdin :: Producer' String IO r
  stdin = forever $ do
    lift $ putStrLn "stdin"
    str <- lift getLine
    yield str

  countLetters :: Consumer' String IO r
  countLetters = forever $ do
    lift $ putStrLn "countLetters"
    str <- await
    lift . putStrLn . show . length $ str

  runEffect $ stdin >-> countLetters
  f >-> g = (\() -> f) +>> g

  runEffect $ (\() -> stdin) +>> countLetters
|]

stdinrewritten = hcode_lines [s| 
  stdin' :: () -> Producer String IO b
  stdin' () = do
    lift $ putStrLn "stdin"
    str <- lift getLine
    nil <- respond str -- yield str
    stdin' nil

  pullbasedpipe = runEffect $ stdin' +>> countLetters
|]

countlettersrewritten = hcode_lines [s|
  countLetters' :: String -> Consumer String IO b
  countLetters' str = do
    lift $ putStrLn "countLetters"
    lift . putStrLn . show . length $ str
    str' <- request () -- await
    countLetters' str'

  pushbasedpipe = runEffect $ stdin >>~ countLetters'
|]

pulloutput = hcode_lines [s|
  >runEffect $ stdin >-> countLetters
  countLetters
  stdin
  fdsa
  4
  countLetters
  stdin
  asdf
  4
  countLetters
  stdin
|]

pullvspushoutput = hcode_lines [s|
  >pullbasedpipe 
  countLetters
  stdin
  asdf
  4
  countLetters
  stdin
  ...

  >pushbasedpipe 
  stdin
  asdf
  countLetters
  4
  stdin
  fdsa
  countLetters
  4
  stdin
  ...
|]

{-
-}

