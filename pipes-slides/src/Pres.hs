{-# LANGUAGE OverloadedStrings #-}
module Pres where

import Lib
import Snippets

import Lucid.Base
import Lucid.Html5
import Data.Monoid

presentation :: Presentation
presentation = Presentation [
    Topic [
      slide intro []
    ],
    Topic [

      slide "Motivations" [],
      slide ("We're used to having composability in haskell." <> functional) [],
      slide functionalio [
        slide "This technically works, but memory is unbounded." []
      ],
      slide functionaliorepeat [
        slide "Without care we may exhaust our file descriptors." []
      ],
      slide copyloop [],
      slide copyloopwithlogging [],
      slide copyloopwithloggingisinteresting [],
      slide copyloopwithloggingisinterestingwithproblem [
        slide "Good on memory, good on resources management, but we completely lost composability." []
      ],
      slide ("What if we could break this into composable pieces?" <> copyloopwithloggingisinterestingwithsteps) [],
      slide (copyloopwithloggingisinterestingwithsteps <> "step 1 -> step 2 -> step 3 -> step 4") []
    ],
    Topic [
      slide ("History") [
        slide (li_ "2009 - Everyone has more or less given up on the prospect of lazy IO") [
          slide (li_ "2010 - John Millikin writes the enumerator library") [
            slide (li_ "It proves a basic idea, but the api is... a little janky") [
              slide (li_ "2011 - Both Michael Snoyman and Gabriel Gonzales start conduits and pipes projects") [
                slide (li_ "2012 - Conduits takes the early lead thanks to its more advanced functionality") [
                  slide (li_ "Over time implementations get closer together and they begin to approach feature parity") []
                ]
              ]
            ]
          ]
        ]
      ]
    ],
    Topic [
      slide "Pipes Introduction" [],
      slide ("The only type in the pipes library is the Proxy type" <> proxytype) [
        slide "Every other type in the pipes library is a type alias to Proxy" []
      ]
    ],
    Topic [
      slide "Basic components of a Pipe" [],

      -- TODO next function
      slide ("The Producer" <> basicproducer) [
        slide producertype [],
        slide "produces an effectful series of a's" [],
        slide "may produce none, may produce infinite" [],
        slide "if it finishes, it returns r" [],
        slide nexttype [
          slide "sort of head and tail combined, but doesn't blow up on an empty list" []
        ],
        slide "producers are an instance of Monad, if m is" []
      ],

      slide ("Producers may yield values down the pipe" <> yieldtype) [],
      slide simpleproducer1 [],
      slide simpleproducer2 [
        slide "Note the return value r means it never ends." []
      ]
    ],

    Topic [
      slide  "Basic components of a Pipe" [],
      slide ("The Consumer" <> basicconsumer) [
        slide (consumertype <> producertype) [],
        slide "consumes an effectful series of a's" [],
        slide "again, may consume none, may consume everything forever" []
      ],

      slide ("Consumers may await values from upstream" <> awaittype) [
        slide yieldtype []
      ],
      slide simpleconsumer1 [
        slide simpleconsumer2 [
          slide simpleconsumer3 []
        ]
      ],

      slide "A Producer can never await, a Consumer can never yield" []
    ],
    Topic [
      slide  "Tying Producers and Consumers Together creates an Effect" [
        slide (firsteffect <> firsteffect_extratypes) [
          slide (pulloperatorsummaryeffect) []
        ],
        slide (firsteffect <> div_ "all yields in the producer are tied to the awaits in the consumer") [
          slide (firsteffectend <> "the pipe will run until any component finishes") [],
          slide "thus an Effect can never yield or await" []
        ]
      ],
      slide ("To get back to your base type" <> runeffect) [],
      slide ("To get back to your base type" <> runeffect <> runeffect_firstexample) []
    ],
    Topic [
      slide "Basic components of a Pipe" [],
      slide ("The Pipe" <> basicpipe) [
        slide pipetype [
          slide (producertype <> consumertype) []
        ],
        slide "a pipe can both await AND yield" [],
        slide "practically, a bridge between consumers and producers" []
      ],
      slide "Pipe examples" [
        slide firstpipe [],
        slide secondpipe []
      ]
    ],
    Topic [
      slide "Putting it all together" [],
      slide chainexamples [],
      slide pulloperatorsummary []
    ],
    Topic [
      slide "Side Note: Producers and Consumers are heavily type restricted." [
        slide (producertype <> consumertype) [],
        slide (producertype <> producertypeapost <> consumertype <> consumertypeapost) [
          slide (yieldtype <> awaittype <> pipetype) []
        ]
      ]
    ],
    Topic [
      slide "With this much knowledge you can be very productive with pipes." [],
      slide "Pipes comes with a lot of utilities" [],
      slide pipesprelude1 [],
      slide pipesprelude2 [],
      slide pipesprelude3 []
    ],
    Topic [
      slide ("Since Producer, Pipe and Consumer are actually type aliases" <> producertype <> pipetype <> consumertype <> effecttype <> proxytype) [
          slide "You might be wondering what these extra variables for?" [],
          slide "It turns out pipes are bidirectional if you need them to be" [],
          slide "And in fact a unidirectional pipe is actually bidirectional under the hood" []
      ],
      slide (producertype <> pipetype <> consumertype <> effecttype <> proxytype) [
        slide proxydirections [],
        slide pulloperator [],
        slide "Normally if you import Pipes, you get yield and await." [],
        slide ("But if you import Pipes.Core, you get different primitives." <> fouridentityfuntions) [
          slide yieldawaitasidentity []
        ]
      ]
--      slide fileHandleExample [ -- TODO save for end of presentation
--        slide "What if you want something other than 4096?" []
--      ]
    ],

    -- for, ~> ?
    Topic [
      slide "Pipes can be stateful" [
        slide counterexample [
          slide counterexampleoutput []
        ],
        slide counterexample2 []
      ]
    ],
    Topic [
      slide "There are many Interesting libraries in the pipes ecosystem" [],
      slide "pipes-zlib" [
        slide pipeszlib []
      ],

      slide "pipes-network (with network-simple)" [
        slide pipesnetwork [
          slide pipesnetworkexample []
        ]
      ]
      -- pipes-text, pipes-bytestring
    ],
    Topic [
      slide  "Pipes can handle resource acquisition and release (via pipes-safe)" [
        slide "This is very similar to the resourcet library, they are almost interchangeable" [],
        slide pipessafeexample [
          slide pipessafeexampleoutput []
        ],
        slide pipessafemultipleexamples [
          slide pipessafemultipleexamplesoutput []
        ]
      ],
      slide ("Pipes has a full suite of error handling " <> pipessafeerrorfunctions) []
    ],
    Topic [
      slide "pipes-async (uses pipes-safe)" [],
      slide ("pipes-async exports only two functions" <> pipesasync) [
        slide pipesasyncexample []
      ],
      slide "Note that that pipes are associative" [
        slide pipesareassociative [],
        slide pipesasyncarealmostassociative [
          slide "pipes-async are not quite associative, technically." [],
          slide "Still pretty cool (and useful!)" []
        ]
      ]
    ],
    Topic [
      slide "process-streaming" [],
      slide pipesprocessexample [
        slide "This library has a ton of flexibility" []
      ],
      slide ("Here's an example of something I've personally done with it" <> pipesprocesspractical) []
    ],
    Topic [
      slide "pipes-parse" [],
      slide ("Representing a parser as a consumer seems like a reasonable idea" <> parsingproblem) [],
      slide ("Until you actually try to implement it" <> parsingproblem2) [],
      slide ("Conduits calls this the 'leftover' problem, deals with it on the type level" <> leftovertypes) [],
      slide ("What you really need to do is save the current state of the producer" <> parsertype) [],
      slide (parsertype <> drawcode) [],
      slide (parsertype <> undrawcode) [],
      slide (parsertype <> parsingexample) [],
      slide (parserexecute) []
    ],
    -- pipes-parse
    -- pipes-concurrency

    Topic [
      slide "Who remembers how categories work?" [
        slide ("Remember that to be a category, you must have:") [
          slide (li_ "left and right identity") [
            slide (li_ "associativity") []
          ]
        ]
      ],
      slide (monadtypes <> monadlaws) [],
      slide pipescategories [],
      slide ("Kleisli" <> monadlaws <> "Pipes Pull" <> pulllaws) [],

      slide ("By default a unidirection pipe is pull based") [
        slide actualpulloperator [
          slide pulloutput [],
          slide pipescategories []
        ]
      ],
      slide originalproxies [],
      slide stdinrewritten [],
      slide "How about push instead of pull?" [
        slide countlettersrewritten [],
        slide pullvspushoutput []
      ]
    ],
    Topic [
      slide ("Honorable mentions:") [
        slide (li_ ("pipes-concurrency - an absolutely brilliant concurrent message passing abstraction")) [
        slide (li_ ("pipes-group - abstract over series' of producers (lists of lists)")) [
        slide (li_ ("pipes-text, pipes-bytestring - pipes specialized for text and bytestring")) [
        slide (li_ ("pipes-aeson - encode or decode json on the fly in your pipe")) [
        slide (li_ ("pipes-http - pipes interface for streaming http responses")) [
        slide (li_ ("pipes-attoparsec - create a producer of parsed values from an attoparsec parser")) [
        slide (li_ ("streaming - an alternate to unidirectional pipes also an alternative for pipes-group")) [
        slide (li_ ("and many more")) []
        ]]]]]]]
      ],
      slide "Thank you" [],
      slide intro []
    ]
  ]


intro :: Html ()
intro = h1_ "Pipes" <>
    h4_ [style_ "margin-bottom: 40px"] "The practical use of Gabriel Gonzales's pipes ecosystem" <>
    h5_ ("View slides online at: " <> span_ "https://mindreader.github.io/pipes-slides")

{-
                        Identity     | Composition |  Point-ful
                 +-------------------+-------------+-------------+
respond category |   respond/yield   |     />/     |     //>     |
request category |   request/await   |     \>\     |     >\\     |
   push category |   push            |     >~>     |     >>~     |
   pull category |   pull            |     >+>     |     +>>     |
Kleisli category |   return          |     >=>     |     >>=     |
                 +-------------------+-------------+-------------+
-}

{-
pipetypeexplanation :: Html ()
pipetypeexplanation = table_ [width_ "100%"] $ do
  thead_ $ tr_ $
-}   

pipescategories :: Html ()
pipescategories = table_ [width_ "100%"] $ do
  thead_ $ tr_ $
    th_ "" <> th_ "Identity" <> th_ "Composition" <> th_ "Point-ful" :: Html ()
  tbody_ $ do
    tr_ $ td_ "Respond" <> td_ "respond/yield" <> td_ "/>/" <> td_ "//>" :: Html ()
    tr_ $ td_ "Request" <> td_ "request/(await ())" <> td_ "\\>\\" <> td_ ">\\\\" :: Html ()
    tr_ $ td_ "Push"    <> td_ "push" <> td_ ">~>" <> td_ ">>~" :: Html ()
    tr_ $ td_ "Pull"    <> td_ "pull" <> td_ ">+>" <> td_ "+>>" :: Html ()
    tr_ $ td_ "Kleisli" <> td_ "return" <> td_ ">=>" <> td_ ">>=" :: Html ()


monadlaws :: Html ()
monadlaws = table_ [width_ "100%"] $ do
  tbody_ $ do
    tr_ $ th_ "Left Identity" <> td_ "return >=> g" <> td_ "===" <> td_ "g"
    tr_ $ th_ "Right Identity" <> td_ "f >=> return" <> td_ "===" <> td_ "f"
    tr_ $ th_ "Associativity" <> td_ "(f >=> g) >=> h" <> td_ "===" <> td_ "f >=> (g >=> h)"

pulllaws :: Html ()
pulllaws = table_ [width_ "100%"] $ do
  tbody_ $ do
    tr_ $ th_ "Left Identity" <> td_ "pull >+> g" <> td_ "===" <> td_ "g"
    tr_ $ th_ "Right Identity" <> td_ "f >+> pull " <> td_ "===" <> td_ "f"
    tr_ $ th_ "Associativity" <> td_ "(f >+> g) >+> h" <> td_ "===" <> td_ "f >+> (g >+> h)"
