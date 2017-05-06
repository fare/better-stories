#lang at-exp racket @; -*- Scheme -*-
#|
Better Stories, Better Languages - 10' presentation at LambdaConf 2017-05-xx

To compile it, use:
  racket fare-lambdaconf2017.rkt > fare-lambdaconf2017.html

This document is based on a previous talk:
  http://fare.tunes.org/computing/bal2009.ss

This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html

|#

(require scribble/html)
(require net/url)

;; http://docs.racket-lang.org/scribble/extra-style.html

;; Reveal and new html stuff
(define/provide-elements/not-empty section video) ; more tags here
(define-values [get-slides slide]
  (let ([slides '()])
    (values (λ () (reverse slides))
            (λ stuff (set! slides (cons (apply section stuff) slides))))))

;; Quick helpers
(define-syntax-rule (defcodes lang ...)
  (begin (define (lang . text) (pre (code class: 'lang text)))
         ...))
(defcodes scheme javascript haskell)

(define (pic-url name url)
  (let ((file (string-append "resources/pic/" name)))
    (unless (file-exists? file)
      (define out (open-output-file file #:exists 'truncate))
      (call/input-url (string->url url)
                      get-pure-port
                      (λ (in) (copy-port in out)))
      (close-output-port out))
    file))

(define (reveal-url . text)
  ;; (cons "http://cdn.jsdelivr.net/reveal.js/3.0.0/" text)
  (cons "resources/reveal/" text))

(define (L . x) (div align: 'left x))
(define (t . x) x)
(define (C . x) (div align: 'center x))
(define (CB . x) (C (b x)))

(define (url x) (a href: x (tt x)))
(define (comment . x) '())

(define (image name url . size)
  (img src: (pic-url name url) alt: name height: (if (empty? size) "75%" size)))

(define *blue* "#0000ff")
(define *light-blue* "#b4b4ff")
(define *red* "#ff0000")
(define *light-red* "#ffb4b4")
(define *green* "#00ff00")
(define *light-green* "#b4ffb4")

(define ~ @p{ })

(define (bg-colorize color text) (span style: (list "background-color:" color ";") text))

(define (spacing* l (space (br)))
  (cond
    ((null? l) (list space))
    ((pair? l) (append (list space)
                       (if (pair? (car l)) (car l) (list (car l)))
                       (spacing* (cdr l))))
    (else (error 'spacing*))))

(define (spacing l)
  (if (list? l)
      (cdr (spacing* (filter-not null? l)))
      l))

(define (aa x) (λ (y) (tr (th x) (td border: 'below (spacing y)))))
(define Issue (aa "Issue"))
(define Story (aa "Story"))
(define Solution (aa "Solution"))
(define Tools (aa "Tools"))

(define (bg-slide text fgcolor bgcolor)
  (λ x
    (slide
     data-background: bgcolor
     (spacing x)
     (div align: 'right valign: 'bottom style: (list "color:" fgcolor ";") text))))


(define sad-slide  (bg-slide "Sad..." *red* *light-red*))
(define rad-slide  (bg-slide "Better!" *blue* *light-blue*))
(define krad-slide (bg-slide "Even better!" *green* *light-green*))
(define (x-slide . x) (slide (spacing x)))

(define (zad-slide slide #:question question #:issue issue #:story story #:solution solution)
  (slide (h1 question)
         (table
          frame: 'below
          (Issue issue)
          (Story story)
          (Solution solution)
          (tr (td) (td " "))
          )))

;; TODO: write a match expander for runtime destructuring of arguments

(define (xad-slide #:sad-issue sad-issue #:sad-question sad-question
                   #:sad-story sad-story #:sad-solution sad-solution
                   #:rad-issue rad-issue #:rad-question rad-question
                   #:rad-story rad-story #:rad-solution rad-solution
                   #:krad-issue (krad-issue #f) #:krad-question (krad-question #f)
                   #:krad-story (krad-story #f) #:krad-solution (krad-solution #f))
  (zad-slide sad-slide #:question sad-question #:issue sad-issue #:story sad-story #:solution sad-solution)
  (zad-slide rad-slide #:question rad-question #:issue rad-issue #:story rad-story #:solution rad-solution)
  (when krad-issue (zad-slide krad-slide #:question krad-question #:issue krad-issue #:story krad-story #:solution krad-solution)))

(x-slide
  @h1{Better Stories, Better Languages}
  @CB{What Would Alyssa P. Hacker Do?}
  @p{}
  @p{François-René Rideau, @em{TUNES Project}}
  @p{}
  @p{LambdaConf 2017, 2017-05-xx}
  @url{http://github.com/fare/better-stories}
)

(x-slide
 @h1{Stories}
 (table
  (tr
   (td (image "gone-with-the-wind.jpg" "http://static.rogerebert.com/uploads/movie/movie_poster/gone-with-the-wind-1939/large_lqPnvmaX4oZY9teAOT7M0txCLkS.jpg" "40%"))
   (td (image "tosca.jpg" "https://s-media-cache-ak0.pinimg.com/originals/d4/cf/34/d4cf34cdac1191dcb38a0a1a61a0069b.jpg" "28%"))
   (td (image "1984.jpg" "https://yifymovie.re/images/bposter/1984-1984-movie-poster.jpg" "50%"))
   (td (image "real-genius.jpg" "https://i.jeded.com/i/real-genius.16835.jpg" "24%")))
  (tr
   (td (image "holy-bible.jpg" "https://thewrittenwordreviews.files.wordpress.com/2009/03/holy-bible-cover.jpg" "10%"))
   (td (image "decline-and-fall.jpg" "https://images-na.ssl-images-amazon.com/images/I/51LaFDYYMGL._SX362_BO1,204,203,200_.jpg" "40%"))
   (td (image "gulag-archipelago.jpg" "https://images-na.ssl-images-amazon.com/images/I/41LZ%2BGzRwjL._AC_UL320_SR206,320_.jpg" "80%"))
   (td (image "the-importance-of-being-earnest.jpg" "http://1tsp5vpomo32awvuo47ta7f1.wpengine.netdna-cdn.com/wp-content/uploads/2017/03/the-importance-of-being-earnest-cover.jpg" "28%"))))
  @comment{
Hi. I'm Faré and I love storytelling.

Who here likes stories: novels, movies, comic books, theater, opera?
[Pic: a collage of famous covers of novels or comic books, of famous movie, theater or opera posters:
 Real Genius, etc.]

Who doesn't like *any* kind of stories? Not even historical or anecdotal stories? [Wow.]
Well, [despite exceptions,] I see a trend.

The human mind is verily attuned to stories, it craves them,
because we like to explain our world in term of stories.
And not just the world, but our role in the world.
})

(slide
  @h1{Universal Stories}
  (let ((theme-pic
         '(("Boy meets girl" "lovelace-babbage.jpg" "https://images-na.ssl-images-amazon.com/images/I/91FCUHSgEAL.jpg" "10%")
           ("Eating the forbidden fruit" "alan-turing.jpg" "http://i.dailymail.co.uk/i/pix/2014/12/11/066267B800000514-0-The_famous_early_computer_inventor_and_war_hero_Alan_Mathison_Tu-m-17_1418322993327.jpg" "60%")
           ("Novice grows into Master" "torvalds.jpg" "https://www.linux.com/sites/lcom/files/gallery/Linux%20Beer%20Use.jpg" "40%")
           ("Horror Hidden at Home" "heartbleed.jpg" "https://www.eff.org/files/2014/04/10/heartbleed-01-sm_0.jpg" "66%"))))
    (table
     (tr (map (λ (x) (td width: "25%" (first x))) theme-pic))
     (tr (map (λ (x) (td valign: "top" (apply image (rest x)))) theme-pic))))
  @comment{
There are many common stories so general that they can apply in any kind human situation.

I am not going to discuss those stories today.
})

@slide{
 @h1{Programming Stories}
 @image["Creation_Machine.jpg" "https://cdn.searchenginejournal.com/wp-content/uploads/2015/07/shutterstock_28130593-1.jpg"]
@comment{
  Today I want to discuss stories specifically about programming.
}}

(x-slide
 @h1{The Take Home Points}
 @L{Software tools imply a story} @comment{Fourier Transform between software and stories}
 @L{New stories can help invent new tools}
 @L{Explicit stories are a great meta-tool...}
 @comment{
I want to show you that the stories we tell *matter*.

that they affect what we do, and the outcome of what we do.
I want to show you that we do tell stories, even when we're not aware of them.
I want to show you that some stories lead to better outcomes than others.
})

(x-slide
 @h1{This Talk}
 @p{Take a @bg-colorize[*light-red*]{sad so-o-ong},
            and make it @bg-colorize[*light-blue*]{be-e-etter}}
 @p{Let's start with easy ones you already know...})

(xad-slide
 #:sad-question "How to fund software?"
 #:sad-issue "Software costs money to produce"
 #:sad-story '("Software is scarce, owned and sold" "Vendors & customers (static)")
 #:sad-solution '("Proprietary Software" "Closed binaries") ;; unmaintainable by anyone but the vendor, if interested
 #:rad-question "How to fund programming?"
 #:rad-issue "Starved programmers don't code"
 #:rad-story '("Labor is scarce, owned and sold" "Contributors & users (dynamic)")
 #:rad-solution '("Free Software" "Open Source")) ;; shaped into maintainability by shared maintenance

(x-slide
 @h1{@q{I disagree!}}
 @L{It's OK to be wrong (for you, for me)}
 @L{Maybe one story isn't @em{always} better}
 @L{Can we agree that the story usually @em{matters}?}
 @L{Slightly different stories lead to vastly different outcomes}
 @comment{
 })

(xad-slide
 #:sad-question "Decompose programs?" ; (how to...)
 #:sad-issue "Software doesn't fit in one brainful"
 #:sad-story '("Hierarchical design into components" "by fully informed designer")
 #:sad-solution '("Top-down management" "UML diagrams")
 #:rad-question "Decompose programming?"
 #:rad-issue "Many programmers must cooperate"
 #:rad-story '("Partially informed programmers" "Growing a network of projects")
 #:rad-solution '("Software distributions" "Distributed version control"))

(xad-slide
 #:sad-question "Achieve great software?" ; (how to...)
 #:sad-issue "Software is hard to design"
 #:sad-story '("Disseminate expert information"
               "Restrict each component to best experts")
 #:sad-solution '("Standards" "Segregation by expertise") ;; Conway's Law
 #:rad-question "Foster better programming?"
 #:rad-issue '("Do our best, compete with others") ;; learn from our and their successes and failures
 #:rad-story '("Experience through experiments" ;; experience as an output, rather than expertise as an input
               "Cultivate good incentives") ;; information isn't the limiting factor
 #:rad-solution '("Select from abundant market" "Learn in communities"))

(xad-slide
 #:sad-question "Make a Device Programmable?" ; (how to...)
 #:sad-issue "Expose the device's features to a PL"
 #:sad-story '("PLs are for machines")
 #:sad-solution '("Match PL features to device capabilities"
                  "Otherwise random Turing tar pit")
 #:rad-question "Express programming ideas?"
 #:rad-issue "Convey all meanings of the humans"
 #:rad-story '("PLs are for humans")
 #:rad-solution '("Match PL structure to human cognition" ;; and social processes
                  "Simpler programming languages"))

(xad-slide
 #:sad-question "Handle repetitive programs?" ; (how to...)
 #:sad-issue "Lots of repetition in programs"
 #:sad-story '("Programmer as grunt worker" "Language as a given")
 #:sad-solution '("Theorize repetitions as Design Patterns" "More programmers, more drudge") ;; manually enforce consistency
 #:rad-question "Remove programming drudge?"
 #:rad-issue "Drudge in programming" ;; "I object to doing things that computers can do." — Olin Shivers
 #:rad-story '("Programmer as abstract thinker" "Language as a platform")
 #:rad-solution '("Metaprograms" "PL extensibility (macros…)")) ;; Turing's theorem is based on metaprograms!

(xad-slide
 #:sad-question "Have an extensible syntax?" ; (how to...)
 #:sad-issue "Hooks into existing syntax" ;; assuming we want extensibility
 #:sad-story '("Side-effect the One True Syntax" "") ;; as in Common Lisp
 #:sad-solution '("Bind symbols to macros" "Dynamic readtable")
 #:rad-question "Explore useful syntaxes?"
 #:rad-issue "Best express each program fragment"
 #:rad-story '("Exploration of many syntaxes" "")
 #:rad-solution '("Scoped syntax specification" "Racket languages, OMeta"))

(xad-slide
 #:sad-question "Users ≠ Programmers" ; (how to address the fact that...)
 #:sad-issue "Users and Programmers differ"
 #:sad-story '("Dumbed down UI for Users" "All-Power for Devs (in VM)")
 #:sad-solution '("Unrelated UI and PL" "Segregation")
 #:rad-question "Using = Programming"
 ;; The difference between a programmer and a user, is that
 ;; the programmer knows there is no difference between using and programming. — Faré
 #:rad-issue "One language, spoken or written"
 #:rad-story '("Computer interaction is programming" "Continuum of proficiency in users")
 #:rad-solution '("Integrated interactive interface" "Language levels and dialects"))

(xad-slide
 #:sad-question "Programmer ≠ PL Implementer" ; (how to address the fact that...)
 #:sad-issue "Writing a compiler is hard" ;; a correct one even worse
 #:sad-story '("Specialists write compiler" "Mere programmers use")
 #:sad-solution '("Closed implementation for each PL" "")
 #:rad-question "Programming = PL Implementing" ; Programming *is* implementing the language spoken by the users!
 #:rad-issue "Incremental DSL development" ; only hard if not done from scratch
 #:rad-story '("Special case of Using = Programming" "P implements PL spoken by users")
 #:rad-solution '("Support for DSL" "First-class implementations")) ; PCLSRing

(xad-slide
 #:sad-question "PL Definer ≠ PL Implementer" ; (how to address the fact that...)
 #:sad-issue "Designing a language is hard" ; once again, only for experts
 #:sad-story '("Specialists define big language" "Others implement")
 #:sad-solution '("Standard for language" "Decades-old design") ; blind spot, slow update cycle, bit rot
 #:rad-question "PL Defining = PL Implementing"
 #:rad-issue "Specifying Semantics = Implementing"
 #:rad-story '("Declarative specification" "Orthogonal implementation strategies")
 #:rad-solution '("Grammatical mixins" "Monadic lifting"))

(xad-slide
 #:sad-question "Get a specialized language?" ; (how to...)
 #:sad-issue "SW involves heterogeneous activities"
 #:sad-story '("Each domain its experts" "Segregation of experts")
 #:sad-solution '("External DSLs" "Scripting languages")
 #:rad-question "Specialize conversation?"
 #:rad-issue "SW "
 #:rad-story '("One brain, many topics" "Adapt PL to domain")
 #:rad-solution '("Internal DSLs" "Universal PL, many contexts"))

(xad-slide
 #:sad-question "Get Programs Debugged?" ; (how to...)
 #:sad-issue "Programs have bugs, need be fixed"
 #:sad-story '("Bug is an exceptional situation" "Ad-hoc tools retrofitted")
 #:sad-solution '("Low-level debugger" "Ad-hoc debugging information")
 #:rad-question "Exploring Program Semantics?"
 #:rad-issue "Programming isn't obvious, needs exploring"
 #:rad-story '("Imperfect is the default situation" "Environment for exploration")
 #:rad-solution '("Compiler as reversible lens" "Experiment in Virtual World"))

(xad-slide
 #:sad-question "Secure existing software?" ; (how to...)
 #:sad-issue "Security is a hard, requires specialists"
 #:sad-story '("Security as afterthought"
                "Security by independent experts")
 #:sad-solution '("Forever patch leaks" "Low-level protection")
 #:rad-question "Build software securely?"
 #:rad-issue "Security is intrinsic to SW design"
 #:rad-story '("Design with security from start" "Educate programmers")
 #:rad-solution '("Whole-system protocol design" "Capabilities"))

(xad-slide
 #:sad-question "Dealing with catastrophes?" ; (how to...)
 #:sad-issue "Bad manipulations cause data loss"
 #:sad-story '("Error is exceptional, catastrophic" "")
 #:sad-solution '("Confirm menus, remove bin" "Undo last (few)") ;; programmer-intensive add-ons
 #:rad-question "Eliminating catastrophes?"
 #:rad-issue "Make bad manipulations unexpressible"
 #:rad-story '("Error is normal, casual" "")
 #:rad-solution '("monotonic storage never loses data"
                   "Infinite undo")) ;; system-provided default

(xad-slide
 #:sad-question "Document software interfaces?" ; (how to...)
 #:sad-issue "PL can't formalize SW intention"
 #:sad-story '("Document what can't be expressed" "PL as a given")
 #:sad-solution '("Informal contracts" "between heterogenous teams")
 #:rad-question "Agree on Responsibilities?"
 #:rad-issue "Formalism has costs and benefits"
 #:rad-story '("Formalize contracts" "PL extended as needed")
 #:rad-solution '("Better contracts and types"
                   "Cost benefit analysis")) ;; if you can afford the testing that went into SQLite, you can afford proofs.

(xad-slide
 #:sad-question "Arbitrate Resource?" ; (how to...)
 #:sad-issue "Need invariants on shared resources"
 #:sad-story '("Central dictator needed" "Schedule resource possession")
 #:sad-solution '("(OS or App) Kernel" "Static set of resources")
 #:rad-question "Resolve Conflicts?"
 #:rad-issue "Identify owner of resource bundles"
 #:rad-story '("Self-enforcing contracts"
                "Linear logic expresses ownership")
 #:rad-solution '("Invariant-enforcing linker" "Dynamic resource bundles"))

(xad-slide
 #:sad-question "Connect Computers?" ; (how to...)
 #:sad-issue "Single machine can't do it all" ; both technical and social limits
 #:sad-story '("Putting together many systems" "")
 #:sad-solution '("Remote method invocation" "Shipping information around")
 #:rad-question "Distribute Computation?"
 #:rad-issue "System is made of many machines"
 #:rad-story '("One system, many agents" "")
 #:rad-solution '("Declarative deployment" "Content-based addressing"))

(xad-slide
 #:sad-question "Handle mistrust?" ; (how to...)
 #:sad-issue "Need protection barriers"
 #:sad-story '("Kernel-managed domains" "Expensive rigid model")
 #:sad-solution '("processes, containers, virtual PCs" "Expensive and inexpressive")
 #:rad-question "Express limited trust?"
 #:rad-issue "Dynamic bundles of capabilities"
 #:rad-story '("Everyone "root" in own VW" "Recursively so, by default")
 #:rad-solution '("PL support for VW" "Cheap to create sub-user"))

(xad-slide
 #:sad-question "Persist important data?" ; (how to...)
 #:sad-issue "Important data must persist" ; against HW/SW failure
 #:sad-story '("Programmer must manually persist data"
                "Transient by default")
 #:sad-solution '("Ad-hoc filesystems, databases" "Explicit I/O")
 #:rad-question "Write persistent software?"
 #:rad-issue "All data is important"
 #:rad-story '("Why else program about it?"
                ;; You don't care when memory is spilled from cache to RAM,
                ;; why care when it's spilled from RAM to disk?
                "Persistence by default") ;; Transients for performance
 #:rad-solution '("Orthogonal persistent"
                   "Implicit support in PL"))

(xad-slide
 #:sad-question "Model a changing world?" ; (how to...)
 #:sad-issue "Mutations happen — Object-Oriented"
 #:sad-story '("Data is always mutable... by others!"
                "Can't trust anything or anyone")  ;; live in a world of fear
 #:sad-solution '("Imperative programming"
                   "Locks: expensive temporary protection")
 #:rad-question "Model changes to the world?"
 #:rad-issue "Transforms compose — Value-Oriented"
 #:rad-story '("Immutable values, at least by default"
                "Can always reason about programs")
 #:rad-solution '("Functional PL (OCaml, Haskell)" ;;  Purity by default, at base-level, at meta-level, too... Unlambda!
                   "Effects or Monads") ;; Problem: too much or too little
 #:krad-question "Discuss relevant change?"
 #:krad-issue "First-class change — Change-Oriented"
 #:krad-story '("State is meta-level modularity"
                 "Mutable vs immutable views on code")
 #:krad-solution '("Differentiation and Integration"
                    "Switch view to/from Monadic style"))

#;
(xad-slide
 #:sad-question "x?" ; (how to...)
 #:sad-issue ""
 #:sad-story '("" "")
 #:sad-solution '("" "")
 #:rad-question "y?"
 #:rad-issue ""
 #:rad-story '("" "")
 #:rad-solution '("" ""))

(x-slide
 @h1{The Grand Challenge}
 @p{None of these Stories is revolutionary} ;; From The Mother of All Demos...
 @p{Each has been foretold in past systems} ~ ;; Implemented, though not always optimized and productized
 @p[class: 'fragment]{But no @em{system} embodies them all at once} ;; Opportunity!
 @p[class: 'fragment]{Missing: @em{vision}, not technical ability})

(slide
 @h1{The Meta-Story}
 ;; programmers as means to acquire the things,
 ;; vs things as byproduct of programmers expressing ideas
 @p[class: 'fragment]{Sad Stories: about Things Created}
 @p[class: 'fragment]{Better Stories: about People Creating} ~
 ;; Desire control and look for solution hardwired in advance by experts who know better vs
 ;; Embrace change and let users express their needs in a safe space where bad situations are impossible by construction
 @p[class: 'fragment]{Sad Stories: bind good early}
 @p[class: 'fragment]{Better Stories: ban bad early})

#| Submission to LambdaConf 2017:

LambdaConf 2017 - Call for Proposals
Inspire Session (10 minutes)
Thanks for your interest in leading an Inspire Session at LambdaConf 2017! Please answer these questions as best you can. While you can always make tweaks after your proposal has been accepted, the determination of whether or not to include your proposal will be based on the answers you provide now.

* 3 Title. What is the title of your proposal?

Better Stories, Better Software

* 4 Introduction. What is this session about?

I will present several pairs of stories about how and why software is written, and what adverse or positive effects they have on what software is written.

While my personal opinion about which story of each a pair is better than the other may be controversial — the fact that some stories have vast effects opposite to each other will hopefully not be.

* 5 Takeaway. What is the ONE takeaway for developers who attend your session?

The structure of software is implied by the stories we tell. To build better software, tell better stories.

* 6 Inspiration. In what way do you hope your session will inspire developers?

I will inspire developers to think not just about the formal structure of software, inside the computer, but also about the informal interactions of which the software is part of, involving humans.

7 Entertainment. If relevant, in what way do you hope your session will entertain developers?

Developers will be entertained by realizing that a lot of the frustration they experience can be summarized in silly stories about software.


* 8 Relevancy. Why is this session relevant to a professional software developer?

Professional software developer sometimes need to step back and think about what they are doing, whether they should keep going one way, and if not, what to do next. At those crucial moments, perspective is crucial. I hope to contribute to such perspective.

* 9 Benefits. How will the subject matter you're covering help developers to better accomplish their job?

"Efficiency is doing things right; effectiveness is doing the right things." — Peter Drucker

I'm hoping to help with effectiveness, not efficiency.


* 10 Outline. Please create a brief outline how you intend to structure the session.

1- "Easy" stories that everyone is familiar with, and how they change the shape of software (e.g. free software vs proprietary software)

* 11 Pitch. What is the main reason developers should come to your session instead of other ones?

Stories are fun. Stories have consequences. Don't let yourself be a NPC in a bad Story.

* 12 Background Requirements. If your session is on statically-typed, category-theoretic functional programming (Haskell, PureScript, Scala, etc.), please choose the category that best matches the contents of your session, such that people who are actively learning or mostly know the category contents will understand your session.

Note: These topic categories are based on LOFP—please see here for more details.

The session is not related to statically-typed, category-theoretic families (Haskell, PureScript, Scala)

13 If relevant, what language(s) will you use to provide code samples?

N/A
|#



(output-xml
 @html{
   @head{
     @link[rel: 'stylesheet href: "resources/my.css"]
     @link[rel: 'stylesheet href: @reveal-url{css/reveal.css}]
     @link[rel: 'stylesheet href: @reveal-url{css/theme/black.css}]
     @link[rel: 'stylesheet href: @reveal-url{lib/css/zenburn.css}]
     @link[rel: 'stylesheet href: "resources/my.css"]
   }
   @body{
     @div[class: 'reveal]{@div[class: 'slides]{@get-slides}}
     @script[src: @reveal-url{lib/js/head.min.js}]
     @script[src: @reveal-url{js/reveal.min.js}]
     @script/inline{
       Reveal.initialize({dependencies: [
         {src: "@reveal-url{plugin/highlight/highlight.js}",
          async: true, callback: () => hljs.initHighlightingOnLoad()}]});
     }}})