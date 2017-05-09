#lang at-exp racket @; -*- Scheme -*-
#|
Better Stories, Better Languages
10 minute presentation at LambdaConf 2017-05-xx

To compile it, use:
  racket fare-lambdaconf2017.rkt > fare-lambdaconf2017.html

This document is based on a previous talk:
  http://fare.tunes.org/computing/bal2009.ss

This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require
 scribble/html
 net/url
 (for-syntax syntax/parse))

;; http://docs.racket-lang.org/scribble/extra-style.html

;; Reveal and new html stuff
(define/provide-elements/not-empty section video) ; more tags here

;; Register sections (but only at the top-level)
(define-values [get-sections register-section]
  (let ([sections '()])
    (values (λ () (reverse sections))
            (λ (section) (set! sections (cons section sections))))))
(define section-toplevel? (make-parameter #t))
(define-syntax-rule (slide stuff ...)
  (let ((toplevel? (section-toplevel?)))
    (parameterize ([section-toplevel? #f])
       (let ((section (section stuff ...)))
         (if toplevel?
             (register-section section)
             section)))))
(define (reveal-url . text)
  ;; (cons "http://cdn.jsdelivr.net/reveal.js/3.0.0/" text)
  (cons "resources/reveal/" text))

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

(define (L . x) (apply div align: 'left x))
(define (t . x) x)
(define (C . x) (apply div align: 'center x))
(define (CB . x) (C (apply b x)))

(define (url x) (a href: x (tt x)))
(define (comment . x) '())

(define (image name url . size)
  (img src: (pic-url name url) alt: name height: (if (empty? size) "75%" size)))

(define *white* "#ffffff")
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

(define (color text #:fg (fgcolor #f) #:bg (bgcolor #f))
  (span style: (list (if fgcolor (list "color:" fgcolor ";") '())
                     (if bgcolor (list "background-color:" bgcolor ";") '()))
        text))

(define (bg-slide text fgcolor bgcolor)
  (λ x
    (slide
     data-background: bgcolor
     (spacing x)
     (div align: 'right valign: 'bottom (color #:fg fgcolor text)))))

(define (x-slide . x) (slide (spacing x)))

(define th-width "4%")
(define td-width "48%")
(define table-width "114%")

(define (th* name)
  (if name (th width: th-width (font size: "5" (color name #:fg *white*))) (td)))

(define (row name left right #:left-bg (left-bg #f) #:right-bg (right-bg #f))
  (tr
   (th* name)
   (td
    width: td-width (when left-bg bgcolor:) (when left-bg left-bg)
    (spacing left))
   (if right
       (td
        class: 'fragment data-fragment-index: 1
        width: td-width bgcolor: right-bg
        (spacing right))
       (td width: td-width))))

(define (krad-slide #:question question #:issue issue #:story story #:solution solution)
  (slide
   (table
    align: 'right width: table-width
    (map (λ (name text) (row name text #f #:left-bg *light-green*))
         '("Question" "Issue" "Story" "Tools" #f)
         (list question issue story solution
               (div align: 'right (color "Even Better!" #:fg *green*)))))))

(define (srad-slide #:sad-issue sad-issue #:sad-question sad-question
                    #:sad-story sad-story #:sad-solution sad-solution
                    #:rad-issue rad-issue #:rad-question rad-question
                    #:rad-story rad-story #:rad-solution rad-solution)
  (slide
   (table
    align: 'right width: table-width
    (map (λ (name sad rad) (row name sad rad #:left-bg *light-red* #:right-bg *light-blue*))
         '("Question" "Issue" "Story" "Tools" #f)
         (list sad-question sad-issue sad-story sad-solution
               (div align: 'right (color "Sad..." #:fg *red*)))
         (list rad-question rad-issue rad-story rad-solution
               (div align: 'right (color "Better!" #:fg *blue*)))))))

(define (xad-slide
         #:sad-issue sad-issue #:sad-question sad-question
         #:sad-story sad-story #:sad-solution sad-solution
         #:rad-issue rad-issue #:rad-question rad-question
         #:rad-story rad-story #:rad-solution rad-solution
         #:krad-issue (krad-issue #f) #:krad-question (krad-question #f)
         #:krad-story (krad-story #f) #:krad-solution (krad-solution #f))
  (list
   (srad-slide #:sad-issue sad-issue #:sad-question sad-question
               #:sad-story sad-story #:sad-solution sad-solution
               #:rad-issue rad-issue #:rad-question rad-question
               #:rad-story rad-story #:rad-solution rad-solution)
   (when krad-issue
     (krad-slide #:question krad-question #:issue krad-issue #:story krad-story #:solution krad-solution))))

(slide
 @h1{Better Stories, Better Languages}
 @CB{What Would Alyssa P. Hacker Do?}
 ~
 ~
 ~
 @p{François-René Rideau, @em{TUNES Project}}
 ~
 ~
 @p{LambdaConf 2017, 2017-05-xx}
 @url{http://github.com/fare/better-stories}
)

(slide ;; Intro: Stories
(x-slide
 @h1{Stories}
 (table
  (tr
   ;;(td (image "gone-with-the-wind.jpg" "http://static.rogerebert.com/uploads/movie/movie_poster/gone-with-the-wind-1939/large_lqPnvmaX4oZY9teAOT7M0txCLkS.jpg" "40%"))
   (td (image "tosca.jpg" "https://s-media-cache-ak0.pinimg.com/originals/d4/cf/34/d4cf34cdac1191dcb38a0a1a61a0069b.jpg" "28%"))
   (td (image "transmetropolitan-one-more-time.jpg" "http://megasad.com/old/comics/covers/large/transmetropolitan-collection10.jpg" "20%"))
   (td (image "1984.jpg" "https://yifymovie.re/images/bposter/1984-1984-movie-poster.jpg" "50%"))
   (td (image "real-genius.jpg" "https://i.jeded.com/i/real-genius.16835.jpg" "24%")))
  (tr
   (td (image "holy-bible.jpg" "https://thewrittenwordreviews.files.wordpress.com/2009/03/holy-bible-cover.jpg" "10%"))
   ;;(td (image "decline-and-fall.jpg" "https://images-na.ssl-images-amazon.com/images/I/51LaFDYYMGL._SX362_BO1,204,203,200_.jpg" "40%"))
   (td (image "gulag-archipelago.jpg" "https://images-na.ssl-images-amazon.com/images/I/41LZ%2BGzRwjL._AC_UL320_SR206,320_.jpg" "80%"))
   (td (image "huckleberry-finn.jpg" "https://cdn2.hubspot.net/hub/237126/file-534578648-jpg/Huckleberry_Finn_Cover.jpg" "33%"))
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
         '(("Boy Meets Girl" "lovelace-babbage.jpg" "https://images-na.ssl-images-amazon.com/images/I/91FCUHSgEAL.jpg" "10%")
           ("Man Eats Forbidden Fruit" "alan-turing.jpg" "http://i.dailymail.co.uk/i/pix/2014/12/11/066267B800000514-0-The_famous_early_computer_inventor_and_war_hero_Alan_Mathison_Tu-m-17_1418322993327.jpg" "60%")
           ("Novice Grows into Master" "torvalds.jpg" "https://www.linux.com/sites/lcom/files/gallery/Linux%20Beer%20Use.jpg" "40%")
           ("Horror Hidden at Home" "heartbleed.jpg" "https://www.eff.org/files/2014/04/10/heartbleed-01-sm_0.jpg" "66%"))))
    (table
     (tr (map (λ (x) (td valign: 'top (apply image (rest x)))) theme-pic))
     (tr (map (λ (x) (td width: "25%" (font color: *white* (first x)))) theme-pic))))
  @comment{
There are many common stories so general that they can apply in any kind human situation.

I am not going to discuss those stories today.
})

@slide{
 @h1{Programming Stories}
 @image["Creation_Machine.jpg" "https://cdn.searchenginejournal.com/wp-content/uploads/2015/07/shutterstock_28130593-1.jpg" "70%"]
 @small{See also my SDR2017 talk
        @a[href: "https://github.com/fare/evo2017"]{@q{From Software Creationism to Software Evolutionism}}}
@comment{
  Today I want to discuss stories specifically about programming.
}}

(x-slide
 @h1{The Take Home Points}
 @L{Stories @em{matter}}
 @L{Software tools imply a story, and @em{vice versa}} @comment{like a Fourier Transform}
 @L{Better tools via better stories}
 @L{Explicit stories are a great meta-tool...}
 @comment{
I want to show you that the stories we tell *matter*.

that they affect what we do, and the outcome of what we do.
I want to show you that we do tell stories, even when we're not aware of them.
I want to show you that some stories lead to better outcomes than others.
}))

(slide ;; First Stories
(x-slide
 @h1{Pairs of Stories}
 @L{Take a @bg-colorize[*light-red*]{sad so-o-ong},
            and make it @bg-colorize[*light-blue*]{be-e-etter}}
 @L{Let's start with a couple easy ones you already know...})

(xad-slide
 #:sad-question "How to fund software?"
 #:sad-issue "Software costly to produce"
 #:sad-story '("own & sell scarce software " "vendors & customers") ;; (static)
 #:sad-solution '("Proprietary Software" "Closed binaries") ;; unmaintainable by anyone but the vendor, if interested
 #:rad-question "How to fund programming?"
 #:rad-issue "Starved coders don't code"
 #:rad-story '("own & sell scarce labor" "contributors & users") ;; (dynamic)
 #:rad-solution '("Free Software" "Open Source")) ;; shaped into maintainability by shared maintenance

(x-slide
 @h1{@q{I disagree!}}
 @L{It's OK to be wrong (for you, for me)}
 @L{@em{Maybe} one story isn't @em{always} better}
 @L{But can we agree that stories usually @em{matter}?}
 @L{Slightly different stories → vastly different outcomes}
 @comment{
 }))

(slide
(slide @h1{Simple Programming Stories})
(xad-slide
 #:sad-question "Decompose programs?" ; (how to...)
 #:sad-issue "Software too large for one brainful"
 #:sad-story '("Hierarchy of components" "by fully informed expert")
 #:sad-solution '("Flowcharts, UML" "Top-down management")
 #:rad-question "Decompose programming?"
 #:rad-issue "Cooperation needed by many brains"
 #:rad-story '("Propagate partial info along" "network of people, projects")
 #:rad-solution '("SW distributions, forums" "Distributed version control"))

(xad-slide
 #:sad-question "Achieve great software?" ; (how to...)
 #:sad-issue "Improving software is hard"
 #:sad-story '("Disseminate expertise"
               "Restrict modules to experts")
 #:sad-solution '("Standards" "Segregation by expertise") ;; Conway's Law
 #:rad-question "Foster better programming?"
 #:rad-issue '("Improving ourselves is hard") ;; learn from our and their successes and failures
 #:rad-story '("Learn from experience" ;; other people's, or your own; experience as an output, rather than expertise as an input
               "Cultivate good incentives") ;; information isn't the limiting factor
 #:rad-solution '("Communities" "Competition in markets")))

(slide
(x-slide @h1{Programming Language Stories})
(xad-slide
 #:sad-question "Make device programmable" ; (how to...)
 #:sad-issue "Expose device features"
 #:sad-story '("PLs are for machines")
 #:sad-solution '("match device capabilities"
                  "Turing tar pit")
 #:rad-question "Express programming ideas"
 #:rad-issue "Convey human meanings"
 #:rad-story '("PLs are for humans")
 #:rad-solution '("match human cognition" ;; and social processes
                  "minimize complexity")) ;; intrinsic vs incidental complexity

(xad-slide
 #:sad-question "Handle repetitive code" ; (how to...)
 #:sad-issue "Lots of repetition in code"
 #:sad-story '("Language as given" "Programmer as drudge")
 #:sad-solution '("Informal Design Patterns" "Plan more drudgery") ;; manually enforce consistency
 #:rad-question "Remove coding drudgery"
 #:rad-issue "Drudgery in programming" ;; "I object to doing things that computers can do." — Olin Shivers
 #:rad-story '("Language as evolving" "Programmer as thinker")
 #:rad-solution '("Formal Metaprograms" "Evolve language")) ;; Turing's theorem is based on metaprograms!

(xad-slide
 #:sad-question "Have an extensible syntax?" ; (how to...)
 #:sad-issue "Hooks into existing syntax" ;; assuming we want extensibility
 #:sad-story '("Side-effect One True Syntax") ;; as in Common Lisp
 #:sad-solution '("Global macros" "Global readtable")
 #:rad-question "Explore useful syntaxes?"
 #:rad-issue "Best express each fragment"
 #:rad-story '("Pure grammar increments")
 #:rad-solution '("Scoped syntax specification" "Racket languages, OMeta"))

(xad-slide
 #:sad-question "Users ≠ Programmers" ; (how to address the fact that...)
 #:sad-issue "Two paradigms, UI vs PL"
 #:sad-story '("Dumbing down for Users" "All-Power for Devs (in VM?)")
 #:sad-solution '("Unrelated UI and PL" "Segregation")
 #:rad-question "Using = Programming"
 ;; The difference between a programmer and a user, is that
 ;; the programmer knows there is no difference between using and programming. — Faré
 #:rad-issue "One PL, spoken or written"
 #:rad-story '("One computer interaction" "Continuum of proficiency")
 #:rad-solution '("Integrated interface" "PL levels and dialects"))

(xad-slide
 #:sad-question "P'er ≠ PL Implementer" ; (how to address the fact that...)
 #:sad-issue "Writing a compiler is hard" ;; a correct one even worse
 #:sad-story '("Specialists implement PL" "Mere programmers use PL")
 #:sad-solution '("Closed PL implementations" "Few, magic, PLs")
 #:rad-question "P'ing = PL Implementing" ; Programming *is* implementing the language spoken by the users!
 #:rad-issue "Modular DSL increments" ; only hard if not done from scratch
 #:rad-story (list "Special case of U = P" @em{Each P is PL spoken by U})
 #:rad-solution (list @em{First-class implementations} "Lots of DSLs")) ; PCLSRing

(xad-slide
 #:sad-question "PL Definer ≠ Implementer" ; (how to address the fact that...)
 #:sad-issue "Designing a PL is hard" ; once again, only for experts
 #:sad-story '("Specialists define big PL" "Others implement")
 #:sad-solution '("Standard for language" "Decades-old design") ; blind spot, slow update cycle, bit rot
 #:rad-question "PL Defining = Implementing"
 #:rad-issue "Specify = Implement"
 #:rad-story '("Declarative specification" "Orthogonal impl. strategies")
 #:rad-solution '("Grammatical mixins" "Pervasive experimentation"))

(xad-slide
 #:sad-question "Get a specialized language?" ; (how to...)
 #:sad-issue "Heterogeneous activities"
 #:sad-story '("Each domain its experts" "Segregation of experts")
 #:sad-solution '("External DSLs" "Scripting languages")
 #:rad-question "Specialize conversation?"
 #:rad-issue "Express domain expertise"
 #:rad-story '("One brain, many topics" "Adapt PL to domain")
 #:rad-solution '("Internal DSLs" "Contexts of universal PL")))

(slide
(x-slide @h1{Programming Quality Stories})
(xad-slide
 #:sad-question "Get Programs Debugged" ; (how to...)
 #:sad-issue "Program bugs need fixed"
 #:sad-story '("Bugs are exceptions" "Ad-hoc tools retrofitted")
 #:sad-solution '("Low-level debugger" "Ad-hoc debug info")
 #:rad-question "Explore Program Semantics"
 #:rad-issue "Semantics isn't obvious"
 #:rad-story '("Imperfection is the default" "Exploration is normal")
 #:rad-solution '("Compiler as reversible lens" "Experiment in Virtual World"))

(xad-slide
 #:sad-question "Secure existing software?" ; (how to...)
 #:sad-issue "Security its own expertise"
 #:sad-story '("Security as afterthought"
               "Independent Sec. experts")
 #:sad-solution '("Forever patch leaks" "Low-level protection")
 #:rad-question "Build software securely?"
 #:rad-issue "Security as aspect of Design"
 #:rad-story '("Sec part and parcel of P" "Programmer education")
 #:rad-solution '("Whole-system design" "High-level capabilities"))

(xad-slide
 #:sad-question "Dealing with catastrophes?" ; (how to...)
 #:sad-issue "Bad manip. → Data loss"
 #:sad-story '("Exceptional catastrophes")
 #:sad-solution (list "Confirm menus, remove bin"
                      @list{Expensive ad hoc "Undo"}) ;; programmer-intensive add-ons
 #:rad-question "Eliminating catastrophes?"
 #:rad-issue "Bad manip. unexpressible"
 #:rad-story '("Everyday trivial failures")
 #:rad-solution '("Monotonic storage"
                  "Universal infinite undo"))) ;; system-provided default

(slide
(x-slide @h1{More Programming Stories})
(xad-slide
 #:sad-question "Document conventions" ; (how to...)
 #:sad-issue "Define module interfaces"
 #:sad-story '("PL as given, modules fixed"
               "PL limit expressible intent")
 #:sad-solution '("Informal contracts"
                  "Fixed team boundaries")
 #:rad-question "Agree on responsibilities"
 #:rad-issue "Define team interfaces"
 #:rad-story '("Extend PL, trade modules"
               "Express if benefit > cost")
 ;; if you can afford the testing that went into SQLite, you can afford proofs.
 #:rad-solution '("Formalize contracts"
                  "Negotiate responsibilities"))

(xad-slide
 #:sad-question "Arbitrate Resource?" ; (how to...)
 #:sad-issue "Maintain shared invariants"
 #:sad-story '("Central dictator needed"
               "Schedule resource use")
 #:sad-solution '("(OS or App) Kernel"
                  "Static set of resources")
 #:rad-question "Resolve Conflicts?"
 #:rad-issue "Owners trade resources"
 #:rad-story '("Self-enforcing contracts"
               "Linear logic of ownership")
 #:rad-solution '("Invariant-enforcing linker"
                  "Dynamic resource bundles"))

(xad-slide
 #:sad-question "Connect Computers" ; (how to...)
 #:sad-issue "Overcome one-system limit" ; both technical and social limits
 #:sad-story '("From machines to meaning"
               "Many cpus, weak federation" )
 #:sad-solution '("Remote method invocation"
                  "Shipping state around")
 #:rad-question "Distribute Computation"
 #:rad-issue "Beat many-cpu complexity"
 #:rad-story '("From meaning to machines"
               "One system, many cpus")
 #:rad-solution '("Declarative deployment"
                  "Content-based addressing"))

(xad-slide
 #:sad-question "Handle mistrust?" ; (how to...)
 #:sad-issue "Need protection barriers"
 #:sad-story '("Kernel-managed domains" "Expensive rigid model")
 #:sad-solution '("Static container hierarchy"
                  "Expensive and inexpressive")
 #:rad-question "Express limited trust?"
 #:rad-issue "Bundle capabilities"
 #:rad-story (list @list{Everyone "root" in own VM}
                   "Recursively so, by default")
 #:rad-solution '("PL support virtualization"
                  "Cheap to create sub-user"))

(xad-slide
 #:sad-question "Persist important data?" ; (how to...)
 #:sad-issue "Important data must persist" ; against HW/SW failure
 #:sad-story '("Manual persistence"
               "Transient by default")
 #:sad-solution '("Filesystems, databases" "Explicit I/O")
 #:rad-question "Write persistent software?"
 #:rad-issue "All data is important"
 #:rad-story '("Why else program about it?"
               ;; You don't care when memory is spilled from cache to RAM,
               ;; why care when it's spilled from RAM to disk?
               "Persistence by default") ;; Transients for performance
 #:rad-solution '("Orthogonal persistence"
                  "Implicit support in PL")))

(slide
(x-slide @h1{Stories about change})
(xad-slide
 #:sad-question "Model a changing world?" ; (how to...)
 #:sad-issue "Mutations happen"
 #:sad-story '("Mutable Object-Oriented"
               "Can't trust any(thing|one)")  ;; live in a world of fear
 #:sad-solution '("Imperative programming"
                  "Locks: transient protection")
 #:rad-question "Model changes to world?"
 #:rad-issue "Transformations compose"
 #:rad-story '("Immutable Value-Oriented"
               "Can always reason")
 #:rad-solution '("Functional Programming" ;; Purity by default, at base-level, at meta-level, too... Unlambda!
                  "Monads, extensible effects") ;; Problem: too much or too little
 #:krad-question "Discuss relevant change?"
 #:krad-issue "Record and Process Events"
 #:krad-story (list "First-class Change-Oriented"
                    @list{Mutable vs immutable @em{views}})
 #:krad-solution '("Differentiate, Integrate"
                   "Switch view to/from FP")))

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

(slide
(slide
 @h1{The Grand Challenge}
 ~
 @L{None of these Stories is revolutionary} ;; From The Mother of All Demos...
 @L{Each has been foretold in past systems} ;; Implemented, though not always optimized and productized
 ~
 (div class: 'fragment
  @L{But no @em{system} embodies them all at once} ;; Opportunity!
  @L{Missing: not technical ability, but @em{vision}}))

(x-slide
 @h1{The Take Home Points (redux)}
 @L{Stories @em{matter}}
 @L{Software tools imply a story, and @em{vice versa}} @comment{like a Fourier Transform}
 @L{Better tools via better stories}
 @L{Explicit stories as great meta-tool...})

(slide
 @h1{The Meta-Story}
 ~
 ;; programmers as means to acquire the things,
 ;; vs things as byproduct of programmers expressing ideas
 (table
  align: 'right width: table-width
  (tr
   (td width: th-width)
   (th width: td-width bgcolor: *light-red* @font[size: 5]{Sad Stories})
   (th width: td-width bgcolor: *light-blue* @font[size: 5]{Better Stories}))
  (tr
   (th* "Topic")
   (td
    class: 'fragment data-fragment-index: 1 bgcolor: *light-red*
    (spacing '("About Programs" "Things Created")))
   (td
    class: 'fragment data-fragment-index: 1 bgcolor: *light-blue*
    (spacing '("About Programming" "People Creating"))))
  (tr
   (th* "Choice")
   (td
    class: 'fragment data-fragment-index: 2 bgcolor: *light-red*
    (spacing (list (span class: 'fragment data-fragment-index: 2 "Bind Good Early")
                   (span class: 'fragment data-fragment-index: 3 "Impose Ignorance"))))
   (td
    class: 'fragment data-fragment-index: 2 bgcolor: *light-blue*
    (spacing (list (span class: 'fragment data-fragment-index: 2 "Ban Bad Early")
                   (span class: 'fragment data-fragment-index: 3 "Create Freedom"))))))
 @comment{Any question?}))


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
     @div[class: 'reveal]{@div[class: 'slides]{@get-sections}}
     @script[src: @reveal-url{lib/js/head.min.js}]
     @script[src: @reveal-url{js/reveal.min.js}]
     @script/inline{
       Reveal.initialize({dependencies: [
         {src: "@reveal-url{plugin/highlight/highlight.js}",
          async: true, callback: () => hljs.initHighlightingOnLoad()}]});
     }}})
