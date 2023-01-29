# Klatchlings Blog

## Writing Techniques
At this point I have heard a couple sources talk about how you should write
and edit in different stages. Rather than write out some things and edit while
you do it in an attempt to get it write the first time, you should just splurge
everything out the first time, and then go back and edit it with a fresh mind.
So I will try that. Hence, I will do the blog posts in two commits. The first
just unedited thought vomit on the page. And the second pass through an edited
version. This will also allow me to look back at my writing progress!

## On the Rebound
A lot has happened since the last blog post. Lets go through the changes one at
a time...

### Core Game Engine
I think this points to the strengths of iterative development, more specifically
to the importance that our brain just keeps information in it without us
the realization of such. It is apparent from my previous post that I was
dreading to restart again. But if I remember correctly it only took 2 hours or
so to regurgitate the previous back and in an even better way. I think this
iteration focused largely on really using Haskell's type system to plan out
the implementation in advance. While this is maybe not so visible in the
commits, I first essentially created a skeleton of function declarations with
the types before writing any actual code. Creating a contract of promises what
the function will be. This I think forces yourself to use somewhat legible
function names, since I was happy that I could understand what each function
was going to do from just the function names and type declarations. Of course,
this is all with a grain of salt since I have rewritten it a couple times now
and so many of the concepts and functionality are engrained into my soul. This
is a key takeway for when I started my development in the C/C++ ui. I noticed
that I was also creating a template of the function declarations and good
function names, but more on that later. I think I am largely happy with how
the core game engine currently is and I can see lots of parralels of this game
and some of the smart-contract languages of the world. Maybe that will be a
future project that could have the prospect of making money :) There are a
couple things I am not happy with. There exists no tests at the moment, I had
intended for tests to be developed alongside the code but got lazy and kept
procrastinating it. Similarily for the documentation. This is almost certaintly
from the feeling of getting burned on the last corrupted project. I had put
substantial effort into the documentation and that is what burned the most
getting deleted. Unfortunately, it seems like a testing/documentation effort
will be needed at some point. On the plus side, I can envision that the testing
harness that is in the projects line of sight will be really nice and easy to
use because of the simplicity of the external API of the core engine. Another
draw is the performance, rather my lack of understanding of the performance
that the core engine would exhibit. I feel as though I do not fully understand
how lazy evaluation will impact the performance of everything. Especially, when
we couple it with the use of ports and tcp connections. However, in many ways
I accepted that given the finiteness of a trading card game, the complexity
of operations used and the computational ability of computers, performance of
the game logic will not be particularly important until port the game to a
server with (hopefully) ~~thousands~~ millions of concurrent games.

### Implementing the Klatchlings Basic Rules
Implementing the rules of the base game was fantastic, to toot my horn. This
was a big revelation from the second to third iteration of the game, that the
rules of the game itself could be implemented as cards. Previously, things such
as the zone of a card and how it moved and other rules were hardcoded into the
system. However, I realized that the core engine would allow for coding the
rules of a card game as cards in the card game. So a card game on top of the
core engine is just a finite set of cards... mindblown. This obviously is
fantastic and exactly what I envisioned being the core value of the game. For
students, curious people and me to go and change the rules of the game just by
programming cards. I recently listend to a Lex Fridman podcast with John
Carmack, where they discussed why Quake, Minecraft, Roblox and Fortnite are such
successes since they provide infinite game design space within the game itself.
I think Klatchlings embodies this better than all of those combined and will
be crucial to the success. When I think of the goal of the project, this is
the critical feature and I am really happy how smooth it has gone for me so far.
Of course, there is a long way to go until it is as smooth for a newbie just
creating their first card or rule change. However, I can see it clearly and
the path to get there.

### Creating a Harness and the Subsequent Port Communications to the UI
Title says it all I would say.

This is the rather boring stuff, but there are a couple important take-aways
I think. First the bad news, it is evidently not optimal for us to be using
Erlang, Haskell and C/C++ with SFML all on a clients machine. This means it
has almost 0 portability. So a lot of this work is quite temporary until we
will be moving much of this code to a server instance. A design decision I took
at the start was that the Haskell game logic and the C ui will not communicate
with each other, rather they will both communicate to the common erlang runtime
that manages them. This creates the unoptimal use of having our gamestate being
saved in Haskell, encoded to a shared format between Haskell and Erlang. Erlang
then decodes this format and then encodes it to the shared format between Erlang
and the C ui. Ya... not very simple is it. Additionally, we use a Erlang port
driver for the ui and erlang runtime to communicate rather than a tcp port.
This choice was largely choosen just for the possibility to explore what using
this was like. And there was a considerable effort that was put into creating
the encoding/decoding schemes for the port that only sends 2 bytes at a time.
So the current system is not very extendable. Furthermore, it will most likely
be deprecated in favour of a tcp port. This tcp port will connect directly from
the ui to the server erlang runtime. This flows into the good things. It appears
that the code is written in a manner such that this port to a server centric
version of the application will not be too difficult. And I keeping this in mind
as we go forward. I am happy with the modularity of the code and that it looks
like the port will likely just consist of copying and pasting the current code
over. The effort was also not as much as I had estimated. Building the packet
framework without having to consider efficiency much makes it a lot easier and
I played well into this trade-off. So the packet decoding/encoding between C and
Erlang will not hurt so much when it is inevitably removed. I am also pleasantly
surprised with the C++ standard library and documentation. Using or playing
around with threads, mutex, atomic is a pleasant experience, I suppose relative
to my past experiences with p_threads for computational programming. Of course,
the complexity of this project with regards to concurrent programming dwarfs the
considerations that I have had to make in previous projects, but so far so good.

## Next Steps
As has been mentioned frequently, there is an upcoming port that will need to
be done. However, this is quite far in the future I would suspect. More pressing
are the ideas to document/test the code base so far. There also needs to be
an effort to making the application gracefully close when the window/tcp port/
erlang runtime are interupted. I had thought this would be taken care of in
the current instance, but I think it points out that my understanding of the
termination of erlang processes is not as solid as I thought. This also ties
in with creating a supervisor structure for the erlang processes. However, I
think this will be after the port to a server-centric model and so is far in
the horizon. The immediate next steps are to begin implementing ways to
provide orderings and targets to the game through the ui. And to write a debug
version that will allow for text-input based input for testing-purposes. Other
considerations or problems that are still not solved are the issue of hidden
information and how to conceal what each player knows from each other.
Especially, in the face of adversarial players. This seems like a component
I overlooked from the core engine and may need some reworking as I would
surprised if it could be representing as a rule defined with a card. However,
that is the approach that I am looking into. Maybe a chance for some fancy
self written encryption that will turn out to not be safe. But my current course
in mathematical cryptography may inspire a way forward.

## Conclusions
A surprisingly productive two weeks! As school has started again, I didn't
expect to find the time to work on this. However, the omission of wifi from
my current apartment does seem to have quite a positive effect on my
productivity. Who would of thought that not being constantly tempted my lazyly
watching a twitch stream or youtube video would help productivity. I really
enjoy that and I will deeply consider if I will get wifi in my future places
of residence.
