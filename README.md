# echoloc

audio only

The monster is CHASING YOU, you can HEAR IT GETTING CLOSER

You can use the arrow keys to turn left / right and move forward.

# how do I make it play?

`cabal run exes`

or clone this repo and then `nix run .` in the checkout

# who?

Idea by and direction by @shapr

Sensible decisions and working linear algebra things due to @awjchen who paired with me and diverted me from many blind alleys and poor decisions.

Credit to @joshmeredith for helping with state and geometry.

# what?

Recurse Center, June 2024, game jam from June 3 to June 6

My first game! My worst game! My best game! It WORKS!

# lessons learned

I don't know linear algebra and I needed it for this game.
Going from zero to one writing a game is hard.
Lucky for me, I had Alex Chen helping out, and he's already written a game in Haskell!

Several times I almost got stuck and Alex said "We'll use OpenAL because it handles stereo sound really well"
and "let's stick to 2D coords and then translate to OpenAL's 3D coords"
and "Let's use SDL for keyboard input because it's easy" (after I'd gotten stuck trying something else)
