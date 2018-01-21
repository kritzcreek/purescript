
# Table of Contents

1.  [Introduction](#org61ee13f)
2.  [What does \`purs ide\` do?](#org14adf38)
3.  [Using \`purs ide\` as a library from Haskell](#orgf5fa31e)
    1.  [handleCommand](#org2dffb76)
    2.  [Ide's State type](#org0121aa0)
        1.  [`IdeFileState`](#orgd16ef47)
        2.  [`IdeVolatileState`](#org722120e)
    3.  [How to invoke `handleCommand` in an executable](#org4b9811c)
    4.  [Concurrency model is up to the caller of handleCommand](#orgcb6ebdf)
4.  [Commands](#org4259369)
    1.  [Completions](#orgfc00e68)
        1.  [The Query Pipeline](#org1503f83)
        2.  [Filters](#org730ab33)
        3.  [Matchers](#org8c56b7e)
    2.  [Adding Imports](#org0471c56)
        1.  [We pretty print the entire import section on every import command instead of patching the existing section](#orgef9e40b)
        2.  [Formatting Rules for imports](#org2055511)
    3.  [Rebuilding](#org791f9ae)
        1.  [The rebuild command acts on a single file input](#org9dc23d0)
        2.  [IDE's rebuilds are fast](#orgd1be17e)
        3.  [Steps rebuilding takes](#org3d27126)
        4.  [Extra Rebuild with open imports (only when the first Rebuild succeeds)](#org887e110)
    4.  [Everything else](#org25b2577)
5.  [Tips and Tricks](#orgc676cac)
    1.  [Running only `ide`'s test suite](#org87edba4)
6.  [Facts and thoughts without a good place yet](#orgd23d2fe)
    1.  [Using externs files as source of truth](#org8ed7e79)
        1.  [Pros](#org892001c)
        2.  [Cons](#orgc5ff229)
    2.  [When source globs are added](#org00b4fe4)
        1.  [New features enabled](#orgb418504)
        2.  [Cons](#org7c90967)
    3.  [PureScript's package story involves downloading all the source](#orga1005e5)
    4.  [Keeping everything in memory](#org0ff7430)
        1.  [Pros](#org6d10ee0)
        2.  [Cons](#org9843a9a)


<a id="org61ee13f"></a>

# Introduction

This document is meant to outline and explain some of the architecture
decisions for `purs ide`. Read this document, if you plan on contributing to
`purs ide` or are just generally interested in the project.


<a id="org14adf38"></a>

# What does \`purs ide\` do?

The `purs ide` project provides functionality for PureScript tooling and
editors.

-   Cross platform
-   Distributed and versioned with the compiler
-   Reuses types and functionality from the compiler -> up-to-date
-   Reduces reimplementation of the same feature for every editor


<a id="orgf5fa31e"></a>

# Using \`purs ide\` as a library from Haskell

`purs ide` is split into a library and an executable. The library code lives
inside `src/Language/PureScript/Ide`. The executable, which is invoked by the
editors is located inside `app/Command/Ide.hs`.

The `purs ide` library is unopinionated about:

-   Protocol
-   Concurrency Model
-   Logging
-   File watchers

And so other executables, like an implementation of the Language Server
Protocol, are supported by this model and can be added in the future.

The main entry point into the library is the `handleCommand` function inside
the `PureScript.Language.Ide` module.


<a id="org2dffb76"></a>

## handleCommand

Break down the type signature:

`handleCommand :: (Ide m, MonadLogger m, MonadError IdeError m) => Command -> m Success`

Ide m expands to (MonadReader IdeEnvironment m, MonadIO m) and so we end up
with 4 constraints/capabilities handleCommand needs to be provided with by
the caller.

-   MonadIO
    
    handleCommand needs access to IO

-   MonadError IdeError
    
    Errors can occur during the evaluation of a Command, and the executable
    gets to decide how to handle them.

-   MonadLogger
    
    purs ide uses the `MonadLogger` constraint to defer the choice of logging
    to the exeutable. This constraint can be fulfilled with a console based
    logger, a file-based one or the log messages can just be discarded (helpful
    during testing)

-   MonadReader IdeEnvironment
    
    The IdeEnvironment holds some configuration type, but crucially it also
    contains a TVar (thread variable), which contains all of purs ide's state.
    We're using a threadvariable over a `MonadState` constraint here, so it's
    easier to evaluate concurrent or asynchronous evaluation of commands.


<a id="org0121aa0"></a>

## Ide's State type

Ide's State is split into `IdeFileState` and `IdeVolatileState`.


<a id="orgd16ef47"></a>

### `IdeFileState`

The file state holds externs files and parsed module ASTs and thus directly
corresponds to entities on the file system. This part of the state can be
changed per module (eg. by a filewatcher).


<a id="org722120e"></a>

### `IdeVolatileState`

The volatile state contains all the derived data, like the declarations we
use to provide autocompletion. The data is denormalized and optimized for
reading/querying, but is harder to invalidate and thus needs to be updated
more coarsely whenever something in FileState changes. Right now we
completely recompute it on every change because it's still very fast. In the
future we might need to be cleverer as the information we collect gets more
sophisticated and more expensive to compute.


<a id="org4b9811c"></a>

## How to invoke `handleCommand` in an executable

Relevant files: tests/Language/PureScript/Ide/Test.hs app/Commands/Ide.hs

Running `handleCommand` requires that we satisfy all the constraints placed
on it. It's easiest to just show how to write a function that accepts a
single command and runs it against an empty `IdeState`. We'll also retrieve
the resulting state and any errors that ocurred.

    runIdeCommand :: Command -> IO (Either IdeError Success, IdeState)
    runIdeCommand command = do
      -- First we'll create a TVar of an empty IdeState.
      stateVar <- newTVarIO emptyIdeState
      -- We create a new IdeEnvironment using the default IdeConfig and our state
      -- variable
      let environment = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = defConfig}
      -- It's easiest to read the next line inside out:
    
      -- 1. apply =handleCommand= to the command 
    
      -- 2. Satisfy the MonadReader IdeEnvironment constraint by passing
      -- =environment= to =runReaderT=
    
      -- 3. Turn any thrown Errors into an Either IdeError with =runExceptT=
    
      -- 4. Finally, discard any log messages with =runNoLoggingT=.
    
      -- (5. The MonadIO constraint is satisfied by choosing IO as the underlying
      -- Monad)
      result <- runNoLoggingT (runExceptT (runReaderT (handleCommand command) environment))
    
      -- We read the resulting IdeState from the state variable
      newState <- readTVarIO stateVar
      -- Return the command result, as well as the resulting state
      pure (result, newState)


<a id="orgcb6ebdf"></a>

## Concurrency model is up to the caller of handleCommand

By using a `TVar` instead of a MonadState constraint `ide`'s design allows to
run multiple invocations of `handleCommand` in parallel. By using `STM`,
`ide` makes sure to not run into deadlocks or data races.

However the current implementation of `purs ide server` runs all the commands
sequentially, because the commmands run fast enough at this point, and a
users interaction with his editor are mostly sequential anyway.


<a id="org4259369"></a>

# Commands

The three most involved commands are completion, adding imports and rebuilding.

-   Completions are found by composing filters and matchers, a \`purs ide\` DSL
-   Adding imports involves file manipulation, some custom parsing and surprisingly complex logic
-   Rebuilding involves calling compiler APIs


<a id="orgfc00e68"></a>

## Completions

Important files: Ide.Filter Ide.Matcher Ide.Completion

The `completion` command filters all of the stored `IdeDeclarations` inside
`ide`'s volatile state through a list of `Filters` as well as an optional
`Matcher`. Completion options can be specified to apply further
post-processing (choosing the maximum number of results, how to group
reexports of the same value)

Afterwards they are turned into a stripped down `Completions`
format, which contains information that can be easily consumed by editor
plugins.


<a id="org1503f83"></a>

### The Query Pipeline

When fulfilling completion requests or other queries, \`ide\` runs the stored
declarations through the following pipeline:

`Declarations |> Filters |> Matcher |> CompletionOptions |> Completions`

First we apply the filters, which either keep a declaration or drop it. Then
we apply Matchers, which can also drop declarations, but assign a score to
the declarations, which determines their ordering. We use this to sort
declarations in terms of how far the edit distance between them and a query
string is, or how many characters we needed to skip for a flex match.

TODO: links for levenshtein and flex match

Finally we apply the completion options, which apply certain a certain
formatting, limit the number of results or apply grouping operations.

All the different filters, matchers and completion options are documented in
the PROTOCOL.md file.


<a id="org730ab33"></a>

### Filters

Filters are functions of type `Map ModuleName [IdeDeclaration] -> Map
    ModuleName [IdeDeclaration]`. We keep the `Map` structure around to make the
common case of filtering by module names fast.


<a id="org8c56b7e"></a>

### Matchers

Matchers operate on individual declarations rather than a `Map`. They also
assign a score to every result, which is a simple Double.


<a id="org0471c56"></a>

## Adding Imports

Important Files: Ide.Imports


<a id="orgef9e40b"></a>

### We pretty print the entire import section on every import command instead of patching the existing section

1.  Pros

    -   Small diffs if you use `ide` all the time
    -   Uniform formatting
    -   Simplifies the implementation

2.  Cons

    -   Big diff on first use
    -   Makes it hard to maintain comments in between imports, so we just remove them


<a id="org2055511"></a>

### Formatting Rules for imports

1.  Unqualified imports
2.  Space divider
3.  All the other imports in alphabetic ordering

1.  Pro

    -   Easy enough to achieve without using `ide` by just sorting the imports linewise

2.  Cons

    -   Can lead to very long import lines


<a id="org791f9ae"></a>

## Rebuilding

Important Files: Ide.Rebuild


<a id="org9dc23d0"></a>

### The rebuild command acts on a single file input

Unlike the compiler which gets paths to all the modules in our program, the
Rebuild command only gets handed the path to a single module.


<a id="orgd1be17e"></a>

### IDE's rebuilds are fast

There are two reasons why ide's rebuilds are an order of magnitude faster than
the compilers incremental builds.

1.  Rebuild ONLY respects downstream modules

2.  All the externs data is already held in RAM


<a id="org3d27126"></a>

### Steps rebuilding takes

1.  Parse input model

2.  Check if FFI file exists and also load that

3.  Grab the Externsfiles out of IDE's state

4.  Delete the Externsfile corresponding to the module to be rebuilt

5.  Convert all the externs files into "shallow modules" which only hold their dependency information

6.  Run the compilers topo-sort to figure out all the transitive dependencies of the module we just parsed

7.  Rebuild the Environment against the set of externs files we just figured out


<a id="org887e110"></a>

### Extra Rebuild with open imports (only when the first Rebuild succeeds)

This is so that we can mitigate the fact that Externsfiles only give us access
to exported declarations. We rebuild the file a second time, but this time we
remove all the export restrictions before doing so, and store the resulting
Externsfile inside IDE's cache. It's important! that we do not write this file
to disc, because it's incorrect when used by a normal compile or rebuild.

1.  The caller gets to decide how the extra Rebuild is run

    The primary motivation for this is that we don't need the second build to run to
    detect all the compiler errors, so in the usual mode of operation we want to run
    it asynchronously and just return the errors/warnings to the editors
    immediately. In a test setting however, we might want to test that the rebuild
    cache was filled properly and serves completions for private members. (Examples:
    Language.PureScript.Ide.RebuildSpec)


<a id="org25b2577"></a>

## Everything else


<a id="orgc676cac"></a>

# Tips and Tricks


<a id="org87edba4"></a>

## Running only `ide`'s test suite

`stack test --test-arguments "-m Language.PureScript.Ide"`


<a id="orgd23d2fe"></a>

# Facts and thoughts without a good place yet


<a id="org8ed7e79"></a>

## Using externs files as source of truth


<a id="org892001c"></a>

### Pros

-   Everything has types, because it went through the compiler
-   Module visibility is respected, because everything went through the compiler
-   Works even when the source file has syntax errors/doesn't compile
-   Easy plug-and-play, people rarely change the \`output/\` directory (as
    opposed to the file structure)
-   Decoding JSON is fast! (As opposed to parsing source code)


<a id="orgc5ff229"></a>

### Cons

-   All type synonyms are expanded (Just something the compiler does)
-   Means non-exported values are unaccessible (They should be in scope while
    editing the corresponding module though)
-   Can serve stale declaration information, eg. a declaration might've been
    removed from a module, but the module doesn't compile yet, so the externs
    hasn't been overridden and we still suggest the declaration
-   Can serve stale module information, when a source file gets deleted, the
    corresponding externs file does not. Which means we can't detect whether a
    module still exists.
-   No source positions or docstrings


<a id="org00b4fe4"></a>

## When source globs are added


<a id="orgb418504"></a>

### New features enabled

-   Enables go-to-definition by allowing us to grab source spans for declarations
-   Enables us to recover type signatures without synonyms expanded
-   Enables us to grab docstrings (We don't do that yet, unfortunately)


<a id="org7c90967"></a>

### Cons

-   Slower startup (Actually the load command takes longer, but because the server
    is useless until load has been run I count that as startup). Startup on
    slamdata is at around 5-6seconds.
-   Higher memory footprint. We hold the ASTs for all the modules and add
    additional information to the declarations TODO: quantify this for slamdata
-   It's harder to watch source files for changes, because they aren't collected
    in a single directory (which is why we don't do it)


<a id="orga1005e5"></a>

## PureScript's package story involves downloading all the source

-   Great for us, because we get go-to-definition and docstrings without having to
    query some external resource


<a id="org0ff7430"></a>

## Keeping everything in memory


<a id="org6d10ee0"></a>

### Pros

-   All data is regenerated on starting ide = no cache invalidation necessary
-   Things are fast, without any effort spent on optimizing things
-   Simple model, keeps complexity low
-   We don't polute projects with ide artifacts


<a id="org9843a9a"></a>

### Cons

-   Imposes a limit on how big of a project we can handle
-   Means we need to be careful about what information we denormalize, since it
    can blow up on us
-   All data is regenerated on starting ide = slower startup than (maybe?) necessary
-   Impossible to share information between projects (for shared dependencies)

