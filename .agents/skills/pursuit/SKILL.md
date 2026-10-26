---
name: pursuit
description: "Find a PureScript function, type, or class member on Pursuit. Search by type signature first — more reliable than guessing names. Whenever you need a PureScript function you cannot name, are unsure a function exists, or are about to search Pursuit by name. Triggers on 'PureScript の関数を探す', 'Pursuit で調べる', 'does PureScript have a function that...'."
user-invocable: true
disable-model-invocation: false
allowed-tools: Bash
---

# Pursuit

Search by type first, not by name. URL-encode the query.

1. Ask Pursuit for the signature you want:

```sh
ax 'https://pursuit.purescript.org/search?q=forall%20a.%20Array%20(Maybe%20a)%20-%3E%20Maybe%20(Array%20a)' 'pre.result__signature' --text --limit 10
```

2. Compare each result to your signature. Pursuit returns near-misses — a returned result is not a match.

3. If none match, get the name from Hoogle, then rerun step 1 with that name as `q`:

```sh
ax 'https://hoogle.haskell.org/?hoogle=%5BMaybe%20a%5D%20-%3E%20Maybe%20%5Ba%5D&scope=set%3Astackage' 'div.ans' --text --limit 10
```

For package and module, rerun step 1 with `'div.result__actions' --text` — same order as the signatures.
