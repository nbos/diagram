tl;dr `composeAddsSub`’s `Maybe` is a **canonical-ownership flag**

`getDelta` is used only from `Diagram.Evolution.pushMut_CIs`, and only
to compute a **delta** of `Cor` terms between an old type state and a
new type state after a mutation. On the new side every add-chain must be
counted exactly once; on the old side every chain that is
destroyed/split by the mutation must be subtracted exactly
once. `composeAddsSub` decides who “owns” a chain.

---

## 1. What `composeAddsSub` tells you

For a given in-CI `super`, `composeAddsSub` scans left and right for the
maximal alternating chain of in-CIs and out-CIs that some other Add
mutation would join.

Its result is:

- `Just chains`: `super` itself is the canonical leftmost in-CI of those
  chains. So `getDelta` can use `onAddMut` directly on each returned
  chain.

- `Nothing`: there is at least one “sub” CI to the left of `super`.
  That means `super` is **not** the canonical leftmost member of the
  chain. The true chain starts further left, at one of those sub-CIs.

The relevant guard in `composeAddsSub` is exactly:

```haskell
| not (null lsubs) -> (Nothing, lsubs ++ rsubs)
```

So `Nothing` is returned precisely when `lsubs` is non-empty.

---

## 2. Why this matters for `getDelta`

`getDelta` has to produce

```text
new add-cor  -  old add-cor
```

For the new side, if `super` is not the canonical start of a chain, then
a chain built starting at `super` is only a *suffix* of the real maximal
chain. The correction `onAddMut_` for a suffix is not the same as the
correction for the whole chain, and counting both the suffix and the
whole chain would double-count symbols.

At the same time, the sub-CIs on the left are **not** in the current
mutation’s `mutCIsL`, so the loop in `pushMut_CIs` will not visit them
by itself. They are exactly the intervals whose old add-cors must be
subtracted. Therefore `composeAddsSub` returns them as `subs`, and
`getDelta` calls plain `onAddMuts` on them.

The `Nothing` branch then does two things:

```haskell
Nothing ->
  (: subsNewAddCor) <$> onAddMuts dly new_tst super
```

1. `onAddMuts` on each returned `sub` generates the real, canonical
   chains.
2. `onAddMuts` on `super` itself is also called, but without the `sub` guard.
   That is safe: plain `composeAdds` cancels if the same Add mutation
   would continue through a previous in-CI, so it will *not* return the
   full chain from `super` again. It only returns chains for which
   `super` really is canonical, e.g. a right-hand chain belonging to a
   different Add mutation.

---

## 3. Concrete example

Suppose after pushing the current mutation the local string pattern is

```text
L  O1  X  O2  R
```

where:

- `X` is the new `super` CI produced by the current mutation,
- `L` and `R` are old in-CIs, not in the current mutation’s joint type,
  so they satisfy `notInMut` and are “sub” CIs,
- `O1`, `O2` are out-CIs that some other pending Add mutation `A` would add.

In the new state, the maximal chain for `A` is

```text
[ L, O1, X, O2, R ]
```

Scans left from `X`:

1. first sees `O1` (out, addable by `A`),
2. then sees `L` (in, sub).

So `lsubs = [L]` is non-empty, and `composeAddsSub` returns `Nothing`
with `subs = [L, R]` (plus possibly other right-side sub-CIs).

The correct delta is therefore:

```text
cor_A([L,O1,X,O2,R]) - cor_A([L,O1]) - cor_A([O2,R])
```

The old chains were split at the place where the current mutation
inserted `X`; the new chain joins them.

If all five intervals have length 2, and we call the shared tail
boundaries `b1..b5`, then roughly:

```text
cor([L,O1,X,O2,R]) = {b1:-1, b2:-1, b3:-1, b4:-1}
cor([L,O1])        = {b1:-1, b2:-1}
cor([O2,R])        = {b4:-1, b5:-1}
```

so the true delta is

```text
{b3:-1, b5:+1}
```

---

## 4. What happens if the distinction is not made

### If `composeAddsSub` returned `Just` here and `L` stayed in `subs`

`getDelta` would:

- use the returned chain `[L,O1,X,O2,R]` directly, and
- also call `onAddMuts L` on the new state.

But `onAddMuts L` would itself return the same full chain
`[L,O1,X,O2,R]`, because `L` is its canonical leftmost in-CI.

`Cor.union` combines corrections with `IM.unionWith (+)`, so every
symbol in the correction gets counted **twice**:

```text
new side = cor([L,O1,X,O2,R]) + cor([L,O1,X,O2,R])
         = {b1:-2, b2:-2, b3:-2, b4:-2}
```

Then the computed delta is wrong, e.g.

```text
{b1:-1, b2:-1, b3:-2, b4:-1, b5:+1}
```

instead of `{b3:-1, b5:+1}`.

### If `composeAddsSub` returned `Just` and `L` was dropped from `subs`

Then the new side is correct, but the old side no longer subtracts
`cor([L,O1])`. The resulting delta keeps stale left-side corrections,
which is also wrong:

```text
{b1:-1, b2:-1, b3:-1, b5:+1}
```

So there is **no consistent way to include `L` in the `Just` branch**:
either the new side is double-counted, or the old side is
under-subtracted. That is precisely why a third state, `Nothing`, is
needed.

`Nothing` says:

> “`super` is not the owner of the full chain. Do not construct the
> chain here. Here are the actual owners (`subs`); use plain `onAddMuts`
> on them, and use plain `onAddMuts` on `super` only for chains that
> *it* truly owns.”

---

## 5. Bottom line

The `Maybe` split exists to preserve the invariant that every add-chain
is counted exactly once on the new side and exactly once on the old side
when computing the `Cor` delta in `pushMut_CIs`.

- `Just` means `super` is canonical owner: use the returned chains.
- `Nothing` means a left sub-CI is the true owner: don’t use a chain
  from `super`; use the returned `subs`, plus ordinary `onAddMuts` on
  `super` for any separately-owned chains.

Without the distinction, the `Cor` deltas for affected mutations would
be either double-counted on the new side or under-subtracted on the old
side, which would corrupt their `ddns`, `dnm`, and therefore their loss
values.
