# 1. String, alphabet, joint graph

Let $w = w_1w_2\cdots w_N \in \Sigma^N$ be a string over the alphabet
$\Sigma=\{0,1,\dots,m-1\}$ (`Sym = Int`), and let

$$n_i \;=\; \bigl|\{t: w_t = i\}\bigr|, \qquad \mathbf n =
(n_0,\dots,n_{m-1}), \qquad \textstyle\sum_i n_i = N .$$

For an ordered pair $(a,b)\in\Sigma^2$ let $c(a,b)$ be the number of
**non‑overlapping** occurrences of the factor $ab$ in $w$ (greedy
left‑to‑right count):

$$c(a,b)=\bigl|\{t: w_t=a,\;w_{t+1}=b\}\bigr| \quad (a\neq b),\qquad
c(a,a)=\sum_{\text{maximal } a\text{-runs of length }\ell}\Bigl\lfloor
\tfrac{\ell}{2}\Bigr\rfloor .$$

The **joint graph** is the weighted bipartite (di)graph

$$G \;=\; \bigl(\Sigma_L \sqcup \Sigma_R,\; E,\; c\bigr),\qquad
E=\{(a,b)\in\Sigma^2 : c(a,b)>0\},$$

i.e. two disjoint copies of $\Sigma$ (left = first component of a joint,
right = second), with an edge exactly where the weight is positive;
**zero‑weight edges do not exist**. Note $|E|\le\min(m^2,\,N-1)$. Write
the neighbourhoods

$$N_R(a)=\{b:(a,b)\in E\}\subseteq\Sigma_R,\qquad N_L(b)=\{a:(a,b)\in
E\}\subseteq\Sigma_L .$$

# 2. Joint types

A **joint type** is a pair of vertex sets $T=(L,R)$ with
$L\subseteq\Sigma_L$, $R\subseteq\Sigma_R$ (`JointType = JT !IntSet
!IntSet`). $T$ *is* its induced subgraph: the edge set of the type is,
by definition, the full induced set

$$E[T] \;=\; E\cap(L\times R),$$

so no edge with an endpoint outside $L\cup R$ belongs to $T$ and every
edge with both endpoints inside does. The number of joints the type
accounts for is

$$n_m \;=\; \sum_{(a,b)\in E[T]} c(a,b).$$

(Whether $n_m$ is achievable simultaneously when $L\cap R\neq\emptyset$
is an objective‑function question and out of scope here; $n_m$ is
*defined* as the edge‑weight sum.)

## 2.1 In/out neighbourhoods and dependents

All bookkeeping is a *function of $T$*; `SymEntry` caches exactly these
four quantities. For $a\in\Sigma_L$, $b\in\Sigma_R$:

| field        | left entry of $a$                 | right entry of $b$                |
|--------------|-----------------------------------|-----------------------------------|
| `isMember`   | $[\,a\in L\,]$                    | $[\,b\in R\,]$                    |
| `coSymsIn`   | $I_L(a)=N_R(a)\cap R$             | $I_R(b)=N_L(b)\cap L$             |
| `coSymsOut`  | $O_L(a)=N_R(a)\setminus R$        | $O_R(b)=N_L(b)\setminus L$        |
| `dependents` | $D_L(a)=\{b\in R: I_R(b)=\{a\}\}$ | $D_R(b)=\{a\in L: I_L(a)=\{b\}\}$ |

Invariants (all established by `init` in time $O(m+|E|)$ and to be
preserved by every mutation):

* **(I1)** $I_\bullet$ and $O_\bullet$ partition the neighbourhood:
  $I_L(a)\sqcup O_L(a)=N_R(a)$, $I_R(b)\sqcup O_R(b)=N_L(b)$. They
  depend on the *co‑side* membership only, hence they are maintained for
  **all** symbols, member or not.
* **(I2)** $b\in D_L(a) \iff a\in L\wedge I_R(b)=\{a\}$; symmetrically
  for $D_R$. Consequently $D_L(a)\subseteq I_L(a)$ and $D_R(b)\subseteq
  I_R(b)$, and **non‑members have empty dependent sets**.
* **(I3)** $D_L(a)$ is exactly the set of co‑symbols that would be
  orphaned by deleting $a$.

## 2.2 Tightness

$$T \text{ is \textbf{tight}} \iff \forall a\in L:\;I_L(a)\neq\emptyset
\;\wedge\; \forall b\in R:\;I_R(b)\neq\emptyset,$$

i.e. the induced subgraph $G[T]$ has no isolated vertex: every member
symbol participates in at least one joint of the type. Under tightness,
$I_L(a)=\emptyset\Rightarrow a\notin L$ and $I_R(b)=\emptyset\Rightarrow
b\notin R$.

Tightness is the standing invariant: the random initial type and every
applied mutation preserve it.

# 3. Mutations

$$\mathcal M \;=\; \{\mathrm{AL}\,a,\ \mathrm{AR}\,b,\
\mathrm{A2}(a,b),\ \mathrm{DL}\,a,\ \mathrm{DR}\,b,\
\mathrm{D2}(a,b)\}$$

acting on types by

$$\mathrm{AL}\,a:(L,R)\mapsto(L\cup\{a\},R),\quad
\mathrm{DL}\,a:(L,R)\mapsto(L\setminus\{a\},R),\quad
\mathrm{A2}(a,b)=\mathrm{AL}\,a\circ\mathrm{AR}\,b,\quad
\mathrm{D2}(a,b)=\mathrm{DL}\,a\circ\mathrm{DR}\,b,$$

and symmetrically on the right. A mutation is **applicable** if it only
adds non‑members / deletes members, and **admissible** at $T$ if in
addition $\mu(T)$ is tight. A two‑symbol mutation is **atomic** at $T$
if neither of its two one‑symbol factors is admissible at $T$
(one‑symbol mutations are always atomic). The mutation set of the
hill‑climber is

$$M(T)\;=\;\{\mu\in\mathcal M:\ \mu \text{ admissible and atomic at } T\}.$$

> **Theorem 1 (local characterisation; = `mutsOf` summed over all joints).** For tight $T$,
> $$
> \begin{aligned}
> \mathrm{AL}\,a\in M(T) &\iff a\notin L \ \wedge\ I_L(a)\neq\emptyset,\\
> \mathrm{AR}\,b\in M(T) &\iff b\notin R \ \wedge\ I_R(b)\neq\emptyset,\\
> \mathrm{A2}(a,b)\in M(T) &\iff (a,b)\in E \ \wedge\ I_L(a)=\emptyset \ \wedge\ I_R(b)=\emptyset,\\
> \mathrm{DL}\,a\in M(T) &\iff a\in L \ \wedge\ D_L(a)=\emptyset,\\
> \mathrm{DR}\,b\in M(T) &\iff b\in R \ \wedge\ D_R(b)=\emptyset,\\
> \mathrm{D2}(a,b)\in M(T) &\iff I_L(a)=\{b\}\ \wedge\ I_R(b)=\{a\}\quad(\text{i.e. } \{a,b\} \text{ is an isolated edge of } G[T]).
> \end{aligned}
> $$

<details>
<summary>Proof sketch</summary>

*Additions.* Adding vertices only enlarges $I_\bullet$, so no member can
become isolated; admissibility of $\mathrm{AL}\,a$ is thus $a\notin L$
plus tightness of $a$ itself, $I_L(a)\neq\emptyset$. $\mathrm{A2}(a,b)$
is admissible whenever $(a,b)\in E$, $a\notin L$, $b\notin R$ (each
endpoint is witnessed by the other), and atomic iff $\mathrm{AL}\,a$,
$\mathrm{AR}\,b$ are both inadmissible, i.e. $I_L(a)=I_R(b)=\emptyset$ —
which under tightness already implies $a\notin L,b\notin R$.

*Deletions.* Deleting $a$ orphans exactly the $b\in R$ with
$I_R(b)=\{a\}$, i.e. $D_L(a)$; hence $\mathrm{DL}\,a$ admissible $\iff
a\in L\wedge D_L(a)=\emptyset$ (existence of an in‑joint for $a$ is
automatic by tightness). If $\mathrm{DL}\,a$ and $\mathrm{DR}\,b$ are
both inadmissible, pick $b'\in D_L(a)$, $a'\in D_R(b)$;
$\mathrm{D2}(a,b)$ leaves $b'$ isolated unless $b'=b$, and $a'$ isolated
unless $a'=a$, so admissibility+atomicity forces $I_R(b)=\{a\}$ and
$I_L(a)=\{b\}$; conversely that condition makes $\mathrm{D2}(a,b)$
admissible and both factors inadmissible. $\square$ </details>

Note $M(T)$ is a **set**: `mutsOf` ranges over joints and therefore
emits duplicates (e.g. $\mathrm{DL}\,a$ once per in‑joint of $a$); the
"Books" must be set‑like (or reference‑counted).

# 4. The two obligations

Let $T$ be tight, $\mu\in M(T)$, $T'=\mu(T)$.

* **(P) push:** update the cached state from $T$ to $T'$ (fields of
  §2.1), in time $O\!\bigl(\deg(\text{flipped symbols})\bigr)$ up to
  `IntSet` logs.
* **(Δ) delta:** compute, **from the pre‑state $T$ only**,
 $$\Delta^+(\mu,T)=M(T')\setminus M(T),\qquad
 \Delta^-(\mu,T)=M(T)\setminus M(T').$$

Both are driven by the following transition laws. Only the flipped
symbols change membership, and (I1) implies only the co‑side in/out sets
move.

> **Lemma 2 (left addition).** For $\mu=\mathrm{AL}\,a_0\in M(T)$ (so $a_0\notin L$, $I_L(a_0)\neq\emptyset$):
> $$I_L'(a)=I_L(a)\ \forall a;\qquad I_R'(b)=I_R(b)\cup\{a_0\}\ \ (b\in N_R(a_0)),\ \ I_R'(b)=I_R(b)\ \text{else};$$
> $$D_R'(b)=D_R(b)\cup\bigl(\{a_0\}\text{ if } I_L(a_0)=\{b\}\bigr);\qquad D_L'(a_0)=\emptyset,\qquad D_L'(a)=D_L(a)\setminus \underbrace{\{b\in I_L(a_0): I_R(b)=\{a\}\}}_{\textstyle \text{“}\mathrm{depsLost}(a)\text{”}}\ (a\in L).$$

> **Lemma 3 (left deletion).** For $\mu=\mathrm{DL}\,a_0\in M(T)$ (so $a_0\in L$, $D_L(a_0)=\emptyset$, hence $|I_R(b)|\ge2$ for all $b\in I_L(a_0)$):
> $$I_L'(a)=I_L(a)\ \forall a;\qquad I_R'(b)=I_R(b)\setminus\{a_0\}\ (b\in N_R(a_0));$$
> $$D_R'(b)=D_R(b)\setminus\{a_0\};\qquad D_L'(a)=D_L(a)\cup\underbrace{\{b\in I_L(a_0): I_R(b)\setminus\{a_0\}=\{a\}\}}_{\textstyle\text{“}\mathrm{depsGained}(a)\text{”}} .$$

> **Lemma 4 (pair mutations).** For $\mu=\mathrm{A2}(a_0,b_0)$ (so $I_L(a_0)=I_R(b_0)=\emptyset$): additionally $I_L'(a_0)=\{b_0\}$, $I_R'(b_0)=\{a_0\}$, $I_L'(a)=I_L(a)\cup\{b_0\}$ for $a\in N_L(b_0)$, and $D_L'(a_0)=\{b_0\}$, $D_R'(b_0)=\{a_0\}$, while $D_\bullet$ is unchanged elsewhere (because no *member* has $b_0\in N_R(\cdot)$ or $a_0\in N_L(\cdot)$). Dually for $\mathrm{D2}(a_0,b_0)$ (so $I_L(a_0)=\{b_0\}$, $I_R(b_0)=\{a_0\}$): $I_L'(a_0)=I_R'(b_0)=\emptyset$, $I_R'(b)=I_R(b)\setminus\{a_0\}$, $I_L'(a)=I_L(a)\setminus\{b_0\}$, and $D_\bullet'=D_\bullet$ on all surviving members, with $D_L'(a_0)=D_R'(b_0)=\emptyset$.

Composing Theorem 1 with Lemmas 2–4 yields closed forms for
$\Delta^\pm$; they are tabulated in Part II. Cost: $\Delta^\pm$ requires
inspecting the **second neighbourhood** of the flipped symbols,
$O\bigl(\sum_{b\in N_R(a_0)}\!\deg b\bigr)$, whereas (P) is
first‑neighbourhood only.
