# Gigadev: The Card Game — Design Doc v1

A solo (optionally co-op) roguelike deckbuilder about running a dev shop.
Designed to be **playable as a real physical card game**; the browser version
is a faithful digital implementation, not a different game.

## Design principle

Because this must work on a tabletop, every rule obeys one constraint:

> **No hidden math. State tracks itself.**

We avoid anything a computer would normally hide (probability tables, derived
stats, per-turn recalculation). Instead, game state lives in things you can see
and touch: cards in piles, tokens on tracks, dice you roll. The cleverest
mechanics are the ones that bookkeep themselves.

---

## The core tension

Ship features for points **now**, or pay down the cost of shipping fast so it
doesn't bury you **later**. That's the whole game:

- **Building features** scores Reputation but creates **Tech Debt**.
- **Tech Debt** literally clogs your team (see the engine below).
- **Refactor, Test, and Backup** cost a turn now but prevent catastrophe.

A good run balances the two. A greedy run ships fast and collapses.

---

## Components (physical)

- **Dev Deck** — your team's capabilities. In v1 this is a **fixed starting
  deck** (no acquisition yet — see *Deck growth* below).
- **Event Deck** — the world acting on you: customer requests, stakeholder
  pivots, disasters, morale checks. Shuffled per act.
- **Debt supply** — a stack of identical junk cards you draw from when you Build
  and return to when you Refactor (see engine).
- **Tokens**: Money (₿), Reputation (★). Two small tracks or token piles.
- **One d6** — used only where weighted randomness is needed.
- **Feature request cards** — show an icon "shape" to fulfill.

That's it: two decks, a supply pile, two tracks, a die.

---

## Resources

| Resource         | Tracked by                                         | Floor / Ceiling         |
|------------------|----------------------------------------------------|-------------------------|
| **Capacity**     | Fixed actions per sprint (e.g. play up to 3 cards) | —                       |
| **Money ₿**      | Token pile / track                                 | Bankruptcy / *The Exit* |
| **Reputation ★** | Track                                              | Hit 0 / *The Legend*    |
| **Tech Debt**    | Debt cards in your deck (self-tracking!)           | Too many = team stalls  |

Capacity is deliberately *not* a token you spend — it's just "you may take N
actions per sprint." Less bookkeeping.

**Money and Reputation are symmetric now**: each has a *floor you must defend*
and a *ceiling you can win at*. Neither is "the score" — see the tension below.

---

## The second tension: cash vs. cred

The Debt mechanic is one axis (ship fast vs. stay sustainable). This is the
second, orthogonal axis, and it's what opens multiple win conditions:

> **You can chase Money or you can chase Reputation, but the same scarce
> capacity buys only one of them per sprint.**

Three things force the two apart so you can't simply hoard both:

1. **The two requests compete.** *Customer Requests* pay **Reputation**
   (happy users spread word of mouth); *Stakeholder Requests* pay **Money**
   (investors fund the work). Both are shape-gated and they **expire**. With
   limited actions you usually clear one and let the other lapse — every sprint
   is "cred work or cash work?"
2. **Salaries drain Money every sprint.** A fixed upkeep (e.g. −1₿ per sprint)
   means even a pure-Reputation player must keep earning cash or go bankrupt.
   Money can never be fully ignored.
3. **Both are spendable, not just hoarded.** You can convert, but only at a
   loss, so a focused player always beats a converter:
   - **PR push** — spend ₿ to gain ★ (buy your way to fame, inefficiently).
   - **Consulting gig** — spend ★ to gain ₿ (cash in your prestige, burning it).
   These live on a couple of Event cards (post-v1), not the core five.

The upshot: you're always defending **two floors** (don't go bankrupt, don't
lose all Reputation) while pushing **one ceiling**. That single shape — *floor
on both, ceiling on one* — is the whole tension.

---

## The engine: Tech Debt as deck pollution

This is the heart of the physical design, and it needs **zero tracking**.

- **Where Debt cards come from:** a shared **Debt supply** — a stack of
  identical junk cards beside the play area (like Curses in Dominion or Wounds
  in Slay the Spire). Every **Build** takes 1 Debt card from the supply into
  your **discard pile**, so it cycles back into your hand later.
- Debt cards do nothing useful. They take up space in your hand, wasting draws —
  exactly like real tech debt eating your team's capacity.
- **Refactor returns a Debt card to the supply** (it leaves your deck for good).
- The more Debt cards you've accumulated, the worse disasters hit (below).

The debt is *visible and physical* — it's the junk in your hands, and the supply
pile shows the whole table how much rot is in play. No debt meter to update, no
formula. You feel it every time you draw a dead card.

### Disasters that scale with debt — without math

When a **Disaster** event resolves (e.g. *Prod DB gone*), severity scales with
your debt automatically:

> *Prod DB gone: Reveal your discard pile. Lose ★ equal to the number of Debt
> cards in it (max 6), unless you have a played **Backup** card — then ignore.*

You just count the junk you already created. The punishment is proportional and
self-evident. **Backups / Tests** act as insurance: play them ahead of time to
cancel or halve specific disaster types.

### Morale / bad DX

> *Burnout: If you have 3+ Debt cards in hand, a dev quits this sprint — take
> only 2 actions instead of 3.*

Again: the condition is visible in your hand. No hidden morale stat.

---

## Card types (Dev Deck)

**One effect per card type.** Every card costs one action to play (so action
isn't worth listing); the only special price is Debt. The v1 Dev Deck is built
from exactly these five cards in fixed quantities. Numbers are placeholders for
the tuning pass.

| Card               | Single effect                                 | Price        |
|--------------------|-----------------------------------------------|--------------|
| **Build**          | Ship a feature to fulfill a Request           | +1 Debt card |
| **Refactor**       | Trash 1 Debt card. *That is all it does.*     | —            |
| **Agile Maneuver** | Reshape one feature icon to fit a request     | —            |
| **Test**           | Prevent the next *Bug / Audit* event          | —            |
| **Backup**         | Prevent the next *data-loss disaster*         | —            |

The split the strict version forces:

- **Refactor is purely hygienic** — it removes Debt, full stop. It can never
  touch a feature's shape.
- **Agile Maneuver is purely about shape** — it bends a feature to fit an
  awkward request, and never touches Debt.

Two different problems (rot vs. fit), two different cards. Neither bleeds into
the other.

We deliberately dropped earlier overlaps: "Pay Down Debt" was just a second
Refactor, and the fast/clean Build split was two cards doing one job. **Hire**
is gone too — with a fixed v1 deck there's nothing to buy (see *Deck growth*).

*(Draw-smoothing like Spike/Research is noted as a possible later addition, not
part of the minimal set.)*

### Deck growth (deferred to v2)

**v1 has none.** Everyone plays the same fixed deck so we can prove the core
loop is fun before adding progression. The deck still *changes* during a run —
through Debt accruing and Refactoring cleaning it — just not by gaining new
cards.

When we add growth in v2, the acquirable pool is **upgraded versions of the five
base cards** (e.g. Senior Refactor trashes 2 Debt; Automated Tests stay
permanently attached; a Build that ships with no Debt). The acquisition method
(a between-acts draft vs. a Money-bought Market) is an open v2 question.

Some of those upgrades will be **"feature creep" cards** — powerful but they add
Debt when played (a flashy new framework that bloats the codebase), turning each
acquisition into its own ship-fast-vs-sustainable choice. Parked until growth
exists to hold them.

---

## Feature requests & "shape" (kept physical)

Requests are Event cards showing a small **icon shape** to satisfy — the simple
tag-match version, because it reads instantly on a physical card:

> *Customer request: needs* 🟦🟦🟩 *. Fulfill by playing Build cards whose icons
> cover this shape. Reward: +3★, +2₿. Ignore: −2★.*

- Each **Build** card shows 1–2 icons; you combine cards to "cover" the request.
- **Agile Maneuver** recolors one icon to flex an awkward request into reach.
- Unmet requests at sprint end cost Reputation.

This gives a little puzzle every sprint without any tracked state — it's all
right there on the cards. (We can deepen to composable slots later if it's fun;
v1 stays tag-match.)

---

## Events: everything the world can throw at you

The **Event Deck** is the world acting on you. The design rule for events
mirrors the whole game: **every event must be answerable by a tool the player
already has.** If an event has no lever, it's just unfair RNG — cut it.

### How events arrive: the Incoming slot

Disasters and pivots are **telegraphed**, not instant. When drawn they go
face-up into an **Incoming** slot and resolve at the **end of next sprint**.
This gives you exactly one sprint to respond — or to decide the hit is small
enough to eat. Defenses become decisions under information, not blind insurance.

Requests and immediate events resolve the sprint they appear (requests then
linger until fulfilled or they expire).

### The six families

- **Customer Request** — needs a feature shape; pays Reputation, expires for a
  Reputation hit if ignored. *Lever:* **Build** (+ Agile Maneuver to flex shape).
- **Stakeholder Request** — bigger shape, internal priority; pays Money, harsher
  Money penalty if ignored. *Lever:* **Build** / prioritizing it.
- **Disaster** (data-loss) — lose Reputation equal to the Debt cards in your
  discard. *Lever:* **Backup** cancels it; low Debt shrinks it.
- **Quality check** (Bug / Audit) — a shipped feature is penalized if untested.
  *Lever:* **Test** played on that feature.
- **Morale / DX** (Burnout) — 3+ Debt cards in hand costs you an action next
  sprint. *Lever:* **Refactor** to keep Debt low. No card cancels it.
- **Pivot** (Scope change) — an Incoming request's shape changes. *Lever:*
  **Agile Maneuver**.

### Boons — the relief valve (not everything is punishment)

- **Quiet Sprint** — no event; a free turn. *Rewards* doing hygiene with it.
- **Hackathon** — one Build this sprint costs no Debt. *Rewards* a ready Build.

*(A third Boon, **Hiring Fair**, returns alongside deck growth in v2 — it needs
a Market to refresh, so it's parked for now.)*

### The completeness check: one card answers each family

This is what validates "one effect per card." Each Dev card is *the answer* to
exactly one threat — no dead cards, no unanswerable threats:

- **Build** → Requests
- **Test** → Quality checks
- **Backup** → Disasters
- **Refactor** → Morale (and quietly shrinks Disaster severity)
- **Agile Maneuver** → Pivots (and flexing awkward request shapes)

With the fixed v1 deck, this is a clean **5-card / 5-family** map: every card is
the answer to exactly one threat, and every threat has an answer. **Refactor**
is the only card pulling double duty (morale + disaster severity), which is
fitting — it's the hygiene card, and hygiene helps everywhere. Boons aren't on
this list on purpose: no single card "answers" them; you cash them in with
*preparation*.

### How the player influences events — the four levers

1. **Prevent** — play a defense (Test, Backup) into the Incoming window to cancel.
2. **Shrink** — keep Debt low so Disasters and Morale hits stay small (Refactor).
3. **Adapt** — reshape with Agile Maneuver when a request is awkward or pivots.
4. **Prepare** — bank Money against salaries and set up your hand to fulfill
   Requests before they expire.

What the player does **not** control is *which* event is drawn — that's the
shuffled deck. You control the *outcome*, not the *draw*.

### Minimal core set for v1

To prove the loop, the first printable build needs:
**both** Customer Request (Reputation) **and** Stakeholder Request (Money) —
they're the whole cash-vs-cred tension, and without the Money source salaries
just bankrupt you — plus Disaster (data-loss), Quality check (Bug), Burnout, and
Quiet Sprint. Pivots, the Hackathon/Hiring Fair Boons, and the lossy
conversions come once the core loop is proven fun.

---

## A sprint (one turn), step by step

1. **Reveal an Event** from the Event Deck and apply/queue it (request, pivot,
   disaster roll, morale check).
2. **Draw** up to your hand size (e.g. 5).
3. **Take up to N actions** (Capacity): play cards to build features, refactor,
   test, arm backups, and fulfill requests.
4. **Resolve** fulfilled requests → gain ★ or ₿ per the request. Apply Debt from
   each Build.
5. **Pay salaries** → lose ₿ upkeep. If this bankrupts you, the run ends.
6. **Sprint end**: unmet obligations cost ★; expired requests lapse. Then
   **discard your entire hand** — every card, played *and* unplayed, goes to the
   discard pile. You do **not** carry cards over between sprints.
   When the Dev Deck runs out, shuffle the discard (Debt and all) into a fresh
   one.

**Why full discard?** It's what makes Debt bite. Because your whole hand cycles
every sprint, Debt cards keep coming back around to clog future hands — you
can't just hold them out of the way. It also means you see your whole deck
regularly, so Refactoring to thin it genuinely pays off.

**Persistent cards are the exception.** A **Test** attached to a feature and an
armed **Backup** sit in a play area, not your hand — they stay until the event
they guard against fires (or the feature leaves play), then go to discard.

Repeat for the act.

---

## Roguelike run structure

- A **run** = 3 acts. Each act is a fixed number of sprints with its own
  shuffled Event Deck.
- **Escalation**: later acts have nastier disasters and bigger requests, so the
  debt you let slide in Act 1 comes due in Act 3.
- **What evolves within a v1 run is your deck's *cleanliness*, not its contents**:
  the deck is fixed, but every Build pollutes it with Debt and every Refactor
  cleans it, so the deck you carry into Act 3 reflects how you've played. (Adding
  *new* cards — the draft/market — is the v2 deck-growth layer.)
- **Permadeath**: dropping below *either* floor — bankruptcy (₿ < 0) or
  Reputation hitting 0 — **ends the run immediately**. You're defending two
  floors at once while pushing one ceiling (see *Win conditions*).
- **Procedural feel** comes from the shuffled Event Deck + your shifting Debt
  load — the physical substitute for procedural generation.

### Meta-progression (optional, for the digital version)

Unlock new card types / starting decks / harder "client tiers" between runs.
Optional physically (campaign envelopes / unlock checklist), natural digitally.

---

## Win conditions

Because Money and Reputation pull apart, there isn't one score — there are
**multiple ways to win a run**, plus a survival floor under all of them.

**First, survive.** Drop below either floor at any point and the run ends in
failure, full stop:

- **Bankruptcy** — Money below 0 (salaries went unpaid).
- **Reputation collapse** — Reputation hits 0.

**Then, if you survive all three acts, your *ending* is whichever ceiling you
reached:**

- **The Exit** *(Money win)* — Money ≥ threshold. You sell the shop and cash out.
- **The Legend** *(Reputation win)* — Reputation ≥ threshold. An industry name.
- **The Institution** *(balance win)* — *both* above a moderate bar. The hardest
  finish and the highest prestige: a shop that's respected *and* solvent.
- **Lifestyle business** *(no ceiling)* — you survived but hit no threshold. A
  modest, honest ending — not a loss, not a triumph.

This is the payoff of the two-currency tension: a Money-focused run and a
Reputation-focused run are *both* winning strategies that play completely
differently, and the balanced run is a distinct, harder target on top. Debt
still taxes every route — it just isn't the score anymore.

---

## What makes it tick (summary)

- **Two crossed tensions**: *ship now vs. pay later* (Debt = junk cards) and
  *cash vs. cred* (Money vs. Reputation, competing for one scarce capacity).
- **Disasters scale with your own visible mistakes** — fair, legible, tense.
- **Requests are tiny puzzles** read straight off the cards.
- **Multiple win conditions**: floor on both currencies, ceiling on one.
- **Permadeath + escalation + a shifting Debt load** give it the roguelike arc
  (deckbuilding proper arrives in v2).

---

## Open questions / tuning knobs (next pass)

1. **Solo only, or co-op?** Co-op (shared shop, each player a "squad") is a
   small rules add-on.
2. **Hand size & actions per sprint** — the master dials for difficulty.
3. **How punishing should Debt cards be?** Pure dead weight, or do some have a
   nasty triggered downside when drawn?
4. **Act length** — fixed sprint count, or "until the Event Deck runs out"?
5. **Win condition** beyond high score? E.g. survive 3 acts above a ★ threshold.
6. **Card count for a print-and-play v1** — target ~60–80 cards total.

---

*Next step after sign-off: a card list with concrete numbers for a printable
v1 vertical slice, then the shared Scala game-state model.*