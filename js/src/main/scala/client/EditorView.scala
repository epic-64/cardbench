package client

import com.raquo.laminar.api.L.*
import engine.*
import EditorFields.*

/** The visual form editor for one game definition: forms over its cards, rules,
  * and stacks. A single `Var[GameDefinition]` is the working copy; every control
  * reads its starting value from it and writes edits back through bounds-safe
  * helpers. Save hands the working copy up to `AppView` (which persists it);
  * cancel discards it.
  *
  * Each list section re-renders its rows only when items are *added or removed*
  * — the row signals key off the list's `size`, made `.distinct` — so typing in a
  * field never rebuilds the row and never steals focus. See [[EditorFields]].
  */
object EditorView:

  def view(
    initial: GameDefinition,
    onSave: GameDefinition => Unit,
    onCancel: () => Unit,
  ): Element =
    // Every stack lives in a group; there is always at least one. Games authored
    // before groups existed (their stacks carry no group, or a stale id) are pulled
    // into a default `group1` here, so the editor never has to special-case an
    // "ungrouped" state. A lone group is hidden as a group later (see `stacksSection`),
    // so a simple game still reads as a plain stack list.
    val draft = Var(EditorView.withDefaultGroup(initial))

    // Bumped whenever an effect is reordered. The effect rows otherwise rebuild
    // only when the *kinds* of effects change, so swapping two same-kind effects
    // (two Deals, say) wouldn't refresh their uncontrolled fields — this tick
    // forces the rebuild that re-reads each row's new position.
    val reorderTick = Var(0)

    // Bumped whenever stacks are regrouped or reordered (by drag and drop) or a
    // group is added/removed. The stack rows otherwise rebuild only when their
    // *count* changes, so a reorder that keeps the count would leave the rows
    // showing their old positions — this tick forces the rebuild that re-reads
    // each row's new index. Renaming a group does *not* bump it, so typing in a
    // group's name never rebuilds the rows beneath it (and never steals focus).
    val stackTick = Var(0)

    // The stack currently being dragged, and the group its pointer is hovering over
    // — drives the drop logic and the drop-target highlight respectively.
    val draggedStack = Var(Option.empty[StackId])
    val dragOverGroup = Var(Option.empty[String])

    // The live id pools the reference drop-downs choose from: card ids for triggers
    // and stack contents, stack ids for triggers and effects. `.distinct` keeps a
    // box from rebuilding unless the *set of ids* actually changes (not on every
    // title or colour keystroke).
    val cardIds  = draft.signal.map(_.catalog.cards.map(_.id.value)).distinct
    val stackIds = draft.signal.map(_.setup.stacks.map(_.id.value)).distinct

    // ── catalog ──────────────────────────────────────────────────────────────
    def setCards(f: List[CardDef] => List[CardDef]): Unit =
      draft.update(d => d.copy(catalog = CardCatalog(f(d.catalog.cards))))
    def updateCard(i: Int)(f: CardDef => CardDef): Unit =
      setCards(cs => cs.lift(i).fold(cs)(c => cs.updated(i, f(c))))

    def cardRow(i: Int): Element =
      val c = draft.now().catalog.cards(i)
      // The accent bar tracks the card's colour live as it is edited.
      val colorSig = draft.signal.map(_.catalog.cards.lift(i).fold(c.color)(_.color)).distinct
      div(
        cls := "editor-card",
        div(cls := "editor-card-bar", backgroundColor <-- colorSig),
        div(
          cls := "editor-card-body",
          textField("Id", c.id.value, v => updateCard(i)(_.copy(id = CardDefId(v)))),
          textField("Title", c.title, v => updateCard(i)(_.copy(title = v))),
          textAreaField("Description", c.description, v => updateCard(i)(_.copy(description = v))),
          div(
            cls := "editor-card-row",
            textField("↗", c.corners.topRight, v => updateCard(i)(cd => cd.copy(corners = cd.corners.copy(topRight = v)))),
            textField("↙", c.corners.bottomLeft, v => updateCard(i)(cd => cd.copy(corners = cd.corners.copy(bottomLeft = v)))),
            textField("↘", c.corners.bottomRight, v => updateCard(i)(cd => cd.copy(corners = cd.corners.copy(bottomRight = v)))),
          ),
          div(
            cls := "editor-card-row",
            colorField("Color", c.color, v => updateCard(i)(_.copy(color = v))),
            div(
              cls := "editor-card-actions",
              duplicateButton(() => setCards(cs => cs.lift(i).fold(cs)(c => cs.patch(i + 1, List(c.copy(id = CardDefId(s"${c.id.value}-copy"))), 0)))),
              removeButton(() => setCards(cs => cs.patch(i, Nil, 1))),
            ),
          ),
        ),
      )

    val cardsSection = section(
      "Cards",
      "+ Add card",
      () => setCards(_ :+ CardDef(CardDefId("new-card"), "#888888", "New Card", "")),
      draft.signal.map(_.catalog.cards.size),
      cardRow,
      rowsClass = "editor-cards-grid",
    )

    // ── rulebook ─────────────────────────────────────────────────────────────
    def setRules(f: List[Rule] => List[Rule]): Unit =
      draft.update(d => d.copy(rulebook = Rulebook(f(d.rulebook.rules))))
    def updateRule(i: Int)(f: Rule => Rule): Unit =
      setRules(rs => rs.lift(i).fold(rs)(r => rs.updated(i, f(r))))

    // The trigger is always a `Moved(card, to)`; edit one field, keep the other.
    def setTriggerCard(i: Int, card: CardDefId): Unit =
      updateRule(i)(r => r.trigger match { case Trigger.Moved(_, to) => r.copy(trigger = Trigger.Moved(card, to)) })
    def setTriggerTo(i: Int, to: StackId): Unit =
      updateRule(i)(r => r.trigger match { case Trigger.Moved(c, _) => r.copy(trigger = Trigger.Moved(c, to)) })

    def effectRow(i: Int, j: Int): Element =
      def updateDeal(g: Effect.Deal => Effect.Deal): Unit =
        updateRule(i): r =>
          r.copy(effects = r.effects.lift(j).fold(r.effects) {
            case d: Effect.Deal => r.effects.updated(j, g(d))
            case _              => r.effects
          })
      def updateShuffle(g: Effect.Shuffle => Effect.Shuffle): Unit =
        updateRule(i): r =>
          r.copy(effects = r.effects.lift(j).fold(r.effects) {
            case s: Effect.Shuffle => r.effects.updated(j, g(s))
            case _                 => r.effects
          })
      def updateManual(g: Effect.Manual => Effect.Manual): Unit =
        updateRule(i): r =>
          r.copy(effects = r.effects.lift(j).fold(r.effects) {
            case m: Effect.Manual => r.effects.updated(j, g(m))
            case _                => r.effects
          })
      def setEffectKind(kind: String): Unit =
        updateRule(i)(r => r.copy(effects = r.effects.lift(j).fold(r.effects)(e => r.effects.updated(j, withEffectKind(e, kind)))))
      def removeEffect(): Unit =
        updateRule(i)(r => r.copy(effects = r.effects.patch(j, Nil, 1)))
      // Swap this effect with its neighbour `delta` away, then nudge `reorderTick`
      // so the rows rebuild even when the two share a kind.
      def moveEffect(delta: Int): Unit =
        updateRule(i): r =>
          val k = j + delta
          r.effects.lift(k).fold(r)(_ => r.copy(effects = r.effects.updated(j, r.effects(k)).updated(k, r.effects(j))))
        reorderTick.update(_ + 1)
      def dealField(pick: Effect.Deal => String): Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).flatMap(_.effects.lift(j)) match { case Some(d: Effect.Deal) => pick(d); case _ => "" }).distinct
      def shuffleField: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).flatMap(_.effects.lift(j)) match { case Some(s: Effect.Shuffle) => s.stack.value; case _ => "" }).distinct
      val effect    = draft.now().rulebook.rules(i).effects(j)
      val count     = draft.now().rulebook.rules(i).effects.size
      // One type select per effect; picking another kind rewrites this slot. The
      // type-specific fields follow. (The row rebuilds on a kind change — see
      // `effectKinds` in `ruleRow` — so the right fields always show.)
      val kindField = selectField("Effect", effectKindOptions, effectKind(effect), setEffectKind)
      // Reorder/remove controls, grouped and pinned to the right of each effect.
      val actions = div(
        cls := "editor-effect-actions",
        if j > 0 then moveButton("↑", "Move up", () => moveEffect(-1)) else emptyNode,
        if j < count - 1 then moveButton("↓", "Move down", () => moveEffect(1)) else emptyNode,
        removeButton(() => removeEffect()),
      )
      effect match
        case Effect.Deal(_, _, dealCount) =>
          div(
            cls := "editor-effect",
            kindField,
            idSelectField("from", stackIds, dealField(_.from.value), v => updateDeal(_.copy(from = StackId(v)))),
            idSelectField("to", stackIds, dealField(_.to.value), v => updateDeal(_.copy(to = StackId(v)))),
            numberField("Count", dealCount, n => updateDeal(_.copy(count = n))),
            actions,
          )
        case _: Effect.Shuffle =>
          div(
            cls := "editor-effect",
            kindField,
            idSelectField("Stack", stackIds, shuffleField, v => updateShuffle(_.copy(stack = StackId(v)))),
            actions,
          )
        case Effect.Manual(description) =>
          div(
            cls := "editor-effect",
            kindField,
            textAreaField("Description", description, v => updateManual(_.copy(description = v))),
            actions,
          )

    def ruleRow(i: Int): Element =
      def triggerCard: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).map(_.trigger) match { case Some(Trigger.Moved(c, _)) => c.value; case _ => "" }).distinct
      def triggerTo: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).map(_.trigger) match { case Some(Trigger.Moved(_, s)) => s.value; case _ => "" }).distinct
      // Rebuild the effect rows when one is added, removed, *or* retyped: the signal
      // keys off the list of effect kinds, not just its length, so switching a Deal
      // to a Shuffle re-renders that row with the new effect's fields.
      val effectKinds: Signal[List[String]] =
        draft.signal.map(_.rulebook.rules.lift(i).fold(List.empty[String])(_.effects.map(effectKind))).distinct
      div(
        cls := "editor-rule",
        div(
          cls := "editor-rule-head",
          span(cls := "rule-badge", "When"),
          idSelectField("Card", cardIds, triggerCard, v => setTriggerCard(i, CardDefId(v))),
          idSelectField("lands on stack", stackIds, triggerTo, v => setTriggerTo(i, StackId(v))),
          removeButton(() => setRules(rs => rs.patch(i, Nil, 1))),
        ),
        div(
          cls := "editor-rule-body",
          div(
            cls := "editor-rule-effects-head",
            span(cls := "rule-badge rule-badge-then", "Then"),
            addButton("+ Add effect", () => updateRule(i)(r => r.copy(effects = r.effects :+ Effect.Deal(StackId(""), StackId(""))))),
          ),
          div(cls := "editor-effects", children <-- effectKinds.combineWith(reorderTick.signal).map((kinds, _) => kinds.indices.toList.map(j => effectRow(i, j)))),
        ),
      )

    val rulesSection = section(
      "Rules",
      "+ Add rule",
      () => setRules(_ :+ Rule(Trigger.Moved(CardDefId(""), StackId("")), Nil)),
      draft.signal.map(_.rulebook.rules.size),
      ruleRow,
    )

    // ── setup (stacks) ─────────────────────────────────────────────────────────
    def setStacks(f: List[StackSpec] => List[StackSpec]): Unit =
      draft.update(d => d.copy(setup = d.setup.copy(stacks = f(d.setup.stacks))))
    def updateStack(i: Int)(f: StackSpec => StackSpec): Unit =
      setStacks(ss => ss.lift(i).fold(ss)(s => ss.updated(i, f(s))))

    def spawnRow(i: Int, j: Int): Element =
      def updateSpawn(g: SpawnSpec => SpawnSpec): Unit =
        updateStack(i): s =>
          s.copy(contents = s.contents.lift(j).fold(s.contents)(sp => s.contents.updated(j, g(sp))))
      val sp = draft.now().setup.stacks(i).contents(j)
      def spawnCard: Signal[String] =
        draft.signal.map(_.setup.stacks.lift(i).flatMap(_.contents.lift(j)).fold("")(_.card.value)).distinct
      div(
        cls := "editor-subrow",
        idSelectField("Card", cardIds, spawnCard, v => updateSpawn(_.copy(card = CardDefId(v)))),
        numberField("Count", sp.count, n => updateSpawn(_.copy(count = n))),
        removeButton(() => updateStack(i)(s => s.copy(contents = s.contents.patch(j, Nil, 1)))),
      )

    // Copy a stack whole — including every spawn in its contents — with a fresh id
    // and a small position nudge so the duplicate doesn't sit exactly on top of the
    // original on the board.
    def duplicateStack(s: StackSpec): StackSpec =
      s.copy(
        id = StackId(s"${s.id.value}-copy"),
        position = s.position.copy(x = s.position.x + 20, y = s.position.y + 20),
      )

    def stackRow(i: Int): Element =
      val s = draft.now().setup.stacks(i)
      // Width only applies to a Row — a Pile is always one card wide — so the field
      // appears and disappears as the layout select changes (the row itself only
      // rebuilds on add/remove, so this has to be its own signal).
      val isRow = draft.signal.map(_.setup.stacks.lift(i).fold(false)(_.layout == Layout.Row)).distinct
      div(
        cls := "editor-stack",
        // Drop onto a stack tile = drop the dragged stack *before* this one, into
        // this stack's group. The id is read live so an in-flight id edit can't
        // make the target stale; a drop onto itself is a no-op.
        onDragOver.preventDefault --> (_ => ()),
        onDrop.preventDefault.stopPropagation --> { _ =>
          draggedStack.now().foreach: dragged =>
            draft.now().setup.stacks.lift(i).map(_.id).foreach(target => moveStackBefore(dragged, target))
          draggedStack.set(None)
          dragOverGroup.set(None)
        },
        div(
          cls := "editor-stack-head",
          // The handle is the only draggable part, so the tile's inputs stay fully
          // editable (a draggable container would swallow text selection).
          span(
            cls := "editor-drag-handle",
            title := "Drag to reorder or move between groups",
            draggable := true,
            "⠿",
            onDragStart --> (_ => draggedStack.set(draft.now().setup.stacks.lift(i).map(_.id))),
            onDragEnd --> { _ => draggedStack.set(None); dragOverGroup.set(None) },
          ),
          textField("Id", s.id.value, v => updateStack(i)(_.copy(id = StackId(v)))),
          textField("Label", s.label, v => updateStack(i)(_.copy(label = v))),
          div(
            cls := "editor-card-actions",
            duplicateButton(() => setStacks(ss => ss.lift(i).fold(ss)(s => ss.patch(i + 1, List(duplicateStack(s)), 0)))),
            removeButton(() => setStacks(ss => ss.patch(i, Nil, 1))),
          ),
        ),
        div(
          cls := "editor-row",
          selectField("Facing", facingOptions, s.facing, f => updateStack(i)(_.copy(facing = f))),
          selectField("Arrangement", arrangementOptions, s.arrangement, a => updateStack(i)(_.copy(arrangement = a))),
          selectField("Layout", layoutOptions, s.layout, l => updateStack(i)(_.copy(layout = l))),
          child <-- isRow.map:
            case true  => numberField("Width", draft.now().setup.stacks.lift(i).fold(3)(_.areaWidth), n => updateStack(i)(_.copy(width = Some(n))))
            case false => emptyNode,
        ),
        contentsSubsection(i),
      )

    // The contents are summarised by default — a count of distinct cards and of
    // total cards — and the per-card rows stay hidden behind a toggle so the stack
    // list reads as a compact overview until you choose to edit the cards.
    def contentsSubsection(i: Int): Element =
      val shown = Var(false)
      // Recomputed only when the contents actually change, so typing elsewhere in
      // the stack never refreshes the summary line.
      val summary = draft.signal
        .map(_.setup.stacks.lift(i).fold(List.empty[SpawnSpec])(_.contents))
        .map(cs => (cs.map(_.card.value).distinct.size, cs.map(_.count).sum))
        .distinct
      div(
        cls := "editor-subsection",
        div(
          cls := "editor-subsection-head",
          span(child.text <-- summary.map((unique, total) => s"Contents — $unique unique, $total total")),
          button(
            cls := "btn btn-add",
            child.text <-- shown.signal.map(s => if s then "Hide cards" else "Edit cards"),
            onClick --> (_ => shown.update(!_)),
          ),
        ),
        div(
          display <-- shown.signal.map(s => if s then "block" else "none"),
          div(cls := "editor-subsection-head", span("Cards"), addButton("+ Add cards", () => updateStack(i)(s => s.copy(contents = s.contents :+ SpawnSpec(CardDefId(""), 1))))),
          div(cls := "editor-rows", children <-- indexed(draft.signal.map(_.setup.stacks.lift(i).fold(0)(_.contents.size)), j => spawnRow(i, j))),
        ),
      )

    // ── stack groups (editor-only organisation) ───────────────────────────────
    def setGroups(f: List[StackGroup] => List[StackGroup]): Unit =
      draft.update(d => d.copy(setup = d.setup.copy(groups = f(d.setup.groups))))

    // A fresh group id that can't collide with an existing one, even across many
    // adds in one session (timestamps could repeat) — the suffix keeps it unique.
    def freshGroupId(): String =
      val taken = draft.now().setup.groups.map(_.id).toSet
      LazyList.from(0).map(n => s"group-$n").find(!taken.contains(_)).get

    def addGroup(): Unit =
      setGroups(_ :+ StackGroup(freshGroupId(), "New Group"))
      stackTick.update(_ + 1)

    // Rename touches only the label, so stacks keep referencing the group by its
    // stable id — and it deliberately skips `stackTick`, so typing never rebuilds.
    def renameGroup(id: String, name: String): Unit =
      setGroups(gs => gs.map(g => if g.id == id then g.copy(name = name) else g))

    // Deleting a group moves its stacks into the first remaining group rather than
    // dropping them. There's always one group, so this only fires when at least two
    // exist (the delete control is hidden on a lone group — see `stacksSection`).
    def removeGroup(id: String): Unit =
      val remaining = draft.now().setup.groups.filterNot(_.id == id)
      remaining.headOption.foreach: target =>
        setStacks(ss => ss.map(s => if s.group == id then s.copy(group = target.id) else s))
        setGroups(_ => remaining)
        stackTick.update(_ + 1)

    // Move the dragged stack so it sits just before `target` in the flat list and
    // adopts `target`'s group. Working by id keeps it correct no matter how indices
    // shifted; dropping a stack before itself is a no-op.
    def moveStackBefore(dragged: StackId, target: StackId): Unit =
      if dragged != target then
        setStacks: ss =>
          (ss.find(_.id == dragged), ss.find(_.id == target)) match
            case (Some(m), Some(t)) =>
              val without = ss.filterNot(_.id == dragged)
              without.patch(without.indexWhere(_.id == target), List(m.copy(group = t.group)), 0)
            case _ => ss
        stackTick.update(_ + 1)

    // Move the dragged stack to the end of `group` — used when a stack is dropped on
    // a group's empty area rather than on another stack.
    def moveStackToGroupEnd(dragged: StackId, group: String): Unit =
      setStacks: ss =>
        ss.find(_.id == dragged) match
          case Some(m) =>
            val without = ss.filterNot(_.id == dragged)
            val last    = without.lastIndexWhere(_.group == group)
            without.patch(if last < 0 then without.size else last + 1, List(m.copy(group = group)), 0)
          case None => ss
      stackTick.update(_ + 1)

    // Add a fresh stack at the end of `group`, so the new tile lands inside the same
    // group its "+ Add stack" button sits under.
    def addStackTo(group: String): Unit =
      setStacks: ss =>
        val fresh = StackSpec(StackId("new-stack"), "New Stack", Position(0, 0), Facing.Down, Nil, group = group)
        val last  = ss.lastIndexWhere(_.group == group)
        ss.patch(if last < 0 then ss.size else last + 1, List(fresh), 0)

    // A group's container: a drop target for "move to the end of this group", its
    // stack tiles, and an "+ Add stack" button that adds into this same group.
    // `header` is the group's controls (empty for a lone, frame-less group); `group`
    // is the id this container drops into; `bare` drops the grouping frame.
    def groupContainer(header: Element, group: String, indices: List[Int], bare: Boolean): Element =
      div(
        cls := "editor-stack-group",
        // A lone group drops its frame so a simple game reads as a plain stack list.
        cls("editor-stack-group-bare") := bare,
        cls("editor-drag-over") <-- dragOverGroup.signal.map(_.contains(group)),
        onDragOver.preventDefault --> (_ => dragOverGroup.set(Some(group))),
        onDrop.preventDefault --> { _ =>
          draggedStack.now().foreach(dragged => moveStackToGroupEnd(dragged, group))
          draggedStack.set(None)
          dragOverGroup.set(None)
        },
        header,
        indices match
          case Nil => div(cls := "editor-stack-group-empty", "Drop a stack here")
          case is  => div(cls := "editor-stacks-grid", is.map(stackRow)),
        addButton("+ Add stack", () => addStackTo(group)),
      )

    def groupHeader(g: StackGroup): Element =
      div(
        cls := "editor-stack-group-head",
        textField("Group name", g.name, v => renameGroup(g.id, v)),
        removeButton(() => removeGroup(g.id)),
      )

    // The grouped stack list rebuilds only when the stack count or `stackTick`
    // changes (an add/remove, a drag, or a group add/remove) — never on a keystroke
    // — then partitions the flat list by group, each group in its authored order. A
    // lone group hides its header (rename/delete), so a simple game reads as a plain
    // stack list; the header appears once a second group exists. The "+ Add group"
    // button trails the last group; each group carries its own "+ Add stack" (see
    // `groupContainer`). There's no section card here on purpose — it would only blur
    // where one group ends and the next begins.
    val stacksSection = div(
      cls := "editor-stack-groups",
      children <-- draft.signal.map(_.setup.stacks.size).distinct.combineWith(stackTick.signal).map { (_, _) =>
        val d      = draft.now()
        val stacks = d.setup.stacks
        val solo   = d.setup.groups.sizeIs <= 1
        def indicesIn(gid: String): List[Int] =
          stacks.indices.toList.filter(i => stacks(i).group == gid)
        d.setup.groups.map(g => groupContainer(if solo then span() else groupHeader(g), g.id, indicesIn(g.id), bare = solo))
      },
      addButton("+ Add group", () => addGroup()),
    )

    // ── stack buttons ───────────────────────────────────────────────────────
    def setButtons(f: List[StackButton] => List[StackButton]): Unit =
      draft.update(d => d.copy(setup = d.setup.copy(buttons = f(d.setup.buttons))))
    def updateButton(i: Int)(f: StackButton => StackButton): Unit =
      setButtons(bs => bs.lift(i).fold(bs)(b => bs.updated(i, f(b))))

    def buttonRow(i: Int): Element =
      val b = draft.now().setup.buttons(i)
      def onStack: Signal[String] =
        draft.signal.map(_.setup.buttons.lift(i).fold("")(_.stackId.value)).distinct
      def actionStackSig: Signal[String] =
        draft.signal.map(_.setup.buttons.lift(i).map(_.action).fold("")(actionStack(_).value)).distinct
      div(
        cls := "editor-row",
        idSelectField("On stack", stackIds, onStack, v => updateButton(i)(_.copy(stackId = StackId(v)))),
        textField("Label", b.label, v => updateButton(i)(_.copy(label = v))),
        selectField("Action", buttonActionOptions, actionKind(b.action), k => updateButton(i)(bn => bn.copy(action = withKind(bn.action, k)))),
        idSelectField("Other stack", stackIds, actionStackSig, v => updateButton(i)(bn => bn.copy(action = withStack(bn.action, StackId(v))))),
        numberField("Count", actionCount(b.action), n => updateButton(i)(bn => bn.copy(action = withCount(bn.action, n)))),
        removeButton(() => setButtons(bs => bs.patch(i, Nil, 1))),
      )

    val buttonsSection = section(
      "Buttons",
      "+ Add button",
      () => setButtons(_ :+ StackButton(StackId(""), "Button", ButtonAction.DealFrom(StackId("")))),
      draft.signal.map(_.setup.buttons.size),
      buttonRow,
    )

    // ── card design (layout) ──────────────────────────────────────────────────
    // One shared template for every card front: a base rectangle (size + fill) whose
    // title and description flow to fill it, plus the shared styling of the four
    // corner slots. A single object, so the panel is plain controls plus a live
    // preview rather than a list of rows.
    def updateLayout(f: CardLayout => CardLayout): Unit =
      draft.update(d => d.copy(layout = f(d.layout)))
    def updateCorner(f: CornerStyle => CornerStyle): Unit =
      updateLayout(l => l.copy(corner = f(l.corner)))

    // The preview redraws whenever the layout or the sample card (the first in the
    // catalog) changes, so adjusting a colour or corner shows on a real card at once.
    val previewData = draft.signal.map(d => (d.layout, d.catalog.cards.headOption)).distinct
    def previewCard(layout: CardLayout, sample: Option[CardDef]): Element =
      div(
        cls       := "design-preview",
        styleAttr := CardFace.vars(layout),
        div(
          cls       := "card card-front",
          styleAttr := CardFace.accent(sample.map(_.color).getOrElse("#888888")),
          CardFace.boxes(
            sample.map(_.title).getOrElse("Title"),
            sample.map(_.description).getOrElse("Card description text shows here."),
            // Fall back to placeholder glyphs so the corner styling is always visible,
            // even when the sample card leaves its own corners blank.
            sample.map(_.corners).filter(hasCorner).getOrElse(sampleCorners),
            layout.corner.fill,
          ),
        ),
      )

    val layout0 = draft.now().layout
    val designSection =
      div(
        cls := "editor-section",
        div(cls := "editor-section-head", h2("Card Design")),
        div(
          cls := "design-layout",
          div(
            cls := "design-controls",
            div(cls := "design-group", h3("Base rectangle"), div(
              cls := "editor-row",
              numberField("Width", layout0.width, n => updateLayout(_.copy(width = n))),
              numberField("Height", layout0.height, n => updateLayout(_.copy(height = n))),
              colorField("Background", layout0.background, v => updateLayout(_.copy(background = v))),
            )),
            div(cls := "design-group", h3("Text"), div(
              cls := "editor-row",
              numberField("Title %", layout0.titleFont, n => updateLayout(_.copy(titleFont = n))),
              numberField("Description %", layout0.descriptionFont, n => updateLayout(_.copy(descriptionFont = n))),
            )),
            div(cls := "design-group", h3("Corners"), div(
              cls := "editor-row",
              selectField("Fill", cornerFillOptions, layout0.corner.fill, f => updateCorner(_.copy(fill = f))),
              selectField("Shape", cornerShapeOptions, layout0.corner.shape, s => updateCorner(_.copy(shape = s))),
              selectField("Font", cornerFontOptions, layout0.corner.font, f => updateCorner(_.copy(font = f))),
            )),
          ),
          div(
            cls := "design-preview-pane",
            span(cls := "design-preview-hint", "Live preview"),
            child <-- previewData.map((l, s) => previewCard(l, s)),
          ),
        ),
      )

    def finalised(): GameDefinition =
      val d = draft.now()
      if d.name.trim.isEmpty then d.copy(name = "Untitled") else d

    // Which of the three editing tabs is showing. All three panels stay mounted
    // (so their field state and scroll survive a tab switch); only the active one
    // is displayed.
    val activeTab = Var("cards")

    def tabButton(id: String, label: String): Element =
      button(
        cls := "editor-tab",
        cls("editor-tab-active") <-- activeTab.signal.map(_ == id),
        label,
        onClick --> (_ => activeTab.set(id)),
      )

    def tabPanel(id: String, content: Element): Element =
      div(
        display <-- activeTab.signal.map(a => if a == id then "block" else "none"),
        content,
      )

    div(
      cls := "editor",
      div(
        cls := "toolbar",
        button(cls := "btn", "← Cancel", onClick --> (_ => onCancel())),
        span(cls := "toolbar-title", "Edit game"),
        button(cls := "btn btn-primary", "Save", onClick --> (_ => onSave(finalised()))),
      ),
      div(
        cls := "editor-body",
        div(
          cls := "editor-section",
          div(
            cls := "editor-row",
            label(
              cls := "field",
              span(cls := "field-label", "Name"),
              input(
                cls := "field-input",
                typ := "text",
                defaultValue := initial.name,
                onInput.mapToValue --> (v => draft.update(_.copy(name = v))),
              ),
            ),
            // At least one player — a count below one would leave nobody to take a turn.
            numberField("Players", initial.players.max(1), n => draft.update(_.copy(players = n.max(1)))),
          ),
        ),
        div(
          cls := "editor-tabs",
          tabButton("cards", "Cards"),
          tabButton("design", "Design"),
          tabButton("stacks", "Stacks"),
          tabButton("buttons", "Buttons"),
          tabButton("rules", "Rules"),
        ),
        tabPanel("cards", cardsSection),
        tabPanel("design", designSection),
        tabPanel("stacks", stacksSection),
        tabPanel("buttons", buttonsSection),
        tabPanel("rules", rulesSection),
      ),
    )

  // A titled section with an "add" button and a list of rows that rebuilds only
  // when the row count changes (so editing a field never disturbs its neighbours).
  private def section(
    title: String,
    addLabel: String,
    onAdd: () => Unit,
    count: Signal[Int],
    renderRow: Int => Element,
    rowsClass: String = "editor-rows",
  ): Element =
    div(
      cls := "editor-section",
      div(cls := "editor-section-head", h2(title), addButton(addLabel, onAdd)),
      div(cls := rowsClass, children <-- indexed(count, renderRow)),
    )

  // The current row count, deduplicated, expanded into one element per index.
  private def indexed(count: Signal[Int], renderRow: Int => Element): Signal[List[Element]] =
    count.distinct.map(n => (0 until n).toList.map(renderRow))

  // The group every stack falls into when a game has none — a normal group that can
  // be renamed or deleted like any other once more groups exist.
  private val defaultGroup = StackGroup("group1", "Group 1")

  // Guarantee the invariant the stack editor relies on: at least one group exists and
  // every stack belongs to one of them. Missing/stale stack groups are pulled into the
  // first group rather than dropped, so nothing can vanish from the editor.
  private def withDefaultGroup(d: GameDefinition): GameDefinition =
    val groups   = if d.setup.groups.isEmpty then List(defaultGroup) else d.setup.groups
    val ids      = groups.map(_.id).toSet
    val fallback = groups.head.id
    val stacks   = d.setup.stacks.map(s => if ids.contains(s.group) then s else s.copy(group = fallback))
    d.copy(setup = d.setup.copy(groups = groups, stacks = stacks))

  private val facingOptions      = List("Up" -> Facing.Up, "Down" -> Facing.Down)
  private val arrangementOptions = List("Ordered" -> Arrangement.Ordered, "Shuffled" -> Arrangement.Shuffled)
  private val layoutOptions      = List("Pile" -> Layout.Pile, "Row" -> Layout.Row)

  // The shared look of a card's corner slots, chosen in the design tab.
  private val cornerFillOptions  = List("Fill" -> CornerFill.Fill, "Outline" -> CornerFill.Outline, "None" -> CornerFill.None)
  private val cornerShapeOptions = List("Circle" -> CornerShape.Circle, "Rounded" -> CornerShape.Rounded, "Square" -> CornerShape.Square)
  private val cornerFontOptions  = List("Default" -> "inherit", "Serif" -> "Georgia, serif", "Mono" -> "monospace", "Rounded" -> "\"Comic Sans MS\", cursive")

  // True if a card fills in any of its four corners — drives whether the design
  // preview shows the card's own corners or placeholder glyphs.
  private def hasCorner(c: CardCorners): Boolean =
    Seq(c.topRight, c.bottomLeft, c.bottomRight).exists(_.nonEmpty)

  // Stand-in corner text for the design preview when the sample card has none, so
  // the chosen background/shape/font are always visible.
  private val sampleCorners = CardCorners(topRight = "3", bottomLeft = "♦", bottomRight = "✦")

  // An effect is a tagged choice too: a single "+ Add effect" button drops in a
  // Deal, and a per-row type select swaps between the kinds, carrying a stack id
  // across so retyping doesn't blank the row.
  private val effectKindOptions = List("Deal" -> "deal", "Shuffle" -> "shuffle", "Manual" -> "manual")

  private def effectKind(e: Effect): String = e match
    case _: Effect.Deal    => "deal"
    case _: Effect.Shuffle => "shuffle"
    case _: Effect.Manual  => "manual"

  private def withEffectKind(e: Effect, kind: String): Effect = (e, kind) match
    case (d: Effect.Deal, "shuffle")    => Effect.Shuffle(d.from)
    case (d: Effect.Deal, "manual")     => Effect.Manual("")
    case (s: Effect.Shuffle, "deal")    => Effect.Deal(s.stack, StackId(""))
    case (_: Effect.Shuffle, "manual")  => Effect.Manual("")
    case (_: Effect.Manual, "deal")     => Effect.Deal(StackId(""), StackId(""))
    case (_: Effect.Manual, "shuffle")  => Effect.Shuffle(StackId(""))
    case (other, _)                     => other

  // A button's action is a tagged choice ("from"/"to") carrying a stack and a
  // count; these read and rebuild it field-by-field so a tab switch never loses
  // the other half of the value.
  private val buttonActionOptions = List("Deal from" -> "from", "Deal to" -> "to")

  private def actionKind(a: ButtonAction): String = a match
    case _: ButtonAction.DealFrom => "from"
    case _: ButtonAction.DealTo   => "to"
  private def actionStack(a: ButtonAction): StackId = a match
    case ButtonAction.DealFrom(s, _) => s
    case ButtonAction.DealTo(s, _)   => s
  private def actionCount(a: ButtonAction): Int = a match
    case ButtonAction.DealFrom(_, c) => c
    case ButtonAction.DealTo(_, c)   => c
  private def withKind(a: ButtonAction, kind: String): ButtonAction = kind match
    case "from" => ButtonAction.DealFrom(actionStack(a), actionCount(a))
    case _      => ButtonAction.DealTo(actionStack(a), actionCount(a))
  private def withStack(a: ButtonAction, s: StackId): ButtonAction = a match
    case d: ButtonAction.DealFrom => d.copy(stack = s)
    case d: ButtonAction.DealTo   => d.copy(stack = s)
  private def withCount(a: ButtonAction, c: Int): ButtonAction = a match
    case d: ButtonAction.DealFrom => d.copy(count = c)
    case d: ButtonAction.DealTo   => d.copy(count = c)
