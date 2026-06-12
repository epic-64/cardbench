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
    val draft = Var(initial)

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
      div(
        cls := "editor-row",
        textField("Id", c.id.value, v => updateCard(i)(_.copy(id = CardDefId(v)))),
        textField("Title", c.title, v => updateCard(i)(_.copy(title = v))),
        colorField("Color", c.color, v => updateCard(i)(_.copy(color = v))),
        textAreaField("Description", c.description, v => updateCard(i)(_.copy(description = v))),
        removeButton(() => setCards(cs => cs.patch(i, Nil, 1))),
      )

    val cardsSection = section(
      "Cards",
      "+ Add card",
      () => setCards(_ :+ CardDef(CardDefId("new-card"), "#888888", "New Card", "")),
      draft.signal.map(_.catalog.cards.size),
      cardRow,
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
          r.copy(effects = r.effects.lift(j).fold(r.effects) { case d: Effect.Deal => r.effects.updated(j, g(d)) })
      def removeEffect(): Unit =
        updateRule(i)(r => r.copy(effects = r.effects.patch(j, Nil, 1)))
      def dealField(pick: Effect.Deal => String): Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).flatMap(_.effects.lift(j)) match { case Some(d: Effect.Deal) => pick(d); case _ => "" }).distinct
      draft.now().rulebook.rules(i).effects(j) match
        case Effect.Deal(_, _, count, facing) =>
          div(
            cls := "editor-subrow",
            idSelectField("Deal from", stackIds, dealField(_.from.value), v => updateDeal(_.copy(from = StackId(v)))),
            idSelectField("to", stackIds, dealField(_.to.value), v => updateDeal(_.copy(to = StackId(v)))),
            numberField("Count", count, n => updateDeal(_.copy(count = n))),
            selectField("Facing", targetFacingOptions, facing, f => updateDeal(_.copy(targetFacing = f))),
            removeButton(() => removeEffect()),
          )

    def ruleRow(i: Int): Element =
      def triggerCard: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).map(_.trigger) match { case Some(Trigger.Moved(c, _)) => c.value; case _ => "" }).distinct
      def triggerTo: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).map(_.trigger) match { case Some(Trigger.Moved(_, s)) => s.value; case _ => "" }).distinct
      div(
        cls := "editor-row editor-row-block",
        div(
          cls := "editor-row",
          idSelectField("When card", cardIds, triggerCard, v => setTriggerCard(i, CardDefId(v))),
          idSelectField("lands on stack", stackIds, triggerTo, v => setTriggerTo(i, StackId(v))),
          removeButton(() => setRules(rs => rs.patch(i, Nil, 1))),
        ),
        div(
          cls := "editor-subsection",
          div(cls := "editor-subsection-head", span("Effects"), addButton("+ Add effect", () => updateRule(i)(r => r.copy(effects = r.effects :+ Effect.Deal(StackId(""), StackId("")))))),
          div(cls := "editor-rows", children <-- indexed(draft.signal.map(_.rulebook.rules.lift(i).fold(0)(_.effects.size)), j => effectRow(i, j))),
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
      draft.update(d => d.copy(setup = GameSetup(f(d.setup.stacks))))
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

    def stackRow(i: Int): Element =
      val s = draft.now().setup.stacks(i)
      div(
        cls := "editor-row editor-row-block",
        div(
          cls := "editor-row",
          textField("Id", s.id.value, v => updateStack(i)(_.copy(id = StackId(v)))),
          textField("Label", s.label, v => updateStack(i)(_.copy(label = v))),
          numberField("X", s.position.x, n => updateStack(i)(st => st.copy(position = st.position.copy(x = n)))),
          numberField("Y", s.position.y, n => updateStack(i)(st => st.copy(position = st.position.copy(y = n)))),
          removeButton(() => setStacks(ss => ss.patch(i, Nil, 1))),
        ),
        div(
          cls := "editor-row",
          selectField("Facing", facingOptions, s.facing, f => updateStack(i)(_.copy(facing = f))),
          selectField("Arrangement", arrangementOptions, s.arrangement, a => updateStack(i)(_.copy(arrangement = a))),
          selectField("Layout", layoutOptions, s.layout, l => updateStack(i)(_.copy(layout = l))),
          checkboxField("Persistent", s.persistent, b => updateStack(i)(_.copy(persistent = b))),
        ),
        div(
          cls := "editor-subsection",
          div(cls := "editor-subsection-head", span("Contents"), addButton("+ Add cards", () => updateStack(i)(s => s.copy(contents = s.contents :+ SpawnSpec(CardDefId(""), 1))))),
          div(cls := "editor-rows", children <-- indexed(draft.signal.map(_.setup.stacks.lift(i).fold(0)(_.contents.size)), j => spawnRow(i, j))),
        ),
      )

    val stacksSection = section(
      "Stacks",
      "+ Add stack",
      () => setStacks(_ :+ StackSpec(StackId("new-stack"), "New Stack", Position(0, 0), Facing.Down, Nil)),
      draft.signal.map(_.setup.stacks.size),
      stackRow,
    )

    def finalised(): GameDefinition =
      val d = draft.now()
      if d.name.trim.isEmpty then d.copy(name = "Untitled") else d

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
        ),
        cardsSection,
        stacksSection,
        rulesSection,
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
  ): Element =
    div(
      cls := "editor-section",
      div(cls := "editor-section-head", h2(title), addButton(addLabel, onAdd)),
      div(cls := "editor-rows", children <-- indexed(count, renderRow)),
    )

  // The current row count, deduplicated, expanded into one element per index.
  private def indexed(count: Signal[Int], renderRow: Int => Element): Signal[List[Element]] =
    count.distinct.map(n => (0 until n).toList.map(renderRow))

  private val facingOptions       = List("Up" -> Facing.Up, "Down" -> Facing.Down)
  private val targetFacingOptions = List("Keep" -> TargetFacing.Keep, "Up" -> TargetFacing.Up, "Down" -> TargetFacing.Down)
  private val arrangementOptions  = List("Ordered" -> Arrangement.Ordered, "Shuffled" -> Arrangement.Shuffled)
  private val layoutOptions       = List("Pile" -> Layout.Pile, "Row" -> Layout.Row)
