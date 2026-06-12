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
      def removeEffect(): Unit =
        updateRule(i)(r => r.copy(effects = r.effects.patch(j, Nil, 1)))
      def dealField(pick: Effect.Deal => String): Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).flatMap(_.effects.lift(j)) match { case Some(d: Effect.Deal) => pick(d); case _ => "" }).distinct
      def shuffleField: Signal[String] =
        draft.signal.map(_.rulebook.rules.lift(i).flatMap(_.effects.lift(j)) match { case Some(s: Effect.Shuffle) => s.stack.value; case _ => "" }).distinct
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
        case _: Effect.Shuffle =>
          div(
            cls := "editor-subrow",
            idSelectField("Shuffle stack", stackIds, shuffleField, v => updateShuffle(_.copy(stack = StackId(v)))),
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
          div(
            cls := "editor-subsection-head",
            span("Effects"),
            addButton("+ Add deal", () => updateRule(i)(r => r.copy(effects = r.effects :+ Effect.Deal(StackId(""), StackId(""))))),
            addButton("+ Add shuffle", () => updateRule(i)(r => r.copy(effects = r.effects :+ Effect.Shuffle(StackId(""))))),
          ),
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
      div(
        cls := "editor-stack",
        div(
          cls := "editor-stack-head",
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
          numberField("X", s.position.x, n => updateStack(i)(st => st.copy(position = st.position.copy(x = n)))),
          numberField("Y", s.position.y, n => updateStack(i)(st => st.copy(position = st.position.copy(y = n)))),
          selectField("Facing", facingOptions, s.facing, f => updateStack(i)(_.copy(facing = f))),
          selectField("Arrangement", arrangementOptions, s.arrangement, a => updateStack(i)(_.copy(arrangement = a))),
          selectField("Layout", layoutOptions, s.layout, l => updateStack(i)(_.copy(layout = l))),
          numberField("Width", s.areaWidth, n => updateStack(i)(_.copy(width = Some(n)))),
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
      rowsClass = "editor-stacks-grid",
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
        div(
          cls := "editor-tabs",
          tabButton("cards", "Cards"),
          tabButton("stacks", "Stacks"),
          tabButton("buttons", "Buttons"),
          tabButton("rules", "Rules"),
        ),
        tabPanel("cards", cardsSection),
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

  private val facingOptions       = List("Up" -> Facing.Up, "Down" -> Facing.Down)
  private val targetFacingOptions = List("Keep" -> TargetFacing.Keep, "Up" -> TargetFacing.Up, "Down" -> TargetFacing.Down)
  private val arrangementOptions  = List("Ordered" -> Arrangement.Ordered, "Shuffled" -> Arrangement.Shuffled)
  private val layoutOptions       = List("Pile" -> Layout.Pile, "Row" -> Layout.Row)

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
