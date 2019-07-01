module PM = BsProsemirror;
module PMPlugins = BsProsemirrorPlugins;
module EditorState = PM.State.EditorState;
module Model = PM.Model;
module State = PM.State;

let textNode: Model.NodeSpec.t = Model.NodeSpec.t();

let noteNode: Model.NodeSpec.t =
  Model.NodeSpec.t(
    ~content="text*",
    ~toDOM=
      _ =>
        Model.DOMOutputSpec.fromSpec(Node("note", Model.Attrs.empty, Hole)),
    ~parseDOM=[|Model.ParseRule.t(~tag="note", ())|],
    (),
  );

let notegroupNode: Model.NodeSpec.t =
  Model.NodeSpec.t(
    ~content="note+",
    ~toDOM=
      _ =>
        Model.DOMOutputSpec.fromSpec(
          Node("notegroup", Model.Attrs.empty, Hole),
        ),
    ~parseDOM=[|Model.ParseRule.t(~tag="notegroup", ())|],
    (),
  );

let docNode: Model.NodeSpec.t =
  Model.NodeSpec.t(~content="(note | notegroup)+", ());

let nodes: OrderedMap.t(Model.NodeSpec.t) =
  OrderedMap.make(
    Js.Dict.fromList([
      ("text", textNode),
      ("note", noteNode),
      ("notegroup", notegroupNode),
      ("doc", docNode),
    ]),
  );

let schema =
  Model.Schema.make(
    Model.SchemaSpec.t(~nodes, ~marks=OrderedMap.make(Js.Dict.empty()), ()),
  );

let makeNoteGroup: PM.Command.t =
  (
    ~state: PM.State.EditorState.t,
    ~dispatch: option(PM.State.Transaction.t => bool)=?,
    ~view as _view: option(PM.View.t)=?,
    (),
  ) => {
    // Get a range around the selected blocks
    let range =
      state
      ->State.EditorState.selection
      ->State.Selection.resolvedFrom
      ->Model.ResolvedPos.blockRange(
          ~other=
            state->State.EditorState.selection->State.Selection.resolvedTo,
          (),
        );
    let noteGroupNodeType =
      schema->PM.Model.Schema.nodes->Js.Dict.get("notegroup");
    switch (range, noteGroupNodeType) {
    | (None, _) => false
    | (_, None) => false
    | (Some(range), Some(notegroupNodeType)) =>
      // See if it is possible to wrap that range in a note group
      let wrapping =
        PM.Transform.Transform.findWrapping(
          ~range,
          ~nodeType=notegroupNodeType,
          (),
        );
      // If not, the command doesn't apply
      switch (wrapping, dispatch) {
      | (None, _) => false
      | (_, None) => false
      | (Some(wrapping_), Some(dispatch_)) =>
        dispatch_(
          state
          ->PM.State.EditorState.tr
          ->PM.State.Transaction.wrap(~range, ~wrappers=wrapping_)
          ->State.Transaction.scrollIntoView,
        )
      };
    };
  };

let config =
  PM.State.EditorState.Config.make(
    ~schema,
    ~plugins=[|
      PMPlugins.History.history(),
      PMPlugins.DropCursor.dropCursor([("color", "red")]->Js.Dict.fromList),
      PMPlugins.Keymap.keymap(
        [
          ("Mod-z", PMPlugins.History.undo),
          ("Mod-y", PMPlugins.History.redo),
          ("Ctrl-Space", makeNoteGroup),
        ]
        |> Js.Dict.fromList,
      ),
      PMPlugins.Keymap.keymap(PM.Commands.baseKeymap),
    |],
    (),
  );

let state = EditorState.create(config);

let viewConfig = PM.View.DirectEditorProps.t(~state, ());

let editorNode = {
  Webapi.(
    Dom.Document.getElementById("editor", Dom.document)->Belt.Option.getExn
  );
};

let view = PM.View.make(`Node(editorNode), viewConfig);

view->PM.View.focus;