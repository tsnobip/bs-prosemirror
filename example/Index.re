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

module StringMeta =
  State.Transaction.Meta.Make({
    type v = string;
  });

type commit = {
  message: string,
  time: Js.Date.t,
  steps: array(PM.Transform.Step.t),
  maps: array(PM.Transform.StepMap.t),
};

type span = {
  from: int,
  to_: int,
  commitId: option(int),
};

let insertIntoBlameMap =
    (map: array(span), ~from: int, ~to_: int, ~commitId: int) =>
  if (from >= to_) {
    ();
  } else {
    let rec aux = pos => {
      let next = map[pos];
      if (Some(commitId) == next.commitId) {
        if (next.to_ >= from) {
          pos;
        } else {
          aux(pos + 1);
        };
      } else if (next.to_ > from) {
        // Different commit, not before
        if (next.from < from) {
          // Sticks out to the left (loop below will handle right side)
          let left = {from: next.from, to_: from, commitId: next.commitId};
          if (next.to_ > to_) {
            map->Js.Array.spliceInPlace(~pos, ~remove=0, ~add=[|left|])
            |> ignore;
          } else {
            map[pos] = left;
          };
          pos + 1;
        } else {
          pos;
        };
      } else {
        aux(pos + 1);
      };
    };
    let pos = aux(0);

    let rec aux2 = (~from, ~to_) => {
      let next = map[pos];
      if (Some(commitId) == next.commitId) {
        if (next.from > to_) {
          (from, to_);
        } else {
          map->Js.Array.spliceInPlace(~pos, ~remove=1) |> ignore;
          aux2(
            ~from=Js.Math.min_int(from, next.from),
            ~to_=Js.Math.max_int(to_, next.to_),
          );
        };
      } else if (next.from >= to_) {
        (from, to_);
      } else if (next.to_ > to_) {
        map[pos] = {from: to_, to_: next.to_, commitId: next.commitId};
        (from, to_);
      } else {
        map->Js.Array.spliceInPlace(~pos, ~remove=1) |> ignore;
        aux2(~from, ~to_);
      };
    };
    let (newFrom, newTo) = aux2(~from, ~to_);
    map->Js.Array.spliceInPlace(
      ~pos,
      ~remove=0,
      ~add=[|{from: newFrom, to_: newTo, commitId: Some(commitId)}|],
    )
    |> ignore;
  };

let updateBlameMap =
    (map: array(span), ~transform: State.Transaction.t, ~id: int) => {
  open PM.Transform;
  let mapping = State.Transaction.mapping(transform);
  let result =
    Belt.Array.keepMap(
      map,
      span => {
        let from = mapping->Mapping.map(~pos=span.from, ~assoc=1, ());
        let to_ = mapping->Mapping.map(~pos=span.to_, ~assoc=-1, ());
        if (from < to_) {
          Some({from, to_, commitId: span.commitId});
        } else {
          None;
        };
      },
    );
  mapping
  ->PM.Transform.Mapping.maps
  ->Belt.Array.forEachWithIndex((i, map) => {
      let after = mapping->Mapping.slice(~from=i + 1, ());
      map->PM.Transform.StepMap.forEach(
        ~fn=(
              ~oldStart as _: int,
              ~oldEnd as _: int,
              ~newStart: int,
              ~newEnd: int,
            ) =>
        insertIntoBlameMap(
          result,
          ~from=after->Mapping.map(~pos=newStart, ~assoc=1, ()),
          ~to_=after->Mapping.map(~pos=newEnd, ~assoc=-1, ()),
          ~commitId=id,
        )
      );
    });
  result;
};

module TrackState = {
  type t = {
    blameMap: array(span),
    commits: array(commit),
    uncommittedSteps: array(PM.Transform.Step.t),
    uncommittedMaps: array(PM.Transform.StepMap.t),
  };

  // Apply a transform to this state
  let applyTransform = (trackState: t, transform: State.Transaction.t) => {
    Belt.Array.(
      State.Transaction.(
        PM.Transform.(
          if (transform->docChanged) {
            // Invert the steps in the transaction, to be able to save them in
            // the next commit
            let inverted =
              transform
              ->steps
              ->mapWithIndex((i, step) =>
                  step->Step.invert(~doc=docs(transform)[i])
                );
            let newBlame =
              updateBlameMap(
                trackState.blameMap,
                ~transform,
                ~id=trackState.commits->Js.Array.length,
              );
            // Create a new state—since these are part of the editor state, a
            // persistent data structure, they must not be mutated.
            {
              blameMap: newBlame,
              commits: trackState.commits,
              uncommittedSteps: trackState.uncommittedSteps->concat(inverted),
              uncommittedMaps:
                trackState.uncommittedMaps
                ->concat(transform->mapping->Mapping.maps),
            };
          } else {
            trackState;
          }
        )
      )
    );
  };

  // When a transaction is marked as a commit, this is used to put any
  // uncommitted steps into a new commit.
  let applyCommit = (trackState: t, message: string, time: Js.Date.t) =>
    if (String.length(message) > 0
        && trackState.uncommittedSteps->Belt.Array.length > 0) {
      let commit = {
        message,
        time,
        steps: trackState.uncommittedSteps,
        maps: trackState.uncommittedMaps,
      };
      {
        blameMap: trackState.blameMap,
        commits: trackState.commits->Belt.Array.concat([|commit|]),
        uncommittedSteps: [||],
        uncommittedMaps: [||],
      };
    } else {
      trackState;
    };
};

let trackPluginKey = State.PluginKey.make(~name="trackPlugin", ());

let trackPluginSpec: State.PluginSpec.t(TrackState.t) = {
  TrackState.(
    State.PluginSpec.make(
      ~key=trackPluginKey,
      ~state=
        State.StateField.make(
          ~init=
            (~config as _, ~instance: State.EditorState.t) =>
              {
                blameMap: [|
                  {
                    from: 0,
                    to_:
                      instance
                      ->State.EditorState.doc
                      ->Model.Node.content
                      ->Model.Fragment.size,
                    commitId: None,
                  },
                |],
                commits: [||],
                uncommittedSteps: [||],
                uncommittedMaps: [||],
              },
          ~apply=
            (
              ~tr: State.Transaction.t,
              ~value as tracked: TrackState.t,
              ~oldState as _oldState,
              ~newState as _newState,
            ) => {
              let transformedTracked = tracked->applyTransform(tr);
              let commitMessage =
                tr->StringMeta.get(~key=`PluginKey(trackPluginKey));
              switch (commitMessage) {
              | None => transformedTracked
              | Some(commitMessage) =>
                transformedTracked->applyCommit(
                  commitMessage,
                  Js.Date.fromFloat(tr->State.Transaction.time),
                )
              };
            },
          (),
        ),
      (),
    )
  );
};

let trackPlugin = State.Plugin.make(~spec=trackPluginSpec);

let elt = (name, attrs, children) : Dom.element => {
  open Webapi;
  let dom = Dom.document |> Dom.Document.createElement(name);
  switch (attrs) {
  | None => ()
  | Some(attrs) =>
    attrs
    ->Js.Dict.entries
    ->Belt.Array.forEach(((name, attr)) =>
        dom |> Dom.Element.setAttribute(name, attr)
      )
  };
  children->Belt.Array.forEach(child =>
    dom |> Dom.Element.appendChild(child)
  );
  dom;
};

type highlightState = {
  deco: PM.View.DecorationSet.t,
  commit: option(commit),
};

type highlight = {
  add: option(commit),
  clear: option(commit),
};

module HighlightMeta =
  State.Transaction.Meta.Make({
    type v = highlight;
  });

let highlightPluginKey: State.PluginKey.t(highlightState) =
  State.PluginKey.make(~name="highlightPlugin", ());

let highlightPluginApply =
    (
      ~tr: State.Transaction.t,
      ~value as prev: highlightState,
      ~oldState,
      ~newState,
    ) => {
  open Belt;
  open State.Transaction;
  let getWithOptionalIndex = array =>
    fun
    | None => None
    | Some(index) => Belt.Array.get(array, index);
  let highlight = tr->HighlightMeta.get(~key=`PluginKey(highlightPluginKey));
  switch (highlight) {
  | Some({add: Some(_) as add, _}) when add != prev.commit =>
    let tState = trackPluginKey->State.PluginKey.getState(oldState);
    let decos =
      tState
      ->Option.map(trackState =>
          trackState.blameMap
          ->Array.keep(span =>
              trackState.commits->getWithOptionalIndex(span.commitId) == add
            )
          ->Array.map(span =>
              PM_Decoration.inline(
                ~from=span.from,
                ~to_=span.to_,
                ~attrs=Js.Dict.fromList([("class", "blame-marker")]),
                (),
              )
            )
        )
      ->Option.getWithDefault([||]);
    {
      deco:
        PM.View.DecorationSet.create(
          ~doc=newState->State.EditorState.doc,
          ~decorations=decos,
        ),
      commit: add,
    };
  | Some({clear: Some(_) as clear, _}) when clear != prev.commit => {
      deco: PM.View.DecorationSet.empty,
      commit: None,
    }
  | _ when tr->docChanged && prev.commit != None => {
      deco:
        prev.deco
        ->PM.View.DecorationSet.map(~mapping=tr->mapping, ~doc=tr->doc, ()),
      commit: prev.commit,
    }
  | _ => prev
  };
};

let highlightPluginSepc: State.PluginSpec.t(highlightState) =
  State.PluginSpec.make(
    ~key=highlightPluginKey,
    ~state=
      State.StateField.make(
        ~init=
          (~config as _, ~instance as _: State.EditorState.t) =>
            {deco: PM.View.DecorationSet.empty, commit: None},
        ~apply=highlightPluginApply,
        (),
      ),
    ~props=
      PM_EditorProps.t(
        ~decorations=
          state =>
            switch (highlightPluginKey->State.PluginKey.getState(state)) {
            | Some(state) => state.deco
            | None => PM.View.DecorationSet.empty
            },
        (),
      ),
    (),
  );

let highlightPlugin = State.Plugin.make(~spec=highlightPluginSepc);

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
      highlightPlugin,
      trackPlugin,
    |],
    (),
  );

let editorState = ref(EditorState.create(config));

let setDisabled = editorState => {
  let input =
    Webapi.(
      Dom.Document.getElementById("message", Dom.document)->Belt.Option.getExn
    );
  let button =
    Webapi.(
      Dom.Document.getElementById("commitbutton", Dom.document)
      ->Belt.Option.getExn
    );
  let disable =
    switch (trackPluginKey->State.PluginKey.getState(editorState)) {
    | Some(trackState) => trackState.uncommittedSteps->Belt.Array.length == 0
    | None => false
    };
  let disableElement = element =>
    fun
    | true => element |> Webapi.Dom.Element.setAttribute("disabled", "")
    | false => element |> Webapi.Dom.Element.removeAttribute("disabled");

  input->disableElement(disable);
  button->disableElement(disable);
};

let lastRendered: ref(option(TrackState.t)) = ref(None);

let renderCommits = (state, dispatch) => {
  let curState =
    trackPluginKey->State.PluginKey.getState(state)->Belt.Option.getExn;
  if (lastRendered^ == Some(curState)) {
    ();
  } else {
    lastRendered := Some(curState);

    let out =
      Webapi.(
        Dom.Document.getElementById("commits", Dom.document)
        ->Belt.Option.getExn
      );
    out->Webapi.Dom.Element.setTextContent("");
    let commits = curState.commits;
    let toString = ({time, message, _}: commit): string =>
      time->Js.Date.toLocaleTimeString ++ {j|\u00a0$message\u00a0|j};
    commits->Belt.Array.forEach(commit => {
      // open Webapi.Dom;
      // let node =
      //   elt(
      //     "div",
      //     Some(Js.Dict.fromList([("class", "commit")])),
      //     [|
      //       document |> Document.createTextNode(commit->toString),
      //       elt(
      //         "button",
      //         Some(Js.Dict.fromList([("class", "commit-revert")])),
      //         [|document |> Document.createTextNode("revert")|],
      //       ),
      //     |],
      //   );
      ();
    });
    // commits.forEach(commit => {
    //   let node = elt("div", {class: "commit"},
    //                  elt("span", {class: "commit-time"},
    //                      commit.time.getHours() + ":" + (commit.time.getMinutes() < 10 ? "0" : "")
    //                      + commit.time.getMinutes()),
    //                  "\u00a0 " + commit.message + "\u00a0 ",
    //                  elt("button", {class: "commit-revert"}, "revert"))
    //   node.lastChild.addEventListener("click", () => revertCommit(commit))
    //   node.addEventListener("mouseover", e => {
    //     if (!node.contains(e.relatedTarget))
    //       dispatch(state.tr.setMeta(highlightPlugin, {add: commit}))
    //   })
    //   node.addEventListener("mouseout", e => {
    //     if (!node.contains(e.relatedTarget))
    //       dispatch(state.tr.setMeta(highlightPlugin, {clear: commit}))
    //   })
    //   out.appendChild(node)
    // })
  };
};

let rec viewConfig =
  PM.View.DirectEditorProps.t(
    ~state=editorState^,
    ~dispatchTransaction=dispatch,
    (),
  )

and editorNode = {
  Webapi.(
    Dom.Document.getElementById("editor", Dom.document)->Belt.Option.getExn
  );
}

and view = PM.View.make(`Node(editorNode), viewConfig)

and dispatch = tr => {
  editorState := (editorState^)->State.EditorState.apply(tr);
  view->PM.View.updateState(editorState^);
  setDisabled(editorState^);
  renderCommits(editorState^, dispatch);
};

let doCommit = message => {
  dispatch(
    editorState^ ->State.EditorState.tr->StringMeta.set(~key=`PluginKey(trackPluginKey), ~value=message),
  );
};

dispatch(
  editorState^
  ->State.EditorState.tr
  ->State.Transaction.insertText(~test="Type something, and then commit it.",()),
);
doCommit("Initial commit");