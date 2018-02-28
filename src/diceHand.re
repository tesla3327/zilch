[%bs.raw {|require('./diceHand.css')|}];

type gamePhase =
  | Start
  | Roll(int, bool) /* Roll number and if a die has been locked */
  | Zilch
  | End;

type die = {
  id: int,
  value: int,
  locked: bool
};

type state = {
  phase: gamePhase,
  dieList: list(die)
};

type action =
  | Lock(int)
  | Roll;

let component = ReasonReact.reducerComponent("DiceHand");

/* Make sure to init the random generator */
Random.self_init();

let rollDie = () => Random.int(6) + 1;

let getMessage = phase =>
  switch phase {
  | Start => "Roll to get started!"
  | Roll(num, locked) =>
    let roll = "On roll " ++ string_of_int(num) ++ ".";
    locked ? roll : roll ++ " Select at least one die to keep.";
  | Zilch => "Haha you got NOTHING!"
  | End => "Game over."
  };

let canRoll = phase =>
  switch phase {
  | Start
  | Roll(_, true) => Js.false_
  | End
  | Zilch
  | Roll(_, false) => Js.true_
  };

let make = _children => {
  ...component,
  initialState: () => {
    phase: Start,
    dieList: List.map(id => {id, locked: false, value: 0}, [0, 1, 2, 3, 4, 5])
  },
  reducer: (action, state) =>
    switch action {
    | Lock(id) =>
      let dieList =
        List.map(
          el => el.id == id ? {...el, locked: true} : el,
          state.dieList
        );
      let allLocked = List.for_all(el => el.locked, dieList);
      ReasonReact.Update({
        phase:
          allLocked ?
            End :
            (
              switch state.phase {
              | Roll(num, _) => Roll(num, true)
              | _ => state.phase
              }
            ),
        dieList
      });
    | Roll =>
      switch state.phase {
      | Start
      | Roll(_, true) =>
        ReasonReact.Update({
          phase:
            switch state.phase {
            | Start => Roll(1, false)
            | Roll(num, true) => Roll(num + 1, false)
            | _ => state.phase
            },
          dieList:
            List.map(
              el => {...el, value: el.locked ? el.value : rollDie()},
              state.dieList
            )
        })
      | Zilch
      | Roll(_, false)
      | End => ReasonReact.NoUpdate
      }
    },
  render: self => {
    let message = getMessage(self.state.phase);
    <div className="dice-hand">
      <div className="dice-list">
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              List.map(
                die =>
                  <Card
                    key=(string_of_int(die.id))
                    value=die.value
                    locked=die.locked
                    onClick=(_event => self.send(Lock(die.id)))
                  />,
                self.state.dieList
              )
            )
          )
        )
      </div>
      (ReasonReact.stringToElement(message))
      <button
        onClick=(_e => self.send(Roll)) disabled=(canRoll(self.state.phase))>
        (ReasonReact.stringToElement("Roll"))
      </button>
    </div>;
  }
};