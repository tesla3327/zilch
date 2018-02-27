[%bs.raw {|require('./diceHand.css')|}];

type die = {
  id: int,
  value: int,
  locked: bool
};

type state = {
  dieList: list(die)
};

type action =
  | Click(int)
  | Roll;

let component = ReasonReact.reducerComponent("DiceHand");

/* Make sure to init the random generator */
Random.self_init();
let rollDie = () => Random.int(6) + 1;

let make = (_children) => {
  ...component,
  initialState: () => {
    dieList: List.map(id => {
      id,
      locked: false,
      value: rollDie(),
    }, [0, 1, 2, 3, 4, 5])
  },
  reducer: (action, state) => switch (action) {
  | Click(id) => {
      let dieList = List.map(el => el.id == id ? { ...el, locked: !el.locked } : el, state.dieList);
      ReasonReact.Update({ dieList: dieList });
    }
  | Roll => ReasonReact.Update({
      dieList: List.map(el => {
        ...el,
        value: el.locked ? el.value : rollDie()
      }, state.dieList)
    });
  },
  render: (self) =>
    <div className="dice-hand">
      <div className="dice-list">
        (ReasonReact.arrayToElement(Array.of_list(
            List.map(die => (
              <Card
                key=string_of_int(die.id)
                value=die.value
                locked=die.locked
                onClick={_event => self.send(Click(die.id))}
              />
            ), self.state.dieList)
        )))
      </div>
      <button
        onClick={_e => self.send(Roll)}
        disabled={Js.Boolean.to_js_boolean(List.for_all(el => el.locked, self.state.dieList))}>
        {ReasonReact.stringToElement("Roll")}
      </button>
    </div>
};