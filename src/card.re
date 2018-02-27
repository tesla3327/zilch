[%bs.raw {|require('./card.css')|}];

let component = ReasonReact.statelessComponent("Card");

let make = (~value, ~locked, ~onClick, _children) => {
  ...component,
  render: (_self) => {
    let className = "card " ++ (locked ? "locked" : "");
    
    <div className=className onClick=onClick>
      {ReasonReact.stringToElement(string_of_int(value))}
    </div>
  }
};