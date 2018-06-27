let component = ReasonReact.statelessComponent("Game Of Life");

module Tile = {
  let component = ReasonReact.statelessComponent("Tile");
  let make = (~neighbours: int, _children) => {
    ...component,
    render: _self =>
      <div className="tile">
        (neighbours |> string_of_int |> ReasonReact.string)
      </div>,
  };
};

/* The Rules
   1. Any live cell with fewer than two live neighbors dies, as if by under population.
   2. Any live cell with two or three live neighbors lives on to the next generation.
   3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction. */
type cell = {neighbours: int};

let children = [|{neighbours: 0}, {neighbours: 1}|];

let createBoard = Array.make_matrix(30, 30, 1);

Js.log(createBoard);

let mapRow = (row: array(int)) =>
  Array.map(row => <Tile neighbours=row />, row);

let make = _children => {
  ...component,
  render: _self =>
    <div className="container">
      <h2 className="title"> (ReasonReact.string("Game of Life")) </h2>
      <div className="board">
        (
          Array.map(mapRow, createBoard)
          |> (
            row =>
              Belt.Array.map(row, cell =>
                <div className="row"> (ReasonReact.array(cell)) </div>
              )
              |> ReasonReact.array
          )
        )
      </div>
    </div>,
};
