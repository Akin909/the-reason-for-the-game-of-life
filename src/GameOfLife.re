module Tile = {
  open Css;
  let cell = alive =>
    style([
      backgroundColor(alive ? black : white),
      border(px(4), solid, whitesmoke),
      width(rem(3.)),
      height(rem(3.)),
    ]);
  let component = ReasonReact.statelessComponent("Tile");
  let make = (~alive, _children) => {
    ...component,
    render: _self => <div className=(cell(alive)) />,
  };
};

/* The Rules
   1. Any live cell with fewer than two live neighbors dies, as if by under population.
   2. Any live cell with two or three live neighbors lives on to the next generation.
   3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction. */
type cell = {alive: bool};

let isAlive = neighbours =>
  switch (neighbours) {
  | 0 => false
  | 1 => false
  | 2 => true
  | 3 => true
  | 4 => false
  | _ => false
  };

type state = {game: array(array(cell))};

let beginningCell = {alive: true};

let createBoard = Array.make_matrix(30, 30, beginningCell);

type action =
  | Update(int, int, bool);

type neighbours = {
  right: option(cell),
  left: option(cell),
  top: option(cell),
  bottom: option(cell),
};

let component = ReasonReact.reducerComponent("Game Of Life");

let get = (row, column, board: array(array(cell))) =>
  switch (board[row][column]) {
  | value => Some(value)
  | exception _ => None
  };

let getNeighbour = (row: int, column: int, board: array(array(cell))) => [
  get(row - 1, column + 1, board), /* Top Right */
  get(row - 1, column - 1, board), /* Top Left */
  get(row + 1, column + 1, board), /* Bottom Right */
  get(row + 1, column - 1, board), /* Bottom Left */
  get(row, column + 1, board), /* Right */
  get(row, column - 1, board), /* left */
  get(row + 1, column, board), /* bottom */
  get(row - 1, column, board) /* top */
];

type selfType = ReasonReact.self(state, ReasonReact.noRetainedProps, action);

let mapRow = (row, data: array(cell), self: selfType) =>
  Array.mapi(
    (column, _c) => {
      let neighbours = getNeighbour(row, column, self.state.game);
      let neighbourCount =
        List.fold_left(
          (acc, neighbour) =>
            switch (neighbour) {
            | Some(_neighbour) => acc + 1
            | None => acc
            },
          0,
          neighbours,
        );
      Js.log(neighbourCount);
      let alive = isAlive(neighbourCount);
      self.send(Update(row, column, alive));
      <Tile key=(string_of_int(column)) alive />;
    },
    data,
  );

let updateGame = (targetRow, targetCol, isAlive, game: array(array(cell))) =>
  Array.mapi(
    (rowNumber, row) =>
      Array.mapi(
        (columnNumber, _column) =>
          targetRow == rowNumber && targetCol == columnNumber ?
            {alive: isAlive} : game[rowNumber][columnNumber],
        row,
      ),
    game,
  );

let make = _children => {
  ...component,
  initialState: () => {game: createBoard},
  reducer: (action, state) =>
    switch (action) {
    | Update(row, column, alive) =>
      ReasonReact.Update({game: updateGame(row, column, alive, state.game)})
    | _ => ReasonReact.NoUpdate
    },
  render: self =>
    <div className="container">
      <h2 className="title"> (ReasonReact.string("Game of Life")) </h2>
      <div className="board">
        (
          Array.mapi((r, c) => mapRow(r, c, self), self.state.game)
          |> (
            row =>
              Belt.Array.mapWithIndex(row, (idx, cellElement) =>
                <div key=(string_of_int(idx)) className="row">
                  (ReasonReact.array(cellElement))
                </div>
              )
              |> ReasonReact.array
          )
        )
      </div>
    </div>,
};
