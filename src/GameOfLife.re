[%%debugger.chrome];

module Styles = {
  open Css;

  let startButton = style([
    width(rem(22.)),
    height(rem(5.)),
    borderRadius(rem(0.8)),
    backgroundColor(skyblue),
    fontSize(rem(1.2)),
    border(px(0), solid, white),
    boxShadow(~x=px(0), ~y=px(1), ~blur=px(2), hex("6e6e6e"))
  ]);
}

module Tile = {
  open Css;
  let cell = alive =>
    style([
      backgroundColor(alive ? black : white),
      border(px(2), solid, whitesmoke),
      width(rem(1.2)),
      height(rem(1.2)),
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

let isAlive = (neighbours, cell) =>
  switch (neighbours, cell.alive) {
  | (0, _) => false
  | (1, _) => false
  | (2, _) => true
  | (3, false) => true /* rule 4 */
  | (3, true) => true
  | (4, _) => false
  | (_, _) => false
  };

type gameBoard = array(array(cell));

type coordinates = {
  row: int,
  column: int,
};

type originalCoords = array(coordinates);

type state = {
  game: gameBoard,
};

let beginningCell = {alive: false};

let createBoard = Array.make_matrix(30, 30, beginningCell);

type action =
  | Start
  | Update(int, int, bool)
  | Seed(gameBoard);

type neighbours = {
  right: option(cell),
  left: option(cell),
  top: option(cell),
  bottom: option(cell),
};

let component = ReasonReact.reducerComponent("Game Of Life");

let get = (row, column, board) =>
  switch (board[row][column]) {
  | value => Some(value)
  | exception _ => None
  };

let getNeighbour = (row: int, column: int, board) => [
  get(row - 1, column + 1, board), /* Top Right */
  get(row - 1, column - 1, board), /* Top Left */
  get(row + 1, column + 1, board), /* Bottom Right */
  get(row + 1, column - 1, board), /* Bottom Left */
  get(row, column + 1, board), /* Right */
  get(row, column - 1, board), /* left */
  get(row + 1, column, board), /* bottom */
  get(row - 1, column, board) /* top */
];

let getRandomCoordinates = (~limit: int, ~numberOfCells: int) => {
  Random.init(int_of_float(Js.Date.now()));
  Array.init(numberOfCells, _i => { row: Random.int(limit), column: Random.int(limit)});
};

type selfType = ReasonReact.self(state, ReasonReact.noRetainedProps, action);

let mapRow = (row, data, self: selfType) =>
  Array.mapi(
    (column, _c) =>
      get(row, column, self.state.game)
      |> (
        cellStatus =>
          (
            switch (cellStatus) {
            | Some(cell) => cell.alive
            | None => false
            }
          )
          |> (alive => <Tile key=(string_of_int(column)) alive />)
      ),
    data,
  );

let checkIsAlive = (row, column, game) =>
  getNeighbour(row, column, game) 
    |> (neighbours) => 
      List.fold_left(
        (acc, neighbour) =>
        switch (neighbour) {
            | Some(_neighbour) => acc + 1
            | None => acc
          },
        0,
        neighbours,
      ) |> nbrs => isAlive(nbrs, game[row][column]);

let updateGame = (targetRow, targetCol, isAlive, game: gameBoard) =>
  Array.mapi(
    (rowNumber, row) =>
      Array.mapi(
        (columnNumber, _column) => {
          let isMatch = targetRow == rowNumber && targetCol == columnNumber;
          switch (isMatch, game[rowNumber][columnNumber]) {
          | (true, _value) => {alive: isAlive}
          | (false, cell) => cell
          };
        },
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
      ReasonReact.Update({ game: updateGame(row, column, alive, state.game) })
    | Seed(board) => ReasonReact.Update({game: board })
    | Start =>
      let newBoard =
        Array.mapi(
          (rowNo, row) =>
            Array.mapi(
              (colNo, _cell) => {alive: checkIsAlive(rowNo, colNo, state.game)},
              row,
            ),
          state.game,
        );
      ReasonReact.Update({ game: newBoard});
    },
  didMount: ({state, send}) => {
    let coords = getRandomCoordinates(~limit=30, ~numberOfCells=50);
    let newState =
      Array.fold_left(
        (acc, coord) => updateGame(coord.row, coord.column, true, acc),
        state.game,
        coords,
      );
    send(Seed(newState));
  },
  render: self =>
    <div className="container">
      <h2 className="title"> (ReasonReact.string("Game of Life")) </h2>
      <button className=Styles.startButton onClick=(_ => self.send(Start))>
      (ReasonReact.string("Start"))
      </button>
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
