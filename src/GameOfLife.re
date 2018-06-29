module Styles = {
  open Css;

  let startButton = style([
    width(rem(22.)),
    height(rem(5.)),
    borderRadius(rem(0.8)),
    color(white),
    backgroundColor(hex("42424F")),
    fontSize(rem(1.2)),
    border(px(0), solid, rgba(0, 0, 0, 0.7)),
    boxShadow(~x=px(0), ~y=px(1), ~blur=px(2), hex("6e6e6e"))
  ]);
}

module Cell = {
  open Css;
  let cell = alive =>
    style([
      backgroundColor(alive ? red : white),
      border(px(1), solid, black),
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

let isAlive = (neighbours, cell) => {
  switch (neighbours, cell.alive) {
    | (0, _) => false
    | (1, _) => false
    | (2, _) => true
    | (3, false) => true /* rule 4 */
    | (3, true) => true
    | (4, _) => false
    | (_, _) => false
    };
}

type gameBoard = array(array(cell));

type coordinates = {
  row: int,
  column: int,
};

type originalCoords = array(coordinates);

type state = {
  game: gameBoard,
  started: bool,
  intervalId: ref(option(Js.Global.intervalId))
};

let beginningCell = {alive: false};

let createBoard = Array.make_matrix(30, 30, beginningCell);

type action =
  | Stop
  | Tick
  | Update(int, int, bool)
  | Seed(gameBoard);

type neighbours = {
  right: option(cell),
  left: option(cell),
  top: option(cell),
  bottom: option(cell),
};

type selfType = ReasonReact.self(state, ReasonReact.noRetainedProps, action);

let component = ReasonReact.reducerComponent("Game Of Life");

/* Safe array access */
let get = (row, column, board) =>
  switch (board[row][column]) {
  | value => Some(value)
  | exception _ => None
  };

/* Check all around the cell if a neighbour is out of bounds return a None */
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
          |> (alive => <Cell key=(string_of_int(column)) alive />)
      ),
    data,
  );

/* Check if the Cell should be alive according to the rules of the game */
let checkIsAlive = (row: int, column: int, game: gameBoard) => {
  getNeighbour(row, column, game)
    |> (neighbours) => 
      List.fold_left(
        (acc, neighbour) =>
        switch (neighbour) {
            | Some(neighbour) => neighbour.alive ? acc + 1 : acc
            | None => acc
          },
        0,
        neighbours,
      ) |> nbrs => isAlive(nbrs, game[row][column]);
};

/* Update a particular cell this is used for the initial seeding */
let updateGame = (targetRow: int, targetCol: int, isAlive: bool, game: gameBoard) =>
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

/* Run the a generation of cells */
let tick = (state: state) =>
    Array.mapi(
      (rowNo, row) =>
      Array.mapi(
        (colNo, _cell) => {alive: checkIsAlive(rowNo, colNo, state.game)},
        row,
      ),
      state.game,
    )
    |> (newBoard) => ReasonReact.Update({ ...state, game: newBoard, started: true});

/* Start the game loop */
let runGeneration = ({ state, send }: selfType) =>  {
  switch (state.started, state.intervalId^) {
    | (true, Some(id)) => Js.Global.clearInterval(id) |> _ => send(Stop)
    | (false, None) => state.intervalId := Some(Js.Global.setInterval(() => send(Tick), 100));
  };
}

let make = _children => {
  ...component,
  initialState: () => {game: createBoard, started: false, intervalId: ref(None)},
  reducer: (action, state) =>
    switch (action) {
    | Update(row, column, alive) =>
      ReasonReact.Update({ ...state, game: updateGame(row, column, alive, state.game) })
    | Seed(board) => ReasonReact.Update({...state, game: board })
    | Tick => tick(state)
    | Stop => ReasonReact.Update({ ...state, started: false, intervalId: ref(None) })
    },
  didMount: ({state, send, onUnmount}) => {
    getRandomCoordinates(~limit=30, ~numberOfCells=15)
      |> (coords) =>
        Array.fold_left(
          (acc, coord) => updateGame(coord.row, coord.column, true, acc),
          state.game,
          coords
        )
        |> (newState) => send(Seed(newState));

  onUnmount(() => 
    switch (state.intervalId^) {
      | Some(id) => Js.Global.clearInterval(id)
      | None => ()
    });
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
      <button className=Styles.startButton onClick=(_ => runGeneration(self))>
      (ReasonReact.string(self.state.started ? "Stop" : "Start"))
      </button>
    </div>,
};
