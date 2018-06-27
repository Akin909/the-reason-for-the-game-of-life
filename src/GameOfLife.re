module Tile = {
  open Css;
  let cell = (alive: int) =>
    style([
      backgroundColor(alive == 1 ? black : white),
      border(px(4), solid, whitesmoke),
      width(rem(3.)),
      height(rem(3.)),
    ]);
  let component = ReasonReact.statelessComponent("Tile");
  let make = (~row: int, ~column: int, _children) => {
    ...component,
    render: _self => <div className=(cell(row + column)) />,
  };
};

/* The Rules
   1. Any live cell with fewer than two live neighbors dies, as if by under population.
   2. Any live cell with two or three live neighbors lives on to the next generation.
   3. Any live cell with more than three live neighbors dies, as if by overpopulation.
   4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction. */
type cell = {neighbours: int};

type state = {game: array(array(cell))};

let beginningCell = {neighbours: 1};

let createBoard = Array.make_matrix(30, 30, beginningCell);

type action =
  | Update;

type neighbours = {
  right: option(cell),
  left: option(cell),
  top: option(cell),
  bottom: option(cell),
};

let component = ReasonReact.reducerComponent("Game Of Life");

let getNeighbour = (row: int, column: int, board: array(array(cell))) =>
  switch (row, column, board) {
  | (0, 0, _) => {
      right: Some(board[row][column + 1]),
      left: None,
      bottom: Some(board[row + 1][column]),
      top: None,
    }
  | (0, _, _) => {
      right: Some(board[row][column + 1]),
      left: Some(board[row][column - 1]),
      bottom: Some(board[row + 1][column]),
      top: None,
    }
  | (_, 0, _) => {
      right: Some(board[row][column + 1]),
      top: Some(board[row - 1][column]),
      bottom: Some(board[row + 1][column]),
      left: None,
    }
  | (30, 30, _) => {
      right: None,
      left: Some(board[row][column - 1]),
      bottom: None,
      top: Some(board[row - 1][column]),
    }
  | (_, 30, _) => {
      right: None,
      top: Some(board[row - 1][column]),
      left: Some(board[row][column - 1]),
      bottom: Some(board[row + 1][column]),
    }
  | (30, _, _) => {
      right: Some(board[row][column + 1]),
      top: Some(board[row - 1][column]),
      left: Some(board[row][column - 1]),
      bottom: None,
    }
  | _ => {
      right: Some(board[row][column + 1]),
      left: Some(board[row][column - 1]),
      bottom: Some(board[row + 1][column]),
      top: Some(board[row - 1][column]),
    }
  | exception _ => {bottom: None, top: None, left: None, right: None}
  };

let mapRow = (row, data: array(cell), board) => {
  let neighbours = getNeighbour(row, 10, board);
  Js.log(row);
  Js.log(neighbours);
  Array.mapi(
    (column, _c) => <Tile key=(string_of_int(column)) row column />,
    data,
  );
};

let make = _children => {
  ...component,
  initialState: () => {game: createBoard},
  reducer: (action: action, _state: state) =>
    switch (action) {
    | _ => ReasonReact.NoUpdate
    },
  render: self =>
    <div className="container">
      <h2 className="title"> (ReasonReact.string("Game of Life")) </h2>
      <div className="board">
        (
          Array.mapi(
            (r, c) => mapRow(r, c, self.state.game),
            self.state.game,
          )
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
