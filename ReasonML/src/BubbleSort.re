type ret = {
  arr: list(int),
  perfectPass: bool,
};

let rec bubblePass = (~arr: list(int), ~perfectPass: bool=true, ()): ret =>  {
  switch (arr) {
  | [] => { arr: [], perfectPass }
  | [a] => { arr: [a], perfectPass }
  | [a, b] when a <= b => { arr: [a, b], perfectPass }
  | [a, b] => { arr: [b, a], perfectPass: false }
  | [a, b, ...rest] when a <= b =>
    let { arr as newArr, perfectPass } = bubblePass(~arr = [b, ...rest], ~perfectPass = perfectPass, ());
    { arr: [a, ...newArr], perfectPass };
  | [a, b, ...rest] =>
    let { arr as newArr } = bubblePass(~arr = [a, ...rest], ~perfectPass = false, ());
    { arr: [b, ...newArr], perfectPass: false };
  };
};

let rec bubbleSort = (arr: list(int)): list(int) => {
  let { arr as newArr, perfectPass } = bubblePass(~arr = arr, ());
  perfectPass ? arr : bubbleSort(newArr);
};
