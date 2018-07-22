module ListUtils = {
  let rec join = (char: string, list: list(string)): string => {
    switch(list) {
    | [] => raise(Failure("Passed an empty list"))
    | [tail] => tail
    | [head, ...tail] => head ++ char ++ join(char, tail)
    };
  };
  let rec zip = (lists: list(list('a))): list(list('a)) => {
    switch(List.hd(lists)) {
      | [] => []
      | [_] => [List.map(List.hd, lists)]
      | [_, ..._] => [List.map(List.hd, lists), ...zip(List.map(List.tl, lists))]
    };
  };
  let headOrNone = (list: list('a)): option('a) => switch(list) {
    | [] => None
    | [head] => Some(head)
    | [head, ..._] => Some(head)
  };
  let rec safeZip = (lists: list(list('a))): list(list(option('a))) => {
    switch(List.hd(lists)) {
      | [] => []
      | [_] => [List.map(headOrNone, lists)]
      | [_, ..._] => [List.map(headOrNone, lists), ...safeZip(List.map(List.tl, lists))]
    };
  };
};

module DynamicProgramming = {
  let memoizeUnary = (fn: 'a => 'b) => {
    let hashTable = Hashtbl.create(1000);
    (arg: 'a): 'b => {
      switch(Hashtbl.mem(hashTable, arg)) {
        | true => Hashtbl.find(hashTable, arg);
        | false => {
          let res = fn(arg);
          Hashtbl.add(hashTable, arg, res);
          res;
        }
      }
    };
  };
};
