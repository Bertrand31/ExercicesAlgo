module ListUtils = {
  open List;

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
      | [_] => [map(hd, lists)]
      | [_, ..._] => [map(hd, lists), ...zip(map(tl, lists))]
    };
  };
  let headOrNone = (list: list('a)): option('a) => switch(list) {
    | [] => None
    | [head] => Some(head)
    | [head, ..._] => Some(head)
  };
  let rec safeZip = (lists: list(list('a))): list(list(option('a))) => {
    switch(hd(lists)) {
      | [] => []
      | [_] => [map(headOrNone, lists)]
      | [_, ..._] => [map(headOrNone, lists), ...safeZip(map(tl, lists))]
    };
  };
  let rec unzip = (lists: list(list('a))): list(list('a)) => {
    switch(hd(lists)) {
      | [] => []
      | [_] => [map(hd, lists)]
      | [_, ..._] => [map(hd, lists), ...unzip(map(tl, lists))]
    };
  };
  /* TODO: safeUnzip */
  let groupBy = (fn: ('a => 'b), list: list('a)): Hashtbl.t('b, list('a)) => {
    fold_left((map, elem) => {
      let key = fn(elem);
      switch(Hashtbl.mem(map, key)) {
        | true => {
          Hashtbl.replace(map, key, [elem, ...Hashtbl.find(map, key)]);
          map;
        }
        | false => {
          Hashtbl.add(map, key, [elem]);
          map;
        }
      };
    }, Hashtbl.create(1000), list)
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
