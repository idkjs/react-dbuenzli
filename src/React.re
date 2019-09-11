/*---------------------------------------------------------------------------
   Copyright (c) 2009 Daniel C. B nzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*/
let err_max_rank = "maximal rank exceeded";

let err_sig_undef = "signal value undefined yet";

let err_fix = "trying to fix a delayed value";

let err_retain_never = "E.never cannot retain a closure";

let err_retain_cst_sig = "constant signals cannot retain a closure";

let err_step_executed = "step already executed";

let err_event_scheduled = "event already scheduled on a step";

let err_signal_scheduled = "signal already scheduled on a step";

module Wa = {
  type t('a) = {
    mutable arr: Weak.t('a),
    mutable len: int
  };
  /* The type for resizeable weak arrays.

     For now the arrays only grow. We could try to compact and
     downsize the array in scan_add if a threshold of empty slots is
     exceeded. */
  let create = (size) => {arr: Weak.create(size), len: 0};
  let length = (a) => a.len;
  let is_empty = (a) =>
    try {
      for (i in 0 to a.len - 1) {
        if (Weak.check(a.arr, i)) {
          raise(Exit);
        };
      };
      true;
    } {
    | Exit => false
    };
  let clear = (a) => {
    a.arr = Weak.create(0);
    a.len = 0;
  };
  let get = (a, i) => Weak.get(a.arr, i);
  let set = (a, i) => Weak.set(a.arr, i);
  let swap = (a, i, i') => {
    let v = Weak.get(a.arr, i');
    Weak.blit(a.arr, i, a.arr, i', 1); /* blit prevents i from becoming live. */
    Weak.set(a.arr, i, v);
  };
  let grow = (a) => {
    let arr' = Weak.create(2 * (a.len + 1));
    Weak.blit(a.arr, 0, arr', 0, a.len);
    a.arr = arr';
  };
  let add = (a, v) => {
    /* adds v at the end of a. */
    if (a.len == Weak.length(a.arr)) {
      grow(a);
    };
    Weak.set(a.arr, a.len, Some(v));
    a.len = a.len + 1;
  };
  let scan_add = (a, v) =>
    /* adds v to a, tries to find an empty slot, O(a.len). */
    try {
      for (i in 0 to a.len - 1) {
        switch (Weak.get(a.arr, i)) {
        | None =>
          Weak.set(a.arr, i, Some(v));
          raise(Exit);
        | Some(_) => ()
        };
      };
      add(a, v);
    } {
    | Exit => ()
    };
  let rem_last = (a) => {
    let l = a.len - 1;
    a.len = l;
    Weak.set(a.arr, l, None);
  };
  let rem = (a, v) =>
    /* removes v from a, uses physical equality, O(a.len). */
    try (
      for (i in 0 to a.len - 1) {
        switch (Weak.get(a.arr, i)) {
        | Some(v') when v === v' =>
          Weak.set(a.arr, i, None);
          raise(Exit);
        | _ => ()
        };
      }
    ) {
    | Exit => ()
    };
  let iter = (f, a) =>
    for (i in 0 to a.len - 1) {
      switch (Weak.get(a.arr, i)) {
      | Some(v) => f(v)
      | None => ()
      };
    };
  let fold = (f, acc, a) => {
    let acc = ref(acc);
    for (i in 0 to a.len - 1) {
      switch (Weak.get(a.arr, i)) {
      | Some(v) => acc := f(acc^, v)
      | None => ()
      };
    };
    acc^;
  };
};

type node = {
  mutable rank: int, /* its rank (height) in the dataflow graph. */
  mutable stamp: step, /* last step in which it was scheduled. */
  mutable retain: unit => unit, /* retained by the node, NEVER invoked. */
  mutable producers: unit => list(node), /* nodes on which it depends. */
  mutable update: step => unit, /* update closure. */
  deps: Wa.t(node)
} /* weak references to dependent nodes. */
/* The type for nodes.

   Each event and (non-constant) signal has an associated node. The
   fields producers and update keep, in their closure environment,
   references to mutables (see later) on which the node depends.
   Defining their contents via a let rec allows the environment to be
   shared by the two closures.

   There are special nodes to represent infinitesimally delayed nodes
   (needed for recursive definitions). These nodes all have a rank of
   Node.delayed_rank and depend only on the node they delay. Since
   they have the highest rank possible they are updated only at the
   end of the step and treated specially at that point (see
   Step.execute). */
and step = {
  mutable over: bool, /* true when the step is over. */
  mutable heap, /* min-heap of nodes sorted by rank. */
  mutable eops: list(unit => unit), /* end of step operations. */
  mutable cops: list(unit => unit)
} /* cleanup step operations. */
/* The type for update steps.

   Note for historical reasons we use the variable names [c] and [c']
   in the code for representing update steps.

   There are four successive phases in the execution of a step c (see
   Step.execute).

   1. Nodes are updated in topological order until c.heap is empty or
      we reach a delayed node.

   2. End of step operations are executed. This may add new
      dependencies (see S.diff and S.changes) and clear the occurence
      of delayed events from a previous step (but used in this
      step).

   3. If there are delayed nodes in c.heap, we create a new step
      c'. Each delayed node is updated and its dependents are put in
      c'.heap. For delayed events, an end of step operation is added
      in c' to clear the occurence at step 2 of c'. Delayed nodes are
      updated in any order as a delayed node updating in a step
      cannot depend on a delayed node updating in the same step.

   4. Cleanup operations are executed. This clears the event occurences of
      non-delayed event that occured in c.

   After this, if a step c' was created in 3. the step gets executed. */
and heap = Wa.t(node);

/* The type for heaps.

   Weak min-heaps of nodes sorted according to their rank. Classic
   imperative implementation with a twist to accomodate the fact
   that nodes may disappear.

   The heap property we maintain is that for any node its descendents
   (vs. children) are either of no smaller rank or they are None. None
   nodes need to be treated specially in percolate up and down. The
   reason is that it blocks information about the rank of their
   descendents.  In percolate down the solution is to systematically
   swap with None children.  So do we in percolate up, however, in
   that case we may violate the property if we swap with a None node
   and stop right after (either because we got the root or we found a
   parent of smaller rank), the property can however be reestablished
   by percolating down from that point. */
type emut('a) = {
  ev: ref(option('a)), /* during steps, holds a potential occurence. */
  enode: node
} /* associated node. */;

type event('a) =
  | Never
  | Emut(emut('a));

/* The type for events.

   An event is either the never occuring event Never or a mutable
   Emut.  A mutable m has some value in m.v iff a step is being
   executed and m has an occurence in the step. m's dependents are
   scheduled for update iff m has a value in m.v.

   Mutables that occur in a step are set back to None when the step
   terminates with an cleanup step operation (see eupdate and
   Step.execute). To avoid a weak reference on m in the cleanup
   operation, the field m.v is a field on a reference instead of a
   mutable field.

   A new node n can be made dependent on a an event mutable m during a
   step. But when n is added to m's dependents, m may already have
   updated and scheduled its dependents. In that case n also need to
   be scheduled (see E.add_dep). If m only occurs later in the step,
   the n will be scheduled as usual with the others. */
type smut('a) = {
  mutable sv: option('a), /* signal value (None only temporary). */
  eq: ('a, 'a) => bool, /* to detect signal value changes. */
  snode: node
} /* associated node. */;

type signal('a) =
  | Const('a)
  | Smut(smut('a));

/* The type for signals.

    A signal is either a constant signal Const or a mutable Smut.  A
    mutable m has a value in m.v iff m.v initialized. m's dependents
    are scheduled for update iff m is initialized and m.v changed
    according to m.eq in the step.

    Signal initialization occurs as follows. If we have an init. value
    we set the signal's value to this value and then :

    1. If the creation occurs outside a step, the signal's update
       function is invoked with Step.nil. This may overwrite the
       init. value, but no dependent will see this change as there
       cannot be any at that time.

    2. If the creation occurs inside a step, the signal is scheduled
       for update. Here again this may overwrite the init. value. If
       the new value is equal to the init. value this will not schedule
       the signals' dependents. However this is not a problem since
       dependents are either new signals and will be scheduled via the
       init. process or a new dependency added by S.switch in which
       case this dependent is also be scheduled.

   Note that in both cases if we had no init. value, the call to the
   update function must unconditionaly write a concrete value for the
   signal.

   To find out whether the creation occurs in a step we walk back the
   signal's producers recursively looking for a node stamp with an
   unfinished step (see Step.find_unfinished). This is not in favor
   of static signal creation but this is the price we have to pay for
   not having global data structures.

   A new node n can be made dependent on a signal mutable m during a
   step. In contrast to events (see above) nothing special has to be
   done. Here's the rationale :

   1. If n is the node of a new event then either the event cannot
      happen in the same step and thus the depency addition occurs at
      the end of the step (S.diff, S.changes) or the event cares only
      about having an up to date value if some other event occurs
      (S.sample, E.on) in the same step and the rank of n ensures
      this.

   2. If n is the node of a new signal then n cares only about having
      m's up to date values whenever n will initialize and the rank of
      n ensures this. */
module H = {
  let size = Wa.length;
  let els = (h) => Wa.fold((acc, e) => [e, ...acc], [], h); /*  no particular order. */
  let compare_down = (h, i, i') =>
    switch (Wa.get(h, i), Wa.get(h, i')) {
    | (Some(n), Some(n')) => compare(n.rank, n'.rank)
    | (Some(_), None) => 1 /* None is smaller than anything. */
    | (None, Some(_)) => (-1) /* None is smaller than anything. */
    | (None, None) => 0
    };
  let rec down = (h, i) => {
    let last = size(h) - 1;
    let start = 2 * i;
    let l = start + 1; /* left child index. */
    let r = start + 2; /* right child index. */
    if (l > last) {
      () /* no child, stop */;
    } else {
      let child =
        /* index of smallest child. */
        if (r > last) {
          l;
        } else if (compare_down(h, l, r) < 0) {
          l;
        } else {
          r;
        };
      if (compare_down(h, i, child) > 0) {
        Wa.swap(h, i, child);
        down(h, child);
      };
    };
  };
  let up = (h, i) => {
    let rec aux = (h, i, last_none) =>
      if (i == 0) {
        if (last_none) {
          down(h, 0);
        };
      } else {
        let p = (i - 1) / 2; /* parent index. */
        switch (Wa.get(h, i), Wa.get(h, p)) {
        | (Some(n), Some(n')) =>
          if (compare(n.rank, n'.rank) < 0) {
            Wa.swap(h, i, p);
            aux(h, p, false);
          } else if (last_none) {
            down(h, i);
          }
        | (Some(_), None) =>
          Wa.swap(h, i, p);
          aux(h, p, true);
        | (None, _) => ()
        };
      };
    aux(h, i, false);
  };
  let rebuild = (h) =>
    for (i in (size(h) - 2) / 2 downto 0) {
      down(h, i);
    };
  let add = (h, n) => {
    Wa.add(h, n);
    up(h, size(h) - 1);
  };
  let rec take = (h) => {
    let s = size(h);
    if (s == 0) {
      None;
    } else {
      let v = Wa.get(h, 0);
      if (s > 1) {
        Wa.set(h, 0, Wa.get(h, s - 1));
        Wa.rem_last(h);
        down(h, 0);
      } else {
        Wa.rem_last(h);
      };
      switch v {
      | None => take(h)
      | v => v
      };
    };
  };
};

let delayed_rank = max_int;

module Step = {
  /* Update steps. */
  type t = step;
  let nil = {over: true, heap: Wa.create(0), eops: [], cops: []};
  let create = () => {
    let h = Wa.create(11);
    {over: false, heap: h, eops: [], cops: []};
  };
  let add = (c, n) =>
    if (n.stamp === c) {
      ();
    } else {
      n.stamp = c;
      H.add(c.heap, n);
    };
  let add_deps = (c, n) => Wa.iter(add(c), n.deps);
  let add_eop = (c, op) => c.eops = [op, ...c.eops];
  let add_cop = (c, op) => c.cops = [op, ...c.cops];
  let allow_reschedule = (n) => n.stamp = nil;
  let rebuild = (c) => H.rebuild(c.heap);
  let rec execute = (c) => {
    let eops = (c) => {
      List.iter((op) => op(), c.eops);
      c.eops = [];
    };
    let cops = (c) => {
      List.iter((op) => op(), c.cops);
      c.cops = [];
    };
    let finish = (c) => {
      c.over = true;
      c.heap = Wa.create(0);
    };
    let rec update = (c) =>
      switch (H.take(c.heap)) {
      | Some(n) when n.rank != delayed_rank =>
        n.update(c);
        update(c);
      | Some(n) =>
        let c' = create();
        eops(c);
        List.iter((n) => n.update(c'), [n, ...H.els(c.heap)]);
        cops(c);
        finish(c);
        execute(c');
      | None =>
        eops(c);
        cops(c);
        finish(c);
      };
    update(c);
  };
  let execute = (c) =>
    if (c.over) {
      invalid_arg(err_step_executed);
    } else {
      execute(c);
    };
  let find_unfinished = (nl) => {
    /* find unfinished step in recursive producers. */
    let rec aux = (next) =>
      fun /* zig-zag breadth-first search. */
      | [] =>
        if (next == []) {
          nil;
        } else {
          aux([], next);
        }
      | [[], ...todo] => aux(next, todo)
      | [nl, ...todo] => find(next, todo, nl)
    and find = (next, todo) =>
      fun
      | [] => aux(next, todo)
      | [n, ...nl] =>
        if (! n.stamp.over) {
          n.stamp;
        } else {
          find([n.producers(), ...next], todo, nl);
        };
    aux([], [nl]);
  };
};

module Node = {
  let delayed_rank = delayed_rank;
  let min_rank = min_int;
  let max_rank = delayed_rank - 1;
  let nop = (_) => ();
  let no_producers = () => [];
  let create = (r) => {
    rank: r,
    stamp: Step.nil,
    update: nop,
    retain: nop,
    producers: no_producers,
    deps: Wa.create(0)
  };
  let rem_dep = (n, n') => Wa.rem(n.deps, n');
  let add_dep = (n, n') => Wa.scan_add(n.deps, n');
  let has_dep = (n) => ! Wa.is_empty(n.deps);
  let deps = (n) => Wa.fold((acc, d) => [d, ...acc], [], n.deps);
  let bind = (n, p, u) => {
    n.producers = p;
    n.update = u;
  };
  let stop = (~strong=false, n) =>
    if (! strong) {
      n.producers = no_producers;
      n.update = nop;
      Wa.clear(n.deps);
    } else {
      let rec loop = (next, to_rem) =>
        fun
        | [] =>
          switch next {
          | [(to_rem, prods), ...next] => loop(next, to_rem, prods)
          | [] => ()
          }
        | [n, ...todo] => {
            rem_dep(n, to_rem); /* N.B. rem_dep could be combined with has_dep */
            if (n.rank == min_rank /* is a primitive */ || has_dep(n)) {
              loop(next, to_rem, todo);
            } else {
              let prods = n.producers();
              n.producers = no_producers;
              n.update = nop;
              Wa.clear(n.deps);
              loop([(n, prods), ...next], to_rem, todo);
            };
          };
      let producers = n.producers();
      n.producers = no_producers;
      n.update = nop;
      Wa.clear(n.deps);
      loop([], n, producers);
    };
  let set_rank = (n, r) => n.rank = r;
  let rmin = create(min_rank);
  let rmax = (n, n') =>
    if (n.rank > n'.rank) {
      n;
    } else {
      n';
    };
  let rsucc = (n) =>
    if (n.rank == delayed_rank) {
      min_rank;
    } else if (n.rank < max_rank) {
      n.rank + 1;
    } else {
      invalid_arg(err_max_rank);
    };
  let rsucc2 = (n, n') => {
    let r = rsucc(n);
    let r' = rsucc(n');
    if (r > r') {
      r;
    } else {
      r';
    };
  };
  /* Rank updates currently only increases ranks. If this is problematic
     udpate ranks orthodoxly by taking the succ of the max of n.producers.
     Note that rank update stops at delayed nodes (otherwise we would
     loop and blow the ranks). */
  let update_rank = (n, r) => {
    /* returns true iff n's rank increased. */
    let rec aux =
      fun
      | [] => ()
      | [n, ...todo] => {
          let update = (todo, d) =>
            if (n.rank < d.rank || n.rank == delayed_rank) {
              todo;
            } else {
              d.rank = rsucc(n);
              [d, ...todo];
            };
          aux(Wa.fold(update, todo, n.deps));
        };
    if (r > n.rank) {
      n.rank = r;
      aux([n]);
      true;
    } else {
      false;
    };
  };
};

/* Shortcuts */
let rsucc = Node.rsucc;

let rsucc2 = Node.rsucc2;

let rmax = Node.rmax;

/* Event value, creation and update */
let eval = (m) =>
  switch m.ev^ {
  | Some(v) => v
  | None => assert false
  };

let emut = (rank) => {ev: ref(None), enode: Node.create(rank)};

let event = (m, p, u) => {
  Node.bind(m.enode, p, u);
  Emut(m);
};

let eupdate = (v, m, c) => {
  let clear = (v, ()) => v := None;
  m.ev := Some(v);
  Step.add_cop(c, clear(m.ev));
  Step.add_deps(c, m.enode);
};

/* Signal value, creation and update */
let sval = (m) =>
  switch m.sv {
  | Some(v) => v
  | None => assert false
  };

let smut = (rank, eq) => {sv: None, eq, snode: Node.create(rank)};

let signal = (~i=?, m, p, u) => {
  Node.bind(m.snode, p, u);
  switch i {
  | Some(_) as v => m.sv = v
  | None => ()
  };
  switch (Step.find_unfinished(m.snode.producers())) {
  | c when c === Step.nil => m.snode.update(Step.nil)
  | c => Step.add(c, m.snode)
  };
  Smut(m);
};

let supdate = (v, m, c) =>
  switch m.sv {
  | Some(v') when m.eq(v, v') => ()
  | Some(_) =>
    m.sv = Some(v);
    if (c !== Step.nil) {
      Step.add_deps(c, m.snode);
    };
  | None => m.sv = Some(v) /* init. without init value. */
  };

module E = {
  type t('a) = event('a);
  let add_dep = (m, n) => {
    Node.add_dep(m.enode, n);
    if (m.ev^ != None) {
      Step.add(m.enode.stamp, n);
    };
  };
  let send = (m, ~step=?, v) =>
    switch step {
    /* sends an event occurence. */
    | Some(c) =>
      if (c.over) {
        invalid_arg(err_step_executed);
      } else if (! m.enode.stamp.over) {
        invalid_arg(err_event_scheduled);
      } else {
        m.enode.stamp = c;
      };
      eupdate(v, m, c);
    | None =>
      let c = Step.create();
      m.enode.stamp = c;
      eupdate(v, m, c);
      Step.execute(c);
    };
  /* Basics */
  let never = Never;
  let create = () => {
    let m = emut(Node.min_rank);
    (Emut(m), send(m));
  };
  let retain = (e, c) =>
    switch e {
    | Never => invalid_arg(err_retain_never)
    | Emut(m) =>
      let c' = m.enode.retain;
      m.enode.retain = c;
      `R(c');
    };
  let stop = (~strong=?) =>
    fun
    | Never => ()
    | Emut(m) => Node.stop(~strong?, m.enode);
  let equal = (e, e') =>
    switch (e, e') {
    | (Never, Never) => true
    | (Never, _)
    | (_, Never) => false
    | (Emut(m), Emut(m')) => m === m'
    };
  let trace = (~iff=Const(true), t, e) =>
    switch iff {
    | Const(false) => e
    | Const(true) =>
      switch e {
      | Never => e
      | Emut(m) =>
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) => {
          let v = eval(m);
          t(v);
          eupdate(v, m', c);
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      }
    | Smut(mc) =>
      switch e {
      | Never => Never
      | Emut(m) =>
        let m' = emut(rsucc2(mc.snode, m.enode));
        let rec p = () => [mc.snode, m.enode]
        and u = (c) =>
          switch m.ev^ {
          | None => () /* mc updated. */
          | Some(v) =>
            if (sval(mc)) {
              t(v);
            };
            eupdate(v, m', c);
          };
        Node.add_dep(mc.snode, m'.enode);
        add_dep(m, m'.enode);
        event(m', p, u);
      }
    };
  /* Transforming and filtering */
  let once =
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) => {
          Node.rem_dep(m.enode, m'.enode);
          eupdate(eval(m), m', c);
          Node.stop(m'.enode);
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let drop_once =
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) => {
          /* first update. */
          let u' = (c) => eupdate(eval(m), m', c); /* subsequent updates. */
          Node.bind(m'.enode, p, u');
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let app = (ef) =>
    fun
    | Never => Never
    | Emut(m) =>
      switch ef {
      | Never => Never
      | Emut(mf) =>
        let m' = emut(rsucc2(m.enode, mf.enode));
        let rec p = () => [m.enode, mf.enode]
        and u = (c) =>
          switch (mf.ev^, m.ev^) {
          | (None, _)
          | (_, None) => ()
          | (Some(f), Some(v)) => eupdate(f(v), m', c)
          };
        add_dep(m, m'.enode);
        add_dep(mf, m'.enode);
        event(m', p, u);
      };
  let map = (f) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) => eupdate(f(eval(m)), m', c);
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let stamp = (e, v) =>
    switch e {
    | Never => Never
    | Emut(m) =>
      let m' = emut(rsucc(m.enode));
      let rec p = () => [m.enode]
      and u = (c) => eupdate(v, m', c);
      add_dep(m, m'.enode);
      event(m', p, u);
    };
  let filter = (pred) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) => {
          let v = eval(m);
          if (pred(v)) {
            eupdate(v, m', c);
          } else {
            ();
          };
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let fmap = (fm) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let rec p = () => [m.enode]
        and u = (c) =>
          switch (fm(eval(m))) {
          | Some(v) => eupdate(v, m', c)
          | None => ()
          };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let diff = (d) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let last = ref(None);
        let rec p = () => [m.enode]
        and u = (c) => {
          let v = eval(m);
          switch last^ {
          | None => last := Some(v)
          | Some(v') =>
            last := Some(v);
            eupdate(d(v, v'), m', c);
          };
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let changes = (~eq=(==)) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let last = ref(None);
        let rec p = () => [m.enode]
        and u = (c) => {
          let v = eval(m);
          switch last^ {
          | None =>
            last := Some(v);
            eupdate(v, m', c);
          | Some(v') =>
            last := Some(v);
            if (eq(v, v')) {
              ();
            } else {
              eupdate(v, m', c);
            };
          };
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let on = (c) =>
    fun
    | Never => Never
    | Emut(m) as e =>
      switch c {
      | Const(true) => e
      | Const(false) => Never
      | Smut(mc) =>
        let m' = emut(rsucc2(m.enode, mc.snode));
        let rec p = () => [m.enode, mc.snode]
        and u = (c) =>
          switch m.ev^ {
          | None => () /* mc updated. */
          | Some(_) =>
            if (sval(mc)) {
              eupdate(eval(m), m', c);
            } else {
              ();
            }
          };
        add_dep(m, m'.enode);
        Node.add_dep(mc.snode, m'.enode);
        event(m', p, u);
      };
  let when_ = on;
  let dismiss = (c) =>
    fun
    | Never => Never
    | Emut(m) as e =>
      switch c {
      | Never => e
      | Emut(mc) =>
        let m' = emut(rsucc2(mc.enode, m.enode));
        let rec p = () => [mc.enode, m.enode]
        and u = (c) =>
          switch mc.ev^ {
          | Some(_) => ()
          | None => eupdate(eval(m), m', c)
          };
        add_dep(mc, m'.enode);
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  let until = (c) =>
    fun
    | Never => Never
    | Emut(m) as e =>
      switch c {
      | Never => e
      | Emut(mc) =>
        let m' = emut(rsucc2(m.enode, mc.enode));
        let rec p = () => [m.enode, mc.enode];
        let u = (c) =>
          switch mc.ev^ {
          | None => eupdate(eval(m), m', c)
          | Some(_) =>
            Node.rem_dep(m.enode, m'.enode);
            Node.rem_dep(mc.enode, m'.enode);
            Node.stop(m'.enode);
          };
        add_dep(m, m'.enode);
        add_dep(mc, m'.enode);
        event(m', p, u);
      };
  /* Accumulating */
  let accum = (ef, i) =>
    switch ef {
    | Never => Never
    | Emut(m) =>
      let m' = emut(rsucc(m.enode));
      let acc = ref(i);
      let rec p = () => [m.enode]
      and u = (c) => {
        acc := (eval(m))(acc^);
        eupdate(acc^, m', c);
      };
      add_dep(m, m'.enode);
      event(m', p, u);
    };
  let fold = (f, i) =>
    fun
    | Never => Never
    | Emut(m) => {
        let m' = emut(rsucc(m.enode));
        let acc = ref(i);
        let rec p = () => [m.enode]
        and u = (c) => {
          acc := f(acc^, eval(m));
          eupdate(acc^, m', c);
        };
        add_dep(m, m'.enode);
        event(m', p, u);
      };
  /* Combining */
  let occurs = (m) => m.ev^ != None;
  let find_muts_and_next_rank = (el) => {
    let rec aux = (acc, max) =>
      fun
      | [] => (List.rev(acc), rsucc(max))
      | [Emut(m), ...l] => aux([m, ...acc], rmax(max, m.enode), l)
      | [Never, ...l] => aux(acc, max, l);
    aux([], Node.rmin, el);
  };
  let select = (el) => {
    let (emuts, r) = find_muts_and_next_rank(el);
    let m' = emut(r);
    let rec p = () => List.rev_map((m) => m.enode, emuts)
    and u = (c) =>
      try (eupdate(eval(List.find(occurs, emuts)), m', c)) {
      | Not_found => assert false
      };
    List.iter((m) => add_dep(m, m'.enode), emuts);
    event(m', p, u);
  };
  let merge = (f, a, el) => {
    let rec fold = (f, acc) =>
      fun
      | [m, ...l] when occurs(m) => fold(f, f(acc, eval(m)), l)
      | [m, ...l] => fold(f, acc, l)
      | [] => acc;
    let (emuts, r) = find_muts_and_next_rank(el);
    let m' = emut(r);
    let rec p = () => List.rev_map((m) => m.enode, emuts)
    and u = (c) => eupdate(fold(f, a, emuts), m', c);
    List.iter((m) => add_dep(m, m'.enode), emuts);
    event(m', p, u);
  };
  let switch_ = (e) =>
    fun
    | Never => e
    | Emut(ms) => {
        let r =
          switch e {
          | Emut(m) => rsucc2(m.enode, ms.enode)
          | Never => rsucc(ms.enode)
          };
        let m' = emut(r);
        let src = ref(e); /* current event source. */
        let rec p = () =>
          switch src^ {
          | Emut(m) => [m.enode, ms.enode]
          | Never => [ms.enode]
          }
        and u = (c) =>
          switch ms.ev^ {
          | None =>
            switch src^ {
            /* only src occurs. */
            | Emut(m) => eupdate(eval(m), m', c)
            | Never => assert false
            }
          | Some(e) =>
            switch src^ {
            | Emut(m) => Node.rem_dep(m.enode, m'.enode)
            | Never => ()
            };
            src := e;
            switch e {
            | Never => ignore(Node.update_rank(m'.enode, rsucc(ms.enode)))
            | Emut(m) =>
              Node.add_dep(m.enode, m'.enode);
              if (Node.update_rank(m'.enode, rsucc2(m.enode, ms.enode))) {
                /* Rank increased because of m. Thus m may stil
                   update and we may be rescheduled. If it happens
                   we'll be in the other branch without any harm
                   but some redundant computation. */
                Step.allow_reschedule(m'.enode);
                Step.rebuild(c);
              } else {
                /* No rank increase, m already updated if needed. */
                switch m.ev^ {
                | Some(v) => eupdate(v, m', c)
                | None => ()
                };
              };
            };
          };
        switch e {
        | Emut(m) => add_dep(m, m'.enode)
        | Never => ()
        };
        add_dep(ms, m'.enode);
        event(m', p, u);
      };
  let fix = (f) => {
    let m = emut(Node.delayed_rank);
    let e = event(m, () => [], (_) => assert false);
    switch (f(e)) {
    | (Never, r) => r
    | (Emut(m'), r) =>
      if (m'.enode.rank == Node.delayed_rank) {
        invalid_arg(err_fix);
      };
      let rec p = () => [] /* avoid cyclic dep. */
      and u = (c) => {
        /* N.B. c is the next step. */
        let clear = (v, ()) => v := None;
        m.ev := Some(eval(m'));
        Step.add_eop(c, clear(m.ev)); /* vs. add_cop for regular events. */
        Step.add_deps(c, m.enode);
      };
      Node.bind(m.enode, p, u);
      add_dep(m', m.enode);
      r;
    };
  };
  /* Lifting */
  let l1 = map;
  let l2 = (f, e0, e1) =>
    switch (e0, e1) {
    | (Never, _) => Never
    | (_, Never) => Never
    | (Emut(m0), Emut(m1)) =>
      let r = rsucc2(m0.enode, m1.enode);
      let m' = emut(r);
      let rec p = () => [m0.enode, m1.enode];
      let u = (c) =>
        switch (m0.ev^, m1.ev^) {
        | (None, _)
        | (_, None) => ()
        | (Some(v0), Some(v1)) => eupdate(f(v0, v1), m', c)
        };
      add_dep(m0, m'.enode);
      add_dep(m1, m'.enode);
      event(m', p, u);
    };
  let l3 = (f, e0, e1, e2) =>
    switch (e0, e1, e2) {
    | (Never, _, _) => Never
    | (_, Never, _) => Never
    | (_, _, Never) => Never
    | (Emut(m0), Emut(m1), Emut(m2)) =>
      let r = rsucc(rmax(rmax(m0.enode, m1.enode), m2.enode));
      let m' = emut(r);
      let rec p = () => [m0.enode, m1.enode, m2.enode];
      let u = (c) =>
        switch (m0.ev^, m1.ev^, m2.ev^) {
        | (None, _, _)
        | (_, None, _)
        | (_, _, None) => ()
        | (Some(v0), Some(v1), Some(v2)) => eupdate(f(v0, v1, v2), m', c)
        };
      add_dep(m0, m'.enode);
      add_dep(m1, m'.enode);
      add_dep(m2, m'.enode);
      event(m', p, u);
    };
  let l4 = (f, e0, e1, e2, e3) =>
    switch (e0, e1, e2, e3) {
    | (Never, _, _, _) => Never
    | (_, Never, _, _) => Never
    | (_, _, Never, _) => Never
    | (_, _, _, Never) => Never
    | (Emut(m0), Emut(m1), Emut(m2), Emut(m3)) =>
      let r = rsucc(rmax(rmax(m0.enode, m1.enode), rmax(m2.enode, m3.enode)));
      let m' = emut(r);
      let rec p = () => [m0.enode, m1.enode, m2.enode, m3.enode];
      let u = (c) =>
        switch (m0.ev^, m1.ev^, m2.ev^, m3.ev^) {
        | (None, _, _, _)
        | (_, None, _, _)
        | (_, _, None, _)
        | (_, _, _, None) => ()
        | (Some(v0), Some(v1), Some(v2), Some(v3)) => eupdate(f(v0, v1, v2, v3), m', c)
        };
      add_dep(m0, m'.enode);
      add_dep(m1, m'.enode);
      add_dep(m2, m'.enode);
      add_dep(m3, m'.enode);
      event(m', p, u);
    };
  let l5 = (f, e0, e1, e2, e3, e4) =>
    switch (e0, e1, e2, e3, e4) {
    | (Never, _, _, _, _) => Never
    | (_, Never, _, _, _) => Never
    | (_, _, Never, _, _) => Never
    | (_, _, _, Never, _) => Never
    | (_, _, _, _, Never) => Never
    | (Emut(m0), Emut(m1), Emut(m2), Emut(m3), Emut(m4)) =>
      let r = rsucc(rmax(rmax(rmax(m0.enode, m1.enode), rmax(m2.enode, m3.enode)), m4.enode));
      let m' = emut(r);
      let rec p = () => [m0.enode, m1.enode, m2.enode, m3.enode, m4.enode];
      let u = (c) =>
        switch (m0.ev^, m1.ev^, m2.ev^, m3.ev^, m4.ev^) {
        | (None, _, _, _, _)
        | (_, None, _, _, _)
        | (_, _, None, _, _)
        | (_, _, _, None, _)
        | (_, _, _, _, None) => ()
        | (Some(v0), Some(v1), Some(v2), Some(v3), Some(v4)) =>
          eupdate(f(v0, v1, v2, v3, v4), m', c)
        };
      add_dep(m0, m'.enode);
      add_dep(m1, m'.enode);
      add_dep(m2, m'.enode);
      add_dep(m3, m'.enode);
      add_dep(m4, m'.enode);
      event(m', p, u);
    };
  let l6 = (f, e0, e1, e2, e3, e4, e5) =>
    switch (e0, e1, e2, e3, e4, e5) {
    | (Never, _, _, _, _, _) => Never
    | (_, Never, _, _, _, _) => Never
    | (_, _, Never, _, _, _) => Never
    | (_, _, _, Never, _, _) => Never
    | (_, _, _, _, Never, _) => Never
    | (_, _, _, _, _, Never) => Never
    | (Emut(m0), Emut(m1), Emut(m2), Emut(m3), Emut(m4), Emut(m5)) =>
      let r =
        rsucc(
          rmax(rmax(rmax(m0.enode, m1.enode), rmax(m2.enode, m3.enode)), rmax(m4.enode, m5.enode))
        );
      let m' = emut(r);
      let rec p = () => [m0.enode, m1.enode, m2.enode, m3.enode, m4.enode, m5.enode];
      let u = (c) =>
        switch (m0.ev^, m1.ev^, m2.ev^, m3.ev^, m4.ev^, m5.ev^) {
        | (None, _, _, _, _, _)
        | (_, None, _, _, _, _)
        | (_, _, None, _, _, _)
        | (_, _, _, None, _, _)
        | (_, _, _, _, None, _)
        | (_, _, _, _, _, None) => ()
        | (Some(v0), Some(v1), Some(v2), Some(v3), Some(v4), Some(v5)) =>
          eupdate(f(v0, v1, v2, v3, v4, v5), m', c)
        };
      add_dep(m0, m'.enode);
      add_dep(m1, m'.enode);
      add_dep(m2, m'.enode);
      add_dep(m3, m'.enode);
      add_dep(m4, m'.enode);
      add_dep(m5, m'.enode);
      event(m', p, u);
    };
  /* Pervasives support */
  module Option = {
    let some = (e) => map((v) => Some(v), e);
    let value = (~default=?, e) =>
      switch default {
      | None => fmap((v) => v, e)
      | Some(Const(dv)) =>
        map(
          fun
          | None => dv
          | Some(v) => v,
          e
        )
      | Some(Smut(ms)) =>
        switch e {
        | Never => Never
        | Emut(m) =>
          let m' = emut(rsucc2(m.enode, ms.snode));
          let rec p = () => [m.enode, ms.snode]
          and u = (c) =>
            switch m.ev^ {
            | None => () /* ms updated. */
            | Some(None) => eupdate(sval(ms), m', c)
            | Some(Some(v)) => eupdate(v, m', c)
            };
          add_dep(m, m'.enode);
          Node.add_dep(ms.snode, m'.enode);
          event(m', p, u);
        }
      };
  };
};

module S = {
  type t('a) = signal('a);
  let set_sval = (v, m, c) => {
    m.sv = Some(v);
    Step.add_deps(c, m.snode);
  };
  let set = (m, ~step=?, v) =>
    /* starts an update step. */
    if (m.eq(sval(m), v)) {
      ();
    } else {
      switch step {
      | Some(c) =>
        if (c.over) {
          invalid_arg(err_step_executed);
        } else if (! m.snode.stamp.over) {
          invalid_arg(err_signal_scheduled);
        } else {
          m.snode.stamp = c;
        };
        m.sv = Some(v);
        Step.add_deps(c, m.snode);
      | None =>
        let c = Step.create();
        m.snode.stamp = c;
        m.sv = Some(v);
        Step.add_deps(c, m.snode);
        Step.execute(c);
      };
    };
  let end_of_step_add_dep = (~post_add_op=() => (), ~stop_if_stopped, m, m') =>
    /* In some combinators, when the semantics of event m' is such
       that it should not occur in the (potential) step it is created,
       we add the dependency [m'] to signal [m] only via an end of
       step operation to avoid being scheduled in the step. */
    switch (Step.find_unfinished(m.snode.producers())) {
    | c when c === Step.nil =>
      Node.add_dep(m.snode, m'.enode);
      post_add_op();
    | c =>
      let add_dep = () =>
        if (m.snode.update === Node.nop) {
          /* m stopped in step */
          if (stop_if_stopped) {
            Node.stop(m'.enode);
          };
        } else {
          ignore(Node.update_rank(m'.enode, rsucc(m.snode)));
          Node.add_dep(m.snode, m'.enode);
          post_add_op();
        };
      Step.add_eop(c, add_dep);
    };
  /* Basics */
  let const = (v) => Const(v);
  let create = (~eq=(==), v) => {
    let m = smut(Node.min_rank, eq);
    m.sv = Some(v);
    (Smut(m), set(m));
  };
  let retain = (s, c) =>
    switch s {
    | Const(_) => invalid_arg(err_retain_cst_sig)
    | Smut(m) =>
      let c' = m.snode.retain;
      m.snode.retain = c;
      `R(c');
    };
  let eq_fun =
    fun
    | Const(_) => None
    | Smut(m) => Some(m.eq);
  let value =
    fun
    | Const(v)
    | Smut({sv: Some(v)}) => v
    | Smut({sv: None}) => failwith(err_sig_undef);
  let stop = (~strong=?) =>
    fun
    | Const(_) => ()
    | Smut(m) =>
      switch m.sv {
      | Some(_) => Node.stop(~strong?, m.snode)
      | None =>
        /* The signal was dynamically created and didn't update yet. Add the
           stop as an end of step operation. */
        switch (Step.find_unfinished(m.snode.producers())) {
        | c when c === Step.nil => assert false
        | c =>
          let stop = () => Node.stop(~strong?, m.snode);
          Step.add_eop(c, stop);
        }
      };
  let equal = (~eq=(==), s, s') =>
    switch (s, s') {
    | (Const(v), Const(v')) => eq(v, v')
    | (Const(_), _)
    | (_, Const(_)) => false
    | (Smut(m), Smut(m')) => m === m'
    };
  let trace = (~iff=const(true), t, s) =>
    switch iff {
    | Const(false) => s
    | Const(true) =>
      switch s {
      | Const(v) =>
        t(v);
        s;
      | Smut(m) =>
        let m' = smut(rsucc(m.snode), m.eq);
        let rec p = () => [m.snode];
        let u = (c) => {
          let v = sval(m);
          t(v);
          supdate(v, m', c);
        };
        Node.add_dep(m.snode, m'.snode);
        signal(m', p, u);
      }
    | Smut(mc) =>
      switch s {
      | Const(v) =>
        let m' = smut(rsucc(mc.snode), (==)) /* we don't care about eq */;
        let rec p = () => [mc.snode]
        and u = (c) => {
          if (sval(mc)) {
            t(v);
          };
          Node.rem_dep(mc.snode, m'.snode);
          Node.stop(m'.snode);
        };
        Node.add_dep(mc.snode, m'.snode);
        signal(~i=v, m', p, u);
      | Smut(m) =>
        let m' = smut(rsucc2(mc.snode, m.snode), m.eq);
        let rec p = () => [mc.snode, m.snode]
        and u = (c) => {
          let v = sval(m);
          switch m'.sv {
          | Some(v') when m'.eq(v, v') => () /* mc updated. */
          | _ => if (sval(mc)) {t(v);}; supdate(v, m', c); /* init or diff. */
          };
        };
        Node.add_dep(mc.snode, m'.snode);
        Node.add_dep(m.snode, m'.snode);
        signal(m', p, u);
      }
    };
  /* From events */
  let hold = (~eq=(==), i) =>
    fun
    | Never => Const(i)
    | Emut(m) => {
        let m' = smut(rsucc(m.enode), eq);
        let rec p = () => [m.enode]
        and u = (c) =>
          switch m.ev^ {
          | None => () /* init. only. */
          | Some(v) => supdate(v, m', c)
          };
        E.add_dep(m, m'.snode);
        signal(~i, m', p, u);
      };
  /* Filtering and transforming */
  let map = (~eq=(==), f) =>
    fun
    | Const(v) => Const(f(v))
    | Smut(m) => {
        let m' = smut(rsucc(m.snode), eq);
        let rec p = () => [m.snode]
        and u = (c) => supdate(f(sval(m)), m', c);
        Node.add_dep(m.snode, m'.snode);
        signal(m', p, u);
      };
  let app = (~eq=(==), sf, sv) =>
    switch (sf, sv) {
    | (Smut(mf), Smut(mv)) =>
      let m' = smut(rsucc2(mf.snode, mv.snode), eq);
      let rec p = () => [mf.snode, mv.snode]
      and u = (c) => supdate((sval(mf))(sval(mv)), m', c);
      Node.add_dep(mf.snode, m'.snode);
      Node.add_dep(mv.snode, m'.snode);
      signal(m', p, u);
    | (Const(f), Const(v)) => Const(f(v))
    | (Const(f), sv) => map(~eq, f, sv)
    | (Smut(mf), Const(v)) =>
      let m' = smut(rsucc(mf.snode), eq);
      let rec p = () => [mf.snode]
      and u = (c) => supdate((sval(mf))(v), m', c);
      Node.add_dep(mf.snode, m'.snode);
      signal(m', p, u);
    };
  let filter = (~eq=(==), pred, i) =>
    fun
    | Const(v) as s =>
      if (pred(v)) {
        s;
      } else {
        Const(i);
      }
    | Smut(m) => {
        let m' = smut(rsucc(m.snode), eq);
        let rec p = () => [m.snode]
        and u = (c) => {
          let v = sval(m);
          if (pred(v)) {
            supdate(v, m', c);
          } else {
            ();
          };
        };
        Node.add_dep(m.snode, m'.snode);
        signal(~i, m', p, u);
      };
  let fmap = (~eq=(==), fm, i) =>
    fun
    | Const(v) =>
      switch (fm(v)) {
      | Some(v') => Const(v')
      | None => Const(i)
      }
    | Smut(m) => {
        let m' = smut(rsucc(m.snode), eq);
        let rec p = () => [m.snode]
        and u = (c) =>
          switch (fm(sval(m))) {
          | Some(v) => supdate(v, m', c)
          | None => ()
          };
        Node.add_dep(m.snode, m'.snode);
        signal(~i, m', p, u);
      };
  let diff = (d) =>
    fun
    | Const(_) => Never
    | Smut(m) => {
        let m' = emut(rsucc(m.snode));
        let last = ref(None);
        let rec p = () => [m.snode]
        and u = (c) => {
          let v = sval(m);
          switch last^ {
          | Some(v') =>
            last := Some(v);
            eupdate(d(v, v'), m', c);
          | None => assert false
          };
        };
        let post_add_op = () => last := Some(sval(m));
        end_of_step_add_dep(~post_add_op, ~stop_if_stopped=true, m, m');
        event(m', p, u);
      };
  let changes =
    fun
    | Const(_) => Never
    | Smut(m) => {
        let m' = emut(rsucc(m.snode));
        let rec p = () => [m.snode]
        and u = (c) => eupdate(sval(m), m', c);
        end_of_step_add_dep(~stop_if_stopped=true, m, m');
        event(m', p, u);
      };
  let sample = (f, e) =>
    fun
    | Const(v) => E.map((ev) => f(ev, v), e)
    | Smut(ms) =>
      switch e {
      | Never => Never
      | Emut(me) =>
        let m' = emut(rsucc2(me.enode, ms.snode));
        let rec p = () => [me.enode, ms.snode]
        and u = (c) =>
          switch me.ev^ {
          | None => () /* ms updated */
          | Some(v) => eupdate(f(v, sval(ms)), m', c)
          };
        E.add_dep(me, m'.enode);
        Node.add_dep(ms.snode, m'.enode);
        event(m', p, u);
      };
  let on = (~eq=(==), c, i, s) =>
    switch c {
    | Const(true) => s
    | Const(false) => Const(i)
    | Smut(mc) =>
      switch s {
      | Const(v) =>
        let m' = smut(rsucc(mc.snode), eq);
        let rec p = () => [mc.snode]
        and u = (c) =>
          if (sval(mc)) {
            supdate(v, m', c);
          } else {
            ();
          };
        Node.add_dep(mc.snode, m'.snode);
        signal(~i, m', p, u);
      | Smut(ms) =>
        let m' = smut(rsucc2(mc.snode, ms.snode), eq);
        let rec p = () => [mc.snode, ms.snode]
        and u = (c) =>
          if (sval(mc)) {
            supdate(sval(ms), m', c);
          } else {
            ();
          };
        Node.add_dep(mc.snode, m'.snode);
        Node.add_dep(ms.snode, m'.snode);
        signal(~i, m', p, u);
      }
    };
  let when_ = on;
  let dismiss = (~eq=(==), c, i, s) =>
    switch c {
    | Never => s
    | Emut(mc) =>
      switch s {
      | Const(v) =>
        let m' = smut(rsucc(mc.enode), eq);
        let rec p = () => [mc.enode]
        and u = (c) =>
          switch mc.ev^ {
          | Some(_) => ()
          | None => supdate(v, m', c)
          };
        Node.add_dep(mc.enode, m'.snode);
        signal(~i, m', p, u);
      | Smut(ms) =>
        let m' = smut(rsucc2(mc.enode, ms.snode), eq);
        let rec p = () => [mc.enode, ms.snode]
        and u = (c) =>
          switch mc.ev^ {
          | Some(_) => ()
          | None => supdate(sval(ms), m', c)
          };
        Node.add_dep(mc.enode, m'.snode);
        Node.add_dep(ms.snode, m'.snode);
        signal(~i, m', p, u);
      }
    };
  /* Accumulating */
  let accum = (~eq=(==), ef, i) =>
    switch ef {
    | Never => Const(i)
    | Emut(m) =>
      let m' = smut(rsucc(m.enode), eq);
      let rec p = () => [m.enode]
      and u = (c) =>
        switch m.ev^ {
        | None => () /* init only. */
        | Some(v) => supdate(v(sval(m')), m', c)
        };
      E.add_dep(m, m'.snode);
      signal(~i, m', p, u);
    };
  let fold = (~eq=(==), f, i) =>
    fun
    | Never => Const(i)
    | Emut(m) => {
        let m' = smut(rsucc(m.enode), eq);
        let rec p = () => [m.enode]
        and u = (c) =>
          switch m.ev^ {
          | None => () /* init only. */
          | Some(v) => supdate(f(sval(m'), v), m', c)
          };
        E.add_dep(m, m'.snode);
        signal(~i, m', p, u);
      };
  /* Combining */
  let merge = (~eq=(==), f, a, sl) => {
    let rmax' = (acc) =>
      fun
      | Const(_) => acc
      | Smut(m) => rmax(acc, m.snode);
    let nodes = (acc) =>
      fun
      | Const(_) => acc
      | Smut(m) => [m.snode, ...acc];
    let merger = (f, a) =>
      fun
      | Const(v) => f(a, v)
      | Smut(m) => f(a, sval(m));
    let m' = smut(rsucc(List.fold_left(rmax', Node.rmin, sl)), eq);
    let rec p = () => List.fold_left(nodes, [], sl)
    and u = (c) => supdate(List.fold_left(merger(f), a, sl), m', c);
    let dep =
      fun
      | Const(_) => ()
      | Smut(m) => Node.add_dep(m.snode, m'.snode);
    List.iter(dep, sl);
    signal(m', p, u);
  };
  let switch_ = (~eq=(==)) =>
    fun
    | Const(s) => s
    | Smut(mss) => {
        let dummy = smut(Node.min_rank, eq);
        let src = ref(Smut(dummy)); /* dummy is overwritten by sig. init */
        let m' = smut(rsucc(mss.snode), eq);
        let rec p = () =>
          switch src^ {
          | Smut(m) => [mss.snode, m.snode]
          | Const(_) => [mss.snode]
          }
        and u = (c) =>
          if (sval(mss) === src^) {
            /* ss didn't change, !src did */
            switch src^ {
            | Smut(m) => supdate(sval(m), m', c)
            | Const(_) => () /* init only. */
            };
          } else {
            /* ss changed */
            switch src^ {
            | Smut(m) => Node.rem_dep(m.snode, m'.snode)
            | Const(_) => ()
            };
            let new_src = sval(mss);
            src := new_src;
            switch new_src {
            | Const(v) =>
              ignore(Node.update_rank(m'.snode, rsucc(mss.snode)));
              supdate(v, m', c);
            | Smut(m) =>
              Node.add_dep(m.snode, m'.snode);
              if (c === Step.nil) {
                ignore(Node.update_rank(m'.snode, rsucc2(m.snode, mss.snode)));
                /* Check if the init src is in a step. */
                switch (Step.find_unfinished([m.snode])) {
                | c when c === Step.nil => supdate(sval(m), m', c)
                | c => Step.add(c, m'.snode)
                };
              } else if (Node.update_rank(m'.snode, rsucc2(m.snode, mss.snode))) {
                /* Rank increased because of m. Thus m may still
                   update and we need to reschedule. Next time we
                   will be in the other branch. */
                Step.allow_reschedule(m'.snode);
                Step.rebuild(c);
                Step.add(c, m'.snode);
              } else {
                /* No rank increase. m already updated if needed, no need
                   to reschedule and rebuild the queue. */
                supdate(sval(m), m', c);
              };
            };
          };
        Node.add_dep(mss.snode, m'.snode);
        /* We add a dep to dummy to avoid a long scan of Wa.rem when we remove
           the dep in the [u] function during static init. */
        Node.add_dep(dummy.snode, m'.snode);
        signal(m', p, u);
      };
  let bind = (~eq=?, s, sf) => switch_(~eq?, map(~eq=(===), sf, s));
  let fix = (~eq=(==), i, f) => {
    let update_delayed = (n, p, u, nl) => {
      Node.bind(n, p, u);
      switch (Step.find_unfinished(nl)) {
      | c when c === Step.nil =>
        /* no pertinent occuring step, create a step for update. */
        let c = Step.create();
        n.update(c);
        Step.execute(c);
      | c => Step.add(c, n)
      };
    };
    let m = smut(Node.delayed_rank, eq);
    let s = signal(~i, m, () => [], (_) => ());
    switch (f(s)) {
    | (Const(v), r) =>
      let rec p = () => []
      and u = (c) => supdate(v, m, c);
      update_delayed(m.snode, p, u, Node.deps(m.snode));
      r;
    | (Smut(m'), r) =>
      if (m'.snode.rank == Node.delayed_rank) {
        invalid_arg(err_fix);
      };
      let rec p = () => [] /* avoid cyclic dep. */
      and u = (c) => supdate(sval(m'), m, c); /* N.B. c is the next step. */
      Node.add_dep(m'.snode, m.snode);
      update_delayed(m.snode, p, u, [m'.snode, ...Node.deps(m.snode)]);
      r;
    };
  };
  /* Lifting */
  let l1 = map;
  let l2 = (~eq=(==), f, s, s') =>
    switch (s, s') {
    | (Smut(m0), Smut(m1)) =>
      let m' = smut(rsucc2(m0.snode, m1.snode), eq);
      let rec p = () => [m0.snode, m1.snode]
      and u = (c) => supdate(f(sval(m0), sval(m1)), m', c);
      Node.add_dep(m0.snode, m'.snode);
      Node.add_dep(m1.snode, m'.snode);
      signal(m', p, u);
    | (Const(v), Const(v')) => Const(f(v, v'))
    | (Const(v), Smut(m)) =>
      let m' = smut(rsucc(m.snode), eq);
      let rec p = () => [m.snode]
      and u = (c) => supdate(f(v, sval(m)), m', c);
      Node.add_dep(m.snode, m'.snode);
      signal(m', p, u);
    | (Smut(m), Const(v)) =>
      let m' = smut(rsucc(m.snode), eq);
      let rec p = () => [m.snode]
      and u = (c) => supdate(f(sval(m), v), m', c);
      Node.add_dep(m.snode, m'.snode);
      signal(m', p, u);
    };
  let l3 = (~eq=(==), f, s0, s1, s2) =>
    switch (s0, s1, s2) {
    | (Smut(m0), Smut(m1), Smut(m2)) =>
      let r = rsucc(rmax(rmax(m0.snode, m1.snode), m2.snode));
      let m' = smut(r, eq);
      let rec p = () => [m0.snode, m1.snode, m2.snode]
      and u = (c) => supdate(f(sval(m0), sval(m1), sval(m2)), m', c);
      Node.add_dep(m0.snode, m'.snode);
      Node.add_dep(m1.snode, m'.snode);
      Node.add_dep(m2.snode, m'.snode);
      signal(m', p, u);
    | (Const(v0), Const(v1), Const(v2)) => Const(f(v0, v1, v2))
    | (s0, s1, s2) => app(~eq, l2(~eq=(===), f, s0, s1), s2)
    };
  let l4 = (~eq=(==), f, s0, s1, s2, s3) =>
    switch (s0, s1, s2, s3) {
    | (Smut(m0), Smut(m1), Smut(m2), Smut(m3)) =>
      let r = rsucc(rmax(rmax(m0.snode, m1.snode), rmax(m2.snode, m3.snode)));
      let m' = smut(r, eq);
      let rec p = () => [m0.snode, m1.snode, m2.snode, m3.snode]
      and u = (c) => supdate(f(sval(m0), sval(m1), sval(m2), sval(m3)), m', c);
      Node.add_dep(m0.snode, m'.snode);
      Node.add_dep(m1.snode, m'.snode);
      Node.add_dep(m2.snode, m'.snode);
      Node.add_dep(m3.snode, m'.snode);
      signal(m', p, u);
    | (Const(v0), Const(v1), Const(v2), Const(v3)) => Const(f(v0, v1, v2, v3))
    | (s0, s1, s2, s3) => app(~eq, l3(~eq=(===), f, s0, s1, s2), s3)
    };
  let l5 = (~eq=(==), f, s0, s1, s2, s3, s4) =>
    switch (s0, s1, s2, s3, s4) {
    | (Smut(m0), Smut(m1), Smut(m2), Smut(m3), Smut(m4)) =>
      let m = rmax;
      let r = rsucc(m(m(m0.snode, m1.snode), m(m2.snode, m(m3.snode, m4.snode))));
      let m' = smut(r, eq);
      let rec p = () => [m0.snode, m1.snode, m2.snode, m3.snode, m4.snode]
      and u = (c) => {
        let v = f(sval(m0), sval(m1), sval(m2), sval(m3), sval(m4));
        supdate(v, m', c);
      };
      Node.add_dep(m0.snode, m'.snode);
      Node.add_dep(m1.snode, m'.snode);
      Node.add_dep(m2.snode, m'.snode);
      Node.add_dep(m3.snode, m'.snode);
      Node.add_dep(m4.snode, m'.snode);
      signal(m', p, u);
    | (Const(v0), Const(v1), Const(v2), Const(v3), Const(v4)) => Const(f(v0, v1, v2, v3, v4))
    | (s0, s1, s2, s3, s4) => app(~eq, l4(~eq=(===), f, s0, s1, s2, s3), s4)
    };
  let l6 = (~eq=(==), f, s0, s1, s2, s3, s4, s5) =>
    switch (s0, s1, s2, s3, s4, s5) {
    | (Smut(m0), Smut(m1), Smut(m2), Smut(m3), Smut(m4), Smut(m5)) =>
      let m = rmax;
      let m = m(m(m0.snode, m(m1.snode, m2.snode)), m(m3.snode, m(m4.snode, m5.snode)));
      let m' = smut(rsucc(m), eq);
      let rec p = () => [m0.snode, m1.snode, m2.snode, m3.snode, m4.snode, m5.snode]
      and u = (c) => {
        let v = f(sval(m0), sval(m1), sval(m2), sval(m3), sval(m4), sval(m5));
        supdate(v, m', c);
      };
      Node.add_dep(m0.snode, m'.snode);
      Node.add_dep(m1.snode, m'.snode);
      Node.add_dep(m2.snode, m'.snode);
      Node.add_dep(m3.snode, m'.snode);
      Node.add_dep(m4.snode, m'.snode);
      Node.add_dep(m5.snode, m'.snode);
      signal(m', p, u);
    | (Const(v0), Const(v1), Const(v2), Const(v3), Const(v4), Const(v5)) =>
      Const(f(v0, v1, v2, v3, v4, v5))
    | (s0, s1, s2, s3, s4, s5) => app(~eq, l5(~eq=(===), f, s0, s1, s2, s3, s4), s5)
    };
  module Bool = {
    let one = Const(true);
    let zero = Const(false);
    let eq: (bool, bool) => bool = (==);
    let (!) = (s) => l1(~eq, (!), s);
    let (&&) = (s, s') => l2(~eq, (&&), s, s');
    let (||) = (s, s') => l2(~eq, (||), s, s');
    let edge = (s) => changes(s);
    let edge_detect = (edge) =>
      fun
      | Const(_) => Never
      | Smut(m) => {
          let m' = emut(rsucc(m.snode));
          let rec p = () => [m.snode]
          and u = (c) =>
            if (sval(m) == edge) {
              eupdate((), m', c);
            };
          end_of_step_add_dep(~stop_if_stopped=true, m, m');
          event(m', p, u);
        };
    let rise = (s) => edge_detect(true, s);
    let fall = (s) => edge_detect(false, s);
    let flip = (b) =>
      fun
      | Never => Const(b)
      | Emut(m) => {
          let m' = smut(rsucc(m.enode), (==));
          let rec p = () => [m.enode]
          and u = (c) =>
            switch m.ev^ {
            | None => ()
            | Some(_) => supdate(Pervasives.(!)(sval(m')), m', c)
            };
          E.add_dep(m, m'.snode);
          signal(~i=b, m', p, u);
        };
  };
  module Int = {
    let zero = Const(0);
    let one = Const(1);
    let minus_one = Const(-1);
    let eq: (int, int) => bool = (==);
    let (~-) = (s) => l1(~eq, (~-), s);
    let succ = (s) => l1(~eq, succ, s);
    let pred = (s) => l1(~eq, pred, s);
    let (+) = (s, s') => l2(~eq, (+), s, s');
    let (-) = (s, s') => l2(~eq, (-), s, s');
    let ( * ) = (s, s') => l2(~eq, ( * ), s, s');
    let (mod) = (s, s') => l2(~eq, (mod), s, s');
    let abs = (s) => l1(~eq, abs, s);
    let max_int = const(max_int);
    let min_int = const(min_int);
    let (land) = (s, s') => l2(~eq, (land), s, s');
    let (lor) = (s, s') => l2(~eq, (lor), s, s');
    let (lxor) = (s, s') => l2(~eq, (lxor), s, s');
    let lnot = (s) => l1(~eq, lnot, s);
    let (lsl) = (s, s') => l2(~eq, (lsl), s, s');
    let (lsr) = (s, s') => l2(~eq, (lsr), s, s');
    let (asr) = (s, s') => l2(~eq, (asr), s, s');
  };
  module Float = {
    let zero = Const(0.);
    let one = Const(1.);
    let minus_one = Const(-1.);
    let eq: (float, float) => bool = (==);
    let (~-.) = (s) => l1(~eq, (~-.), s);
    let (+.) = (s, s') => l2(~eq, (+.), s, s');
    let (-.) = (s, s') => l2(~eq, (-.), s, s');
    let ( *. ) = (s, s') => l2(~eq, ( *. ), s, s');
    let (/.) = (s, s') => l2(~eq, (/.), s, s');
    let ( ** ) = (s, s') => l2(~eq, ( ** ), s, s');
    let sqrt = (s) => l1(~eq, sqrt, s);
    let exp = (s) => l1(~eq, exp, s);
    let log = (s) => l1(~eq, log, s);
    let log10 = (s) => l1(~eq, log10, s);
    let cos = (s) => l1(~eq, cos, s);
    let sin = (s) => l1(~eq, sin, s);
    let tan = (s) => l1(~eq, tan, s);
    let acos = (s) => l1(~eq, acos, s);
    let asin = (s) => l1(~eq, asin, s);
    let atan = (s) => l1(~eq, atan, s);
    let atan2 = (s, s') => l2(~eq, atan2, s, s');
    let cosh = (s) => l1(~eq, cosh, s);
    let sinh = (s) => l1(~eq, sinh, s);
    let tanh = (s) => l1(~eq, tanh, s);
    let ceil = (s) => l1(~eq, ceil, s);
    let floor = (s) => l1(~eq, floor, s);
    let abs_float = (s) => l1(~eq, abs_float, s);
    let mod_float = (s, s') => l2(~eq, mod_float, s, s');
    let frexp = (s) => l1(~eq=(==), frexp, s);
    let ldexp = (s, s') => l2(~eq, ldexp, s, s');
    let modf = (s) => l1(~eq=(==), modf, s);
    let float = (s) => l1(~eq, float, s);
    let float_of_int = (s) => l1(~eq, float_of_int, s);
    let truncate = (s) => l1(~eq=Int.eq, truncate, s);
    let int_of_float = (s) => l1(~eq=Int.eq, int_of_float, s);
    let infinity = const(infinity);
    let neg_infinity = const(neg_infinity);
    let nan = const(nan);
    let max_float = const(max_float);
    let min_float = const(min_float);
    let epsilon_float = const(epsilon_float);
    let classify_float = (s) => l1(~eq=(==), classify_float, s);
  };
  module Pair = {
    let pair = (~eq=?, s, s') => l2(~eq?, (x, y) => (x, y), s, s');
    let fst = (~eq=?, s) => l1(~eq?, fst, s);
    let snd = (~eq=?, s) => l1(~eq?, snd, s);
  };
  module Option = {
    let none = Const(None);
    let some = (s) => {
      let eq =
        switch (eq_fun(s)) {
        | None => None
        | Some(eq) =>
          let eq = (v, v') =>
            switch (v, v') {
            | (Some(v), Some(v')) => eq(v, v')
            | _ => assert false
            };
          Some(eq);
        };
      map(~eq?, (v) => Some(v), s);
    };
    let value = (~eq=(==), ~default, s) =>
      switch s {
      | Const(Some(v)) => Const(v)
      | Const(None) =>
        switch default {
        | `Always(d) => d
        | `Init(d) =>
          switch d {
          | Const(d) => Const(d)
          | Smut(md) =>
            switch (Step.find_unfinished([md.snode])) {
            | c when c === Step.nil => Const(sval(md))
            | c =>
              let m' = smut(rsucc(md.snode), eq);
              let rec p = () => [md.snode]
              and u = (c) => {
                Node.rem_dep(md.snode, m'.snode);
                supdate(sval(md), m', c);
                Node.stop(m'.snode);
              };
              Node.add_dep(md.snode, m'.snode);
              signal(m', p, u);
            }
          }
        }
      | Smut(m) =>
        switch default {
        | `Init(Const(d)) => fmap(~eq, (v) => v, d, s)
        | `Always(Const(d)) =>
          map(
            ~eq,
            fun
            | None => d
            | Some(v) => v,
            s
          )
        | `Init(Smut(md)) =>
          switch (Step.find_unfinished([md.snode])) {
          | c when c === Step.nil =>
            let m' = smut(rsucc(m.snode), eq);
            let rec p = () => [m.snode]
            and u = (c) =>
              switch (sval(m)) {
              | Some(v) => supdate(v, m', c)
              | None => ()
              };
            Node.add_dep(m.snode, m'.snode);
            signal(~i=sval(md), m', p, u);
          | c =>
            let m' = smut(rsucc2(m.snode, md.snode), eq);
            let rec p = () => [m.snode]; /* subsequent updates */
            let u = (c) =>
              switch (sval(m)) {
              | Some(v) => supdate(v, m', c)
              | None => ()
              };
            let rec p_first = () => [m.snode, md.snode]; /* first update */
            let u_first = (c) => {
              Node.rem_dep(md.snode, m'.snode);
              switch (sval(m)) {
              | None => supdate(sval(md), m', c)
              | Some(v) => supdate(v, m', c)
              };
              Node.bind(m'.snode, p, u);
            };
            Node.add_dep(m.snode, m'.snode);
            Node.add_dep(md.snode, m'.snode);
            signal(m', p_first, u_first);
          }
        | `Always(Smut(md)) =>
          let m' = smut(rsucc2(m.snode, md.snode), eq);
          let rec p = () => [m.snode, md.snode];
          let u = (c) =>
            switch (sval(m)) {
            | Some(v) => supdate(v, m', c)
            | None => supdate(sval(md), m', c)
            };
          Node.add_dep(m.snode, m'.snode);
          Node.add_dep(md.snode, m'.snode);
          signal(m', p, u);
        }
      };
  };
  module Compare = {
    let eq = Bool.eq;
    let (==) = (s, s') => l2(~eq, (==), s, s');
    let (!=) = (s, s') => l2(~eq, (!=), s, s');
    let (<) = (s, s') => l2(~eq, (<), s, s');
    let (>) = (s, s') => l2(~eq, (>), s, s');
    let (<=) = (s, s') => l2(~eq, (<=), s, s');
    let (>=) = (s, s') => l2(~eq, (>=), s, s');
    let compare = (s, s') => l2(~eq=Int.eq, compare, s, s');
    let (===) = (s, s') => l2(~eq, (===), s, s');
    let (!==) = (s, s') => l2(~eq, (!==), s, s');
  };
  /* Combinator specialization */
  module type EqType = {type t('a); let equal: (t('a), t('a)) => bool;};
  module type S = {
    type v('a);
    let create: v('a) => (signal(v('a)), (~step: step=?, v('a)) => unit);
    let equal: (signal(v('a)), signal(v('a))) => bool;
    let hold: (v('a), event(v('a))) => signal(v('a));
    let app: (signal('a => v('b)), signal('a)) => signal(v('b));
    let map: ('a => v('b), signal('a)) => signal(v('b));
    let filter: (v('a) => bool, v('a), signal(v('a))) => signal(v('a));
    let fmap: ('a => option(v('b)), v('b), signal('a)) => signal(v('b));
    let when_: (signal(bool), v('a), signal(v('a))) => signal(v('a));
    let dismiss: (event('b), v('a), signal(v('a))) => signal(v('a));
    let accum: (event(v('a) => v('a)), v('a)) => signal(v('a));
    let fold: ((v('a), 'b) => v('a), v('a), event('b)) => signal(v('a));
    let merge: ((v('a), 'b) => v('a), v('a), list(signal('b))) => signal(v('a));
    let switch_: signal(signal(v('a))) => signal(v('a));
    let bind: (signal('b), 'b => signal(v('a))) => signal(v('a));
    let fix: (v('a), signal(v('a)) => (signal(v('a)), 'b)) => 'b;
    let l1: ('a => v('b), signal('a)) => signal(v('b));
    let l2: (('a, 'b) => v('c), signal('a), signal('b)) => signal(v('c));
    let l3: (('a, 'b, 'c) => v('d), signal('a), signal('b), signal('c)) => signal(v('d));
    let l4:
      (('a, 'b, 'c, 'd) => v('e), signal('a), signal('b), signal('c), signal('d)) => signal(v('e));
    let l5:
      (
        ('a, 'b, 'c, 'd, 'e) => v('f),
        signal('a),
        signal('b),
        signal('c),
        signal('d),
        signal('e)
      ) =>
      signal(v('f));
    let l6:
      (
        ('a, 'b, 'c, 'd, 'e, 'f) => v('g),
        signal('a),
        signal('b),
        signal('c),
        signal('d),
        signal('e),
        signal('f)
      ) =>
      signal(v('g));
  };
  module Make = (Eq: EqType) => {
    type v('a) = Eq.t('a);
    let eq = Eq.equal;
    let create = (v) => create(~eq, v);
    let equal = (s, s') => equal(~eq, s, s');
    let hold = (v, e) => hold(~eq, v, e);
    let app = (sf, sv) => app(~eq, sf, sv);
    let map = (f, s) => map(~eq, f, s);
    let filter = (pred, i) => filter(~eq, pred, i);
    let fmap = (fm, i) => fmap(~eq, fm, i);
    let when_ = (c, i, s) => when_(~eq, c, i, s);
    let dismiss = (c, s) => dismiss(~eq, c, s);
    let accum = (ef, i) => accum(~eq, ef, i);
    let fold = (f, i) => fold(~eq, f, i);
    let merge = (f, a, sl) => merge(~eq, f, a, sl);
    let switch_ = (s) => switch_(~eq, s);
    let bind = (s, sf) => bind(~eq, s, sf);
    let fix = (f) => fix(~eq, f);
    let l1 = map;
    let l2 = (f, s, s') => l2(~eq, f, s, s');
    let l3 = (f, s0, s1, s2) => l3(~eq, f, s0, s1, s2);
    let l4 = (f, s0, s1, s2, s3) => l4(~eq, f, s0, s1, s2, s3);
    let l5 = (f, s0, s1, s2, s3, s4) => l5(~eq, f, s0, s1, s2, s3, s4);
    let l6 = (f, s0, s1, s2, s3, s4, s5) => l6(~eq, f, s0, s1, s2, s3, s4, s5);
  };
  module Special = {
    module Sb =
      Make(
        {
          type t('a) = bool;
          let equal = Bool.eq;
        }
      );
    module Si =
      Make(
        {
          type t('a) = int;
          let equal = Int.eq;
        }
      );
    module Sf =
      Make(
        {
          type t('a) = float;
          let equal = Float.eq;
        }
      );
  };
};
/*---------------------------------------------------------------------------
   Copyright (c) 2009 Daniel C. B nzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/