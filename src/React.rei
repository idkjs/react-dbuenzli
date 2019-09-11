/*---------------------------------------------------------------------------
   Copyright (c) 2009 Daniel C. B nzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*/
/** Declarative events and signals.

    React is a module for functional reactive programming (frp).  It
    provides support to program with time varying values : declarative
    {{!E}events} and {{!S}signals}. React
    doesn't define any primitive event or signal, this lets the client
    choose the concrete timeline.

    Consult the {{!sem}semantics}, the {{!basics}basics} and
    {{!ex}examples}. Open the module to use it, this defines only two
    types and modules in your scope.

    {e Release %%VERSION%% - %%MAINTAINER%% } */;

/**    {1 Interface} */;

/** The type for events of type ['a]. */
type event('a);

/** The type for signals of type ['a]. */
type signal('a);

/** The type for update steps. */
type step;

/** Event combinators.

    Consult their {{!evsem}semantics.} */
module E: {
  /** {1:prim Primitive and basics} */;
  /** The type for events with occurrences of type ['a]. */
  type t('a) = event('a);
  /** A never occuring event. For all t, \[[never]\]{_t} [= None]. */
  let never: event('a);
  /** [create ()] is a primitive event [e] and a [send] function. The
      function [send] is such that:
      {ul
      {- [send v] generates an occurrence [v] of [e] at the time it is called
         and triggers an {{!steps}update step}.}
      {- [send ~step v] generates an occurence [v] of [e] on the step [step]
         when [step] is {{!Step.execute}executed}.}
      {- [send ~step v] raises [Invalid_argument] if it was previously
         called with a step and this step has not executed yet or if
         the given [step] was already executed.}}

      {b Warning.} [send] must not be executed inside an update step. */
  let create: unit => (event('a), (~step: step=?, 'a) => unit);
  /** [retain e c] keeps a reference to the closure [c] in [e] and
      returns the previously retained value. [c] will {e never} be
      invoked.

      {b Raises.} [Invalid_argument] on {!E.never}. */
  let retain: (event('a), unit => unit) => [ | `R(unit => unit)];
  /** [stop e] stops [e] from occuring. It conceptually becomes
      {!never} and cannot be restarted. Allows to
      disable {{!sideeffects}effectful} events.

      The [strong] argument should only be used on platforms
      where weak arrays have a strong semantics (i.e. JavaScript).
      See {{!strongstop}details}.

      {b Note.} If executed in an {{!steps}update step}
      the event may still occur in the step. */
  let stop: (~strong: bool=?, event('a)) => unit;
  /** [equal e e'] is [true] iff [e] and [e'] are equal. If both events are
      different from {!never}, physical equality is used. */
  let equal: (event('a), event('a)) => bool;
  /** [trace iff tr e] is [e] except [tr] is invoked with e's
      occurence when [iff] is [true] (defaults to [S.const true]).
      For all t where \[[e]\]{_t} [= Some v] and \[[iff]\]{_t} =
      [true], [tr] is invoked with [v]. */
  let trace: (~iff: signal(bool)=?, 'a => unit, event('a)) => event('a);
  /** {1:transf Transforming and filtering} */;
  /** [once e] is [e] with only its next occurence.
      {ul
      {- \[[once e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      \[[e]\]{_<t} [= None].}
      {- \[[once e]\]{_t} [= None] otherwise.}} */
  let once: event('a) => event('a);
  /** [drop_once e] is [e] without its next occurrence.
      {ul
      {- \[[drop_once e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      \[[e]\]{_<t} [= Some _].}
      {- \[[drop_once e]\]{_t} [= None] otherwise.}} */
  let drop_once: event('a) => event('a);
  /** [app ef e] occurs when both [ef] and [e] occur
      {{!simultaneity}simultaneously}.
      The value is [ef]'s occurence applied to [e]'s one.
      {ul
      {- \[[app ef e]\]{_t} [= Some v'] if \[[ef]\]{_t} [= Some f] and
      \[[e]\]{_t} [= Some v] and [f v = v'].}
      {- \[[app ef e]\]{_t} [= None] otherwise.}} */
  let app: (event('a => 'b), event('a)) => event('b);
  /** [map f e] applies [f] to [e]'s occurrences.
      {ul
      {- \[[map f e]\]{_t} [= Some (f v)] if \[[e]\]{_t} [= Some v].}
      {- \[[map f e]\]{_t} [= None] otherwise.}} */
  let map: ('a => 'b, event('a)) => event('b);
  /** [stamp e v] is [map (fun _ -> v) e]. */
  let stamp: (event('b), 'a) => event('a);
  /** [filter p e] are [e]'s occurrences that satisfy [p].
      {ul
      {- \[[filter p e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      [p v = true]}
      {- \[[filter p e]\]{_t} [= None] otherwise.}} */
  let filter: ('a => bool, event('a)) => event('a);
  /** [fmap fm e] are [e]'s occurrences filtered and mapped by [fm].
      {ul
      {- \[[fmap fm e]\]{_t} [= Some v] if [fm] \[[e]\]{_t} [= Some v]}
      {- \[[fmap fm e]\]{_t} [= None] otherwise.}} */
  let fmap: ('a => option('b), event('a)) => event('b);
  /** [diff f e] occurs whenever [e] occurs except on the next occurence.
      Occurences are [f v v'] where [v] is [e]'s current
      occurrence and [v'] the previous one.
      {ul
      {- \[[diff f e]\]{_t} [= Some r] if \[[e]\]{_t} [= Some v],
      \[[e]\]{_<t} [= Some v'] and [f v v' = r].}
      {- \[[diff f e]\]{_t} [= None] otherwise.}} */
  let diff: (('a, 'a) => 'b, event('a)) => event('b);
  /** [changes eq e] is [e]'s occurrences with occurences equal to
      the previous one dropped. Equality is tested with [eq] (defaults to
      structural equality).
      {ul
      {- \[[changes eq e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v]
      and either \[[e]\]{_<t} [= None] or \[[e]\]{_<t} [= Some v'] and
      [eq v v' = false].}
      {- \[[changes eq e]\]{_t} [= None] otherwise.}} */
  let changes: (~eq: ('a, 'a) => bool=?, event('a)) => event('a);
  /** [on c e] is the occurrences of [e] when [c] is [true].
      {ul
      {- \[[on c e]\]{_t} [= Some v]
         if \[[c]\]{_t} [= true] and \[[e]\]{_t} [= Some v].}
      {- \[[on c e]\]{_t} [= None] otherwise.}} */
  let on: (signal(bool), event('a)) => event('a);
  /** @deprecated Use {!on}. */
  let when_: (signal(bool), event('a)) => event('a);
  /** [dismiss c e] is the occurences of [e] except the ones when [c] occurs.
      {ul
      {- \[[dimiss c e]\]{_t} [= Some v]
         if \[[c]\]{_t} [= None] and \[[e]\]{_t} [= Some v].}
      {- \[[dimiss c e]\]{_t} [= None] otherwise.}} */
  let dismiss: (event('b), event('a)) => event('a);
  /** [until c e] is [e]'s occurences until [c] occurs.
      {ul
      {- \[[until c e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
         \[[c]\]{_<=t} [= None]}
      {- \[[until c e]\]{_t} [= None] otherwise.}} */
  let until: (event('a), event('b)) => event('b);
  /** {1:accum Accumulating} */;
  /** [accum ef i] accumulates a value, starting with [i], using [e]'s
      functional occurrences.
      {ul
      {- \[[accum ef i]\]{_t} [= Some (f i)] if \[[ef]\]{_t} [= Some f]
      and \[[ef]\]{_<t} [= None].
      }
      {- \[[accum ef i]\]{_t} [= Some (f acc)] if \[[ef]\]{_t} [= Some f]
      and \[[accum ef i]\]{_<t} [= Some acc].}
      {- \[[accum ef i]\] [= None] otherwise.}} */
  let accum: (event('a => 'a), 'a) => event('a);
  /** [fold f i e] accumulates [e]'s occurrences with [f] starting with [i].
      {ul
      {- \[[fold f i e]\]{_t} [= Some (f i v)] if
      \[[e]\]{_t} [= Some v] and \[[e]\]{_<t} [= None].}
      {- \[[fold f i e]\]{_t} [= Some (f acc v)] if
      \[[e]\]{_t} [= Some v] and \[[fold f i e]\]{_<t} [= Some acc].}
      {- \[[fold f i e]\]{_t} [= None] otherwise.}} */
  let fold: (('a, 'b) => 'a, 'a, event('b)) => event('a);
  /** {1:combine Combining} */;
  /** [select el] is the occurrences of every event in [el].
      If more than one event occurs {{!simultaneity}simultaneously}
      the leftmost is taken and the others are lost.
      {ul
      {- \[[select el]\]{_ t} [=] \[[List.find (fun e -> ]\[[e]\]{_t}
      [<> None) el]\]{_t}.}
      {- \[[select el]\]{_ t} [= None] otherwise.}}  */
  let select: list(event('a)) => event('a);
  /** [merge f a el] merges the {{!simultaneity}simultaneous}
    occurrences of every event in [el] using [f] and the accumulator [a].

    \[[merge f a el]\]{_ t}
    [= List.fold_left f a (List.filter (fun o -> o <> None)
           (List.map] \[\]{_t}[ el))]. */
  let merge: (('a, 'b) => 'a, 'a, list(event('b))) => event('a);
  /** [switch e ee] is [e]'s occurrences until there is an
      occurrence [e'] on [ee], the occurrences of [e'] are then used
      until there is a new occurrence on [ee], etc..
      {ul
      {- \[[switch e ee]\]{_ t} [=] \[[e]\]{_t} if \[[ee]\]{_<=t} [= None].}
      {- \[[switch e ee]\]{_ t} [=] \[[e']\]{_t} if \[[ee]\]{_<=t}
    [= Some e'].}} */
  let switch_: (event('a), event(event('a))) => event('a);
  /** [fix ef] allows to refer to the value an event had an
      infinitesimal amount of time before.

      In [fix ef], [ef] is called with an event [e] that represents
      the event returned by [ef] delayed by an infinitesimal amount of
      time.  If [e', r = ef e] then [r] is returned by [fix] and [e]
      is such that :
      {ul
      {- \[[e]\]{_ t} [=] [None] if t = 0 }
      {- \[[e]\]{_ t} [=] \[[e']\]{_t-dt} otherwise}}

      {b Raises.} [Invalid_argument] if [e'] is directly a delayed event (i.e.
      an event given to a fixing function). */
  let fix: (event('a) => (event('a), 'b)) => 'b;
  /** {1 Lifting}

      Lifting combinators. For a given [n] the semantics is:
      {ul
      {- \[[ln f e1 ... en]\]{_t} [= Some (f v1 ... vn)] if for all
         i : \[[ei]\]{_t} [= Some vi].}
      {- \[[ln f e1 ... en]\]{_t} [= None] otherwise.}} */;
  let l1: ('a => 'b, event('a)) => event('b);
  let l2: (('a, 'b) => 'c, event('a), event('b)) => event('c);
  let l3: (('a, 'b, 'c) => 'd, event('a), event('b), event('c)) => event('d);
  let l4: (('a, 'b, 'c, 'd) => 'e, event('a), event('b), event('c), event('d)) => event('e);
  let l5:
    (('a, 'b, 'c, 'd, 'e) => 'f, event('a), event('b), event('c), event('d), event('e)) =>
    event('f);
  let l6:
    (
      ('a, 'b, 'c, 'd, 'e, 'f) => 'g,
      event('a),
      event('b),
      event('c),
      event('d),
      event('e),
      event('f)
    ) =>
    event('g);
  /** {1 Pervasives support} */;
  /** Events with option occurences. */
  module Option: {
    /** [some e] is [map (fun v -> Some v) e]. */
    let some: event('a) => event(option('a));
    /** [value default e] either silences [None] occurences if [default] is
        unspecified or replaces them by the value of [default] at the occurence
        time.
        {ul
        {- \[[value ~default e]\]{_t}[ = v] if \[[e]\]{_t} [= Some (Some v)].}
        {- \[[value ?default:None e]\]{_t}[ = None] if \[[e]\]{_t} = [None].}
        {- \[[value ?default:(Some s) e]\]{_t}[ = v]
           if \[[e]\]{_t} = [None] and \[[s]\]{_t} [= v].}} */
    let value: (~default: signal('a)=?, event(option('a))) => event('a);
  };
};

/** Signal combinators.

    Consult their {{!sigsem}semantics.}  */
module S: {
  /** {1:prim Primitive and basics} */;
  /** The type for signals of type ['a]. */
  type t('a) = signal('a);
  /** [const v] is always [v], \[[const v]\]{_t} [= v]. */
  let const: 'a => signal('a);
  /** [create i] is a primitive signal [s] set to [i] and a
      [set] function. The function [set] is such that:
      {ul
      {- [set v] sets the signal's value to [v] at the time it is called and
         triggers an {{!steps}update step}.}
      {- [set ~step v] sets the signal's value to [v] at the time it is
         called and updates it dependencies when [step] is
         {{!Step.execute}executed}}
      {- [set ~step v] raises [Invalid_argument] if it was previously
         called with a step and this step has not executed yet or if
         the given [step] was already executed.}}
      {b Warning.} [set] must not be executed inside an update step. */
  let create: (~eq: ('a, 'a) => bool=?, 'a) => (signal('a), (~step: step=?, 'a) => unit);
  /** [value s] is [s]'s current value.

      {b Warning.} If executed in an {{!steps}update
      step} may return a non up-to-date value or raise [Failure] if
      the signal is not yet initialized. */
  let value: signal('a) => 'a;
  /** [retain s c] keeps a reference to the closure [c] in [s] and
      returns the previously retained value. [c] will {e never} be
      invoked.

      {b Raises.} [Invalid_argument] on constant signals. */
  let retain: (signal('a), unit => unit) => [ | `R(unit => unit)];
  /**
  /**/**/
  let eq_fun: signal('a) => option(('a, 'a) => bool);
  /** [stop s], stops updating [s]. It conceptually becomes {!const}
      with the signal's last value and cannot be restarted. Allows to
      disable {{!sideeffects}effectful} signals.

      The [strong] argument should only be used on platforms
      where weak arrays have a strong semantics (i.e. JavaScript).
      See {{!strongstop}details}.

      {b Note.} If executed in an update step the signal may
      still update in the step. */
  let stop: (~strong: bool=?, signal('a)) => unit;
  /** [equal s s'] is [true] iff [s] and [s'] are equal. If both
      signals are {!const}ant [eq] is used between their value
      (defauts to structural equality). If both signals are not
      {!const}ant, physical equality is used.*/
  let equal: (~eq: ('a, 'a) => bool=?, signal('a), signal('a)) => bool;
  /** [trace iff tr s] is [s] except [tr] is invoked with [s]'s
      current value and on [s] changes when [iff] is [true] (defaults
      to [S.const true]). For all t where \[[s]\]{_t} [= v] and (t = 0
      or (\[[s]\]{_t-dt}[= v'] and [eq v v' = false])) and
      \[[iff]\]{_t} = [true], [tr] is invoked with [v]. */
  let trace: (~iff: t(bool)=?, 'a => unit, signal('a)) => signal('a);
  /** {1 From events} */;
  /** [hold i e] has the value of [e]'s last occurrence or [i] if there
      wasn't any.
      {ul
      {- \[[hold i e]\]{_t} [= i] if \[[e]\]{_<=t} [= None]}
      {- \[[hold i e]\]{_t} [= v] if \[[e]\]{_<=t} [= Some v]}} */
  let hold: (~eq: ('a, 'a) => bool=?, 'a, event('a)) => signal('a);
  /** {1:tr Transforming and filtering} */;
  /** [app sf s] holds the value of [sf] applied
      to the value of [s], \[[app sf s]\]{_t}
      [=] \[[sf]\]{_t} \[[s]\]{_t}. */
  let app: (~eq: ('b, 'b) => bool=?, signal('a => 'b), signal('a)) => signal('b);
  /** [map f s] is [s] transformed by [f], \[[map f s]\]{_t} = [f] \[[s]\]{_t}.
   */
  let map: (~eq: ('b, 'b) => bool=?, 'a => 'b, signal('a)) => signal('b);
  /** [filter f i s] is [s]'s values that satisfy [p]. If a value does not
      satisfy [p] it holds the last value that was satisfied or [i] if
      there is none.
      {ul
      {- \[[filter p s]\]{_t} [=] \[[s]\]{_t} if [p] \[[s]\]{_t}[ = true].}
      {- \[[filter p s]\]{_t} [=] \[[s]\]{_t'} if [p] \[[s]\]{_t}[ = false]
          and t' is the greatest t' < t with [p] \[[s]\]{_t'}[ = true].}
      {- \[[filter p e]\]{_t} [= i] otherwise.}} */
  let filter: (~eq: ('a, 'a) => bool=?, 'a => bool, 'a, signal('a)) => signal('a);
  /** [fmap fm i s] is [s] filtered and mapped by [fm].
      {ul
      {- \[[fmap fm i s]\]{_t} [=] v if [fm] \[[s]\]{_t}[ = Some v].}
      {- \[[fmap fm i s]\]{_t} [=] \[[fmap fm i s]\]{_t'} if [fm]
         \[[s]\]{_t} [= None] and t' is the greatest t' < t with [fm]
         \[[s]\]{_t'} [<> None].}
      {- \[[fmap fm i s]\]{_t} [= i] otherwise.}} */
  let fmap: (~eq: ('b, 'b) => bool=?, 'a => option('b), 'b, signal('a)) => signal('b);
  /** [diff f s] is an event with occurrences whenever [s] changes from
      [v'] to [v] and [eq v v'] is [false] ([eq] is the signal's equality
      function).  The value of the occurrence is [f v v'].
      {ul
      {- \[[diff f s]\]{_t} [= Some d]
      if \[[s]\]{_t} [= v] and \[[s]\]{_t-dt} [= v'] and [eq v v' = false]
      and [f v v' = d].}
      {- \[[diff f s]\]{_t} [= None] otherwise.}} */
  let diff: (('a, 'a) => 'b, signal('a)) => event('b);
  /** [changes s] is [diff (fun v _ -> v) s]. */
  let changes: signal('a) => event('a);
  /** [sample f e s] samples [s] at [e]'s occurrences.
      {ul
      {- \[[sample f e s]\]{_t} [= Some (f ev sv)] if \[[e]\]{_t} [= Some ev]
         and  \[[s]\]{_t} [= sv].}
      {- \[[sample e s]\]{_t} [= None] otherwise.}} */
  let sample: (('b, 'a) => 'c, event('b), signal('a)) => event('c);
  /** [on c i s] is the signal [s] whenever [c] is [true].
      When [c] is [false] it holds the last value [s] had when
      [c] was the last time [true] or [i] if it never was.
      {ul
      {- \[[on c i s]\]{_t} [=] \[[s]\]{_t} if \[[c]\]{_t} [= true]}
      {- \[[on c i s]\]{_t} [=] \[[s]\]{_t'} if \[[c]\]{_t} [= false]
         where t' is the greatest t' < t with \[[c]\]{_t'} [= true].}
      {- \[[on c i s]\]{_t} [=] [i] otherwise.}} */
  let on: (~eq: ('a, 'a) => bool=?, signal(bool), 'a, signal('a)) => signal('a);
  /** @deprecated Use {!on}. */
  let when_: (~eq: ('a, 'a) => bool=?, signal(bool), 'a, signal('a)) => signal('a);
  /** [dismiss c i s] is the signal [s] except changes when [c] occurs
      are ignored. If [c] occurs initially [i] is used.
      {ul
      {- \[[dismiss c i s]\]{_t} [=] \[[s]\]{_t'}
         where t' is the greatest t' <= t with \[[c]\]{_t'} [= None] and
         \[[s]\]{_t'-dt} [<>] \[[s]\]{_t'}}
       {- \[[dismiss_ c i s]\]{_0} [=] [v] where [v = i] if
    \[[c]\]{_0} [= Some _] and [v =] \[[s]\]{_0} otherwise.}} */
  let dismiss: (~eq: ('a, 'a) => bool=?, event('b), 'a, signal('a)) => signal('a);
  /** {1:acc Accumulating} */;
  /** [accum e i] is [S.hold i (]{!E.accum}[ e i)]. */
  let accum: (~eq: ('a, 'a) => bool=?, event('a => 'a), 'a) => signal('a);
  /** [fold f i e] is [S.hold i (]{!E.fold}[ f i e)]. */
  let fold: (~eq: ('a, 'a) => bool=?, ('a, 'b) => 'a, 'a, event('b)) => signal('a);
  /** {1:combine Combining} */;
  /** [merge f a sl] merges the value of every signal in [sl]
    using [f] and the accumulator [a].

    \[[merge f a sl]\]{_ t}
    [= List.fold_left f a (List.map] \[\]{_t}[ sl)]. */
  let merge: (~eq: ('a, 'a) => bool=?, ('a, 'b) => 'a, 'a, list(signal('b))) => signal('a);
  /** [switch ss] is the inner signal of [ss].
      {ul
      {- \[[switch ss]\]{_ t} [=] \[\[[ss]\]{_t}\]{_t}.}} */
  let switch_: (~eq: ('a, 'a) => bool=?, signal(signal('a))) => signal('a);
  /** [bind s sf] is [switch (map ~eq:( == ) sf s)]. */
  let bind: (~eq: ('b, 'b) => bool=?, signal('a), 'a => signal('b)) => signal('b);
  /** [fix i sf] allow to refer to the value a signal had an
      infinitesimal amount of time before.

      In [fix sf], [sf] is called with a signal [s] that represents
      the signal returned by [sf] delayed by an infinitesimal amount
      time. If [s', r = sf s] then [r] is returned by [fix] and [s]
      is such that :
      {ul
      {- \[[s]\]{_ t} [=] [i] for t = 0. }
      {- \[[s]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}}

      [eq] is the equality used by [s].

      {b Raises.} [Invalid_argument] if [s'] is directly a delayed signal (i.e.
      a signal given to a fixing function).

      {b Note.} Regarding values depending on the result [r] of
      [s', r = sf s] the following two cases need to be distinguished :
      {ul
      {- After [sf s] is applied, [s'] does not depend on
         a value that is in a step and [s] has no dependents in a step (e.g
         in the simple case where [fix] is applied outside a step).

         In that case if the initial value of [s'] differs from [i],
         [s] and its dependents need to be updated and a special
         update step will be triggered for this. Values
         depending on the result [r] will be created only after this
         special update step has finished (e.g. they won't see
         the [i] of [s] if [r = s]).}
      {- Otherwise, values depending on [r] will be created in the same
         step as [s] and [s'] (e.g. they will see the [i] of [s] if [r = s]).}}
   */
  let fix: (~eq: ('a, 'a) => bool=?, 'a, signal('a) => (signal('a), 'b)) => 'b;
  /** {1:lifting Lifting}

     Lifting combinators. For a given [n] the semantics is :

     \[[ln f a1] ... [an]\]{_t} = f \[[a1]\]{_t} ... \[[an]\]{_t} */;
  let l1: (~eq: ('b, 'b) => bool=?, 'a => 'b, signal('a)) => signal('b);
  let l2: (~eq: ('c, 'c) => bool=?, ('a, 'b) => 'c, signal('a), signal('b)) => signal('c);
  let l3:
    (~eq: ('d, 'd) => bool=?, ('a, 'b, 'c) => 'd, signal('a), signal('b), signal('c)) => signal('d);
  let l4:
    (
      ~eq: ('e, 'e) => bool=?,
      ('a, 'b, 'c, 'd) => 'e,
      signal('a),
      signal('b),
      signal('c),
      signal('d)
    ) =>
    signal('e);
  let l5:
    (
      ~eq: ('f, 'f) => bool=?,
      ('a, 'b, 'c, 'd, 'e) => 'f,
      signal('a),
      signal('b),
      signal('c),
      signal('d),
      signal('e)
    ) =>
    signal('f);
  let l6:
    (
      ~eq: ('g, 'g) => bool=?,
      ('a, 'b, 'c, 'd, 'e, 'f) => 'g,
      signal('a),
      signal('b),
      signal('c),
      signal('d),
      signal('e),
      signal('f)
    ) =>
    signal('g);
  /** The following modules lift some of [Pervasives] functions and
      operators. */;
  module Bool: {
    let zero: signal(bool);
    let one: signal(bool);
    let (!): signal(bool) => signal(bool);
    let (&&): (signal(bool), signal(bool)) => signal(bool);
    let (||): (signal(bool), signal(bool)) => signal(bool);
    /** [edge s] is [changes s]. */
    let edge: signal(bool) => event(bool);
    /** [rise s] is [E.fmap (fun b -> if b then Some () else None) (edge s)].*/
    let rise: signal(bool) => event(unit);
    /** [fall s] is [E.fmap (fun b -> if b then None else Some ()) (edge s)].*/
    let fall: signal(bool) => event(unit);
    /** [flip b e] is a signal whose boolean value flips each time
        [e] occurs. [b] is the initial signal value.
        {ul
        {- \[[flip b e]\]{_0} [= not b] if \[[e]\]{_0} [= Some _]}
        {- \[[flip b e]\]{_t} [= b] if \[[e]\]{_<=t} [= None]}
        {- \[[flip b e]\]{_t} [=] [not] \[[flip b e]\]{_t-dt}
           if \[[e]\]{_t} [= Some _]}}
*/
    let flip: (bool, event('a)) => signal(bool);
  };
  module Int: {
    let zero: signal(int);
    let one: signal(int);
    let minus_one: signal(int);
    let (~-): signal(int) => signal(int);
    let succ: signal(int) => signal(int);
    let pred: signal(int) => signal(int);
    let (+): (signal(int), signal(int)) => signal(int);
    let (-): (signal(int), signal(int)) => signal(int);
    let ( * ): (signal(int), signal(int)) => signal(int);
    let (mod): (signal(int), signal(int)) => signal(int);
    let abs: signal(int) => signal(int);
    let max_int: signal(int);
    let min_int: signal(int);
    let (land): (signal(int), signal(int)) => signal(int);
    let (lor): (signal(int), signal(int)) => signal(int);
    let (lxor): (signal(int), signal(int)) => signal(int);
    let lnot: signal(int) => signal(int);
    let (lsl): (signal(int), signal(int)) => signal(int);
    let (lsr): (signal(int), signal(int)) => signal(int);
    let (asr): (signal(int), signal(int)) => signal(int);
  };
  module Float: {
    let zero: signal(float);
    let one: signal(float);
    let minus_one: signal(float);
    let (~-.): signal(float) => signal(float);
    let (+.): (signal(float), signal(float)) => signal(float);
    let (-.): (signal(float), signal(float)) => signal(float);
    let ( *. ): (signal(float), signal(float)) => signal(float);
    let (/.): (signal(float), signal(float)) => signal(float);
    let ( ** ): (signal(float), signal(float)) => signal(float);
    let sqrt: signal(float) => signal(float);
    let exp: signal(float) => signal(float);
    let log: signal(float) => signal(float);
    let log10: signal(float) => signal(float);
    let cos: signal(float) => signal(float);
    let sin: signal(float) => signal(float);
    let tan: signal(float) => signal(float);
    let acos: signal(float) => signal(float);
    let asin: signal(float) => signal(float);
    let atan: signal(float) => signal(float);
    let atan2: (signal(float), signal(float)) => signal(float);
    let cosh: signal(float) => signal(float);
    let sinh: signal(float) => signal(float);
    let tanh: signal(float) => signal(float);
    let ceil: signal(float) => signal(float);
    let floor: signal(float) => signal(float);
    let abs_float: signal(float) => signal(float);
    let mod_float: (signal(float), signal(float)) => signal(float);
    let frexp: signal(float) => signal((float, int));
    let ldexp: (signal(float), signal(int)) => signal(float);
    let modf: signal(float) => signal((float, float));
    let float: signal(int) => signal(float);
    let float_of_int: signal(int) => signal(float);
    let truncate: signal(float) => signal(int);
    let int_of_float: signal(float) => signal(int);
    let infinity: signal(float);
    let neg_infinity: signal(float);
    let nan: signal(float);
    let max_float: signal(float);
    let min_float: signal(float);
    let epsilon_float: signal(float);
    let classify_float: signal(float) => signal(fpclass);
  };
  module Pair: {
    let pair: (~eq: (('a, 'b), ('a, 'b)) => bool=?, signal('a), signal('b)) => signal(('a, 'b));
    let fst: (~eq: ('a, 'a) => bool=?, signal(('a, 'b))) => signal('a);
    let snd: (~eq: ('a, 'a) => bool=?, signal(('b, 'a))) => signal('a);
  };
  module Option: {
    /** [none] is [S.const None]. */
    let none: signal(option('a));
    /** [some s] is [S.map ~eq (fun v -> Some v) None], where [eq] uses
        [s]'s equality function to test the [Some v]'s equalities. */
    let some: signal('a) => signal(option('a));
    /** [value default s] is [s] with only its [Some v] values.
        Whenever [s] is [None], if [default] is [`Always dv] then
        the current value of [dv] is used instead. If [default]
        is [`Init dv] the current value of [dv] is only used
        if there's no value at creation time, otherwise the last
        [Some v] value of [s] is used.
        {ul
        {- \[[value ~default s]\]{_t} [= v] if \[[s]\]{_t} [= Some v]}
        {- \[[value ~default:(`Always d) s]\]{_t} [=] \[[d]\]{_t}
          if \[[s]\]{_t} [= None]}
        {- \[[value ~default:(`Init d) s]\]{_0} [=] \[[d]\]{_0}
          if \[[s]\]{_0} [= None]}
        {- \[[value ~default:(`Init d) s]\]{_t} [=]
           \[[value ~default:(`Init d) s]\]{_t'}
          if \[[s]\]{_t} [= None] and t' is the greatest t' < t
          with \[[s]\]{_t'} [<> None] or 0 if there is no such [t'].}} */
    let value:
      (
        ~eq: ('a, 'a) => bool=?,
        ~default: [ | `Init(signal('a)) | `Always(signal('a))],
        signal(option('a))
      ) =>
      signal('a);
  };
  module Compare: {
    let (==): (signal('a), signal('a)) => signal(bool);
    let (!=): (signal('a), signal('a)) => signal(bool);
    let (<): (signal('a), signal('a)) => signal(bool);
    let (>): (signal('a), signal('a)) => signal(bool);
    let (<=): (signal('a), signal('a)) => signal(bool);
    let (>=): (signal('a), signal('a)) => signal(bool);
    let compare: (signal('a), signal('a)) => signal(int);
    let (===): (signal('a), signal('a)) => signal(bool);
    let (!==): (signal('a), signal('a)) => signal(bool);
  };
  /** {1:special Combinator specialization}

      Given an equality function [equal] and a type [t], the functor
      {!Make} automatically applies the [eq] parameter of the combinators.
      The outcome is combinators whose {e results} are signals with
      values in [t].

      Basic types are already specialized in the module {!Special}, open
      this module to use them.  */;
  /** Input signature of {!Make} */
  module type EqType = {type t('a); let equal: (t('a), t('a)) => bool;};
  /** Output signature of {!Make} */
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
  /** Functor specializing the combinators for the given signal value type */
  module Make: (Eq: EqType) => S with type v('a) = Eq.t('a);
  /** Specialization for booleans, integers and floats.

      Open this module to use it. */
  module Special: {
    /** Specialization for booleans. */
    module Sb: S with type v('a) = bool;
    /** Specialization for integers. */
    module Si: S with type v('a) = int;
    /** Specialization for floats. */
    module Sf: S with type v('a) = float;
  };
};

/** Update steps.

    Update functions returned by {!S.create} and {!E.create}
    implicitely create and execute update steps when used without
    specifying their [step] argument.

    Using explicit {!step} values with these functions gives more control on
    the time when the update step is perfomed and allows to perform
    simultaneous {{!primitives}primitive} signal updates and event
    occurences. See also the documentation about {{!steps}update steps} and
    {{!simultaneity}simultaneous events}. */
module Step: {
  /** {1 Steps} */;
  /** The type for update steps. */
  type t = step;
  /** [create ()] is a new update step. */
  let create: unit => step;
  /** [execute step] executes the update step.

      @raise Invalid_argument if [step] was already executed. */
  let execute: step => unit;
};

/** {1:sem Semantics}

    The following notations are used to give precise meaning to the
    combinators. It is important to note that in these semantic
    descriptions the origin of time t = 0 is {e always} fixed at
    the time at which the combinator creates the event or the signal and
    the semantics of the dependents is evaluated relative to this timeline.

    We use dt to denote an infinitesimal amount of time.
    {2:evsem Events}

    An event is a value with discrete occurrences over time.

    The semantic function \[\] [: 'a event -> time -> 'a option] gives
    meaning to an event [e] by mapping it to a function of time
    \[[e]\] returning [Some v] whenever the event occurs with value
    [v] and [None] otherwise. We write \[[e]\]{_t} the evaluation of
    this {e semantic} function at time t.

    As a shortcut notation we also define \[\]{_<t} [: 'a event -> 'a option]
    (resp. \[\]{_<=t}) to denote the last occurrence, if any, of an
    event before (resp. before or at) [t]. More precisely :
    {ul
    {- \[[e]\]{_<t} [=] \[[e]\]{_t'} with t' the greatest t' < t
      (resp. [<=]) such that
       \[[e]\]{_t'} [<> None].}
    {- \[[e]\]{_<t} [= None] if there is no such t'.}}

    {2:sigsem Signals}

    A signal is a value that varies continuously over time. In
    contrast to {{!evsem}events} which occur at specific point
    in time, a signal has a value at every point in time.

    The semantic function \[\] [: 'a signal -> time -> 'a] gives
    meaning to a signal [s] by mapping it to a function of time
    \[[s]\] that returns its value at a given time. We write \[[s]\]{_t}
    the evaluation of this {e semantic} function at time t.
    {3:sigeq Equality}

    Most signal combinators have an optional [eq] parameter that
    defaults to structural equality. [eq] specifies the equality
    function used to detect changes in the value of the resulting
    signal. This function is needed for the efficient update of
    signals and to deal correctly with signals that perform
    {{!sideeffects}side effects}.

    Given an equality function on a type the combinators can be automatically
    {{!S.special}specialized} via a functor.

    {3:sigcont Continuity}

    Ultimately signal updates depend on
    {{!primitives}primitives} updates. Thus a signal can
    only approximate a real continuous signal. The accuracy of the
    approximation depends on the variation rate of the real signal and
    the primitive's update frequency.

    {1:basics Basics}

    {2:primitives Primitive events and signals}

    React doesn't define primitive events and signals, they must be
    created and updated by the client.

    Primitive events are created with {!E.create}. This function
    returns a new event and an update function that generates an
    occurrence for the event at the time it is called. The following
    code creates a primitive integer event [x] and generates three
    occurrences with value [1], [2], [3]. Those occurrences are printed
    on stdout by the effectful event [pr_x].  {[open React;;

let x, send_x = E.create ()
let pr_x = E.map print_int x
let () = List.iter send_x [1; 2; 3]]}
    Primitive signals are created with {!S.create}. This function
    returns a new signal and an update function that sets the signal's value
    at the time it is called. The following code creates an
    integer signal [x] initially set to [1] and updates it three time with
    values [2], [2], [3]. The signal's values are printed on stdout by the
    effectful signal [pr_x]. Note that only updates that change
    the signal's value are printed, hence the program prints [123], not [1223].
    See the discussion on
    {{!sideeffects}side effects} for more details.

{[open React;;

let x, set_x = S.create 1
let pr_x = S.map print_int x
let () = List.iter set_x [2; 2; 3]]}
    The {{!clock}clock} example shows how a realtime time
    flow can be defined.

   {2:steps Update steps}

   The {!E.create} and {!S.create} functions return update functions
   used to generate primitive event occurences and set the value of
   primitive signals. Upon invocation as in the preceding section
   these functions immediatly create and invoke an update step.
   The {e update step} automatically updates events and signals that
   transitively depend on the updated primitive. The dependents of a
   signal are updated iff the signal's value changed according to its
   {{!sigeq}equality function}.

   The update functions have an optional [step] argument. If they are
   given a concrete [step] value created with {!Step.create}, then it
   updates the event or signal but doesn't update its dependencies. It
   will only do so whenever [step] is executed with
   {!Step.execute}. This allows to make primitive event occurences and
   signal changes simultaneous. See next section for an example.

    {2:simultaneity Simultaneous events}

    {{!steps}Update steps} are made under a
    {{:http://dx.doi.org/10.1016/0167-6423(92)90005-V}synchrony hypothesis} :
    the update step takes no time, it is instantenous. Two event occurrences
    are {e simultaneous} if they occur in the same update step.

    In the code below [w], [x] and [y] will always have simultaneous
    occurrences. They {e may} have simulatenous occurences with [z]
    if [send_w] and [send_z] are used with the same update step.

{[let w, send_w = E.create ()
let x = E.map succ w
let y = E.map succ x
let z, send_z = E.create ()

let () =
  let () = send_w 3 (* w x y occur simultaneously, z doesn't occur *) in
  let step = Step.create () in
  send_w ~step 3;
  send_z ~step 4;
  Step.execute step (* w x z y occur simultaneously *)
]}

    {2:update The update step and thread safety}

    {{!primitives}Primitives} are the only mean to drive the reactive
    system and they are entirely under the control of the client. When
    the client invokes a primitive's update function without the
    [step] argument or when it invokes {!Step.execute} on a [step]
    value, React performs an update step.

    To ensure correctness in the presence of threads, update steps
    must be executed in a critical section. Let uset([p]) be the set
    of events and signals that need to be updated whenever the
    primitive [p] is updated.  Updating two primitives [p] and [p']
    concurrently is only allowed if uset([p]) and uset([p']) are
    disjoint. Otherwise the updates must be properly serialized.

    Below, concurrent, updates to [x] and [y] must be serialized (or
    performed on the same step if it makes sense semantically), but z
    can be updated concurently to both [x] and [y].

{[open React;;

let x, set_x = S.create 0
let y, send_y = E.create ()
let z, set_z = S.create 0
let max_xy = S.l2 (fun x y -> if x > y then x else y) x (S.hold 0 y)
let succ_z = S.map succ z]}

    {2:sideeffects Side effects}

    Effectful events and signals perform their side effect
    exactly {e once} in each {{!steps}update step} in which there
    is an update of at least one of the event or signal it depends on.

    Remember that a signal updates in a step iff its
    {{!sigeq}equality function} determined that the signal
    value changed. Signal initialization is unconditionally considered as
    an update.

    It is important to keep references on effectful events and
    signals. Otherwise they may be reclaimed by the garbage collector.
    The following program prints only a [1].
{[let x, set_x = S.create 1
let () = ignore (S.map print_int x)
let () = Gc.full_major (); List.iter set_x [2; 2; 3]]}
    {2:lifting Lifting}

    Lifting transforms a regular function to make it act on signals.
    The combinators
    {!S.const} and {!S.app} allow to lift functions of arbitrary arity n,
    but this involves the inefficient creation of n-1 intermediary
    closure signals. The fixed arity {{!S.lifting}lifting
    functions} are more efficient. For example :
{[let f x y = x mod y
let fl x y = S.app (S.app ~eq:(==) (S.const f) x) y (* inefficient *)
let fl' x y = S.l2 f x y                            (* efficient *)
]}
    Besides, some of [Pervasives]'s functions and operators are
    already lifted and availables in submodules of {!S}. They can be
    be opened in specific scopes. For example if you are dealing with
    float signals you can open {!S.Float}.
{[open React
open React.S.Float

let f t = sqrt t *. sin t (* f is defined on float signals *)
...
open Pervasives (* back to pervasives floats *)
]}
   If you are using OCaml 3.12 or later you can also use the [let open]
   construct
{[let open React.S.Float in
let f t = sqrt t *. sin t in (* f is defined on float signals *)
...
]}

  {2:recursion Mutual and self reference}

  Mutual and self reference among time varying values occurs naturally
  in programs. However a mutually recursive definition of two signals
  in which both need the value of the other at time t to define
  their value at time t has no least fixed point. To break this
  tight loop one signal must depend on the value the other had at time
  t-dt where dt is an infinitesimal delay.

  The fixed point combinators {!E.fix} and {!S.fix} allow to refer to
  the value an event or signal had an infinitesimal amount of time
  before. These fixed point combinators act on a function [f] that takes
  as argument the infinitesimally delayed event or signal that [f]
  itself returns.

  In the example below [history s] returns a signal whose value
  is the history of [s] as a list.
{[let history ?(eq = ( = )) s =
  let push v = function
    | [] -> [ v ]
    | v' :: _ as l when eq v v' -> l
    | l -> v :: l
  in
  let define h =
    let h' = S.l2 push s h in
    h', h'
  in
  S.fix [] define]}
  When a program has infinitesimally delayed values a
  {{!primitives}primitive} may trigger more than one update
  step. For example if a signal [s] is infinitesimally delayed, then
  its update in a step [c] will trigger a new step [c'] at the end
  of the step in which the delayed signal of [s] will have the value
  [s] had in [c]. This means that the recursion occuring between a
  signal (or event) and its infinitesimally delayed counterpart must
  be well-founded otherwise this may trigger an infinite number
  of update steps, like in the following examples.
{[let start, send_start = E.create ()
let diverge =
  let define e =
    let e' = E.select [e; start] in
    e', e'
  in
  E.fix define

let () = send_start ()        (* diverges *)

let diverge =                 (* diverges *)
  let define s =
    let s' = S.Int.succ s in
    s', s'
  in
  S.fix 0 define]}
  For technical reasons, delayed events and signals (those given to
  fixing functions) are not allowed to directly depend on each
  other. Fixed point combinators will raise [Invalid_argument] if
  such dependencies are created. This limitation can be
  circumvented by mapping these values with the identity.

  {2:strongstop Strong stops}

  Strong stops should only be used on platforms where weak arrays have
  a strong semantics (i.e. JavaScript). You can safely ignore that
  section and the [strong] argument of {!E.stop} and {!S.stop}
  if that's not the case.

  Whenever {!E.stop} and {!S.stop} is called with [~strong:true] on a
  reactive value [v], it is first stopped and then it walks over the
  list [prods] of events and signals that it depends on and
  unregisters itself from these ones as a dependent (something that is
  normally automatically done when [v] is garbage collected since
  dependents are stored in a weak array). Then for each element of
  [prod] that has no dependents anymore and is not a primitive it
  stops them aswell and recursively.

  A stop call with [~strong:true] is more involved. But it allows to
  prevent memory leaks when used judiciously on the leaves of the
  reactive system that are no longer used.

  {b Warning.} It should be noted that if direct references are kept
  on an intermediate event or signal of the reactive system it may
  suddenly stop updating if all its dependents were strongly stopped. In
  the example below, [e1] will {e never} occur:
{[let e, e_send = E.create ()
let e1 = E.map (fun x -> x + 1) e (* never occurs *)
let () =
  let e2 = E.map (fun x -> x + 1) e1 in
  E.stop ~strong:true e2
]}
  This can be side stepped by making an artificial dependency to keep
  the reference:
{[let e, e_send = E.create ()
let e1 = E.map (fun x -> x + 1) e (* may still occur *)
let e1_ref = E.map (fun x -> x) e1
let () =
  let e2 = E.map (fun x -> x + 1) e1 in
  E.stop ~strong:true e2
]}

  {1:ex Examples}

  {2:clock Clock}

  The following program defines a primitive event [seconds] holding
  the UNIX time and occuring on every second. An effectful event
  converts these occurences to local time and prints them on stdout
  along with an
  {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}ANSI
  escape sequence} to control the cursor position.
{[let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "[8D%02d:%02d:%02d%!"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

open React;;

let seconds, run =
  let e, send = E.create () in
  let run () =
    while true do send (Unix.gettimeofday ()); Unix.sleep 1 done
  in
  e, run

let printer = E.map pr_time seconds

let () = run ()]}
*/;
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