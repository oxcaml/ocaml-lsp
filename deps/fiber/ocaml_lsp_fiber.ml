include Core
module Cancel = Cancel
module Pool = Pool
module Stream = Stream
module Mvar = Mvar
module Svar = Svar
module Throttle = Throttle
module Mutex = Mutex
module Scheduler = Scheduler
module Lazy = Lazy

module Let_syntax = struct
  let return = return
  let bind = bind
  let map = map
  let both = both

  module Let_syntax = struct
    let return = return
    let bind = bind
    let map = map
    let both = both
  end
end

let run =
  let rec loop ~iter (s : _ Scheduler.step) =
    match s with
    | Done a -> a
    | Stalled w -> loop ~iter (Scheduler.advance w (iter ()))
  in
  fun t ~iter -> loop ~iter (Scheduler.start t)
;;

type fill = Scheduler.fill = Fill : 'a ivar * 'a -> fill

module Fiber_async = struct
  (* When a deferred fills the ivar created by [fiber_of_deferred], the fiber scheduler
     must also receive that fill so it can resume the jobs waiting on the ivar. The
     scheduler created by [deferred_of_fiber] puts a callback for this purpose in
     fiber-local storage. *)
  let key = Var.create ()

  let fiber_of_deferred deferred =
    let ivar = Ivar.create () in
    bind (Var.get key) ~f:(function
      | None -> failwith "[fiber_of_deferred] invoked outside of [deferred_of_fiber]"
      | Some fill ->
        Async.upon deferred (fun value -> fill (Fill (ivar, value)));
        Ivar.read ivar)
  ;;

  let deferred_of_fiber fiber () =
    let reader, writer = Async.Pipe.create () in
    let fiber =
      Var.set key (Async.Pipe.write_without_pushback writer) (fun () -> fiber)
    in
    let rec loop = function
      | Scheduler.Done x -> Async.return x
      | Scheduler.Stalled stalled ->
        Async.Deferred.bind (Async.Pipe.read_exn reader) ~f:(fun fill ->
          loop (Scheduler.advance stalled [ fill ]))
    in
    loop (Scheduler.start fiber)
  ;;
end

module Expert = struct
  type nonrec 'a k = 'a k

  let suspend f k = suspend f k
  let resume a x k = resume a x k
end
