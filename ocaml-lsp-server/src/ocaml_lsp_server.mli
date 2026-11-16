open Async

val run
  :  Lsp.Cli.Channel.t
  -> dot_merlin:string option
  -> stage:Stage.t
  -> worker_name:string option
  -> username:string option
  -> unit Deferred.t

module Config_data = Config_data
module Diagnostics = Diagnostics
module Dune_subscriptions = Dune_subscriptions
module Version = Ocaml_lsp_version
module Position = Position
module Doc_to_md = Doc_to_md
module Compl = Compl
module Testing = Testing
module Custom_request = Custom_request
module References_req = References_req
module Stage = Stage

module For_testing : sig
  val relative_dir_of_uri : dune_root:string option -> Lsp.Uri.t -> string option

  val dune_open_dir_impl
    :  Dune_subscriptions.t
    -> [ `Register | `Unregister ]
    -> Lsp.Uri.t
    -> dune_root:string option
    -> unit Ocaml_lsp_fiber.t
end
