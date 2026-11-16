module Fiber = Ocaml_lsp_fiber
module Feature_id = Ocaml_lsp_uri.Feature_id

module Metadata_for_query = struct
  type t =
    { feature_id : Feature_id.t
    ; workspace : File_path.Absolute.t
    ; file_contents : string
    ; file_path : File_path.t
    ; client_info : Lsp.Types.Client_info.t option
    }
end

let no_remote_lsp ~disable_remote_lsp =
  disable_remote_lsp ();
  Fiber.return None
;;

let go_to_query ~log_info:_ ~stage:_ ~disable_remote_lsp ~metadata:_ _ _ =
  no_remote_lsp ~disable_remote_lsp
;;

let hover_query ~log_info:_ ~stage:_ ~disable_remote_lsp ~metadata:_ _ =
  no_remote_lsp ~disable_remote_lsp
;;

let references_query ~log_info:_ ~stage:_ ~disable_remote_lsp ~metadata:_ _ _ =
  no_remote_lsp ~disable_remote_lsp
;;
