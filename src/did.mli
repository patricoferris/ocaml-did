(** {1 Decentralized Identifiers}
    
    This library provides OCaml functions for parsing, manipulating and building
    DIDs. Read more about DIDs {{: https://www.w3.org/TR/did-1.0/} in the W3C spec}.
*)

type t
(** A decentralized identifier (DID) *)

val of_string_exn : string -> t
(** [of_string_exn s] attempts to extract a DID from [s]. [Invalid_argument]
    may be raised if the string is deemed to be malformed. *)

val to_uri : t -> Uri.t
(** Convert the DID to a URI *)

val method' : t -> string
(** The DID method *)

val identifier : t -> string
(** The DID method-specific identifier *)

val path : t -> string option
(** Any path component of the DID *)

val query : t -> (string * string list) list
(** Any query parameters of the DID *)

val fragment : t -> string option
(** Any fragment of the DID *)

(** {2 Common DID Parameters} *)

val service : t -> string option
(** A service ID *)

val relative_ref : t -> string option
(** A relative URI reference that identifies a resource at a service endpoint *)

val version_id : t -> string option
(** Identifies a specific version of a {! Document.t} to be resolve *)

val version_time : t -> string option
(** Identifies a certain version timestamp of a {! Document.t} to be resolved *)

val hashlink : t -> string option
(** A resource hash of the {! Document.t} to add integrity protection *)

(** {2 DID Documents}
    
    A DID document is what a DID {e resolves} to. The specification {{:https://www.w3.org/TR/did-1.0/#core-properties}
    outlines what a DID document is}. It also specifies the {{: https://www.w3.org/TR/did-1.0/#resolution} resolution process}. *)

module Document : sig
  type did := t

  type t
  (** A DID document *)

  val id : t -> did
  (** The DID identifier for a given document. *)

  val aka : t -> string list
  (** A potentially empty list of ["alsoKnownAs"] aliases for the document. *)

  val to_json : t -> Yojson.Basic.t
  (** [to_json t] is the JSON-LD representation of the document [t]. *)

  val of_json_exn : Yojson.Basic.t -> t
  (** [of_json_exn json] tries to read a document from its JSON-LD representation. *)

  (** {3 Verification Methods} *)

  type verification_method
  (** A {{: https://www.w3.org/TR/did-1.0/#dfn-verification-method} verification method}. *)

  (** {3 Services} *)

  type service
  (** A {{: https://www.w3.org/TR/did-1.0/#dfn-service} service}. *)
end
