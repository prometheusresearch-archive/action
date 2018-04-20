(**
 * Implementation of the DATABASE interface which queries values against an
 * in-memory JSON structure.
 *
 * Physical Data Layout
 * ====================
 *
 * The root value is an object with a set of key-value collections for each
 * entity present in a database, indexed by its "id" key:
 *
 *     {
 *       "region": {
 *         "ASIA": { ... },
 *         "AMERICA": { ... },
 *         ...
 *       },
 *       "nation": {
 *         "CHINA": { ... },
 *         "US": { ... },
 *         ...
 *       },
 *       ...
 *     }
 *
 * Then relationships are represented via references, for example a region can
 * include a list of nations:
 *
 *     {
 *       "id": "ASIA",
 *       "nation": [
 *         {"$ref": {"entity": "nation", "id": "CHINA"}},
 *         ...
 *       ],
 *       ...
 *     }
 *
 * Query Semantics Regarding References
 * ====================================
 *
 * References are not expanded always and are filtered out if not explicitly
 * queried (via navigation or select + navigation).
 *
 * That means the query "region" won't include the "nation" key
 * but "region { nation }" will.
 *
 * Note that the reflected type information will still include info for the
 * absent fields. Clients are supposed to assume those fields are absent from
 * query result values.
 *
 * TODO: Consider compressed format for references.
 *)

include Abstract.DATABASE

(**
 * Database configuration API.
 *
 * This allows to specify database schema and produce a universe.
 *
 * The example usage would be:
 *
 *   let univ = Config.(
 *     init
 *     |> hasMany "user" ...
 *     |> hasMany "site" ...
 *     ...
 *     |> hasScreen "view" ...
 *     |> hasScreen "pick" ...
 *     |> finish
 *   ) in ...
 *
 *)
module Config : sig

  (** Configuration *)
  type t

  (** An initial, empty configuration *)
  val init : t

  (** Define a singleton entity *)
  val hasOne :
    ?args : Query.Type.Syntax.arg list
    -> string
    -> (Query.Type.t -> Query.Type.field list)
    -> t -> t

  (** Define an optional singleton entity *)
  val hasOpt :
    ?args : Query.Type.Syntax.arg list
    -> string
    -> (Query.Type.t -> Query.Type.field list)
    -> t -> t

  (** Define a collection of entities *)
  val hasMany :
    ?args : Query.Type.Syntax.arg list
    -> string
    -> (Query.Type.t -> Query.Type.field list)
    -> t -> t

  (** Define a screen *)
  val hasScreen :
    string
    -> Screen.t
    -> t -> t

  (** Finish configuration and produce a universe *)
  val finish : t -> Universe.t

end

(**
 * Construct database out of a JSON value.
 *)
val ofJson : univ : Universe.t -> Js.Json.t -> t

(**
 * Construct database out of a string value.
 *
 * This throws an exception on an invalid JSON value.
 *)
val ofStringExn : univ : Universe.t -> string -> t
