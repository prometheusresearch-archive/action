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
 * Construct database out of a JSON value.
 *)
val ofJson : univ : Universe.t -> Js.Json.t -> t

(**
 * Construct database out of a string value.
 *
 * This throws an exception on an invalid JSON value.
 *)
val ofStringExn : univ : Universe.t -> string -> t
