/** Models provide an interface to the postgres database.
  * In general, models provide all of the information necessary to serve requests, but do not do any validation or permission checking beyond what the database does itself.
  * Controllers and other callers should do any necessary checking to ensure appropriate semantics.
  */
package object models
