'use strict';

module.factory('dataModel', [
  '$q',
  function ($q) {
    function Model(init) {
      this.update(init);
    }

    /* an immediately successful promise. */
    Model.successful = function (v) {
      var p = $q.defer();
      p.resolve(v);
      return p.promise;
    };

    /* lift functions from object methods to direct calls. */
    Model.lift = function (obj /*, fields...*/) {
      for (var i = 1; i < arguments.length; i ++)
	obj[arguments[i]] = obj.prototype[arguments[i]].call;
    };

    Model.prototype.staticFields = [];

    Model.prototype.update = function (init) {
      this.staticFields.forEach(function (k) {
	if (!init.hasOwnProperty(k) && this.hasOwnProperty(k))
	  delete this[k];
      }, this);
      angular.forEach(init, function (v, k) {
	this[k] = v;
      }, this);
      return this;
    };

    /* determine whether the given object satisfies all the given dependency options already.
     * returns the missing options, or null if nothing is missing. */
    Model.prototype.checkOptions = function (options) {
      var opts = {};
      var need = null;
      if (Array.isArray(options)) {
	for (var i = 0; i < options.length; i ++)
	  if (!this.hasOwnProperty(options[i])) {
	    opts[options[i]] = undefined;
	    need = opts;
	  }
      }
      else if (typeof options === 'object') {
	angular.forEach(options, function (v, o) {
	  if (v || !this.hasOwnProperty(o)) {
	    opts[o] = v;
	    need = opts;
	  }
	}, this);
      }
      return need;
    };

    return Model;
  }
]);
