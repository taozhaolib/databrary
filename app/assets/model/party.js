'use strict';

module.factory('Party', [
  'dataModel', '$cacheFactory', 'routerService', function (dataModel, $cacheFactory, router) {
    function Party(init) {
      this.update(init);
    }

    Party.prototype = Object.create(dataModel.prototype);
    Party.prototype.constructor = Party;

    Party.prototype.staticFields = ['id', 'name', 'orcid', 'affiliation', 'email', 'institution', 'url'];

    var cache = $cacheFactory('Party');

    Party.clear = function (id) {
      if (id === undefined)
	cache.removeAll();
      else
	cache.remove(id);
    };

    Party.peek = cache.get;
    Party.poke = function (p) {
      return cache.put(p.id, p);
    };

    Party.make = function (p) {
      var c = cache.get(p.id);
      return c ? c.update(p) : Party.poke(new Party(p));
    };

    function got(res) {
      return Party.make(res.data);
    }

    Party.get = function (id, options) {
      var p = cache.get(id);
      if (!p || (options = p.checkOptions(options)))
	return router.http(id === Party.user ? // may both be undefined
	    router.controllers.PartyApi.profile :
	    router.controllers.PartyApi.get,
	  id, options).then(got);
      else
	return dataModel.successful(p);
    };

    Party.profile = function (options) {
      return Party.get(Party.user, options);
    };

    Party.prototype.save = function (data) {
      return router.http(router.controllers.PartyApi.update, this.id, data).then(got);
    };

    Party.prototype.upload = function (fd) {
      return router.http(router.controllers.PartyApi.update, this.id, fd, {
	transformRequest: angular.identity,
	headers: {
	  'Content-Type': undefined
	},
      }).then(got);
    };

    Party.query = function (data) {
      return router.http(router.controllers.PartyApi.query, data)
	.then(function (res) {
	  return res.data.map(Party.make);
	});
    };

    Object.defineProperty(Party.prototype, 'route', {
      get: function () {
	return router.party([this.id]);
      }
    });

    Party.prototype.editRoute = function (page) {
      var params = {};
      if (page)
	params.page = page;

      return router.partyEdit([this.id], params);
    };

    Party.prototype.avatarRoute = function (size, nonce) {
      if (!angular.isNumber(size)) {
	size = 56;
      }

      var params = {};
      if (nonce) {
	params.nonce = nonce;
      }

      return router.partyAvatar([this.id, size], params);
    };

    /*
    dataModel.lift(Party, 'editRoute', 'avatarRoute');
    */

    return Party;
  }
]);
