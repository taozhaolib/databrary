'use strict';

module.factory('modelService', [
  '$q', '$cacheFactory', 'routerService',
  function ($q, $cacheFactory, router) {

    ///////////////////////////////// Model: common base class and utils

    function Model(init) {
      this.update(init);
    }

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
    function checkOptions(obj, options) {
      var opts = {};
      var need = null;
      if (Array.isArray(options)) {
	for (var i = 0; i < options.length; i ++)
	  if (!obj || !obj.hasOwnProperty(options[i])) {
	    opts[options[i]] = '';
	    need = opts;
	  }
      }
      else if (!(options && obj))
	return options;
      else
	angular.forEach(options, function (v, o) {
	  if (v || !obj.hasOwnProperty(o)) {
	    opts[o] = v;
	    need = opts;
	  }
	});
      return need;
    }

    function modelCache(obj, name, size) {
      obj.prototype = Object.create(Model.prototype);
      obj.prototype.constructor = obj;

      var opts = {};
      if (size)
	opts.number = size;
      obj.cache = $cacheFactory(name, opts);

      obj.clear = function (id) {
	if (id === undefined)
	  obj.cache.removeAll();
	else
	  obj.cache.remove(id);
      };

      obj.peek = obj.cache.get;
      obj.poke = function (x) {
	return obj.cache.put(x.id, x);
      };
    }

    /* lift functions from object methods to direct calls. */
    function lift(obj /*, fields...*/) {
      for (var i = 1; i < arguments.length; i ++)
	obj[arguments[i]] = obj.prototype[arguments[i]].call;
    }

    ///////////////////////////////// Party

    function Party(init) {
      this.update(init);
    }

    modelCache(Party, 'Party', 256);

    Party.prototype.staticFields = ['id', 'name', 'orcid', 'affiliation', 'email', 'institution', 'url'];

    Party.make = function (p) {
      if (p.volumes)
	for (var i = 0; i < p.volumes.length; i ++)
	  p.volumes[i].volume = Volume.make(p.volumes[i].volume);
      var c = Party.peek(p.id);
      return c ? c.update(p) : Party.poke(new Party(p));
    };

    function partyRes(res) {
      return Party.make(res.data);
    }

    Party.get = function (id, options) {
      var p = Party.peek(id);
      if ((options = checkOptions(p, options)))
	return router.http(id === Party.user ? // may both be undefined
	    router.controllers.PartyApi.profile :
	    router.controllers.PartyApi.get,
	  id, options).then(partyRes);
      else
	return $q.successful(p);
    };

    Party.profile = function (options) {
      return Party.get(Party.user, options);
    };

    Party.prototype.save = function (data) {
      return router.http(router.controllers.PartyApi.update, this.id, data).then(partyRes);
    };

    Party.prototype.upload = function (fd) {
      return router.http(router.controllers.PartyApi.update, this.id, fd, {
	transformRequest: angular.identity,
	headers: {
	  'Content-Type': undefined
	},
      }).then(partyRes);
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

    ///////////////////////////////// Login

    function Login(init) {
      Party.call(this, init);
    }

    Login.user = undefined;

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;

    Login.prototype.staticFields = Party.prototype.staticFields.concat(['access', 'superuser']);

    function loginRes(res) {
      var l = res.data;
      var c = Party.peek(l.id);
      if (c)
	l = c.update(l);
      if (c instanceof Login)
	Login.user = c;
      else
	Login.user = Party.poke(new Login(l));
      Party.user = Login.user.id;
      return Login.user;
    }

    angular.forEach({
      get: 'get',
      login: 'post',
      logout: 'logout',
      superuserOn: 'superuserOn',
      superuserOff: 'superuserOff'
    }, function (api, f) {
      Login[f] = function (data) {
	return router.http(router.controllers.LoginApi[api], data).then(loginRes);
      };
    });

    ///////////////////////////////// Volume

    function Volume(init) {
      this.update(init);
    }

    modelCache(Volume, 'Volume', 8);

    Volume.prototype.staticFields = ['id', 'name', 'alias', 'body', 'creation', 'permission'];

    Volume.make = function (v) {
      if (v.access)
	for (var i = 0; i < v.access.length; i ++)
	  v.access[i].party = Party.make(v.access[i].party);
      var c = Volume.peek(v.id);
      return c ? c.update(v) : Volume.poke(new Volume(v));
    };

    function volumeRes(res) {
      return Volume.make(res.data);
    }

    Volume.get = function (id, options) {
      var v = Volume.peek(id);
      if ((options = checkOptions(v, options)))
	return router.http(router.controllers.VolumeApi.get,
	  id, options).then(volumeRes);
      else
	return $q.successful(v);
    };

    Volume.prototype.save = function (data) {
      return router.http(router.controllers.VolumeApi.update, this.id, data).then(volumeRes);
    };

    Volume.create = function (data, owner) {
      return router.http(router.controllers.VolumeApi.create, owner, data).then(volumeRes);
    };
    
    Volume.query = function (data) {
      return router.http(router.controllers.VolumeApi.query, data)
	.then(function (res) {
	  return res.data.map(Volume.make);
	});
    };

    Object.defineProperty(Volume.prototype, 'route', {
      get: function () {
	return router.volume([this.id]);
      }
    });

    Volume.prototype.editRoute = function (page) {
      var params = {};
      if (page)
	params.page = page;

      return router.volumeEdit([this.id], params);
    };

    /////////////////////////////////

    return {
      Party: Party,
      Login: Login,
      Volume: Volume,
      analytic: function () {
	return router.http(router.controllers.SiteApi.void, {}, {cache:false});
      },
    };
  }
]);
