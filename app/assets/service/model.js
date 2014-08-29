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

    Model.prototype.clear = function (/*f...*/) {
      for (var i = 0; i < arguments.length; i ++)
	if (this.hasOwnProperty(arguments[i]))
	  delete this[arguments[i]];
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

      obj.clear = function (/*id...*/) {
	if (arguments.length)
	  for (var i = 0; i < arguments.length; i ++)
	    obj.cache.remove(arguments[i]);
	else
	  obj.cache.removeAll();
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

    Party.prototype.staticFields = ['name', 'orcid', 'affiliation', 'email', 'institution', 'url'];

    Party.make = function (p) {
      volumeMakeSubArray(p.volumes);
      partyMakeSubArray(p.parents);
      partyMakeSubArray(p.children);
      var c = Party.peek(p.id);
      return c ? c.update(p) : Party.poke(new Party(p));
    };

    function partyMakeSubArray(l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i].party = Party.make(l[i].party);
      return l;
    }

    function partyRes(res) {
      return Party.make(res.data);
    }

    function partyResArray(res) {
      var l = res.data;
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = Party.make(l[i]);
      return l;
    }

    function partyGet(id, p, options) {
      if ((options = checkOptions(p, options)))
	return router.http(id === Login.user.id ? // may both be undefined
	    router.controllers.PartyApi.profile :
	    router.controllers.PartyApi.get,
	  id, options).then(partyRes);
      else
	return $q.successful(p);
    }

    Party.get = function (id, options) {
      return partyGet(id, Party.peek(id), options);
    };

    Party.prototype.get = function (options) {
      return partyGet(this.id, this, options);
    };

    Party.profile = function (options) {
      return Party.get(Login.user.id, options);
    };

    Party.prototype.save = function (data) {
      return router.http(router.controllers.PartyApi.update, this.id, data)
	.then(partyRes);
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
      size = size || 56;

      var params = {};
      if (nonce) {
	params.nonce = nonce;
      }

      return router.partyAvatar([this.id, size], params);
    };

    Party.prototype.authorizeSearch = function (apply, param) {
      return router.http(router.controllers.PartyApi.authorizeSearch, this.id, apply, param)
	.then(function (res) {
	  if (typeof res.data === 'object')
	    return partyResArray(res);
	});
    };

    Party.prototype.authorizeApply = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeApply, this.id, target, data)
	.finally(function () {
	  p.clear('parents');
	});
    };

    Party.prototype.authorizeSave = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeChange, this.id, target, data)
	.finally(function () {
	  p.clear('children');
	});
    };

    Party.prototype.authorizeDelete = function (target) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeDelete, this.id, target)
	.finally(function () {
	  p.clear('children');
	});
    };

    ///////////////////////////////// Login

    function Login(init) {
      Party.call(this, init);
    }

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;

    Login.prototype.staticFields = Party.prototype.staticFields.concat(['access', 'superuser']);

    function loginPoke(l) {
      Login.user = Party.poke(new Login(l));
    }

    loginPoke(window.$play.user);

    function loginRes(res) {
      var l = res.data;
      var c = Login.user.id === l.id ? Login.user : Party.peek(l.id);
      if (c)
	l = c.update(l);
      if (c instanceof Login)
	Login.user = c;
      else
	loginPoke(l);
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

    Volume.prototype.staticFields = ['name', 'alias', 'body', 'creation'];

    Volume.make = function (v) {
      partyMakeSubArray(v.access);
      var c = Volume.peek(v.id);
      return c ? c.update(v) : Volume.poke(new Volume(v));
    };

    function volumeMakeSubArray(l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i].volume = Volume.make(l[i].volume);
      return l;
    }

    function volumeRes(res) {
      return Volume.make(res.data);
    }

    function volumeGet(id, v, options) {
      if ((options = checkOptions(v, options)))
	return router.http(router.controllers.VolumeApi.get,
	  id, options).then(volumeRes);
      else
	return $q.successful(v);
    }

    Volume.get = function (id, options) {
      return volumeGet(id, Volume.peek(id), options);
    };

    Volume.prototype.get = function (options) {
      return volumeGet(this.id, this, options);
    };

    Volume.prototype.save = function (data) {
      return router.http(router.controllers.VolumeApi.update, this.id, data)
	.then(volumeRes);
    };

    Volume.create = function (data, owner) {
      return router.http(router.controllers.VolumeApi.create, owner, data)
	.then(volumeRes);
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

    Volume.prototype.accessSearch = function (param) {
      return router.http(router.controllers.VolumeApi.accessSearch, this.id, param)
	.then(partyResArray);
    };

    Volume.prototype.accessSave = function (target, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessChange, this.id, target, data)
	.finally(function () {
	  v.clear('access');
	});
    };

    Volume.prototype.accessDelete = function (target) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessDelete, this.id, target)
	.finally(function () {
	  v.clear('access');
	});
    };

    Volume.funderSearch = function (query, all) {
      return router.http(router.controllers.VolumeApi.funderSearch, query, all);
    };

    Volume.prototype.fundingSave = function (funder, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingChange, this.id, funder, data)
	.finally(function () {
	  v.clear('funding');
	});
    };

    Volume.prototype.fundingDelete = function (funder) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingDelete, this.id, funder)
	.finally(function () {
	  v.clear('funding');
	});
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
