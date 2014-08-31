'use strict';

module.factory('modelService', [
  '$q', '$cacheFactory', 'routerService', 'constantService', 'Segment',
  function ($q, $cacheFactory, router, constants, Segment) {

    ///////////////////////////////// Model: common base class and utils

    // just a bit more efficient than angular's
    function extend(dst, src) {
      for (var key in src)
	dst[key] = src[key];
    }

    function Model(init) {
      this.init(init);
    }

    /* optional fields that are always returned (so missingness is significant) */
    Model.prototype.staticFields = [];

    Model.prototype.init = function (init) {
      extend(this, init);
    };

    Model.prototype.update = function (init) {
      if (!angular.isObject(init))
	return this;
      if (init.id !== this.id)
	throw new Error("update id mismatch");
      var s = this.staticFields;
      for (var i = 0; i < s.length; i ++) {
	var k = s[i];
	if (!(k in init) && k in this)
	  delete this[k];
      }
      this.init(init);
      return this;
    };

    Model.prototype.clear = function (/*f...*/) {
      for (var i = 0; i < arguments.length; i ++)
	if (arguments[i] in this)
	  delete this[arguments[i]];
    };

    /* determine whether the given object satisfies all the given dependency options already.
     * returns the missing options, or null if nothing is missing. */
    function checkOptions(obj, options) {
      var opts = {};
      var need = null;
      if (Array.isArray(options)) {
	for (var i = 0; i < options.length; i ++)
	  if (!obj || !(options[i] in obj)) {
	    opts[options[i]] = '';
	    need = opts;
	  }
      }
      else if (options && obj)
	angular.forEach(options, function (v, o) {
	  if (v || !(o in obj)) {
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
    function lift(obj /*, field...*/) {
      for (var i = 1; i < arguments.length; i ++)
	obj[arguments[i]] = obj.prototype[arguments[i]].call;
    }

    /* delegate the given fields on instances of obj to the sub-object sub */
    function delegate(obj, sub /*, field... */) {
      function getter(f) {
	return function() {
	  return this[sub][f];
	};
      }
      for (var i = 2; i < arguments.length; i ++) {
	var f = arguments[i];
	Object.defineProperty(obj.prototype, f, {
	  get: getter(f)
	});
      }
    }

    ///////////////////////////////// Party

    function Party(init) {
      Model.call(this, init);
    }

    modelCache(Party, 'Party', 256);

    Party.prototype.staticFields = ['orcid', 'affiliation', 'email', 'institution', 'url'];

    Party.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if (init.volumes)
	volumeMakeSubArray(this.volumes);
      if (init.parents)
	partyMakeSubArray(this.parents);
      if (init.children)
	partyMakeSubArray(this.children);
    };

    function partyMake(init) {
      var p = Party.peek(init.id);
      return p ? p.update(init) : Party.poke(new Party(init));
    }

    function partyMakeSubArray(l) {
      for (var i = 0; i < l.length; i ++)
	l[i].party = partyMake(l[i].party);
      return l;
    }

    function partyMakeArray(l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = partyMake(l[i]);
      return l;
    }

    function partyGet(id, p, options) {
      if ((options = checkOptions(p, options)))
	return router.http(id === Login.user.id ? // may both be undefined
	    router.controllers.PartyApi.profile :
	    router.controllers.PartyApi.get,
	  id, options)
	  .then(function (res) {
	    return p ? p.update(res.data) : Party.poke(new Party(res.data));
	  });
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
      var p = this;
      return router.http(router.controllers.PartyApi.update, this.id, data)
	.then(function (res) {
	  return p.update(res.data);
	});
    };

    Party.query = function (data) {
      return router.http(router.controllers.PartyApi.query, data)
	.then(function (res) {
	  return res.data.map(partyMake);
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
	  if (Array.isArray(res.data))
	    return partyMakeArray(res.data);
	  return res;
	});
    };

    Party.prototype.authorizeApply = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeApply, this.id, target, data)
	.finally(function () {
	  p.clear('parents');
	}).then(function (res) {
	  return p.update(res.data);
	});
    };

    Party.prototype.authorizeSave = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeChange, this.id, target, data)
	.finally(function () {
	  p.clear('children');
	}).then(function (res) {
	  return p.update(res.data);
	});
    };

    Party.prototype.authorizeDelete = function (target) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeDelete, this.id, target)
	.finally(function () {
	  p.clear('children');
	}).then(function (res) {
	  return p.update(res.data);
	});
    };

    ///////////////////////////////// Login

    function Login(init) {
      Party.call(this, init);
    }

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;

    Login.prototype.staticFields = Party.prototype.staticFields.concat(['superuser']);

    function loginPoke(l) {
      return (Login.user = Party.poke(new Login(l)));
    }

    loginPoke(window.$play.user);

    function loginRes(res) {
      var l = res.data;
      var c = Login.user.id === l.id ? Login.user : Party.peek(l.id);
      if (c)
	l = c.update(l);
      if (c instanceof Login)
	return (Login.user = c);
      return loginPoke(l);
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
      Model.call(this, init);
    }

    modelCache(Volume, 'Volume', 8);

    Volume.prototype.staticFields = ['alias'];

    Volume.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if (init.access)
	partyMakeSubArray(this.access);
      if (init.containers) {
	var cl = this.containers;
	var cm = {};
	for (var ci = 0; ci < cl.length; ci ++)
	  cm[cl[ci].id] = new Container(this, cl[ci]);
	this.containers = cm;
      }
      if (init.top) {
	if (this.containers && this.top.id in this.containers)
	  this.top = this.containers[this.top.id].update(this.top);
	else
	  this.top = new Container(this, this.top);
      }
      if (init.records) {
	var rl = this.records;
	var rm = {};
	for (var ri = 0; ri < rl.length; ri ++)
	  rm[rl[ri].id] = new Record(this, rl[ri]);
	this.records = rm;
      }
    };

    function volumeMake(init) {
      var v = Volume.peek(init.id);
      return v ? v.update(init) : Volume.poke(new Volume(init));
    }

    function volumeMakeSubArray(l) {
      for (var i = 0; i < l.length; i ++)
	l[i].volume = volumeMake(l[i].volume);
      return l;
    }

    function volumeGet(id, v, options) {
      if ((options = checkOptions(v, options)))
	return router.http(router.controllers.VolumeApi.get,
	  id, options).then(function (res) {
	    return v ? v.update(res.data) : Volume.poke(new Volume(res.data));
	  });
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
      var v = this;
      return router.http(router.controllers.VolumeApi.update, this.id, data)
	.then(function (res) {
	  return v.update(res.data);
	});
    };

    Volume.create = function (data, owner) {
      return router.http(router.controllers.VolumeApi.create, owner, data)
	.then(function (res) {
	  return volumeMake(res.data);
	});
    };
    
    Volume.query = function (data) {
      return router.http(router.controllers.VolumeApi.query, data)
	.then(function (res) {
	  return res.data.map(volumeMake);
	});
    };

    Object.defineProperty(Volume.prototype, 'type', {
      get: function () {
	if ('citation' in this)
	  return this.citation ? 'study' : 'dataset';
      }
    });

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
	.then(function (res) {
	  return partyMakeArray(res.data);
	});
    };

    Volume.prototype.accessSave = function (target, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessChange, this.id, target, data)
	.finally(function () {
	  v.clear('access');
	}).then(function (res) {
	  return v.update(res.data);
	});
    };

    Volume.prototype.accessDelete = function (target) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessDelete, this.id, target)
	.finally(function () {
	  v.clear('access');
	}).then(function (res) {
	  return v.update(res.data);
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
	}).then(function (res) {
	  return v.update(res.data);
	});
    };

    Volume.prototype.fundingDelete = function (funder) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingDelete, this.id, funder)
	.finally(function () {
	  v.clear('funding');
	}).then(function (res) {
	  return v.update(res.data);
	});
    };

    ///////////////////////////////// Container/Slot
    // This does not handle cross-volume inclusions

    function Slot(container, init) {
      this.container = container;
      if (init)
	Model.call(this, init);
    }

    Slot.prototype = Object.create(Model.prototype);
    Slot.prototype.constructor = Slot;

    Slot.prototype.staticFields = ['consent', 'segment'];

    Slot.prototype.init = function (init) {
      var c = this.container;
      Model.prototype.init.call(this, init);
      if ('container' in init)
	this.container = c.update(init.container);
      if ('assets' in init)
	slotAssetMakeArray(this.container, this.assets);
    };

    delegate(Slot, 'container',
	'id', 'volume', 'top', 'date', 'name');

    delegate(Slot, 'volume',
	'permission');

    Object.defineProperty(Slot.prototype, 'displayName', {
      get: function () {
	return constants.message(this.top ? 'materials' : 'session') + (this.name ? ': ' + this.name : '');
      }
    });

    function Container(volume, init) {
      this.volume = volume;
      Slot.call(this, this, init);
    }

    Container.prototype = Object.create(Slot.prototype);
    Container.prototype.constructor = Container;

    Container.prototype.staticFields = ['name', 'date', 'top'].concat(Slot.prototype.staticFields);

    Container.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      this.container = this;
      if ('volume' in init)
	this.volume = v.update(init.volume);
    };

    function peekContainer(volume, container) {
      if (container instanceof Container)
	return container;
      if (volume.containers && container in volume.containers)
	return volume.containers[container];
      if (volume.top && volume.top.id === container)
	return volume.top;
    }

    function slotMake(volume, init) {
      var c = peekContainer(volume, init.contianer.id);
      if (!c)
	c = new Container(volume);
      else if (c.segment === init.segment) // only for undefined
	return c.update(init);
      return new Slot(c, init);
    }

    Volume.prototype.getSlot = function (container, segment, options) {
      var c = peekContainer(this, container);
      if (c)
	return c.getSlot(segment, options);
      var v = this;
      return router.http(router.controllers.SlotApi.get,
	this.id, container, Segment.format(segment), options)
	.then(function (res) {
	  return new Slot(new Container(v), res.data);
	});
    };

    Container.prototype.getSlot = function (segment, options) {
      var c = this;
      if (this.segment === segment) { // only for undefined
	if ((options = checkOptions(this, options)))
	  return router.http(router.controllers.SlotApi.get,
	    this.volume.id, this.id, Segment.format(segment), options)
	    .then(function (res) {
	      return c.update(res.data);
	    });
	else
	  return $q.successful(this);
      }
      return router.http(router.controllers.SlotApi.get,
	this.volume.id, this.id, Segment.format(segment), options)
	.then(function (res) {
	  return new Slot(c, res.data);
	});
    };

    Slot.prototype.save = function (data) {
      var s = this;
      return router.http(router.controllers.SlotApi.update, this.container.id, Segment.format(this.segment), data)
	.then(function (res) {
	  return s.update(res.data);
	});
    };

    Slot.prototype.addRecord = function (data) {
      var s = this;
      return router.http(router.controllers.RecordApi.add, this.container.id, Segment.format(this.segment), data)
	.finally(function () {
	  s.clear('records');
	});
    };

    Object.defineProperty(Slot.prototype, 'route', {
      get: function () {
	return router.slot([this.volume.id, this.container.id, Segment.format(this.segment)]);
      }
    });

    Slot.prototype.editRoute = function () {
      return router.slotEdit([this.container.id, Segment.format(this.segment)]);
    };

    ///////////////////////////////// Record

    function Record(volume, init) {
      this.volume = volume;
      Model.call(this, init);
    }

    Record.prototype = Object.create(Model.prototype);
    Record.prototype.constructor = Record;

    Record.prototype.staticFields = ['category'];

    Record.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      if ('volume' in init)
	this.volume = v.update(init.volume);
    };

    delegate(Record, 'volume',
	'permission');

    function peekRecord(volume, record) {
      if (record instanceof Record)
	return record;
      if (volume.records && record in volume.records)
	return volume.records[record];
    }

    Volume.prototype.getRecord = function (record) {
      var r = peekRecord(this, record);
      if (r)
	return $q.successful(r);
      var v = this;
      return router.http(router.controllers.RecordApi.get, record)
	.then(function (res) {
	  return new Record(v, res.data);
	});
    };

    Record.prototype.save = function (data) {
      var r = this;
      return router.http(router.controllers.RecordApi.update, this.id, data)
	.then(function (res) {
	  return r.update(res.data);
	});
    };

    Record.prototype.measureSet = function (metric, value) {
      var r = this;
      return router.http(router.controllers.RecordApi.measureUpdate, this.id, metric, {datum:value})
	.then(function (res) {
	  return r.update(res.data);
	});
    };

    Object.defineProperty(Record.prototype, 'displayName', {
      get: function () {
	var cat = constants.data.category[this.category];
	var idents = cat && cat.ident || [constants.data.metricName.ident.id];
	var ident = [];
	for (var i = 0; i < idents.length; i ++)
	  if (idents[i] in this.measures)
	    ident.push(this.measures[i]);

	return ident.length ? ident.join(', ') : this.id;
      }
    });

    Object.defineProperty(Record.prototype, 'route', {
      get: function () {
	return router.record([this.id]);
      }
    });

    Record.prototype.editRoute = function () {
      return router.recordEdit([this.id]);
    };

    ///////////////////////////////// Asset

    function Asset(volume, init) {
      this.volume = volume;
      Model.call(this, init);
    }

    Asset.prototype = Object.create(Model.prototype);
    Asset.prototype.constructor = Asset;

    Asset.prototype.staticFields = ['name', 'duration', 'segment'];

    Asset.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      if ('revisions' in init)
	assetMakeArray(this.revisions);
      if ('volume' in init)
	this.volume = v.update(init.volume);
      if ('format' in init)
	this.format = constants.data.format[this.format];
    };

    function assetMake(volume, init) {
      return init.asset ? 
	new SlotAsset(new Container(volume), init) :
	new Asset(volume, init);
    }

    function assetMakeArray(volume, l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = assetMake(volume, l[i]);
      return l;
    }

    Asset.prototype.get = function (options) {
      var a = this;
      if ((options = checkOptions(a, options)))
	return router.http(router.controllers.AssetApi.get, a.id, options)
	  .then(function (res) {
	    return a.update(res.data);
	  });
      else
	return $q.successful(a);
    };

    Asset.prototype.save = function (data) {
      var a = this;
      return router.http(router.controllers.AssetApi.update, this.id, data)
	.then(function (res) {
	  return a.update(res.data);
	});
    };

    Volume.prototype.createAsset = function (data) {
      var v = this;
      return router.http(router.controllers.AssetApi.upload, this.id, data)
	.then(function (res) {
	  return assetMake(v, res.data);
	});
    };

    ///////////////////////////////// SlotAsset

    function SlotAsset(container, init) {
      Slot.call(this, container, init);
    }

    // We don't actually inherit from Slot, but we do use much of the functionality:
    SlotAsset.prototype = Object.create(Model.prototype);
    SlotAsset.prototype.constructor = SlotAsset;

    SlotAsset.prototype.staticFields = ['format', 'excerpt'].concat(Slot.prototype.staticFields);

    SlotAsset.prototype.init = function (init) {
      var a = this.asset;
      Slot.prototype.init.call(this, init);
      this.asset = a ? a.update(init.asset) : new Asset(init.asset);
      if ('format' in init)
	this.format = constants.data.format[this.format];
    };

    function slotAssetMakeArray(container, l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = new SlotAsset(container, l[i]);
      return l;
    }

    delegate(SlotAsset, 'asset',
	'id', 'format', 'classification', 'name', 'duration');

    Object.defineProperty(SlotAsset.prototype, 'displayName', {
      get: function () {
	return this.name || this.format.name;
      }
    });

    Object.defineProperty(SlotAsset.prototype, 'icon', {
      get: function () {
	return '/public/images/filetype/16px/' + this.format.extension + '.png';
      }
    });

    SlotAsset.prototype.replace = function (data) {
      var a = this.asset;
      return router.http(router.controllers.AssetApi.replace, a.id, data)
	.then(function (res) {
	  return assetMake(a.volume, res.data);
	});
    };

    SlotAsset.prototype.remove = function () {
      var a = this.asset;
      return router.http(router.controllers.AssetApi.remove, a.id)
	.then(function (res) {
	  return a.update(res.data);
	});
    };

    SlotAsset.prototype.thumbRoute = function(size) {
      return router.assetThumb([this.container.id, Segment.format(this.segment), this.asset.id, size]);
    };

    SlotAsset.prototype.downloadRoute = function(inline) {
      return router.assetDownload([this.container.id, Segment.format(this.segment), this.asset.id, inline]);
    };

    SlotAsset.prototype.editRoute = function() {
      return router.assetEdit([this.asset.id]);
    };

    /////////////////////////////////

    return {
      Segment: Segment,
      Party: Party,
      Login: Login,
      Volume: Volume,
      Container: Container,
      Slot: Slot,
      Record: Record,
      Asset: Asset,
      SlotAsset: SlotAsset,
      analytic: function () {
	return router.http(router.controllers.SiteApi.void, {}, {cache:false});
      },
    };
  }
]);
