'use strict';

app.factory('modelService', [
  '$q', '$cacheFactory', '$play', 'routerService', 'constantService', 'Segment',
  function ($q, $cacheFactory, $play, router, constants, Segment) {

    ///////////////////////////////// Model: common base class and utils

    function resData(res) {
      return res.data;
    }

    function Model(init) {
      this.init(init);
    }

    /* map of fields to to true (static, missingness is significant) or false (update when present) */
    Model.prototype.fields = {
      id: true,
      permission: false,
    };

    Model.prototype.init = function (init) {
      var fields = this.fields;
      for (var f in fields) {
        if (f in init)
          this[f] = init[f];
        else if (fields[f])
          delete this[f];
      }
    };

    Model.prototype.update = function (init) {
      if (typeof init !== 'object')
        return this;
      if ('id' in init && init.id !== this.id)
        throw new Error("update id mismatch");
      this.init(init);
      return this;
    };

    Model.prototype.clear = function (/*f...*/) {
      for (var i = 0; i < arguments.length; i ++)
        if (arguments[i] in this)
          delete this[arguments[i]];
    };

    function hasField(obj, opt) {
      return obj && opt in obj && (!obj[opt] || typeof obj[opt] !== 'object' || !obj[opt]._PLACEHOLDER);
    }

    /* determine whether the given object satisfies all the given dependency options already.
     * returns the missing options, or null if nothing is missing. */
    function checkOptions(obj, options) {
      var opts = {};
      var need = obj ? null : opts;
      if (Array.isArray(options)) {
        for (var i = 0; i < options.length; i ++)
          if (!hasField(obj, options[i])) {
            opts[options[i]] = '';
            need = opts;
          }
      }
      else if (!obj)
        return options || opts;
      else if (options)

        _.each(options, function(v, o){
	  if (v || !hasField(obj, o)) {
            opts[o] = v;
            need = opts;
          } 
	});

      return need;
    }

    function modelCache(obj, name, size) {
      obj.prototype = Object.create(Model.prototype);
      obj.prototype.constructor = obj;
      obj.prototype.class = name;

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

      obj.poke = function (x) {
        return obj.cache.put(x.id, x);
      };
    }

    /* delegate the given (missing) fields on instances of obj to the sub-object sub,
     * but allow assignments to work directly as usual. */
    function delegate(obj, sub /*, field... */) {
      function descr(f) {
        return {
          get: function () {
            return this[sub].hasOwnProperty(f) ? this[sub][f] : undefined;
          },
          set: function (v) {
            Object.defineProperty(this, f, {
              configurable: true,
              enumerable: true,
              writable: true,
              value: v
            });
          }
        };
      }
      for (var i = 2; i < arguments.length; i ++) {
        var f = arguments[i];
        Object.defineProperty(obj.prototype, f, descr(f));
      }
    }

    ///////////////////////////////// Party

    function Party(init) {
      Model.call(this, init);
    }

    modelCache(Party, 'party', 256);

    Party.prototype.fields = {
      id: true,
      permission: false,
      name: true,
      sortname: true,
      prename: true,
      orcid: true,
      affiliation: true,
      email: true,
      institution: true,
      url: true,
      access: false,
      openid: false,
    };

    Party.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('volumes' in init)
        this.volumes = volumeMakeSubArray(init.volumes);
      if ('parents' in init)
        this.parents = partyMakeSubArray(init.parents);
      if ('children' in init)
        this.children = partyMakeSubArray(init.children);
      if ('comments' in init)
        this.comments = commentMakeArray(null, init.comments);
    };

    function partyPeek(id) {
      return id === Login.user.id && Login.user || Party.cache.get(id);
    }

    function partyMake(init) {
      var p = partyPeek(init.id);
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
        return router.http(id == Login.user.id ? // may both be undefined (id may be string)
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
      return partyGet(id, partyPeek(id), options);
    };

    Party.prototype.get = function (options) {
      return partyGet(this.id, this, options);
    };

    Party.prototype.save = function (data) {
      var p = this;
      return router.http(router.controllers.PartyApi.update, this.id, data)
        .then(function (res) {
          return p.update(res.data);
        });
    };

    Party.search = function (data) {
      return router.http(router.controllers.PartyApi.search, data)
        .then(function (res) {
          return _.map(res.data, partyMake);
        });
    };

    Party.prototype.route = function () {
      return router.party([this.id]);
    };

    Object.defineProperty(Party.prototype, 'lastName', {
      get: function () {
        return this.name.substr(this.name.lastIndexOf(' ')+1);
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
      if (nonce)
        params.nonce = nonce;

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
        .then(function (res) {
          p.clear('parents');
          return p.update(res.data);
        });
    };

    Party.prototype.authorizeSave = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeChange, this.id, target, data)
        .then(function (res) {
          p.clear('children');
          return p.update(res.data);
        });
    };

    Party.prototype.authorizeRemove = function (target) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeRemove, this.id, target)
        .then(function (res) {
          p.clear('children');
          return p.update(res.data);
        });
    };

    ///////////////////////////////// Login

    function Login(init) {
      Party.call(this, init);
    }

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;
    Login.prototype.fields = angular.extend({
      superuser: false,
    }, Login.prototype.fields);

    Login.user = new Login({id:constants.party.NOBODY});

    function loginPoke(l) {
      return (Login.user = Party.poke(new Login(l)));
    }

    loginPoke($play.user);

    function loginRes(res) {
      var l = res.data;
      if (Login.user.id === l.id && Login.user.superuser === l.superuser)
        return Login.user.update(l);
      $cacheFactory.removeAll();
      return loginPoke(l);
    }

    Login.isLoggedIn = function () {
      return Login.user.id !== constants.party.NOBODY;
    };

    Login.checkAccess = function (level) {
      return Login.user.access >= level;
    };

    Model.prototype.checkPermission = function (level) {
      return this.permission >= level || Login.user.superuser;
    };

    /* a little hacky, but to get people SUPER on themselves: */
    Login.prototype.checkPermission = function (/*level*/) {
      return this.id !== constants.party.NOBODY;
    };

    Login.isAuthorized = function () {
      return Login.isLoggedIn() && Login.checkAccess(constants.permission.PUBLIC);
    };

    Login.prototype.route = function () {
      return router.profile();
    };



    _.each({
      get: 'get',
      login: 'post',
      logout: 'logout',
      superuserOn: 'superuserOn',
      superuserOff: 'superuserOff'
    }, function(api, f){
      Login[f] = function (data) {
        return router.http(router.controllers.LoginApi[api], data).then(loginRes);
      };
    });

    Login.register = function (data) {
      return router.http(router.controllers.LoginApi.register, data);
    };

    Login.issuePassword = function (data) {
      return router.http(router.controllers.TokenHtml.issuePassword, data);
    };

    Login.getToken = function (token, auth) {
      return router.http(router.controllers.TokenApi.token, token, auth)
        .then(resData);
    };

    Login.passwordToken = function (party, data) {
      return router.http(router.controllers.TokenApi.password, party, data)
        .then(loginRes);
    };

    ///////////////////////////////// Volume

    function Volume(init) {
      this.containers = {_PLACEHOLDER:true};
      this.records = {_PLACEHOLDER:true};
      this.assets = {_PLACEHOLDER:true};
      Model.call(this, init);
    }

    modelCache(Volume, 'volume', 8);

    Volume.prototype.fields = {
      id: true,
      permission: false,
      name: true,
      alias: true,
      body: true,
      creation: true,
      citation: false,
      links: false,
      funding: false,
      tags: false,
      // consumers: false,
      // producers: false,
    };

    Volume.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('access' in init)
        this.access = partyMakeSubArray(init.access);
      if ('records' in init) {
        var rl = init.records;
        for (var ri = 0; ri < rl.length; ri ++)
          recordMake(this, rl[ri]);
        delete this.records._PLACEHOLDER;
      }
      if ('containers' in init) {
        var cl = init.containers;
        for (var ci = 0; ci < cl.length; ci ++)
          containerMake(this, cl[ci]);
        delete this.containers._PLACEHOLDER;
      }
      if ('assets' in init) {
        var al = init.assets;
        for (var ai = 0; ai < al.length; ai ++) {
          var a = assetMake(this, al[ai]);
          /* this assumes when we get assets we get all or none for each container: */
          var c = a.container;
          if (c) {
            if (!c.assets)
              c.assets = {};
            c.assets[a.id] = a;
          }
        }
        delete this.assets._PLACEHOLDER;
      }
      if ('top' in init)
        this.top = containerMake(this, init.top);
      if ('excerpts' in init)
        this.excerpts = assetMakeArray(this, init.excerpts);
      if ('comments' in init)
        this.comments = commentMakeArray(this, init.comments);
    };

    function volumeMake(init) {
      var v = Volume.cache.get(init.id);
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
      return volumeGet(id, Volume.cache.get(id), options);
    };

    Volume.prototype.get = function (options) {
      return volumeGet(this.id, this, options);
    };

    Volume.prototype.save = function (data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.update, this.id, data)
        .then(function (res) {
          if ('citation' in data)
            v.clear('citation');
          if ('links' in data)
            v.clear('links');
          return v.update(res.data);
        });
    };

    Volume.create = function (data, owner) {
      return router.http(router.controllers.VolumeApi.create, owner, data)
        .then(function (res) {
          if ((owner = (owner === undefined ? Login.user : partyPeek(owner))))
            owner.clear('volumes');
          return volumeMake(res.data);
        });
    };
    
    Volume.search = function (data) {
      return router.http(router.controllers.VolumeApi.search, data)
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

    Volume.prototype.route = function () {
      return router.volume([this.id]);
    };

    Volume.prototype.editRoute = function (page) {
      var params = {};
      if (page)
        params.page = page;

      return router.volumeEdit([this.id], params);
    };

    Volume.prototype.thumbRoute = function (size) {
      return router.volumeThumb([this.id, size]);
    };

    Volume.prototype.zipRoute = function () {
      return router.volumeZip([this.id]);
    };

    Volume.prototype.csvRoute = function () {
      return router.volumeCSV([this.id]);
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
        .then(function (res) {
          v.clear('access');
          return v.update(res.data);
        });
    };

    Volume.prototype.accessRemove = function (target) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessRemove, this.id, target)
        .then(function (res) {
          v.clear('access');
          return v.update(res.data);
        });
    };

    Volume.prototype.fundingSave = function (funder, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingChange, this.id, funder, data)
        .then(function (res) {
          v.clear('funding');
          return v.update(res.data);
        });
    };

    Volume.prototype.fundingRemove = function (funder) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingRemove, this.id, funder)
        .then(function (res) {
          v.clear('funding');
          return v.update(res.data);
        });
    };

    ///////////////////////////////// Container/Slot
    // This does not handle cross-volume inclusions

    function Slot(context, init) {
      this.container =
        context instanceof Container ? context :
        containerPrepare(context, init.container);
      if (init)
        Model.call(this, init);
    }

    Slot.prototype = Object.create(Model.prototype);
    Slot.prototype.constructor = Slot;
    Slot.prototype.class = 'slot';

    Slot.prototype.fields = {
      release: true,
      tags: false,
      releases: false,
    };

    Slot.prototype.clear = function (/*f...*/) {
      Model.prototype.clear.apply(this, arguments);
      Model.prototype.clear.apply(this.container, arguments);
    };

    function slotInit(slot, init) {
      if ('assets' in init) {
        var al = init.assets;
        slot.assets = {};
        for (var ai = 0; ai < al.length; ai ++) {
          var a = assetMake(slot.container, al[ai]);
          slot.assets[a.id] = a;
        }
      }
      if ('comments' in init)
        slot.comments = commentMakeArray(slot.container, init.comments);
      if ('records' in init) {
        var rl = init.records;
        for (var ri = 0; ri < rl.length; ri ++)
          rl[ri].record = rl[ri].record ? recordMake(slot.volume, rl[ri].record) : slot.volume.records[rl[ri].id];
        slot.records = rl;
      }
      if ('excerpts' in init)
        slot.excerpts = assetMakeArray(slot.container, init.excerpts);
    }

    Slot.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      this.segment = new Segment(init.segment);
      if ('container' in init)
        this.container.update(init.container);
      if ('volume' in init)
        this.volume.update(init.volume);
      slotInit(this, init);
    };

    delegate(Slot, 'container',
        'id', 'volume', 'top', 'date', 'name');

    delegate(Slot, 'volume',
        'permission');

    Object.defineProperty(Slot.prototype, 'displayName', {
      get: function () {
        return constants.message(this.container.top ? 'materials' : 'session') + (this.name ? ': ' + this.name : '');
      }
    });

    Slot.prototype.asSlot = function () {
      return this.segment.full ? this.container : angular.extend(new Slot(this.container), this);
    };

    function Container(volume, init) {
      this.volume = volume;
      volume.containers[init.id] = this;
      Slot.call(this, this, init);
    }

    Container.prototype = Object.create(Slot.prototype);
    Container.prototype.constructor = Container;

    Container.prototype.fields = angular.extend({
      id: false,
      _PLACEHOLDER: true,
      name: true,
      top: true,
      date: true,
    }, Container.prototype.fields);

    Container.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('volume' in init)
        this.volume.update(init.volume);
      if ('container' in init)
        this.update(init.container);
      slotInit(this, init);
    };

    Object.defineProperty(Container.prototype, 'segment', {
      get: function () {
        return Segment.full;
      }
    });

    Container.prototype.remove = function () {
      var c = this;
      return router.http(router.controllers.SlotApi.remove, this.id)
        .then(function () {
          delete c.volume.containers[c.id];
          return true;
        }, function (res) {
          if (res.status == 409) {
            c.update(res.data);
            return false;
          }
          return $q.reject(res);
        });
    };

    function containerMake(volume, init) {
      var c = volume.containers[init.id];
      if (c) {
        if (!init._PLACEHOLDER)
          c.update(init);
        return c;
      } else
        return new Container(volume, init);
    }

    function containerPrepare(volume, init) {
      if (typeof init == 'number')
        init = {id:init,_PLACEHOLDER:true};
      return containerMake(volume || volumeMake(init.volume), init);
    }

    Volume.prototype.getSlot = function (container, segment, options) {
      return containerPrepare(this, parseInt(container, 10)).getSlot(segment, options);
    };

    Container.prototype.getSlot = function (segment, options) {
      var c = this;
      if (Segment.isFull(segment))
        if ((options = checkOptions(this, options)) || this._PLACEHOLDER)
          return router.http(router.controllers.SlotApi.get,
            this.volume.id, this.id, Segment.format(segment), options)
            .then(function (res) {
              return c.update(res.data);
            });
        else return $q.successful(this);
      else return router.http(router.controllers.SlotApi.get,
        this.volume.id, this.id, Segment.format(segment), checkOptions(null, options))
        .then(function (res) {
          return new Slot(c, res.data);
        });
    };

    Slot.prototype.save = function (data) {
      var s = this;
      return router.http(router.controllers.SlotApi.update, this.container.id, this.segment.format(), data)
        .then(function (res) {
          if ('release' in data) {
            s.clear('releases');
            s.container.clear('releases');
          }
          return s.update(res.data);
        });
    };

    Volume.prototype.createContainer = function (data) {
      var v = this;
      return router.http(router.controllers.SlotApi.create, this.id, data)
        .then(function (res) {
          return new Container(v, res.data);
        });
    };

    function recordAdd(slot, record) {
      var r = {
        id: record.id,
        segment: slot.segment,
        record: record,
      };
      if ('records' in slot)
        slot.records.push(r);
      if (slot.container !== slot && 'records' in slot.container)
        slot.container.records.push(r);
    }

    Slot.prototype.addRecord = function (r, seg) {
      if (!seg)
        seg = this.segment;
      var s = this;
      return router.http(router.controllers.RecordApi.add, this.container.id, seg.format(), {record:r.id})
        .then(function (res) {
          recordAdd(s, r);
          return r.update(res.data);
        });
    };

    Slot.prototype.newRecord = function (c) {
      var s = this;
      if (c && typeof c === 'object')
        c = c.id;
      return router.http(router.controllers.RecordApi.add, this.container.id, this.segment.format(), {category:c})
        .then(function (res) {
          var r = new Record(s.volume, res.data);
          recordAdd(s, r);
          return r;
        });
    };

    Slot.prototype.removeRecord = Slot.prototype.moveRecord = function (r, src, dst) {
      if (arguments.length < 3) {
        dst = null;
        if (src == null)
          src = this.segment;
      }
      var s = this;
      return router.http(router.controllers.RecordApi.move, r.id, this.container.id, {src: Segment.data(src), dst: Segment.data(dst)})
        .then(function (res) {
          if (!('container' in res.data))
            return null;
          var d = new Slot(s.container, res.data);
          if (s.records) {
            var ss = Segment.make(src);
            for (var ri = 0; ri < s.records.length; ri ++)
              if (s.records[ri].id === r.id && ss.contains(s.records[ri].segment)) {
                if (d.segment.empty)
                  s.records.splice(ri, 1);
                else
                  s.records[ri].segment = d.segment;
                break;
              }
          }
          return d;
        });
    };

    Slot.prototype.route = function (params) {
      return router.slot([this.volume.id, this.container.id, this.segment.format()], params);
    };

    Slot.prototype.editRoute = function () {
      return router.slotEdit([this.volume.id, this.container.id, this.segment.format()]);
    };

    Slot.prototype.zipRoute = function () {
      return router.slotZip([this.volume.id, this.container.id, this.segment.format()]);
    };

    ///////////////////////////////// Record

    function Record(volume, init) {
      this.volume = volume;
      volume.records[init.id] = this;
      Model.call(this, init);
    }

    Record.prototype = Object.create(Model.prototype);
    Record.prototype.constructor = Record;
    Record.prototype.class = 'record';

    Record.prototype.fields = {
      id: true,
      category: true,
      measures: true,
      // slots: false,
    };

    Record.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('volume' in init)
        this.volume.update(init.volume);
    };

    delegate(Record, 'volume',
        'permission');

    function recordMake(volume, init) {
      var r = volume.records[init.id];
      return r ? r.update(init) : new Record(volume, init);
    }

    Volume.prototype.getRecord = function (record) {
      if (record instanceof Record)
        return $q.successful(record);
      if (record in this.records)
        return $q.successful(this.records[record]);
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
        var cat = constants.category[this.category];
        var idents = cat && cat.ident || [constants.metricName.ID.id];
        var ident = [];
        for (var i = 0; i < idents.length; i ++)
          if (idents[i] in this.measures)
            ident.push(this.measures[idents[i]]);

        ident = ident.length && ident.join(', ');
        cat = cat && cat.name;
        if (cat && ident)
          return cat + ' ' + ident;
        return cat || ident || '[' + this.id + ']';
      }
    });

    Record.prototype.route = function () {
      return router.record([this.id]);
    };

    Record.prototype.editRoute = function () {
      return router.recordEdit([this.id]);
    };

    ///////////////////////////////// AssetSlot

    // This usually maps to an AssetSegment
    function AssetSlot(context, init) {
      this.asset =
        context instanceof Asset ? context :
        new assetMake(context, init.asset);
      Model.call(this, init);
    }

    AssetSlot.prototype = Object.create(Slot.prototype);
    AssetSlot.prototype.constructor = AssetSlot;
    AssetSlot.prototype.class = 'asset-slot';

    AssetSlot.prototype.fields = angular.extend({
      permission: true,
      excerpt: true,
      context: true
    }, AssetSlot.prototype.fields);

    AssetSlot.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      this.asset.update(init.asset);
      this.segment = new Segment(init.segment);
      if ('format' in init)
        this.format = constants.format[init.format];
    };

    delegate(AssetSlot, 'asset',
        'id', 'container', 'format', 'duration', 'release', 'name', 'pending');

    Object.defineProperty(AssetSlot.prototype, 'displayName', {
      get: function () {
        return this.name || this.format.name;
      }
    });

    AssetSlot.prototype.route = function (params) {
      params.asset = this.id;
      return router.slot([this.volume.id, this.container.id, this.segment.format()], params);
    };

    AssetSlot.prototype.inContext = function () {
      return 'context' in this ?
        angular.extend(Object.create(AssetSlot.prototype), this, {segment:Segment.make(this.context)}) :
        this.asset;
    };

    Object.defineProperty(AssetSlot.prototype, 'icon', {
      get: function () {
        return '/public/images/filetype/16px/' + this.format.extension + '.svg';
      }
    });

    AssetSlot.prototype.inSegment = function (segment) {
      segment = this.segment.intersect(segment);
      if (segment.equals(this.segment))
        return this;
      return new AssetSlot(this.asset, {permission:this.permission, segment:segment});
    };

    AssetSlot.prototype.setExcerpt = function (release) {
      var a = this;
      return router.http(release != null ? router.controllers.AssetSlotApi.setExcerpt : router.controllers.AssetSlotApi.removeExcerpt, this.container.id, this.segment.format(), this.id, {release:release})
        .then(function (res) {
          a.clear('excerpts');
          a.volume.clear('excerpts');
          return a.update(res.data);
        });
    };

    AssetSlot.prototype.thumbRoute = function (size) {
      return router.assetThumb([this.container.id, this.segment.format(), this.id, size]);
    };

    AssetSlot.prototype.downloadRoute = function (inline) {
      return router.assetDownload([this.container.id, this.segment.format(), this.id, inline]);
    };

    ///////////////////////////////// Asset

    // This usually maps to a SlotAsset, but may be an unlinked Asset
    function Asset(context, init) {
      if (init.container || context instanceof Container)
        Slot.call(this, context, init);
      else {
        this.volume = context;
        Model.call(this, init);
      }
      this.asset = this;
      this.volume.assets[init.id] = this;
    }

    Asset.prototype = Object.create(AssetSlot.prototype);
    Asset.prototype.constructor = Asset;
    Asset.prototype.class = 'asset';

    Asset.prototype.fields = angular.extend({
      id: true,
      release: true,
      name: true,
      duration: true,
      pending: true,
      creation: false,
    }, Asset.prototype.fields);

    Asset.prototype.init = function (init) {
      if (!this.container && 'container' in init)
        this.container = containerPrepare(this.volume, init.container);
      Slot.prototype.init.call(this, init);
      if ('format' in init)
        this.format = constants.format[init.format];
      if ('revisions' in init)
        this.revisions = assetMakeArray(this.volume, init.revisions);
    };

    function assetMake(context, init) {
      var v = context.volume || context;
      if (typeof init === 'number')
        return v.assets[init];
      if ('id' in init) {
        var a = v.assets[init.id];
        return a ? a.update(init) : new Asset(context, init);
      } else
        return new AssetSlot(context, init);
    }

    function assetMakeArray(context, l) {
      if (l) for (var i = 0; i < l.length; i ++)
        l[i] = assetMake(context, l[i]);
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
          if ('excerpt' in data) {
            a.clear('excerpts');
            a.volume.clear('excerpts');
          }
          return a.update(res.data);
        });
    };

    Asset.prototype.link = function (slot, data) {
      if (!data)
        data = {};
      data.container = slot.container.id;
      data.position = slot.segment.l;
      return this.save(data);
    };

    Slot.prototype.createAsset = function (data) {
      var s = this;
      if (!data)
        data = {};
      data.container = this.container.id;
      if (!('position' in data) && isFinite(this.segment.l))
        data.position = this.segment.l;
      return router.http(router.controllers.AssetApi.upload, this.volume.id, data)
        .then(function (res) {
          s.clear('assets');
          return assetMake(s.volume, res.data);
        });
    };

    Asset.prototype.replace = function (data) {
      var a = this;
      return router.http(router.controllers.AssetApi.replace, this.id, data)
        .then(function (res) {
          if (a.container)
            a.container.clear('assets');
          return assetMake(a.volume, res.data);
        });
    };

    Asset.prototype.remove = function () {
      var a = this;
      return router.http(router.controllers.AssetApi.remove, this.id)
        .then(function (res) {
          if (a.container)
            a.container.clear('assets');
          return a.update(res.data);
        });
    };

    Asset.prototype.editRoute = function () {
      return router.assetEdit([this.id]);
    };

    ///////////////////////////////// Comment

    function Comment(context, init) {
      Slot.call(this, context, init);
    }

    Comment.prototype = Object.create(Slot.prototype);
    Comment.prototype.constructor = Comment;
    Comment.prototype.class = 'comment';

    Comment.prototype.fields = angular.extend({
      id: true,
      time: true,
      text: true,
      parents: true
    }, Comment.prototype.fields);

    Comment.prototype.init = function (init) {
      Slot.prototype.init.call(this, init);
      if ('who' in init)
        this.who = partyMake(init.who);
    };

    function commentMakeArray(context, l) {
      if (l) for (var i = 0; i < l.length; i ++)
        l[i] = new Comment(context, l[i]);
      return l;
    }

    Slot.prototype.postComment = function (data, segment, reply) {
      if (segment === undefined)
        segment = this.segment;
      if (arguments.length < 3 && this instanceof Comment)
        reply = this.id;
      var s = this;
      return router.http(router.controllers.CommentApi.post, this.container.id, segment.format(), reply, data)
        .then(function (res) {
          s.volume.clear('comments');
          s.clear('comments');
          return new Comment(s.container, res.data);
        });
    };

    ///////////////////////////////// Tag
    
    // no point in a model, really
    var Tag = {};

    Tag.search = function (query) {
      return router.http(router.controllers.TagApi.search, query)
        .then(function(res) {
          //do other stuff?
          return res.data;
        });
    };

    Tag.top = function () {
      return router.http(router.controllers.TagApi.top)
        .then(function(res) {
          return res.data;
        });
    };

    Slot.prototype.setTag = function (tag, vote, keyword, segment) {
      if (segment === undefined)
        segment = this.segment;
      var s = this;
      return router.http(router.controllers.TagApi.update, tag, this.container.id, segment.format(), {vote:vote,keyword:!!keyword})
        .then(function (res) {
          var tag = res.data;
          s.volume.clear('tags');
          if ('tags' in s)
            s.tags[tag.id] = tag;
          return tag;
        });
    };

    /////////////////////////////////

    return {
      Party: Party,
      Login: Login,
      Volume: Volume,
      Container: Container,
      Slot: Slot,
      Record: Record,
      Asset: Asset,
      AssetSlot: AssetSlot,
      Comment: Comment,
      Tag: Tag,

      funder: function (query, all) {
        return router.http(router.controllers.VolumeApi.funderSearch, query, all)
          .then(resData);
      },
      cite: function (url) {
        return router.http(router.controllers.AngularController.cite, {url:url})
          .then(resData);
      },
      analytic: function () {
        return router.http(router.controllers.AngularController.void, {}, {cache:false});
      },
      activity: function () {
        return router.http(router.controllers.SiteApi.activity)
          .then(function (res) {
            for (var i = 0; i < res.data.length; i ++) {
              if ('volume' in res.data[i])
                res.data[i].volume = volumeMake(res.data[i].volume);
              if ('party' in res.data[i])
                res.data[i].party = partyMake(res.data[i].party);
            }
            return res.data;
          });
      }
    };
  }
]);
