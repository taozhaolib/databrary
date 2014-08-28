'use strict';

module.factory('Login', [
  'dataModel', 'routerService', 'Party', function (dataModel, router, Party) {
    function Login(init) {
      Party.call(this, init);
    }

    Login.user = undefined;

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;

    Login.prototype.staticFields = Party.prototype.staticFields.concat(['access', 'superuser']);

    function got(res) {
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
	return router.http(router.controllers.LoginApi[api], data).then(got);
      };
    });

    return Login;
  }
]);
