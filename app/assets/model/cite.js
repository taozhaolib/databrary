'use strict';

module.factory('cite', [
  '$q', 'routerService', function ($q, router) {
    return function (url) {
      var deferred = $q.defer();

      router.http(router.controllers.SiteApi.cite, {
	url: url }, {
        cache: false
      }).success(function (res) {
        deferred.resolve(res);
      }).error(function () {
        deferred.reject(arguments);
      });

      return deferred.promise;
    };
  }
]);
