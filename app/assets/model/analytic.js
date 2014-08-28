'use strict';

module.factory('analytic', [
  'routerService', function (router) {
    return function () {
      return router.http(router.controllers.SiteApi.void, {}, {cache:false});
    };
  }
]);
