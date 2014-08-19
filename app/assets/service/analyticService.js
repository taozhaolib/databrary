'use strict';

module.factory('analyticService', [
  '$rootScope',
  '$location',
  '$cacheFactory',
  function ($rootScope, $location, $cacheFactory) {
    var analytics = {};
    var queue = [];

    //

    analytics.dump = function (config) {
      if (!queue.length) {
        return false;
      }

      if (config.url.contains('/api/null')) {
        return JSON.stringify(queue.splice(0, queue.length));
      }

      var info = $cacheFactory.info();
      var cache, key;

      for (key in info) {
        if (config.url.contains(key)) {
          cache = $cacheFactory.get(key);

          if (!cache.get(config.url)) {
            return JSON.stringify(queue.splice(0, queue.length));
          }

          break;
        }
      }

      return false;
    };

    analytics.add = function (action, route, data) {
      var analytic = {};

      analytic.action = action;
      analytic.route = angular.isString(route) ? route : $location.url();
      analytic.data = angular.isObject(route) ? route : data || {};

      if (analytic.data.error && angular.isString(analytic.data.error.data) && analytic.data.error.data.length > 512) {
        return;
      }

      queue.push(analytic);
    };

    //

    $rootScope.$on('$routeChangeSuccess', function (event, current, previous) {
      analytics.add('open', $location.url(), {
        current: current && current.controller,
        previous: previous && previous.controller,
      });
    });

    $rootScope.$on('$routeChangeError', function (event, next, current, error) {
      analytics.add('close', $location.url(), {
        next: next && next.controller,
        current: current && current.controller,
        error: error,
      });
    });

    //

    return analytics;
  }
]);

