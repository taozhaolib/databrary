'use strict';

app.factory('analyticService', [
  '$rootScope', '$location',
  function ($rootScope, $location) {
    var analytics = {};
    var queue = [];

    analytics.dump = function () {
      if (!queue.length)
        return;
      return JSON.stringify(queue.splice(0, queue.length));
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

