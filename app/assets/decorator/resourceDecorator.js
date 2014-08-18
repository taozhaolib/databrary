'use strict';

module.config([
  '$provide', function ($provide) {
    var isCallable = function (fn) {
      angular.isFunction(fn);
    };

    $provide.decorator('$resource', [
      '$delegate', '$cacheFactory', function ($resource, $cacheFactory) {
        return function (url, params, actions, options) {
          if (!angular.isObject(options)) {
            options = {};
          }

          actions = angular.isObject(actions) ? actions : {};
          actions.get = actions.get || {method: 'GET'};
          actions.query = actions.query || {method: 'GET', isArray: true};

          if (options.cache && !options.cache.info) {
            options.cache = options.cache || '$http';
            options.cache = $cacheFactory.get(options.cache) || $cacheFactory(options.cache);
          }

          for (var prop in actions) {
            if (actions.hasOwnProperty(prop) && actions[prop].method.toUpperCase() === 'GET') {
              actions[prop].cache = options.cache;
            }
          }

          var resource = $resource.apply($resource, arguments);

          if (options.cache && options.cache.info) {
            resource.$cache = options.cache;
          }

          return resource;
        };
      }
    ]);
  }
]);
