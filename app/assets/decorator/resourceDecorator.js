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

          if (angular.isString(options.cache)) {
            options.cache = $cacheFactory.get(options.cache) || $cacheFactory(options.cache);
          }

          for (var prop in actions) {
            if (actions.hasOwnProperty(prop) && actions[prop].method.toUpperCase() === 'GET' && !('cache' in actions[prop])) {
              actions[prop].cache = options.cache;
            }
          }

          var resource = $resource.apply($resource, arguments);

          if (options.cache) {
	    resource.$cache = angular.isObject(options.cache) ? options.cache : $cacheFactory.get('$http');
          }

          return resource;
        };
      }
    ]);
  }
]);
