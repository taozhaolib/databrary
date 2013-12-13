define(['app/config/module'], function (module) {
	'use strict';

	module.factory('authInterceptor', ['$injector', 'RetryService', function ($injector, retryService) {
		return function (promise) {
			var $http = $injector.get('$http');

			return promise.then(null, function (response) {
				if (response.status === 401) {
					promise = retryService.add(function () {
						return $http(response.config);
					});
				}

				return promise;
			});
		}
	}]);
});
