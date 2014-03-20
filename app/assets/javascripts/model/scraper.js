define(['config/module'], function (module) {
	'use strict';

	module.factory('Scraper', ['$rootScope', '$q', '$http', function ($rootScope, $q, $http) {
		return function (url) {
			var deferred = $q.defer();

			$http
				.get(url)
				.success(function (response) {
					deferred.resolve(response);
				});

			return deferred.promise;
		};
	}]);
});
