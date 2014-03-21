define(['config/module'], function (module) {
	'use strict';

	module.factory('Scraper', ['$q', '$http', function ($q, $http) {
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
