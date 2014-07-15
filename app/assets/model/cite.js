'use strict';

module.factory('cite', [
	'$http', '$q', function ($http, $q) {
		return function (url) {
			var deferred = $q.defer();

			$http.get('/api/cite?url=' + encodeURIComponent(url), {
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
