define(['config/module'], function (module) {
	'use strict';

	module.factory('Scraper', ['$q', '$http', 'Page', function ($q, $http, page) {
		return function (url) {
			var deferred = $q.defer();

			$http
				.get(url)
				.success(function (res) {
					deferred.resolve(res);
				})
				.error(function (data, status, headers, config) {
					page.messages.addError({
						countdown: 5000,
						body: page.constants.message('scraper.error', config.url.split('/').pop())
					});

					deferred.reject();
				});

			return deferred.promise;
		};
	}]);
});
