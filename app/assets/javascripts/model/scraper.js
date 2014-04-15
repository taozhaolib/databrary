module.factory('Scraper', [
	'$q', '$http', 'pageService', function ($q, $http, page) {
		return function (url) {
			var deferred = $q.defer();

			$http
				.get(url)
				.success(function (res) {
					deferred.resolve(res);
				})
				.error(function (errors, status, headers, config) {
					page.messages.addError({
						body: page.constants.message('scraper.error', config.url.split('/').pop()),
						errors: errors,
						status: status
					});

					deferred.reject();
				});

			return deferred.promise;
		};
	}
]);
