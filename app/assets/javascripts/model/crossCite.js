module.factory('CrossCite', [
	'$http', '$q', function ($http, $q) {
		var url = 'http://data.crossref.org/';
		var fundingUrl = 'http://search.crossref.org/funders?q=';

		return {
			apa: function (doi) {
				var deferred = $q.defer();

				$http.get(url + encodeURIComponent(doi), {
					cache: false,
					headers: {
						Accept: 'text/x-bibliography;style=apa',
					},
				}).success(function (res) {
					if (res.indexOf('Quagga Mussels') > -1) {
						deferred.reject(arguments);
					} else {
						deferred.resolve(res);
					}
				}).error(function () {
					deferred.reject(arguments);
				});

				return deferred.promise;
			},

			json: function (doi) {
				var deferred = $q.defer();

				$http.get(url + encodeURIComponent(doi), {
					cache: false,
					headers: {
						Accept: 'application/vnd.citationstyles.csl+json',
					},
				}).success(function (res) {
					if (res.title.indexOf('Quagga Mussels') > -1) {
						deferred.reject(arguments);
					} else {
						deferred.resolve(res);
					}
				}).error(function () {
					deferred.reject(arguments);
				});

				return deferred.promise;
			},

			searchFunding: function (query) {
				var deferred = $q.defer();

				$http.get(fundingUrl + encodeURIComponent(query), {
					query: query,
				}, {
					cache: false,
					headers: {
						Accept: 'application/vnd.citationstyles.csl+json',
					},
				}).success(function (res) {
					deferred.resolve(res);
				}).error(function () {
					deferred.reject(arguments);
				});

				return deferred.promise;
			},
		};
	}
]);
