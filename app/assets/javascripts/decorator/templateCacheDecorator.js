module.config([
	'$provide', function ($provide) {
		$provide.decorator('$templateCache', [
			'$delegate', '$http', '$injector', function ($delegate, $http, $injector) {

				var promise,
					returned = false,
					allTemplatesUrl = '/public/templates/_all.html';

				var loadAllTemplates = function (url) {
					if (!promise) {
						promise = $http
							.get(allTemplatesUrl)
							.then(function (response) {
								$injector.get('$compile')(response.data);
								return response;
							}, function (response) {
								$location.url('http://databrary.org');
							});
					}

					return promise.then(function (response) {
						returned = true;

						return {
							status: response.status,
							data: get(url)
						};
					});
				};

				var get = $delegate.get;

				$delegate.get = function (url) {
					if (!returned) {
						return loadAllTemplates(url);
					}

					return get(url);
				};

				return $delegate;
			}
		]);
	}
]);