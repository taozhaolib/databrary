module.factory('Analytic', [
	'resourceFactory', '$route', function (resource, $route) {
		var analytic = resource('/api/null', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, 'analytic');

		analytic.send = function () {
			analytic.$cache.removeAll();
			analytic.get();
		};

		return analytic;
	}
]);
