module.factory('Analytic', [
	'resourceFactory', '$route', function (resource, $route) {
		var analytic = resource('/api/null', {}, 'analytic');

		analytic.send = function () {
			analytic.$cache.removeAll();
			analytic.get();
		};

		return analytic;
	}
]);
