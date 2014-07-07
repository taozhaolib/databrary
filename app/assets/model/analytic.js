module.factory('Analytic', [
	'resourceFactory', function (resource) {
		var analytic = resource('/api/null', {}, 'analytic');

		analytic.send = function () {
			analytic.$cache.removeAll();
			analytic.get();
		};

		return analytic;
	}
]);
