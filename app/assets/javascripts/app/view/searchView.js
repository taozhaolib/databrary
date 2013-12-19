define(['app/config/module'], function (module) {
	'use strict';

	module.controller('SearchView', ['$scope',
//		'volumes',
		function ($scope, volumes) {
		$scope.volumes = volumes;

		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
