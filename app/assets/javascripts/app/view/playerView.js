define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PlayerView', ['$scope', function ($scope) {


		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
