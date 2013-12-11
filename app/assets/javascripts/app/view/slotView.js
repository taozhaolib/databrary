define(['app/config/module'], function (module) {
	'use strict';

	module.controller('SlotView', ['$scope', function ($scope) {


		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
