define([
	'app/modules/dbControllers'
], function (db) {
	'use strict';

	db.controller('ViewSearchCtrl', ['$scope', function ($scope) {


		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
