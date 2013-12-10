define([
	'app/modules/dbControllers'
], function (db) {
	'use strict';

	db.controller('ViewAssetCtrl', ['$scope', function ($scope) {


		//

		var start = function () {
			$scope.id = 0;
		};

		start();
	}]);
});
