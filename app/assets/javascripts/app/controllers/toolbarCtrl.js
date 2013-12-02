define([
	'app/modules/dbControllers',
	'app/services/eventService'
], function (db) {
	'use strict';

	db.controller('ToolbarCtrl', ['$scope', '$location', '$anchorScroll', '$timeout', 'EventService', function ($scope, $location, $anchorScroll, $timeout, eventService) {

		$scope.scrollTo = function (panel) {
			$location.hash(panel.id);
			$anchorScroll();
		}
	}]);
});
