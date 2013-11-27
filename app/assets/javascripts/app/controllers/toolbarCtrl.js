define([
	'app/modules/dbControllers',
	'app/services/eventService'
], function (db) {
	'use strict';

	db.controller('ToolbarCtrl', ['$scope', '$location', 'EventService', function ($scope, $location, $anchorScroll, eventService) {

		$scope.updateToolbars = function () {

		};

		//

		$scope.scrollTo = function (panel) {
			$location.hash(panel.id);
			$anchorScroll();
		};

		//

		$scope.$on('toolbarsUpdate', function () {
			console.log(arguments);
		});

		$scope.$on('toolbarsPanels', function ($event, panels) {
			$scope.panels = panels;
		})
	}]);
});
