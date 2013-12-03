define([
	'app/modules/dbDirectives',
	'app/services/messageService'
], function (db) {
	'use strict';

	db.directive('message', ['MessageService', function (messageService) {
		var link = function ($scope, $element, $attrs) {
			messageService.postRawMessage($scope, $element, $attrs);

			$element.remove();
		};

		return {
			restrict: 'A',
			link: link
		}
	}]);
});
