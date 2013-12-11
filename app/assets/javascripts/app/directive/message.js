define(['app/config/module'], function (module) {
	'use strict';

	module.directive('message', ['MessageService', function (messageService) {
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
