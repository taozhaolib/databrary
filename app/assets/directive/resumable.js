'use strict';

module.directive('resumable', [
	/*'pageService',*/ function (/*page*/) {
		var link = function (/*$scope, $element, $attrs*/) {
			//var r = new Resumable();

		};

		return {
			restrict: 'E',
			scope: true,
			replace: true,
			templateUrl: 'resumable.html',
			link: link
		};
	}
]);
