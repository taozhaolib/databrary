'use strict';
//to be paired with directive trash. 'outside' is a shared parent scope

module.directive('trashable', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr) {

		  $el.addClass("trashable");

		  $el.bind('dragstart', function(){
			$scope.outside.thumbDragged = this;
		  });

		  $el.bind('dragend', function(){
			$scope.outside.thumbDragged = undefined;
		  });

		};

		return {
			restrict: 'A',
			scope: {
			    outside: "="
			},
			link: link,
		};
}]);
