'use strict';

module.directive('trashable', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr) {

		  $el.bind('dragstart', function(e){
			console.log(e);
			$scope.volumeEditMaterialsForm.thumbDragged = e.originalEvent.srcElement;
			console.log($scope.volumeEditMaterialsForm);
		  });

		  $el.bind('dragend', function(e){
			$scope.volumeEditMaterialsForm.thumbDragged = undefined;
		  });

		};

		return {
			restrict: 'A',
			link: link,
		};
}]);
