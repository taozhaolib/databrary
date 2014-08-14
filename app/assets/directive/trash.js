'use strict';

module.directive('trash', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr) {
			var INDICATOR_ON = "trash can open";
			var INDICATOR_OFF = "trash can closed";
			$scope[$attr.indicatorVar] = INDICATOR_OFF;

			$el.bind('dragenter', function(e){
				if($scope.volumeEditMaterialsForm.thumbDragged){
				    $scope[$attr.indicatorVar] = INDICATOR_ON;
				}
			});

			$el.bind('dragleave', function(e){
				    $scope[$attr.indicatorVar]= INDICATOR_OFF;
			});

			$el.bind('drop', function(e){
				console.log($scope.volumeEditMaterialsForm.thumbDragged);
				var angElScope = angular.element($scope.volumeEditMaterialsForm.thumbDragged).scope();
				console.log(angElScope);
				angElScope.form = angElScope.$parent.$parent.form;	
				if(angElScope && angElScope.form) $scope.volumeEditMaterialsForm.remove(angElScope.form.subform); //don't like this..
				else console.log("no");
				$scope[$attr.indicatorVar] = INDICATOR_OFF;
				e.stopPropagation();
				e.preventDefault();
			});
		};

		return {
			restrict: 'A',
			link: link,
		};
}]);

