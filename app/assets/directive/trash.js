'use strict';
//to be paired with directive trashable. 'outside' is a shared parent scope

module.directive('trash', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr) {
			var INDICATOR_ON = "trash can open";
			var INDICATOR_OFF = "trash can closed";
			$scope.indicator = INDICATOR_OFF;

			$el.addClass("trash");

			$el.bind('dragenter', function(e){
				if($scope.outside.thumbDragged){
				    $scope.indicator = INDICATOR_ON;
				}
			});

			$el.bind('dragleave', function(e){
				$scope.indicator = INDICATOR_OFF;
			});

			$el.bind('drop', function(e){
				var angElScope = angular.element($scope.outside.thumbDragged).scope();
				if(angElScope && angElScope.form){
				  $scope.outside.remove(angElScope.form.subform);
				}
				else{
				  //an error thru messaging function determined by attribute?
				  console.log("no");
				}
				$scope.indicator = INDICATOR_OFF;
				e.stopPropagation();
				e.preventDefault();
			});
		};

		return {
			restrict: 'A',
			scope: {
			    outside: "=",
			    indicator: "="  
			},
			link: link,
		};
}]);

