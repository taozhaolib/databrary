'use strict';
//to be paired with directive trashable. 'outside' is a shared parent scope

module.directive('trash', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr) {
			var indicatorOn = $attr.indicatorOn || "trash can open";
			var indicatorOff = $attr.indicatorOff || "trash can closed";
			$scope.indicator = indicatorOff;

			$el.addClass("trash");

			$el.bind('dragenter', function(){
				if($scope.outside.thumbDragged){
				    $scope.indicator = indicatorOn;
				}
			});

			$el.bind('dragover', function(){
				if($scope.outside.thumbDragged){
				    $scope.indicator = indicatorOn;
				}
			});

			$el.bind('dragleave', function(){
				$scope.indicator = indicatorOff;
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
				$scope.indicator = indicatorOff;
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

