'use strict';

module.directive('assetEditForm', [
  'pageService', function (page) {
      var link = function ($scope){
	var form = $scope.assetEditForm;
	$scope.ctrl.assetEditForm = form;
      };
	
    //

    return {
      restrict: 'E',
      templateUrl: 'assetEditForm.html',
      link: link
    };
  }
]);
