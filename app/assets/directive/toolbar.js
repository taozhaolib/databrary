'use strict';

module.directive('toolbar', [
  'pageService', function (page) {
    return {
      restrict: 'A',
      templateUrl: 'toolbar.html',
      scope: {},
      link: function ($scope) {
	$scope.page = page;
	$scope.hoverUserToggle = function ($event) {
	  $scope.hoverUser = !$scope.hoverUser;
	  if ($event)
	    $event.stopPropagation();
	};
	page.$rootScope.$on('$locationChangeStart', function () {
	  $scope.hoverUser = false;
	});
      }
    };
  }
]);
