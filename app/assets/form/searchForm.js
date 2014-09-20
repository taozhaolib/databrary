'use strict';

module.directive('searchForm', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    templateUrl: 'searchForm.html',
    scope: true,
    link: function ($scope) {
      $scope.search = {};

      $scope.keypress = function (event) {
	var code = (event.keyCode ? event.keyCode : event.which);
	if (code == 13) {
	  page.$location.path(page.router.search());
	  page.$location.search($scope.search);
	}
      };
    }
  }; }
]);
