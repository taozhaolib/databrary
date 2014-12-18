'use strict';

app.directive('toolbar', [
  'pageService', function (page) {
    return {
      restrict: 'E',
      templateUrl: 'site/toolbar.html',
      scope: {},
      link: function ($scope) {
        $scope.page = page;
        $scope.hoverUserToggle = function ($event) {
          $scope.hoverUser = !$scope.hoverUser;
          if ($event)
            $event.stopPropagation();
        };
        $scope.$on('$locationChangeStart', function () {
          $scope.hoverUser = false;
        });
        $scope.search = function () {
          page.$location.path(page.router.search()).search($scope.search.data);
        };
        $scope.search.data = {};
      }
    };
  }
]);
