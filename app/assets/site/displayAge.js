'use strict';

app.directive('displayAge', [
  'pageService', function (page) {
    var link = function ($scope) {
      $scope.change = page.display.toggleAge;

      var formatAge = function () {
        $scope.age = page.display.formatAge($scope.value);
      };

      formatAge();
      $scope.$on('displayService-toggleAge', formatAge);
    };

    return {
      restrict: 'E',
      templateUrl: 'site/displayAge.html',
      replace: true,
      scope: {
        value: '='
      },
      link: link
    };
  }
]);
