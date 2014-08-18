'use strict';

module.directive('browserIcon', [
  'pageService', function (page) {
    var link = function ($scope) {
      $scope.toggleExpand = function (data) {
        if (data.group !== 'session')
          $scope.data = page.browser.setItemExpand($scope.data);
      };

      $scope.iconLink = function (data) {
        if (data.group === 'session')
          return page.router.slot(data.volume.id, data.object.id, data.segment);
      };

      $scope.expanderClasses = function (data) {
        var classes = [];

        if (page.browser.canExpand(data)) {
          classes.push('enabled');
        }

        if (data.expand) {
          classes.push('active');
        }

        return classes;
      };
    };

    return {
      restrict: 'E',
      scope: false,
      templateUrl: 'browserIcon.html',
      replace: true,
      link: link
    };
  }
]);
