'use strict';

module.controller('volume/overview', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = function () {
      $scope.enabled = angular.isObject($scope.volume);
    };
    
    $scope.ageSummary = function (summary) {
      var age = page.$filter('age');
      var range = age(summary.agerange[0]);

      if (summary.agerange[0] != summary.agerange[1]) {
        range += ' - ' + age(summary.agerange[1]);
      }

      return page.constants.message('volume.ages', range, age(summary.agemean));
    };
  }
]);
