'use strict';

module.directive('browserGroup', [
  'pageService', function (page) {
    var link = function ($scope) {
      $scope.groupClasses = function (group) {
        var classes = [];

        if (page.browser.options[group].active) {
          classes.push('on');
        }
        else {
          classes.push('off');
        }

        if (page.browser.isLastGroup(group)) {
          classes.push('last');
        }

        return classes;
      };

      $scope.recordGroupClasses = function (group, $last) {
        var classes = [];

        classes.push('on');
        classes.push(page.browser.isRecordGroupToggle(group) ? 'expanded' : '');

        if ($last && !page.browser.canAddRecordGroup()) {
          classes.push('last');
        }

        return classes;
      };
    };

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'volume/browserGroup.html',
      replace: true,
      priority: 100,
      link: link
    };
  }
]);
