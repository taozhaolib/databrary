'use strict';

app.directive('tooltips', [
  'pageService',
  function (page) {
    var link = function ($scope) {
      $scope.getTooltipClasses = function (tooltip) {
        var classes = tooltip.cls.split(' ');

        classes.push('tooltip');
        classes.push('tooltip-' + tooltip.type);

        classes.push.apply(classes,
          tooltip.position.map(function (p) {
            return 'tooltip-' + p;
          }));

        if (tooltip.visible)
          classes.push('tooltip-visible');

        return classes;
      };

      $scope.tooltips = page.tooltips.list;
    };

    return {
      restrict: 'E',
      scope: {},
      templateUrl: 'site/tooltips.html',
      link: link
    };
  }
]);
