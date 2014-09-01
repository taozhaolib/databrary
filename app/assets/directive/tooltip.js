'use strict';

module.directive('tooltip', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var tooltip = {};

      tooltip.id = $attrs.tooltipId;
      tooltip.cls = $attrs.tooltipClass;
      tooltip.type = $attrs.tooltip || $attrs.tooltipType;
      tooltip.message = $scope.$eval($attrs.tooltip);
      tooltip.$target = $element;

      if (tooltip.message)
        page.tooltips.add(tooltip);
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
