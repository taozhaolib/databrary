'use strict';

module.directive('tooltip', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      var tooltip = {};

      if ('tooltipId' in $attrs)
	tooltip.id = $attrs.tooltipId;
      if ('tooltipClass' in $attrs)
	tooltip.cls = $attrs.tooltipClass;
      if ('tooltipType' in $attrs)
	tooltip.type = $attrs.tooltipType;
      tooltip.message = $scope.$eval($attrs.tooltip);
      tooltip.$target = $element;

      if (tooltip.message)
        new page.tooltips(tooltip);
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
