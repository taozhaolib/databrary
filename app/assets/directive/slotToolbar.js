'use strict';

module.directive('slotToolbar', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var toolbar = this;
        toolbar.slot = page.$parse($attrs.slot)($scope);
        toolbar.clock = page.$parse($attrs.clock)($scope);
      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotToolbar.html',
      controller: controller,
      controllerAs: 'toolbar',
    };
  }
]);
