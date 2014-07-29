'use strict';

module.directive('slotToolbar', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var toolbar = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        toolbar.action = {
          play: function () {
            ctrl.clock.play();
          },
        }
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
