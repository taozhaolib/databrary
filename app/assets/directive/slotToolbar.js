'use strict';

module.directive('slotToolbar', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var toolbar = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        toolbar.buttons = {};

        toolbar.registerButton = function (button) {
          toolbar.buttons[button.name] = button;

          switch (button.name) {
            case 'pause':
              button.enabled = false;
              break;
          }
        };

        // button actions

        toolbar.action = {
          play: function () {
            ctrl.clock.play();
          },
          pause: function () {
            ctrl.clock.pause();
          },
        };

        // watchers

        ctrl.clock.playFn(function () {
          toolbar.buttons.play.enabled = true;
          toolbar.buttons.pause.enabled = false;
        });

        ctrl.clock.pauseFn(function () {
          toolbar.buttons.play.enabled = false;
          toolbar.buttons.pause.enabled = true;
        });

        // controller

        return toolbar;
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
