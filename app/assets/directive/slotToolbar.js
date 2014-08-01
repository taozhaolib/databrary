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
            if (ctrl.media.hasPosition(ctrl.media.current[0])) {
              ctrl.clock.play();
            } else {
              ctrl.clock.play(true, ctrl.media.current[0]);
            }
          },
          pause: function () {
            ctrl.clock.pause();
          },
        };

        // watchers

        ctrl.clock.playFn(function () {
          toolbar.buttons.play.enabled = false;
          toolbar.buttons.pause.enabled = true;
        });

        ctrl.clock.pauseFn(function () {
          toolbar.buttons.play.enabled = true;
          toolbar.buttons.pause.enabled = false;
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
