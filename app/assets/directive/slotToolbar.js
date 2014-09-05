'use strict';

module.directive('slotToolbar', [
  'pageService', function (page) {
    var controller = [
      '$scope',
      function ($scope) {
        var toolbar = this;
        var ctrl = $scope.ctrl;

        toolbar.buttons = {};
        toolbar.time = page.$filter('timecode')(ctrl.clock.position);

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
          toolbar.buttons.play.enabled = false;
          toolbar.buttons.pause.enabled = true;
        });

        ctrl.clock.pauseFn(function () {
          toolbar.buttons.play.enabled = true;
          toolbar.buttons.pause.enabled = false;
        });

        ctrl.clock.timeFn(function () {
          toolbar.time = page.$filter('timecode')(ctrl.clock.position);
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
