'use strict';

module.directive('slotToolbarButton', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs, $transclude, slotToolbar) {
        var button = this;

        button.name = $attrs.name || undefined;
        button.enabled = page.$parse($attrs.enabled)($scope) || true;
        button.$element = $element;

        button.classes = function () {
          var cls = [];

          if (button.name) {
            cls.push(button.name);
          }

          if (!button.enabled) {
            cls.push('hide');
          }

          return cls;
        };

        //

        $element.attrs('id', button.name);

        if ($attrs.click) {
          $element.click(function (e) {
            $scope.$apply(function () {
              
            });
          });
        }

        //

        slotToolbar.registerButton(button);
        return button;
      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotToolbarButton.html',
      controller: controller,
      controllerAs: 'button',
      require: '^slotToolbar',
    };
  }
]);
