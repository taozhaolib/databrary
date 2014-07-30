'use strict';

module.directive('slotToolbarButton', [
  'pageService', function (page) {
    var pre = function ($scope, $element, $attrs, slotToolbar) {
      var button = {};
      $scope.button = button;

      if (!$attrs.name) {
        throw new Error('Every slotToolbarButton must have a name.');
      }

      button.name = $attrs.name;
      button.icon = $attrs.icon || $attrs.name;

      button.enabled = page.$parse($attrs.enabled)($scope) || true;
      button.$element = $element;

      button.classes = function () {
        var cls = [];

        if (!button.enabled) {
          cls.push('hide');
        }

        return cls;
      };

      //

      if ($attrs.click) {
        $element.click(function () {
          $scope.$apply(function () {
            page.$parse($attrs.click)($scope)();
          });
        });
      }

      //

      slotToolbar.registerButton(button);
    };

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotToolbarButton.html',
      link: {
        pre: pre
      },
      require: '^slotToolbar',
    };
  }
]);
