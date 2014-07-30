'use strict';

module.directive('slotToolbarButton', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs, slotToolbar) {
        var button = {};
        $scope.button = button;

        if (!$attrs.name) {
          throw new Error('Every slotToolbarButton must have a name.');
        }

        button.name = $attrs.name;

        button.enabled = page.$parse($attrs.enabled)($scope) || true;
        button.$element = $element;

        button.bindClasses = function () {
          return [
            'icon',
            button.name,
            $attrs.icon
          ];
        };

        button.classes = function () {
          var cls = [];

          if (!button.enabled) {
            cls.push('hide');
          }

          return cls;
        };

        //

        if ($attrs.click) {
          $element.click(function (e) {
            $scope.$apply(function () {
              page.$parse($attrs.click)($scope)(e);
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
      link: link,
      require: '^slotToolbar',
    };
  }
]);
