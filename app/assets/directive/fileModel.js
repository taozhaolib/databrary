'use strict';

module.directive('fileModel', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      $element.bind('change', function () {
        $scope.$apply(function () {
          page.$parse($attrs.fileModel).assign($scope, $element[0].files);

          if ($attrs.fileModelForm)
            $scope.$eval($attrs.fileModelForm).$setDirty();
        });
      });

      $scope.$watch($attrs.fileModel, function (val) {
        if (!val)
          $element[0].value = '';
      });
    };

    return {
      restrict: 'A',
      link: link
    };
  }
]);
