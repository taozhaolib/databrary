'use strict';

app.directive('focus', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      if ($attrs.focus === '' || $scope.$eval($attrs.focus))
        page.$timeout($element.focus);
    };

    return {
      restrict: 'A',
      link: link,
    };
  }
]);
