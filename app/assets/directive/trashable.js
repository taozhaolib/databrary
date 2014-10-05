'use strict';
//to be paired with directive trash. 'outside' is a shared parent scope

app.directive('trashable', [
  function () {
    var link = function ($scope, $el) {

      $el.addClass("trashable");

      $el.bind('dragstart', function () {
        $scope.outside.thumbDragged = this;
      });

      $el.bind('dragend', function () {
        $scope.outside.thumbDragged = undefined;
      });

    };

    return {
      restrict: 'A',
      scope: {
        outside: "="
      },
      link: link,
    };
  }]);
