'use strict';

app.directive('assetDisplay', [
  function () { return {
    restrict: 'E',
    templateUrl: 'asset/display.html',
    scope: {
      assetFn: '&asset',
    },
    link: function ($scope) {
      var asset = $scope.assetFn();
      $scope.asset = asset.inContext();

      if ($scope.asset !== asset)
        $scope.clip = asset.segment.relativeTo($scope.asset.segment);
    }
  }; }
]);
