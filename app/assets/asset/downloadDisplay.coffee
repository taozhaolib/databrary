'use strict'

app.directive 'downloadDisplay', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'asset/downloadDisplay.html'
    scope:
      assetFn: '&asset'
      close: '&close'
    link: ($scope) ->
      $scope.asset = asset = $scope.assetFn()
      $scope.volume = asset.volume
      $scope.release = constants.release[asset.release]
      $scope.hasThumbnail = (asset.format.type == 'image' || asset.format.type == 'video' && asset.duration && !asset.pending)
  ]
