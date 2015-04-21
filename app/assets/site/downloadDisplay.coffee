'use strict'

app.directive 'downloadDisplay', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'site/downloadDisplay.html'
    scope: false
    link: ($scope) ->
      $scope.hasThumbnail = (asset) ->
        asset.checkPermission(constants.permission.READ) && (asset.format.type == 'image' || asset.format.type == 'video' && asset.asset.duration)

      $scope.toggleDownload = () ->
        $scope.downloading = !$scope.downloading
  ]