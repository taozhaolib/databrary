'use strict'

app.directive 'volumeExcerpts', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'volume/excerpts.html'
    scope: false
    link: ($scope) ->
      $scope.current = $scope.volume.excerpts[0]

      $scope.setCurrent = (asset) ->
        $scope.current = asset

      $scope.hasThumbnail = (asset) ->
        asset.checkPermission(constants.permission.READ) && (asset.format.type == 'image' || asset.format.type == 'video' && asset.asset.duration)

      $scope.jumpLink = (excerpt) ->
        excerpt.container.route {asset: excerpt.id, select:excerpt.segment.format()}

      $scope.download = () ->
        $scope.downloading = true
]
