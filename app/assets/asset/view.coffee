'use strict'

app.controller 'asset/view', [
  '$scope', 'constantService', 'asset',
  ($scope, constants, asset) ->
    $scope.asset = asset
    $scope.volume = asset.volume
    $scope.hasThumbnail = (asset.format.type == 'image' || asset.format.type == 'video' && asset.duration && !asset.pending)
  ]
