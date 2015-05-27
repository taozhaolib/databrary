'use strict'

app.controller 'asset/zipView', [
  '$scope', 'displayService', 'asset','slot', 'volume'
  ($scope, display, asset, volume) ->
    $scope.close = ->
      window.history.back()
    $scope.asset = $scope.volume = volume
    console.log $scope.asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
