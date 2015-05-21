'use strict'

app.controller 'asset/zipView', [
  '$scope', 'displayService', 'asset','slot'
  ($scope, display, asset, slot) ->
    $scope.close = ->
      window.history.back()
    $scope.asset = $scope.slot = slot
    console.log $scope.asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
