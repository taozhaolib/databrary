'use strict'

app.controller 'asset/zipView', [
  '$scope', 'displayService', 'asset',
  ($scope, display, asset, slot) ->
    $scope.close = ->
      window.history.back()
    $scope.asset = slot 
    console.log $scope.asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
