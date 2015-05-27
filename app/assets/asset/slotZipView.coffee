'use strict'

app.controller 'asset/slotZipView', [
  '$scope', 'displayService', 'slot', 'asset', 'routerService'
  ($scope, display, slot, asset,  router) ->
    $scope.close = ->
      window.history.back()
    console.log "router.controllers", router.controllers
    console.log "asset",asset
    console.log "slot",slot.volume.zip
    router.http(router.controllers.SlotApi.zip slot.volume.id, slot.id).then (list) ->
      console.log list
    $scope.asset = $scope.slot = slot
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
