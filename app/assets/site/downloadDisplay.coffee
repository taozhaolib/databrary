'use strict'

app.directive 'downloadDisplay', [
  'constantService','pageService'
  (constants,page) ->
    restrict: 'E'
    templateUrl: 'site/downloadDisplay.html'
    scope:
      assetFn: '&asset'
    link: ($scope) ->
      $scope.page = page
      $scope.current = asset = $scope.assetFn()
      $scope.volume = asset.volume
      $scope.classification = page.constants.classification[asset.classification]
      $scope.hasThumbnail = asset.checkPermission(constants.permission.READ) && (asset.format.type == 'image' || asset.format.type == 'video' && asset.asset.duration)
      
      $scope.closeDownload = () ->
        console.log 'meep'

  ]