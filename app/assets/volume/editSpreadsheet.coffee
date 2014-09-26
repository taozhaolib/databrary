'use strict'

module.controller('volume/spreadsheet', [
  '$scope', 'volume', 'pageService',
  ($scope, volume, page) ->
    $scope.volume = volume
    page.display.title = volume.title
])
