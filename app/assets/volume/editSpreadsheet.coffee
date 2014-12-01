'use strict'

app.controller('volume/spreadsheet', [
  '$scope', 'volume', 'pageService',
  ($scope, volume, page) ->
    $scope.volume = volume
    page.display.title = volume.title

    page.display.toolbarLinks.push
      type: 'yellow'
      html: page.constants.message('volume.edit.exit')
      url: volume.route()

    return
])
