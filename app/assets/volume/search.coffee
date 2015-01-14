'use strict'

app.controller 'volume/search', [
  '$scope', '$location', 'constantService', 'displayService', 'volumes',
  ($scope, $location, constants, display, volumes) ->
    display.title = 'Search'
    $scope.volumes = volumes
    return
]
