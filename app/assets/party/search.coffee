'use strict'

app.controller 'party/search', [
  '$scope', '$filter', 'displayService', 'parties',
  ($scope, $filter, display, parties) ->
    display.title = 'Users'
    $scope.parties = parties
    return
]
