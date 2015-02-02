'use strict'

app.controller 'party/search', [
  '$scope', '$location', 'displayService', 'parties',
  ($scope, $location, display, parties) ->
    limit = 24 # server-side default
    offset = $location.search().offset ? 0
    display.title = 'Users'
    $scope.parties = parties
    $scope.page = 1 + (offset / limit)
    if parties.length > limit
      $scope.next = -> $location.search('offset', limit + offset)
      $scope.parties.pop()
    if offset > 0
      $scope.prev = -> $location.search('offset', Math.max(0, offset - limit))
    return
]
