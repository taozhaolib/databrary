'use strict'

app.controller('party/comments', [
  '$scope',
  ($scope) ->
    $scope.refreshPanel = ->
      $scope.comments = $scope.party.comments
      $scope.enabled = !$.isEmptyObject($scope.comments)
      return
    return
])
