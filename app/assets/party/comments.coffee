'use strict'

module.controller('party/comments', [
  '$scope', 'pageService', '$sanitize',
  ($scope, page, $sanitize) ->
    $scope.refreshPanel = ->
      $scope.comments = $scope.party.comments
      $scope.enabled = !$.isEmptyObject($scope.comments)
])
