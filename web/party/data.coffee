'use strict'

app.directive 'partyData', [
  () ->
    restrict: 'E'
    templateUrl: 'party/data.html'
    scope: false
    link: ($scope) ->
      $scope.volumes = _.pluck $scope.party.volumes, 'volume'
      return
]
