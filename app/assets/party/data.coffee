'use strict'

app.directive 'partyData', [
  () ->
    restrict: 'E'
    templateUrl: 'party/data.html'
    scope: false
    link: ($scope) ->
      $scope.volumes = (va.volume for va in $scope.party.volumes)
      return
]
