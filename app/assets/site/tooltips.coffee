'use strict'

app.directive 'tooltips', [
  'tooltipService',
  (tooltips) ->
    restrict: 'E'
    scope: {}
    templateUrl: 'site/tooltips.html'
    controller: () ->
    link: ($scope) ->
      $scope.tooltips = tooltips.list
]
