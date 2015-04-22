'use strict'

app.directive 'classificationSelect', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'asset/classificationSelect.html'
    scope:
      value: '=ngModel'
      name: '@'
    link: ($scope) ->
      $scope.classification = constants.release.slice(0)
      $scope.max = constants.release.PUBLIC
      $scope.check = _.map constants.release, (l, i) -> $scope.value <= i
      $scope.update = () ->
        $scope.value = $scope.check.indexOf(true)
      return
]
