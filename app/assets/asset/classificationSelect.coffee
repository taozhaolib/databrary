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
      $scope.classifications = constants.classification.slice(1)
      $scope.selects = (($scope.value > i)+'' for l, i in $scope.classifications)
      $scope.update = () ->
        for l, i in $scope.classifications
          break if $scope.selects[i] == 'false'
        $scope.value = i
]
