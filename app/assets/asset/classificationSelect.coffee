'use strict'

app.directive 'classificationSelect', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'asset/classificationSelect.html'
    scope:
      value: '=ngModel'
      name: '@'
    link:
      pre: ($scope) ->
        $scope.release = constants.release
        return
      post: ($scope) ->
        $scope.form = form =
          check: `$scope.value == '0'`
          value: parseInt($scope.value,10)-1+''
        $scope.update = () ->
          $scope.value = if form.check then '0' else parseInt(form.value,10)+1+''
        return
]
