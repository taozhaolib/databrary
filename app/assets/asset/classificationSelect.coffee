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
        $scope.form = Object.defineProperties {},
          check:
            get: ->
              `$scope.value == '0'`
            set: (c) ->
              $scope.value = if c then '0' else ''
          value:
            get: ->
              ~~$scope.value-1+''
            set: (v) ->
              $scope.value = ++v+''
        return
]
