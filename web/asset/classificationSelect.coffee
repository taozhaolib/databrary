'use strict'

app.directive 'classificationSelect', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'asset/classificationSelect.html'
    scope:
      value: '=ngModel'
      name: '@'
      defaultFn: '&default'
    link: ($scope) ->
      def = ($scope.defaultFn() || 1)+''
      $scope.releases = constants.release.slice(1).map (l) ->
        constants.message('release.'+l+'.title') + ': ' + constants.message('release.'+l+'.select')
      $scope.form = Object.defineProperties {},
        check:
          get: ->
            `$scope.value != '0'`
          set: (c) ->
            $scope.value = if c then def else '0'
            return
        value:
          get: ->
            ~~$scope.value-1+''
          set: (v) ->
            $scope.value = ++v+''
            return
      return
]
