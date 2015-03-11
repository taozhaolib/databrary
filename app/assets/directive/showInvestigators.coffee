'use strict'

app.directive 'showInvestigators', [
  '$window',
  (window) ->
    restrict: 'E'
    scope:
        volume: '=volume'
        permission: '=permission'
        title: '=title'
        page: '=page'
    transclude: true,
    templateUrl: 'directive/showInvestigators.html',
    link: ($scope, $element) ->
      # 0 and 1000 have no significance except that 1000 is bigger than all the permissions
      # and 0 is smaller than all of them. 
      maxPerm = $scope.permission.max ? 1000
      minPerm = $scope.permission.min ? 0
      $scope.investigators = $scope.volume.access.filter (i) ->
        minPerm <= i.individual < maxPerm 

]
