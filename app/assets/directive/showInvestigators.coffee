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
        maxperm: '=maxperm'
        minperm: '=minperm'
    transclude: true,
    templateUrl: 'directive/showInvestigators.html',
    link: ($scope, $element) ->
      maxPerm = $scope.maxperm ? 1000
      minPerm = $scope.minperm ? 0
      $scope.investigators = $scope.volume.access.filter (i) ->
        console.log "The Permission: ", i.individual
        console.log "Scope Permission", $scope.minperm
        minPerm <= i.individual < maxPerm 
        
      console.log "Investigators: ", $scope.investigators
      console.log "Access", $scope.volume.access
]
