'use strict'

app.directive 'showInvestigators', [
  'constantService',
  (constants) ->
    restrict: 'E'
    scope: true
    # scope:
    #     volume: '=volume'
    #     permission: '=permission'
    #     title: '=title'
    transclude: true,
    templateUrl: 'volume/showInvestigators.html',
    link: ($scope, $element, $attrs) ->
      $scope.title = $attrs.title
      
      # 0 and 1000 have no significance except that 1000 is bigger than all the permissions
      # and 0 is smaller than all of them.

      maxPerm = constants.permission[$attrs.maxlevel] ? constants.permission.length
      minPerm = constants.permission[$attrs.minlevel] ? 0
      $scope.investigators = $scope.volume.access.filter (i) ->
        minPerm <= i.individual < maxPerm 

]
