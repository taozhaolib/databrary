'use strict'

app.directive 'searchForm', [
  '$location', 'constantService', 'routerService',
  ($location, constants, routes) ->
    restrict: 'E'
    templateUrl: 'site/search.html'
    scope: {}
    link: ($scope, $element, $attrs) ->
      $scope.types =
        volume: 'Data'
        principal: 'Authorized Investigators'
        affiliate: 'Affiliate Investigators'
        institution: 'Authorized Institutions'
        party: 'All users and groups'
      params = $location.search()
      $scope.type = $attrs.type
      if $scope.type == 'party'
        if params.institution == 'true'
          $scope.type = 'institution'
        else if `params.access == constants.permission.EDIT`
          $scope.type = 'principal'
        else if `params.access == constants.permission.READ`
          $scope.type = 'affiliate'
      $scope.query = params.query
      $scope.search = ->
        (switch $scope.type
          when 'volume'
            $location.url(routes.volumeSearch())
          when 'institution'
            $location.url(routes.partySearch()).search({institution:'true',access:constants.permission.ADMIN})
          when 'principal'
            $location.url(routes.partySearch()).search({institution:'false',access:constants.permission.EDIT})
          when 'affiliate'
            $location.url(routes.partySearch()).search({institution:'false',access:constants.permission.READ})
          when 'party'
            $location.url(routes.partySearch())
          ).search('query', $scope.query)
        return
      return
]
