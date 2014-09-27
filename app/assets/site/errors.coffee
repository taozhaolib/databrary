'use strict'

module.directive('errors', [
  'pageService',
  (page) ->
    restrict: 'E'
    scope: {}
    templateUrl: 'site/errors.html'
    link: ($scope) ->
      $scope.page = page
      $scope.$on 'displayService-error', (event, error) ->
        page.display.title = page.constants.message('error.title')
        $scope.error = error
])
