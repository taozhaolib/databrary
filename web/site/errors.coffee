'use strict'

app.directive('errors', [
  'pageService',
  (page) ->
    restrict: 'E'
    scope: {}
    templateUrl: 'site/errors.html'
    link: ($scope) ->
      $scope.page = page
      $scope.$on 'displayService-error', (event, error) ->
        page.display.title = page.constants.message('error.title')
        error.message ?= page.constants.message(
            if (msg = 'error.r' + error.status) of page.constants.messages
              msg
            else
              'error.resolve'
          , {sce:page.$sce.HTML})
        $scope.error = error
        return
      return
])
