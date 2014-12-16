'use strict'

app.directive 'scrollFloat', [
  '$window',
  (window) ->
    restrict: 'E'
    transclude: true,
    templateUrl: 'directive/scrollFloat.html',
    link: ($scope, $element) ->
      floater = $element[0].firstChild
      while floater && floater.tagName != 'DIV'
        floater = floater.nextSibling

      scroll = ->
        box = floater.getBoundingClientRect()
        $scope.scrollFloating = box.height && box.top < 36
      window.addEventListener 'scroll', $scope.$lift(scroll)
      $scope.$on '$destroy', ->
        window.removeEventListener 'scroll', scroll
      return
]
