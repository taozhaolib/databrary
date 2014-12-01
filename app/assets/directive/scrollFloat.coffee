'use strict'

app.directive 'scrollFloat', [
  '$window',
  (window) ->
    restrict: 'EA'
    transclude: true,
    templateUrl: 'directive/scrollFloat.html',
    link: ($scope, $element) ->
      floater = $element[0]
      scroll = ->
        box = floater.getBoundingClientRect()
        if box.top < 36
          floater.style['min-height'] = box.height + 'px'
          floater.firstChild.classList.add 'scroll-float'
        else
          delete floater.style['min-height']
          floater.firstChild.classList.remove 'scroll-float'
      window.addEventListener 'scroll', scroll
      $scope.$on '$destroy', ->
        window.removeEventListener 'scroll', scroll
      return
]
