'use strict'

app.directive 'tooltip', [
  '$window', '$timeout',
  (window, $timeout) ->
    restrict: 'E'
    scope: false
    require: '^tooltips',
    templateUrl: 'site/tooltip.html'
    link: ($scope, $element) ->
      tooltip = $scope.tooltip
      $scope.classes = [tooltip.cls]
      $scope.style = {}

      $timeout ->
        doc = window.document.documentElement
        box = tooltip.target.getBoundingClientRect()
        tip = $element.children()

        ww = doc.clientWidth
        wx = (window.pageXOffset || doc.scrollLeft) - (doc.clientLeft || 0)
        center = box.left + box.width/2
        if 2*center > ww
          $scope.style.left = (center + wx - tip.outerWidth() + 32) + 'px'
          $scope.classes.push('tooltip-left')
        else
          $scope.style.left = (center + wx - 32) + 'px'
          $scope.classes.push('tooltip-right')

        wh = doc.clientHeight
        wy = (window.pageYOffset || doc.scrollTop) - (doc.clientTop || 0)
        bottom = wh - box.bottom
        if box.top > bottom
          $scope.style.top = (box.top + wy - tip.outerHeight() - 8) + 'px'
          $scope.classes.push('tooltip-top')
        else
          $scope.style.top = (box.bottom + wy + 8) + 'px'
          $scope.classes.push('tooltip-bottom')

        $scope.classes.push('tooltip-visible')
]
