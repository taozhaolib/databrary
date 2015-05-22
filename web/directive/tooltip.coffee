'use strict'

app.directive 'tooltip', [
  'tooltipService',
  (tooltips) ->
    restrict: 'A'
    scope: false
    link: ($scope, $element, $attrs) ->
      tooltip = {}

      if 'tooltipId' of $attrs
        tooltip.id = $attrs.tooltipId
      if 'tooltipClass' of $attrs
        tooltip.cls = $attrs.tooltipClass
      tooltip.message = $scope.$eval($attrs.tooltip)
      tooltip.$target = $element

      if tooltip.message
        new tooltips(tooltip)

      return
]
