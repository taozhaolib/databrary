'use strict'

app.directive 'inputDate', [
  () ->
    restrict: 'A'
    link: ($scope, $element, $attrs) ->
      $element[0].type = 'date'
]
