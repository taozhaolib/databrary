'use strict'

app.directive 'inputDate', [
  () ->
    restrict: 'A'
    link: ($scope, $element) ->
      $element[0].type = 'date'
      return
]
