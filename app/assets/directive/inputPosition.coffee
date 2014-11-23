'use strict'

app.directive 'inputPosition', [
  'Offset',
  (Offset) ->
    restrict: 'A'
    require: 'ngModel'
    link: ($scope, $element, $attrs, ngModel) ->
      blank = if 'inputPosition' of $attrs then parseFloat($attrs.inputPosition) else null
      ngModel.$parsers.push (value) ->
        return blank if value == ''
        value = Offset.parse(value)
        if isFinite(value) then value
      ngModel.$formatters.push (value) ->
        return '' if value == blank
        Offset.format(value)
]
