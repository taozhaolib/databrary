'use strict'

app.directive 'mouseDrag', [
  '$parse',
  ($parse) ->
    restrict: 'A'
    compile: ($element, $attrs) ->
      action = $parse($attrs.mouseDrag)
      ($scope, $element) ->
        $element.on 'mousedown', (down) ->
          startTime = down.timeStamp
          mouseup = (up) ->
            final = up.type != 'mousemove'
            $element.off 'mouseleave mouseup mousemove', mouseup if final
            # maybe should also count enough movement as a drag, even under time thresh
            if startTime != undefined
              return if final || up.timeStamp - startTime < 250000
              startTime = undefined
            $scope.$apply () ->
              action $scope, {$down:down, $up:up}
          $element.on 'mouseleave mouseup mousemove', mouseup
]
