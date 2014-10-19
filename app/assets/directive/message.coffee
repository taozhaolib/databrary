'use strict'

app.directive 'message', [
  'constantService', '$sce',
  (constants, $sce) ->
    restrict: 'A'
    compile: ($scope, $element, $attrs) ->
      opts = {}
      opts.sce = $sce.HTML if 'messageHtml' in $attrs
      if $attrs.message.contains('{{')
        ($scope, $element, $attrs) ->
          $element[0].textContent = constants.message $attrs.message, opts
      else
        $element[0].textContent = constants.message $attrs.message, opts
        undefined
]
