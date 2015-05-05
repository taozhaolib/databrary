'use strict'

app.directive 'pdfViewer', [
  () ->
    restrict: 'E'
    transclude: true
    scope:
      asset:  '=asset'
    link: ($scope, $elem, $attrs,_Controller, $transclude) ->
      pdfElement = $elem[0]
      objectTag = document.createElement('object')
      objectTag.setAttribute('data', $scope.asset.downloadRoute(true))
      objectTag.setAttribute('width', '80%')
      objectTag.setAttribute('type', $scope.asset.format.mimetype)
      $transclude (elem) ->
        objectTag.appendChild elem[0]
      pdfElement.appendChild(objectTag)
      return 

]
