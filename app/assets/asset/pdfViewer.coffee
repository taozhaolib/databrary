'use strict'

app.directive 'pdfViewer', [
  () ->
    restrict: 'E'
    scope:
      asset:  '=asset'
    link: ($scope, $elem) ->
      pdfElement = $elem[0]
      objectTag = document.createElement('object')
      objectTag.setAttribute('data', $scope.asset.downloadRoute(true))
      objectTag.setAttribute('width', '80%')
      objectTag.setAttribute('type', $scope.asset.format.mimetype)
      pdfElement.appendChild(objectTag)
      return 

]
