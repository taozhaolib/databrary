'use strict'

app.directive 'pdfViewer', [
  () ->
    restrict: 'E'
    scope:
      asset:  '=asset'
    link: ($scope, $elem) ->
      pdfElement = $($elem)[0]
      objectTag = document.createElement('object')
      objectTag.setAttribute('data', $scope.asset.downloadRoute(true))
      pdfElement.appendChild(objectTag)
      return 

]
