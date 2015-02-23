'use strict';

app.controller('volume/view', [
  '$scope', 'volume', 'pageService',
  function ($scope, volume, page) {
    $scope.volume = volume;
    page.display.title = volume.name;
  }
]);
