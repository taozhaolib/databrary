'use strict';

app.controller('volume/view', [
  '$scope', 'volume', 'pageService',
  function ($scope, volume, page) {
    $scope.volume = volume;

    $scope.volumeType = volume.citation ? "study" : "volume";
    $scope.volumeMessage = function (msg /*, args...*/) {
      arguments[0] = ((($scope.volumeType + "." + msg) in page.constants.messages) ? $scope.volumeType : "volume") + "." + msg;
      return page.constants.message.apply(this, arguments);
    };

    page.display.title = volume.name;
    if (volume.checkPermission(page.permission.EDIT))
      page.display.toolbarLinks.push({
        type: 'yellow',
        html: page.constants.message('volume.edit'),
        url: volume.editRoute(),
      });
  }
]);
