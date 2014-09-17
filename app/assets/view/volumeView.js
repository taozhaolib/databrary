'use strict';

module.controller('volumeView', [
  '$scope', 'volume', 'pageService', function ($scope, volume, page) {
    $scope.volume = volume;

    $scope.volumeType = volume.citation ? "study" : "volume";
    $scope.volumeMessage = function (msg /*, args...*/) {
      arguments[0] = ((($scope.volumeType + "." + msg) in page.constants.messages) ? $scope.volumeType : "volume") + "." + msg;
      return page.constants.message.apply(this, arguments);
    };

    $scope.viewClass = function () {
      return [volume.type];
    };

    page.display.title = volume.name;
    if (page.models.Login.checkAccess(page.permission.EDIT, volume))
      page.display.toolbarLinks.push({
        type: 'yellow',
        html: page.constants.message('volume.edit'),
        url: volume.editRoute(),
      });

    page.browser.initialize(volume);
  }
]);
