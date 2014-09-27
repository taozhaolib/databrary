'use strict';

module.controller('asset/formats', [
  '$scope', 'pageService', function ($scope, page) {
    page.display.title = page.constants.message('help.formats.title');

    $scope.groups = {};
    angular.forEach(page.constants.format, function (format) {
      var general = format.mimetype.split('/').shift();

      if (!$scope.groups.hasOwnProperty(general)) {
        $scope.groups[general] = [];
      }

      $scope.groups[general].push({
        format: format,
        description: format.name == 'Image' ? 'Joint Photographic Experts Group' :
            format.name == 'Video' ? 'Moving Picture Expert Group-4' : format.name,
      });
    });
  }
]);
