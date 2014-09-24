'use strict';

module.controller('site/search', [
  '$scope', 'volumes', 'pageService', function ($scope, volumes, page) {
    $scope.volumes = volumes;
    page.display.title = page.constants.message('page.title.search');
  }
]);
