'use strict';

app.controller('site/search', [
  '$scope', '$location', 'constantService', 'displayService', 'volumes',
  function ($scope, $location, constants, display, volumes, page) {
    $scope.volumes = volumes;
    display.title = constants.message('search.title');
    $scope.query = $location.search().query;
  }
]);
