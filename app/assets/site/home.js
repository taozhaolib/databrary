'use strict';

app.controller('site/home', [
  '$scope', '$filter', 'constantService', 'displayService', 'volume', 'tags', 'activity',
  function ($scope, $filter, constants, display, volume, tags, activity) {
    display.title = constants.message('welcome.title');
    $scope.volume = volume;
    $scope.tags = tags;
    $scope.activity = activity;
  }
]);
