'use strict';

app.controller('volume/edit', [
  '$scope', 'constantService', 'displayService', 'routerService', 'Store', 'volume',
  function ($scope, constants, display, router, Store, volume) {
    $scope.flowOptions = Store.flowOptions;
    $scope.volume = volume;
    display.title = volume ? volume.title : constants.message('volume.edit.create');

    $scope.registerStep = function (step) {
      step.form = step.$scope['volumeEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    function leavingSoSoon() {
      return !$scope.activeStep || !$scope.activeStep.form || $scope.activeStep.form.resetAll(false, true);
    }

    $scope.switchStep = leavingSoSoon;

    $scope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.includes(volume ? volume.editRoute() : router.volumeCreate()))
        return;
      if (!leavingSoSoon())
        return display.cancelRouteChange(event); 
    });

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
        step.complete = !step.form || step.form.$pristine;
        step.error = step.form && step.form.$invalid;
      });
    });
  }
]);
