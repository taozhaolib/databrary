'use strict';

app.controller('volume/edit', [
  '$scope', 'volume', 'pageService', 'Store',
  function ($scope, volume, page, Store) {
    $scope.flowOptions = Store.flowOptions;
    $scope.volume = volume;
    page.display.title = volume ? volume.title : page.constants.message('volume.edit.create');

    if (volume)
      page.display.toolbarLinks.push({
        type: 'yellow',
        html: page.constants.message('volume.edit.exit'),
        url: volume.route()
      });

    $scope.registerStep = function (step) {
      step.form = step.$scope['volumeEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    function leavingSoSoon() {
      return !$scope.activeStep || !$scope.activeStep.form || $scope.activeStep.form.resetAll(false, true);
    }

    $scope.switchStep = leavingSoSoon;

    $scope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.includes(volume ? volume.editRoute() : page.router.volumeCreate()))
        return;
      if (!leavingSoSoon())
        return page.display.cancelRouteChange(event); 
    });

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
        step.complete = !step.form || step.form.$pristine;
        step.error = step.form && step.form.$invalid;
      });
    });
  }
]);
