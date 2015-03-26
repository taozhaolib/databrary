'use strict';

app.controller('volume/edit', [
  '$scope', 'constantService', 'displayService', 'routerService', 'modelService', 'Store', 'volume',
  function ($scope, constants, display, router, models, Store, volume) {
    $scope.flowOptions = Store.flowOptions;
    $scope.volume = volume;
    display.title = volume ? volume.title : constants.message('volume.edit.create');

    if (!volume) {
      $scope.owners = _.chain(models.Login.user.parents
                             ).filter(function (p) {
                               return p.member >= constants.permission.ADMIN && p.party.access >= constants.permission.EDIT;
                             }).map(function (p) {
                               return p.party;
                             }).value();
      if (models.Login.user.access >= constants.permission.EDIT)
        $scope.owners.unshift(models.Login.user);
    }

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
      _.each($scope.steps, function (step) {
        step.complete = !step.form || step.form.$pristine;
        step.error = step.form && step.form.$invalid;
      });
    });
  }
]);
