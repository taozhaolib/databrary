'use strict';

app.controller('party/edit', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    page.display.title = page.constants.message('party.edit');

    $scope.registerStep = function (step) {
      step.form = step.$scope['partyEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    function leavingSoSoon() {
      return !$scope.activeStep || $scope.activeStep.form.resetAll(false, true);
    }

    $scope.switchStep = leavingSoSoon;

    $scope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.includes(party.editRoute()))
        return;
      if (!leavingSoSoon())
        return page.display.cancelRouteChange(event);
    });

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
        step.complete = step.form.$pristine;
        step.error = step.form.$invalid;
      });
    });
  }
]);
