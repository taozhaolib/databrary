'use strict';

module.controller('party/edit', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    page.display.title = page.constants.message('party.edit');

    page.display.toolbarLinks.push({
      type: 'yellow',
      html: page.constants.message('party.view'),
      url: party.route,
    });

    $scope.registerStep = function (step) {
      step.form = step.$scope['partyEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    function leavingSoSoon() {
      return !$scope.activeStep || $scope.activeStep.form.resetAll(false, true);
    }

    $scope.switchStep = function (step) {
      if (!leavingSoSoon())
        return false;

      //to avoid bug where "float" elements fixed to top of page at lower scrolls are already at top
      if (step.form.scrollFn)
        page.$timeout(step.form.scrollFn);

      return true;
    };

    var done = page.$rootScope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.contains(party.editRoute()))
        return;
      if (!leavingSoSoon())
        return event.preventDefault();
      done();
    });

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
        step.complete = step.form.$pristine;
        step.error = step.form.$invalid;
      });
    });
  }
]);
